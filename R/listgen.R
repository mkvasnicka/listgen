# various hard-coded names
name_page <- function() {
    "Strana"
}

name_seminar <- function() {
    stringi::stri_unescape_unicode("Semin\\u00e1\\u0159")
}

name_uco <- function() {
    stringi::stri_unescape_unicode("U\\u010CO")
}

name_last_name <- function() {
    stringi::stri_unescape_unicode("P\\u0159\\u00edjmen\\u00ed")
}

name_first_name <- function() {
    stringi::stri_unescape_unicode("Jm\\u00e9no")
}

name_points <- function() {
    "b."
}


# output TeX header, footer, new page, etc.
tex_header <- function() {
    "
    \\documentclass[a4paper,12pt]{article}
    \\usepackage[utf8]{inputenc}
    % \\usepackage[czech]{babel}
    \\usepackage{mathptmx}
    \\usepackage[left=10mm, right=10mm, top=20mm, bottom=20mm]{geometry}
    \\usepackage{graphicx}
    \\usepackage[breakwords]{truncate}
    \\renewcommand{\\arraystretch}{1.5}
    \\usepackage{fancyhdr}
    \\usepackage[table]{xcolor}
    \\pagestyle{fancy}
    \\lhead{}
    \\rhead{}
    \\cfoot{}
    \\lfoot{\\seminar}
    \\rfoot{\\bf PAGE~\\thepage}
    \\def\\firstname#1{\\truncate{25mm}{#1}}
    \\def\\lastname#1{\\truncate{35mm}{#1}}
    \\begin{document}
    " |>
        stringr::str_replace_all("PAGE", name_page()) |>
        stringr::str_remove_all(
            stringr::regex("^\\s+", multiline = TRUE)
        ) |>
        stringr::str_split("\n") |>
        unlist() |>
        stringr::str_subset("^.+$")
}

tex_footer <- function() {
    "\\end{document}"
}

tex_newpage <- function() {
    "\\newpage"
}

tex_hline <- function() {
    "\\hline"
}


# tibble to LaTeX
latexize <- function(tab, cols_per_page) {
    tuples <- ncol(tab) / cols_per_page
    align <- "|p{12mm}|p{35mm}|p{25mm}|p{5mm}|"
    align <- stringr::str_c(rep(align, cols_per_page), collapse = "")
    head <- stringr::str_c(
        "\\hskip-1.4em",
        "\\begin{tabular}{", align, "}"
    )
    header <- stringr::str_c(
        stringr::str_c("{\\bf ", names(tab), "}", collapse = " & "),
        "\\\\"
    )
    names(tab) <- stringr::str_c("v", seq_along(names(tab)))
    fnames <- names(tab)[c(FALSE, FALSE, TRUE, FALSE)]
    lnames <- names(tab)[c(FALSE, TRUE, FALSE, FALSE)]
    body <- tab |>
        dplyr::mutate(
            dplyr::across(
                tidyselect::all_of(fnames),
                ~ stringr::str_c("\\firstname{", ., "}")
            ),
            dplyr::across(
                tidyselect::all_of(lnames),
                ~ stringr::str_c("\\lastname{", ., "}")
            )
        ) |>
        tidyr::unite(boo, tidyselect::all_of(names(tab)), sep = " & ") |>
        dplyr::pull() |>
        stringr::str_c(" \\\\", "\n", tex_hline()) |>
        stringr::str_split("\n") |>
        unlist()
    tail <- "\\end{tabular}"
    c(head, tex_hline(), header, tex_hline(), tex_hline(), body, tail)
}


# add (D) to duplicated first and last names
append_duplicated_names <- function(tab) {
    duplicated_last_names <- tab$last_name[duplicated(tab$last_name)]
    duplicated_first_names <- tab$first_name[duplicated(tab$first_name)]
    tab |>
        dplyr::mutate(
            first_name = dplyr::if_else(first_name %in% duplicated_first_names,
                stringr::str_c(first_name, " (D)"),
                first_name
            ),
            last_name = dplyr::if_else(last_name %in% duplicated_last_names,
                stringr::str_c(last_name, " (D)"),
                last_name
            )
        )
}


# note first in round
note_first_in_group <- function(tab, number_of_students, number_of_rounds) {
    first_in_group <- rep(
        c(TRUE, rep(FALSE, number_of_students - 1)),
        length.out = number_of_rounds * number_of_students
    )
    tab |>
        dplyr::mutate(
            last_name = dplyr::if_else(
                first_in_group,
                stringr::str_c("*", last_name),
                last_name
            ),
            first_name = dplyr::if_else(
                first_in_group,
                stringr::str_c("*", first_name),
                first_name
            ),
            uco = dplyr::if_else(
                first_in_group,
                stringr::str_c("*", uco),
                uco
            ),
            body = dplyr::if_else(
                first_in_group,
                stringr::str_c("*", body),
                body
            )
        )
}


# replicate and permute students
create_replications <- function(
    tab,
    replications_in_round,
    number_of_rounds,
    max_lag,
    max_iter) {
    ucos <- rep(unique(tab$uco), each = replications_in_round)
    i <- 1
    repeat {
        permuted_ucos <- purrr::map(1:number_of_rounds, ~ sample(ucos)) |>
            unlist()
        test <- purrr::map_lgl(
            1:max_lag,
            ~ any(permuted_ucos == dplyr::lag(permuted_ucos, n = .),
                na.rm = TRUE
            )
        )
        if (!any(test)) {
            break
        }
        if (i > max_iter) {
            i <- 0
            max_lag <- max_lag - 1
        }
        i <- i + 1
    }
    dplyr::left_join(
        tibble::tibble(uco = permuted_ucos),
        tab,
        by = "uco"
    ) |>
        note_first_in_group(length(ucos), number_of_rounds)
}


# split to pages
split_to_pages <- function(tab, rows_per_page, cols_per_page) {
    number_of_pages <- ceiling(nrow(tab) / (rows_per_page * cols_per_page))
    blanks <- rep(
        "",
        number_of_pages * rows_per_page * cols_per_page - nrow(tab)
    )
    tab <- dplyr::bind_rows(
        dplyr::mutate(tab, dplyr::across(everything(), as.character)),
        tibble::tibble(
            uco = blanks,
            last_name = blanks,
            first_name = blanks,
            body = blanks
        )
    )
    split(
        tab,
        rep(1:number_of_pages,
            each = (rows_per_page * cols_per_page)
        )[1:nrow(tab)]
    )
}


# process one page
one_page <- function(tab, cols_per_page) {
    colnames <- c(
        name_uco(),
        name_last_name(),
        name_first_name(),
        name_points()
    )
    x <- as.data.frame(split(
        tab,
        rep(1:cols_per_page,
            each = nrow(tab) / cols_per_page
        )
    ))
    names(x) <- rep(colnames, cols_per_page)
    lines <- latexize(x, cols_per_page)
    c(lines, tex_newpage())
}


# set header and footer
set_header_and_footer <- function(lines, seminar) {
    c(
        stringr::str_c(
            "\\def\\seminar{\\bf ",
            name_seminar(),
            "~",
            seminar,
            "}"
        ),
        lines,
        "\\setcounter{page}{1}"
    )
}


# emphasize groups of students
show_groups <- function(lines, show_groups) {
    emphasize <- if (show_groups) {
        "\\\\cellcolor[HTML]{D3D3D3}\\\\bf "
    } else {
        ""
    }
    stringr::str_replace_all(lines, "\\*", emphasize)
}


# process one seminar
process_one_seminar <- function(tab,
                                replications_in_round, number_of_rounds,
                                rows_per_page, cols_per_page,
                                max_lag,
                                max_iter,
                                show_groups) {
    no <- nrow(tab)
    ori_ucos <- sort(as.character(tab$uco))
    seminar <- tab$seminar[1]
    tab |>
        dplyr::select(uco, last_name, first_name) |>
        dplyr::mutate(
            across(everything(), as.character),
            body = ""
        ) |>
        append_duplicated_names() |>
        create_replications(
            replications_in_round,
            number_of_rounds,
            max_lag,
            max_iter
        ) |>
        split_to_pages(rows_per_page, cols_per_page) |>
        purrr::map(~ one_page(., cols_per_page)) |>
        unlist() |> 
        set_header_and_footer(seminar) |>
        show_groups(show_groups = show_groups)
}


#' Generate a permuted list of students
#'
#' @description `listgen()` generates a permuted list of students
#' for seminars based on the input data.
#'
#' @param students A data frame containing information about the students.
#' It must contain the following columns: `course`, `seminar`, `uco`,
#' `last_name`, and `first_name`. Other columns are ignored.
#' @param filename The name of the output file without extension;
#' default is "listgen".
#' @param folder The folder where the output file will be saved.
#' @param replications_in_rounds How many times should each person be
#' present in one round.
#' @param number_of_rounds How many rounds.
#' @param cols_per_page Columns per page.
#' @param rows_per_page Rows per page.
#' @param max_lag How many ucos in a row cannot be the same.
#' @param max_iter How many iterations should be performed before
#' decreasing `max_lag` by 1.
#' @param show_groups A logical value indicating whether to show groups of
#' students. Default is `TRUE`.
#' @param open A logical value indicating whether to open the output file.
#' @param open_with The program to use to open the output file.
#'
#' @return None
#'
#' @export
#'
#' @examples \dontrun{
#' students <- tibble::tribble(
#'     ~course, ~seminar, ~uco, ~last_name, ~first_name,
#'     "BPE_AAA", "01", 123456, "Aloha", "Adam",
#'     "BPE_AAA", "01", 234567, "Brown", "Betty",
#'     "BPE_AAA", "01", 345678, "Cook", "Charlie",
#'     "BPE_AAA", "01", 456789, "Delacroix", "Diana"
#' )
#' listgen(students)
#' }
listgen <- function(
    students,
    filename = "listgen",
    folder = tempdir(),
    replications_in_rounds = 1,
    number_of_rounds = 19,
    cols_per_page = 2,
    rows_per_page = 31,
    max_lag = 5,
    max_iter = 1e4,
    show_groups = TRUE,
    open = FALSE,
    open_with = "evince") {
    tex_file <- file.path(folder, stringr::str_c(filename, ".tex"))
    pdf_file <- file.path(folder, stringr::str_c(filename, ".pdf"))
    tex_file_content <- students |>
        tibble::as_tibble() |>
        dplyr::select(course, seminar, uco, last_name, first_name) |>
        dplyr::arrange(seminar, last_name, first_name) |>
        dplyr::group_by(seminar) |>
        dplyr::group_split() |>
        purrr::map(~ process_one_seminar(.,
            replications_in_round = replications_in_rounds,
            number_of_rounds = number_of_rounds,
            rows_per_page = rows_per_page,
            cols_per_page = cols_per_page,
            max_lag = max_lag,
            max_iter = max_iter,
            show_groups = show_groups
        )) |>
        unlist()
    tex_file_content <- c(tex_header(), tex_file_content, tex_footer())
    readr::write_lines(tex_file_content, tex_file)
    system(stringr::str_c(
        "pdflatex -output-directory", folder, tex_file,
        sep = " "
    ))
    if (open) {
        system(
            stringr::str_c(open_with, pdf_file, sep = " "),
            wait = FALSE
        )
    }
}
