devtools::load_all(".")
devtools::use_dev_version()
if (FALSE) {
packager::provide_cran_comments(check_log = "log/check.Rout", travis_session_info = "travis-cli")
    set_package_info(path = ".", title = "Helps Me Build Packages", 
                     description = paste("Helper functions for a build system,",
                                         "heavily borrowing from and extending", 
                                         "package `devtools 1.13.3`."),
                     details = paste("I find devtools very helpful but it lacks",
                                     "some functionality I would want while", 
                                     "creating and building a package.\n", 
                                     "Maybe \\pkg{packager} should be two packages."),
                     force = is_force()
                     )
}


packager::check_codetags()
# make
ml <- provide_make_list()

fakemake::make(".log.Rout", ml)
fakemake::visualize(ml)


packager::create("../cuwintest")


print_lints <- function(x, sort = TRUE, invert = FALSE,
                 file_name_markers = c("_internals", "_verbatim", "_modified")
                 ) {

    mark <- "COPY:"
    nomark <- "NO COPY:"
    pattern <- paste0("(", 
                      paste0("^.*",file_name_markers , ".*\\.R$", 
                             collapse = "|"),
                      ")")
    set_mark <- function(x) {
        if (grepl(pattern, x$filename)) 
            x$mark <- mark
        else
            x$mark <- nomark
        return(x)
    }
    x <- lapply(x, function(x) {set_mark(x)})


    fill_with <- function (character = " ", length = 1L) {
        paste0(collapse = "", rep.int(character, length))
    }
    highlight_string <- function (message, column_number = NULL, ranges = NULL) {
        maximum <- max(column_number, unlist(ranges))
        line <- fill_with(" ", maximum)
        lapply(ranges, function(range) {
                   substr(line, range[1], range[2]) <<- fill_with("~", range[2] -
                                                                  range[1] + 1L)
                      })
        substr(line, column_number, column_number + 1L) <- "^"
        line
    }
    if (isTRUE(sort)) {
        marked <- sapply(x, function(x) return(x[["mark"]] == mark))
        if (isTRUE(invert))
            x <- c(x[marked], x[! marked])
        else
            x <- c(x[! marked], x[marked])

    }
    print_lint <- function (x) {
        color <- switch(x$type, warning = crayon::magenta, error = crayon::red,
                        style = crayon::blue, crayon::bold)
        cat(sep = "", crayon::bold(x$mark, " ", x$filename, ":", as.character(x$line_number),
                                   ":", as.character(x$column_number), ": ", sep = ""),
            color(x$type, ": ", sep = ""), crayon::bold(x$message),
            "\n", x$line, "\n", highlight_string(x$message, x$column_number,
                                                 x$ranges), "\n")
        invisible(x)
    }

    if (has_lints <- length(x) > 0) invisible(lapply(x, print_lint))
    return(invisible(x))
}
    lints <- lintr::lint_package(path = path)
print_lints(lints, invert = TRUE)
