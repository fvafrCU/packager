info <- function(session = NULL) {
    if (is.null(session)) 
        session <- utils::sessionInfo()
    if (identical(class(session), "sessionInfo"))
        warning("Argument session is not of class `sessionInfo`!")
    info <- c(session[["R.version"]][["version.string"]],
              paste0("Platform: ", session[["platform"]]),
              paste0("Running under: ", session[["running"]]))
    return(info)
}

# get_travis_info(travis_session_info = system.file("files", "travis_log.txt", package = "packager"))
get_travis_info <- function(travis_session_info = NULL, path = ".") {
    if (! is.null(travis_session_info)) {
        if (identical(travis_session_info, "travis-cli")) {
            info <- tryCatch(travis_cli(path),
                             error = function(e) return(NULL))
        } else { 
            if (file.exists(travis_session_info)) {
                travis_log <- readLines(travis_session_info)
                k <- grep("sessionInfo()", travis_log)
                info <- travis_log[(k + 1): (k + 3)]
                info <- c(info, grep("^Status", travis_log, value = TRUE))
            } else {
                throw(paste0(travis_session_info, 
                             "is neither the string `travis-cli` nor a path ", 
                             "to existing file."))
            }
        }
        info <- unlist(strsplit(info, "\n"))
    } else {
        info <- NULL
    }
    return(info)
}
#' Provide a Template for Your Comments To CRAN
#'
#' Devtools' \code{\link{release}} reads a file \emph{cran-comments.md}. This
#' function provides a template based on your R version your R CMD check log and
#' the package's NEWS.md.
#' @param path The path to the package.
#' @param initial Is this an initial release?
#' @param check_log Path to the check log relative to \code{path}. Typically
#' file.path("log", "check.Rout").
#' @param travis_session_info Travis session info, search for\cr
#' "$ Rscript -e 'sessionInfo()'" \cr
#' in the raw log of the travis build and copy the
#' following three lines. This could read\cr
#' travis_session_info <- c("\cr
#'                     R version 3.4.0 (2017-04-21)\cr
#'                     Platform: x86_64-pc-linux-gnu (64-bit)\cr
#'                     Running under: Ubuntu precise (12.04.5 LTS) \cr
#'                     ")\cr
#' Or provide a path to a file containing the current travis log.\cr
#' Set to `travis-cli` to retrieve Session info automatically if your system is
#' set up to use \url{https://github.com/travis-ci/travis.rb}.
#' @param name The name to sign with, if NA, the given name of the package
#' maintainer as stated in file DESCRIPTION is used.
#' @write_to_file Do write the comment to \emph{cran-comment.md}?
#' @note By default this function writes to disk as side effect.
#' @return Character vector containing the cran comments, which are written to
#' cran-comments.md (see Note).
#' @export
#' @examples
#' cat(provide_cran_comments(check_log = system.file("files", "check.Rout", 
#'                                                     package = "packager"),
#'                             travis_session_info = system.file("files", 
#'                                                               "travis.log", 
#'                                                               package =
#'                                                                   "packager"),
#'                             write_to_file = FALSE),
#'     sep = "")
provide_cran_comments <- function(check_log = NULL,
                                  path = ".",
                                  initial = FALSE,
                                  travis_session_info = NULL,
                                  write_to_file = TRUE,
                                  name = NA) {
    if (is.na(name)) {
        name <- tryCatch({
            maintainer <- desc::desc_get_author(role = "cre", file = path)
            # NOTE: a person object is a strange thing, we seem to unclass() it,
            # see https://stackoverflow.com/questions/9765493/
            #      how-do-i-reference-specific-tags-in-the-bibentry
            #      -class-using-the-or-conv
            paste(getElement(unclass(maintainer)[[1]], "given"), collapse = " ")
        },
        error = function(e) return(name)
        )
    }
    pkg <- devtools::as.package(path)
    comments_file <- file.path(pkg[["path"]], "cran-comments.md")
    here <- info(session = NULL)
    if (! is.null(check_log)) {
        check <- parse_check_results(check_log)
        check_output <- utils::capture.output(print.check_results(check),
                                              type = "message")[2]
    } else {
        check_output  <- "ERROR: No check log given!"
    }
    here <- c(here, check_output)
    if (file.exists(file.path(pkg[["path"]], "NEWS.md"))) {
        news <- get_news(path = path)
    } else {
        news <- "\nXXX: State your changes and consider using a NEWS.md\n\n"
    }
    comments <- c("Dear CRAN Team,\n")
    if (isTRUE(initial)) {
        comments <- c(comments, "this is the initial commit of package '",
                      pkg$package, "'.\n\n", "XXX: Describe what it does.\n\n",
                      "Please consider uploading it to CRAN.\n")

    } else {
        comments <- c(comments, "this is a resubmission of package '",
                      pkg$package, "'. I have added the following changes:\n",
                      news,
                      "Please upload to CRAN.\n")
    }
    comments <- c(comments, "Best, ", name, "\n\n")
    comments <- c(comments, "# Package ", pkg$package, " ",
                  pkg$version, "\n## Test  environments ", "\n")
    comments <- c(comments, "- ", paste(here, collapse = "\n    "), "\n")
    travis_info <- get_travis_info(travis_session_info, path) 
    if (!is.null(travis_info)) 
        comments <- c(comments, "- ", paste(travis_info, collapse = "\n    "), "\n")
    comments <- c(comments, "- win-builder (devel)", "\n")
    if (! as.logical(file.access(".", mode = 2))) # see ?file.acces return/note
        if (isTRUE(write_to_file))
            writeLines(comments, con = comments_file, sep = "")
    return(invisible(comments))
}

