#' Provide a Template For Your Comments To CRAN
#' 
#' Devtools' \code{\link{release}} reads a file \emph{cran-comments.md}. This
#' function provides a template based on your R version and your check log.
#' @param path The path to the package.
#' @param initial Is this an initial release?
#' @param check_log Path to the check log relative to \code{path}. Typically 
#' file.path("log", "dev_check.Rout").
#' @param travis_session_info Travis session info, search for\cr   
#' "$ Rscript -e 'sessionInfo()'" \cr 
#' in the raw log of the travis build and copy the
#' following three lines. This could read\cr 
#' travis_session_info <- c("\cr 
#'                     R version 3.4.0 (2017-04-21)\cr 
#'                     Platform: x86_64-pc-linux-gnu (64-bit)\cr 
#'                     Running under: Ubuntu precise (12.04.5 LTS) \cr 
#'                     ")\cr 
#' Set to "travis-cli" to retrieve Session info automatically if your system is 
#' set up to use \url{https://github.com/travis-ci/travis.rb}.
#' @param name The name to sign with.
#' @note This function writes to disk as side effect.
#' @return Character vector containing the cran comments, which are written to
#' cran-comments.md (see Note).
#' @export
provide_cran_comments <- function(check_log = NULL,
                                  path = ".",
                                  initial = FALSE,
                                  travis_session_info = NULL,
                                  name = "Dominik") {
    pkg <- devtools::as.package(path)
    comments_file = file.path(pkg[["path"]], "cran-comments.md")
    session <- utils::sessionInfo()
    here <- c("",
              session[["R.version"]][["version.string"]],
              paste0("Platform: ", session[["platform"]]),
              paste0("Running under: ", session[["running"]]))
    if (file.exists(file.path(pkg[["path"]], "NEWS.md"))) {
        news <- get_news(path = path)
    } else {
        news <- "\nXXX: State your changes and consider using a NEWS.md\n\n"
    }
    if (! is.null(check_log)) {
        check <- parse_check_results(file.path(path, check_log))
        check_output <- utils::capture.output(print.check_results(check), 
                                               type = "message")[2]
    } else {
        check_output  <- "ERROR: No check log given!"
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
    comments <- c(comments, "Best, ", name, "\n\n# Package ", pkg$package," ", 
                  pkg$version, "\n## Test  environments ", "\n",
                  "- ", paste(here[here != ""], collapse = "\n  "), 
                  "\n")
    if (! is.null(travis_session_info)) {
        if (length(travis_session_info) == 1 && 
            travis_session_info == "travis-cli") {
            r <- git2r::repository(path, discover = TRUE)
            travis_repo <- sub("https://github.com/", "", 
                               grep("github", value = TRUE, git2r::remote_url(r)))
            travis_log <- system2("sudo", paste("travis logs --repo", travis_repo), 
                                  stdout = TRUE)
            k <- grep("sessionInfo()", travis_log)
            travis_session_info <- travis_log[(k + 1): (k + 3)]
        }
        travis_session_info <- unlist(strsplit(travis_session_info, "\n"))
        comments <- c(comments, "- ", 
                      paste(travis_session_info[travis_session_info != ""], 
                            collapse = "\n  "), "\n")
    }
    comments <- c(comments, "- win-builder (devel)", "\n", 
                  "\n## R CMD check results\n", check_output, "\n")
    if (! as.logical(file.access(".", mode = 2))) # see ?file.acces return/note
        writeLines(comments, con = comments_file, sep = "")
    return(invisible(comments))
}


get_news <- function(path = ".") {
    root <- tryCatch(rprojroot::find_root(rprojroot::is_r_package, 
                                          path = path),
                     error = function(e) return(FALSE)
                     )
    if(root == FALSE) stop("Can't find the R package")
    description <- read.dcf(file.path(root, "DESCRIPTION"))
    news <- unlist(strsplit(paste(readLines("NEWS.md"), collapse = "\n"), split = "# "))
    package_pattern <- paste0("^", description[1, "Package"], " ", 
                            description[1, "Version"])
    news <- grep(package_pattern, news, value = TRUE) 
    news <- sub(paste0(package_pattern, "\n"), "", news)
    return(news)
}
