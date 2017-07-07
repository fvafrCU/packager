#' Provide a Template For Your Comments To CRAN
#' 
#' Devtools' \code{\link{release}} reads a file \emph{cran-comments}. This
#' function provides a template based on your R version and your check log.
#' @param path The path to the package.
#' @param check_log Path to the check log relative to \code{path}. Typically 
#' file.path("log", "dev_check.Rout").
#' @param travis_raw_log Travis check info, search for\cr   
#' "$ Rscript -e 'sessionInfo()'" \cr 
#' in the raw log of the travis build and copy the
#' following three lines. This could read\cr 
#' travis_raw_log <- c("\cr 
#'                     R version 3.4.0 (2017-04-21)\cr 
#'                     Platform: x86_64-pc-linux-gnu (64-bit)\cr 
#'                     Running under: Ubuntu precise (12.04.5 LTS) \cr 
#'                     ")\cr 
#' @return NULL.
#' @export
provide_cran_comments <- function(check_log,
                                  path = ".",
                                  travis_raw_log = NULL) {
    pkg <- devtools::as.package(path)
    if (travis_raw_log == "auto") {
        r <- git2r::repository(path, discover = TRUE)
        travis_repo <- sub("https://github.com/", "", 
                           grep("github", value = TRUE, git2r::remote_url(r)))
        travis_log <- system(paste("sudo travis logs --repo", travis_repo), 
                             intern = TRUE)
    }
    if (is.na(travis_raw_log)) {
            # search for  $ Rscript -e 'sessionInfo()' 
            # in the raw log of the travis build
            travis_raw_log <- c("
                                R version 3.4.0 (2017-04-21)
                                Platform: x86_64-pc-linux-gnu (64-bit)
                                Running under: Ubuntu precise (12.04.5 LTS) 
                                ")
    }
    comments_file = file.path(pkg[["path"]], "cran-comments.md")
    cat("Dear CRAN Team,\n", 
        "this is a resubmission of package ", pkg[["package"]], ". I have\n", 
        "XXX", "\nBest, Dominik\n", sep = "", file = comments_file, 
        append = FALSE)
    cat("\n# Package ", pkg[["package"]], pkg[["version"]], 
        file = comments_file, append = TRUE)
    session <- utils::sessionInfo()
    here <- c("",
              session[["R.version"]][["version.string"]],
              paste0("Platform: ", session[["platform"]]),
              paste0("Running under: ", session[["running"]]))

    cat("\n## Test  environments ", "\n", file = comments_file, append = TRUE)
    cat("-", paste(here[here != ""], collapse = "\n  "), "\n", 
        file = comments_file, append = TRUE)
    if (! is.null(travis_raw_log)) {
            travis <- unlist(strsplit(travis_raw_log, "\n"))
            cat("-", paste(travis[travis != ""], collapse = "\n  "), "\n", 
                file = comments_file, append = TRUE)
        }
    cat("- win-builder (devel)", "\n", file = comments_file, append = TRUE)
    cat("\n## R CMD check results\n", file = comments_file, append = TRUE)
    check <- parse_check_results(file.path(path, check_log))
    cat(capture.output(devtools:::print.check_results(check), type = "message"),
        "\n" , file = comments_file, 
        append = TRUE, sep = "\n")
    return(invisible(NULL))
}
