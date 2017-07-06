provide_cran_comments <- function(check_log = "log/dev_check.Rout",
                                  travis_raw_log = NULL) {
    #FIXME: check_log without default!
    if (is.null(travis_raw_log)) {
            # search for  $ Rscript -e 'sessionInfo()' 
            # in the raw log of the travis build
            travis_raw_log <- c("
                                R version 3.4.0 (2017-04-21)
                                Platform: x86_64-pc-linux-gnu (64-bit)
                                Running under: Ubuntu precise (12.04.5 LTS) 
                                ")
    }
    pkg <- devtools::as.package(".")
    comments_file = file.path(pkg[["path"]], "cran-comments.md")
    cat("Dear CRAN Team,\n", 
        "this is a resubmission of package ", pkg[["package"]], ". I have\n", 
        "XXX", "\nBest, Dominik\n", sep = "", file = comments_file, 
        append = FALSE)
    cat("\n# Package ", pkg[["package"]], pkg[["version"]], 
        file = comments_file, append = TRUE)
    travis <- unlist(strsplit(travis_raw_log, "\n"))
    session <- utils::sessionInfo()
    here <- c("",
              session[["R.version"]][["version.string"]],
              paste0("Platform: ", session[["platform"]]),
              paste0("Running under: ", session[["running"]]))

    cat("\n## Test  environments ", "\n", file = comments_file, append = TRUE)
    cat("-", paste(here[here != ""], collapse = "\n  "), "\n", 
        file = comments_file, append = TRUE)
    cat("-", paste(travis[travis != ""], collapse = "\n  "), "\n", 
        file = comments_file, append = TRUE)
    cat("- win-builder (devel)", "\n", file = comments_file, append = TRUE)
    cat("\n## R CMD check results\n", file = comments_file, append = TRUE)
    check <- parse_check_results(check_log)
    cat(utils::capture.output(print(check)), "\n" , file = comments_file, 
        append = TRUE, sep = "\n")
    return(invisible(NULL))
}
