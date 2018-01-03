if (interactive()) devtools::load_all()

provide_fake_package <- function() {
    tmp <- tempfile()
    dir.create(tmp)
    path <- file.path(tmp, "fakePackage")
    tryCatch(suppressMessages(devtools::create(path, quiet = TRUE)), 
                              error = identity
    )
    return(path)
}


notest_cyclocomp <- function() { # somehow fails on install...
    path <- file.path(tempdir(), "fakePackge")
    on.exit(unlink(path, recursive = TRUE))
    packager::create(path)
    result <- packager::check_cyclomatic_complexity(path)
    # news too new
    writeLines(c("# fakePackge 0.1.1", new), news_file)
    RUnit::checkTrue(packager::check_news(path))


    RUnit::checkTrue(result)
    pccc <- packager::check_cyclomatic_complexity
    RUnit::checkException(pccc(path, max_complexity = 0))
}

test_news <- function() {
    path <- file.path(tempdir(), "fakePackge")
    on.exit(unlink(path, recursive = TRUE))
    packager::create(path)
    result <- packager::check_news(path)
    RUnit::checkTrue(result)

    # Bump version
    desc::desc_bump_version(path, which = "minor")

    # unmachted devel version
    RUnit::checkException(packager::check_news(path))

    # add machting version, keep devel versoin
    news_file <- file.path(path, "NEWS.md")

    old <- readLines(news_file)
    new <- "# fakePackge 0.1.0"
    writeLines(c(new, old), news_file)
    RUnit::checkException(packager::check_news(path))

    # remove devel version
    writeLines(new, news_file)
    RUnit::checkTrue(packager::check_news(path))

    # actual version not covered in NEWS.md
    writeLines(c("# fakePackge 0.1.1"), news_file)
    RUnit::checkException(packager::check_news(path))
}

test_cran_comments <- function() {
    path <- provide_fake_package()
    on.exit(unlink(path, recursive = TRUE))
    expectation <- 
        structure(c("Dear CRAN Team,\n", "this is a resubmission of package '",
                    "fakePackage", "'. I have added the following changes:\n", 
                    "\nXXX: State your changes and consider using a NEWS.md\n\n",
                    "Please upload to CRAN.\n", "Best, ", "Andreas Dominik", 
                    "\n\n# Package ",
                    "fakePackage", " ", "0.0.0.9000", 
                    "\n## Test  environments ",
                    "\n", "- ", 
                    "R version 3.3.3 (2017-03-06)\n  Platform: x86_64-pc-linux-gnu (64-bit)\n  Running under: Debian GNU/Linux 9 (stretch)",
                    "\n", "- win-builder (devel)", "\n", "\n## R CMD check results\n",
                    "ERROR: No check log given!", "\n"), .Names = c("", "", "", "",
                    "", "", "", "given", "", "", "", "", "", "", "", "", "", "",
                    "", "", "", ""))
    expectation <- grep("^R ", expectation, value = TRUE, invert = TRUE)
    result <- packager::provide_cran_comments(path = path)
    result <- grep("^R ", result, value = TRUE, invert = TRUE)
    message(result)
    RUnit::checkIdentical(expectation, result)
}
