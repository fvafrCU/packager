test_create <- function() {
    if (interactive()) devtools::load_all(".")
    d <- file.path(tempdir(), "prutp")
    on.exit(unlink(d, recursive = TRUE))
    packager::create(path = d, make = FALSE) 
    files <- sort(dir(d, recursive = TRUE, full.names = FALSE))
    file_paths <- sort(dir(d, recursive = TRUE, full.names = TRUE))
    contents <- sapply(file_paths, readLines)
    names(contents) <- files
    contents <- sapply(contents, function(x) grep("date", x, value = TRUE, 
                                                  invert = TRUE))
    # checking on file contents does not work as covr and RUnit give different
    # digest::sha1()-values.
    result <- files
    expected <- c("DESCRIPTION", "devel.R", 
                  "inst/runit_tests/runit-throw.R", "LICENSE", "Makefile", 
                  "man/prutp-package.Rd", "man/throw.Rd", "NAMESPACE", 
                  "NEWS.md", "R/prutp-package.R", "R/throw.R", "README.Rmd",
                  "tests/runit.R", "tests/testthat.R", 
                  "tests/testthat/test-throw.R",
                  "vignettes/An_Introduction_to_prutp.Rmd")
    if (identical(expected, result)) {
        message("Checking file listings but skipping file contents on ", 
                Sys.info()["nodename"], "!")
        runit_messsage <- "File listings differ!"
    } else {
        # with changing R versions, the output of dir() (and sort()?)
        # appears change. So I just check the cardinality of the
        # intersection.
        result <- length(intersect(expected, result))
        expected <- length(expected)
        message("Checking file listings cardinality but skipping file ",  
                "contents on ", Sys.info()["nodename"], "!")
        runit_messsage <- "Cardinality of file listings intersection died!"
    }
    if (! identical(expected, result)) {
        message("=== result: ", paste(result, collapse = " "))
    }
    RUnit::checkIdentical(result, expected, msg = runit_messsage)
}

test_create_make <- function() {
    if (interactive()) devtools::load_all(".")
    d <- file.path(tempdir(), "prutp")
    packager::create(path = d, make = TRUE) 
    on.exit(unlink(d, recursive = TRUE))
    files <- sort(dir(d, recursive = TRUE, full.names = FALSE))
    # checking on file contents does not work as R and R CMD check give
    #  different digest::sha1()-values.
    result <- files
    expected <- c("DESCRIPTION", "devel.R", 
                  "inst/runit_tests/runit-throw.R", "LICENSE", "Makefile", 
                  "man/prutp-package.Rd", "man/throw.Rd", "NAMESPACE", 
                  "NEWS.md", "R/prutp-package.R", "R/throw.R", "README.Rmd",
                  "tests/runit.R", "tests/testthat.R", 
                  "tests/testthat/test-throw.R",
                  "vignettes/An_Introduction_to_prutp.Rmd")
    if (identical(expected, result)) {
        message("Checking file listings but skipping file contents on ", 
                Sys.info()["nodename"], "!")
        runit_messsage <- "File listings differ!"
    } else {
        # with changing R versions, the output of dir() (and sort()?)
        # appears change. So I just check the cardinality of the
        # intersection.
        result <- length(intersect(expected, result))
        expected <- length(expected)
        message("Checking file listings cardinality but skipping file ",  
                "contents on ", Sys.info()["nodename"], "!")
        runit_messsage <- "Cardinality of file listings intersection died!"
    }
    if (! identical(expected, result)) {
        message("=== result: ", paste(result, collapse = " "))
    }
    RUnit::checkIdentical(result, expected, msg = runit_messsage)
}
