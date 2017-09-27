test_create <- function() {
    if (interactive()) devtools::load_all(".")
    d <- file.path(tempdir(), "prutp")
    on.exit(unlink(d, recursive = TRUE))
    packager::create(path = d, make = FALSE) 
    files <- sort(dir(d, recursive = TRUE, full.names = FALSE))
    # checking on file contents may not work as timestamps and proc.time()
    # recordings in files will change. 
    file_paths <- sort(dir(d, recursive = TRUE, full.names = TRUE))
    contents <- sapply(file_paths, readLines)
    names(contents) <- files
    contents <- sapply(contents, function(x) grep("date", x, value = TRUE, 
                                                  invert = TRUE))
    if (paste(Sys.info()[c("login", "nodename")], collapse = "@") %in% 
        c("qwer@h6")) {
        result <- digest::sha1(c(contents, files))
        expected <- "39010240e1246ddf37474cdd168ff878d1c86c98"
        RUnit::checkIdentical(result, expected, 
                              msg = "Value of digest::sha1() differs!")
    } else {
        message("Checking file listings but skipping file contents on ", 
                Sys.info()["nodename"], "!")
        result <- files
        expected <- c("DESCRIPTION", "devel.R", 
                      "inst/runit_tests/runit-throw.R", "LICENSE", "Makefile", 
                      "man/prutp-package.Rd", "man/throw.Rd", "NAMESPACE", 
                      "NEWS.md", "R/prutp-package.R", "R/throw.R", "README.Rmd",
                      "tests/runit.R", "tests/testthat.R", 
                      "tests/testthat/test-throw.R",
                      "vignettes/An_Introduction_to_prutp.Rmd")
        if (! identical(expected, result)) {
            # with changing R versions, the output of dir() (and sort()?)
            # appears change. So I just check the cardinality of the
            # intersection.
            result <- length(intersect(expected, result))
            expected <- length(expected)
        }
        if (! identical(expected, result)) {
            message("=== result: ", paste(result, collapse = " "))
        }
        RUnit::checkIdentical(result, expected, msg = "File listings differ!")
    }
}

test_create_make <- function() {
    if (interactive()) devtools::load_all(".")
    d <- file.path(tempdir(), "prutp")
    packager::create(path = d, make = TRUE) 
    on.exit(unlink(d, recursive = TRUE))
    files <- sort(dir(d, recursive = TRUE, full.names = FALSE))
    # checking on file contents will not work as timestamps and proc.time()
    # recordings in files will change. 
    result <- digest::sha1(files)
    expected <- "165e2be7d6b3613eccd23b14e96a3a19c12a90d2"
    if (paste(Sys.info()[c("login", "nodename")], collapse = "@") %in% 
        c("qwer@h6")) {
        RUnit::checkIdentical(result, expected, 
                              msg = "Value of digest::sha1() differs!")
    } else {
        message("Skipping test_create_make on ", Sys.info()["nodename"], "!")
    }
}
