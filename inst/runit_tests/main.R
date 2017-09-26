test_create <- function() {
    if (interactive()) devtools::load_all(".")
    d <- file.path(tempdir(), "prutp")
    on.exit(unlink(d, recursive = TRUE))
    packager::create(path = d, make = FALSE) 
    files <- dir(d, recursive = TRUE, full.names = FALSE)
    # checking on file contents may not work as timestamps and proc.time()
    # recordings in files will change. 
    f <- dir(d, recursive = TRUE, full.names = TRUE)
    contents <- sapply(f, readLines)
    names(contents) <- files
    contents <- sapply(contents, function(x) grep("date", x, value = TRUE, invert = TRUE))
    result <- digest::sha1(c(contents, files))
    expected <- "39010240e1246ddf37474cdd168ff878d1c86c98"
    RUnit::checkIdentical(result, expected, 
                          msg = "Value of digest::sha1() differs!")
}

notest_create_make <- function() {
    if (interactive()) devtools::load_all(".")
    d <- file.path(tempdir(), "prutp")
    packager::create(path = d, make = TRUE) 
    on.exit(unlink(d, recursive = TRUE))
    l <- dir(d, recursive = TRUE, full.names = FALSE)
    # checking on file contents will not work as timestamps and proc.time()
    # recordings in files will change. 
    result <- digest::sha1(l)
    expected <- "165e2be7d6b3613eccd23b14e96a3a19c12a90d2"
    RUnit::checkIdentical(result, expected, 
                          msg = "Value of digest::sha1() differs!")
}
