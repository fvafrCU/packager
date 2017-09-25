test_exception <- function() {
    d <- file.path(tempfile())
        packager::create(path = d) 
    l <- dir(d, recursive = TRUE, full.names = TRUE)
    result <- digest::sha1(c(l, sapply(l, readLines)))
    expected <- "44b553663a6f0bf53bdc038f6b462082d5877868"
    RUnit::checkIdentical(result, expected, 
                          msg = "Value of digest::sha1() differs!")
}

