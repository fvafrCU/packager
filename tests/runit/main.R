test_exception <- function() {
    d <- file.path(tempdir(), "prutp")
    packager::create(path = d) 
    l <- dir(d, recursive = TRUE, full.names = FALSE)
    # checking on file contents will not work as timestamps and proc.time()
    # recordings in files will change. 
    result <- digest::sha1(l)
    expected <- "76cb57ec226dd9ece66399bdc86939db59b96662"
    RUnit::checkIdentical(result, expected, 
                          msg = "Value of digest::sha1() differs!")
}

