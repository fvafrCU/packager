provide_fake_package <- function() {
    tmp <- tempdir()
    path <- file.path(tmp, "fakePackage")
    tryCatch(devtools::create(path, quiet = TRUE), error = identity)
    return(path)
}

test_is_null_or_true <- function() {
    result <- packager:::is_null_or_true(NULL)
    RUnit::checkTrue(result)
    result <- packager:::is_null_or_true(TRUE)
    RUnit::checkTrue(result)
    result <- packager:::is_null_or_true("foobar")
    RUnit::checkTrue(! result)
}

test_is_force <- function() {
    result <- packager:::is_force()
    RUnit::checkTrue(result)
}

test_get_news <- function() {
    path <- provide_fake_package()
    devtools::use_news_md(path)
    result <- packager:::get_news(path)
    expectation <- "\n* Added a `NEWS.md` file to track changes to the package.\n\n\n"
    RUnit::checkIdentical(result, expectation)
}
