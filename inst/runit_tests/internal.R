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
