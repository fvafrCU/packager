test_exception <- function() {
    RUnit::checkException(throw("Hello, error!"))
}
