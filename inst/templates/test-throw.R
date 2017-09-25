testthat::context("Testing {{{package}}}:::throw()")
testthat::test_that("throw the {{{ package }}} exception", {
                        error_message <- "hello, testthat"
                        testthat::expect_error({{{package}}}:::throw("hello, testthat"),
                                               error_message)
}
)
