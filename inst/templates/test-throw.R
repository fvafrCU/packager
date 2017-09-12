testthat::context("test throw")
testthat::test_that("throw the {{{ package }}} exception", {
                        error_message <- "hello, testthat"
                        testthat::expect_error(throw("hello, testthat"), 
                                               error_message)
}
)

