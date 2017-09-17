is_null_or_true <- function(x) isTRUE(x) || is.null(x)
is_force <- function() return(is_null_or_true(getOption("packager")[["force"]]))
