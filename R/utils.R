is_null_or_true <- function(x) isTRUE(x) || is.null(x)
is_null_or_false <- function(x) isFALSE(x) || is.null(x)
false_or_null <- function(x) ! is_null_or_false(x)
