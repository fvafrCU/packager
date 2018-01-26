#' Provide a Makelist Suitable for Packages with \pkg{packager}.
#'
#' @return An extension of \pkg{fakemake}'s \code{makelist} provided by
#' \code{\link[fakemake:provide_make_list]{provide_make_list}.
#' @export
provide_make_list <- function() {
    fml <- fakemake::provide_make_list("package")
    dir_r <- "list.files(\"R\", full.names = TRUE, recursive = TRUE)"
    a <- list(
              list(alias = "cyclocomp", 
                   target = "log/cyclocomp.Rout", 
                   code = "tryCatch(print(packager::check_cyclomatic_complexity()), error = identity))", 
                   prerequisites = dir_r),
              list(alias = "code_tags", 
                   target = "log/code_tags.Rout", 
                   code = "tryCatch(print(packager::check_codetags(), error = identity))", 
                   prerequisites = dir_r)
              )
    return(c(fml, a))
}

