#' Provide a Makelist Suitable for Packages with \pkg{packager}.
#'
#' @return An extension of \pkg{fakemake}'s \code{makelist} provided by
#' \code{\link[fakemake:provide_make_list]{provide_make_list}}.
#' @export
provide_make_list <- function() {
    fml <- fakemake::provide_make_list(type = "standard")
    dir_r <- "list.files(\"R\", full.names = TRUE, recursive = TRUE)"
    cyclo_code <- paste("tryCatch(print(",
                        "packager::check_cyclomatic_complexity()),",
                        "error = identity))")
    code_tags_code <- paste("tryCatch(print(packager::check_codetags(),",
                            "error = identity))")
    a <- list(
              list(alias = "cyclocomp", target = "log/cyclocomp.Rout",
                   code = cyclo_code,
                   prerequisites = c(dir_r, ".log.Rout")),
              list(alias = "codetags", target = "log/codetags.Rout",
                   code = code_tags_code,
                   prerequisites = c(dir_r, ".log.Rout"))
              )
    return(c(fml, a))
}
