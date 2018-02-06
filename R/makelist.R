#' Provide a Makelist Suitable for Packages with \pkg{packager}.
#'
#' @return An extension of \pkg{fakemake}'s \code{makelist} provided by
#' \code{\link[fakemake:provide_make_list]{provide_make_list}}.
#' @export
provide_make_list <- function() {
    fml <- fakemake::provide_make_list("package")
    # add the log directory as prerequisite to all targets
    fml <- lapply(fml, function(x) {
                      x[["prerequisites"]] <- 
                          c(".log.Rout", x[["prerequisites"]])
                      return(x)})
    # add the log directory
    log_dir_code <- c("packager:::use_directory(\"log\", ignore = TRUE)")
    a <- list(
              list(target = ".log.Rout",
                   code = log_dir_code
                   )
              )
    fml <- c(a, fml)
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
