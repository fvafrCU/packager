#' Check Usage with \pkg{codetools}' \code{\link{checkUsagePackage}}
#'
#' This is just a convenience wrapper to \code{\link{checkUsagePackage}} (which
#' needs loading of the [developement version of] package).
#' @param path Path to the package directory.
#' @return A character vector of issues found by
#' \code{\link{checkUsagePackage}}.
#' @export 
check_usage <- function(path = ".") {
    root <- rprojroot::find_root(path = path, rprojroot::is_r_package)
    devtools::load_all(root)
    package_name <- as.character(desc::desc_get("Package", path)) 
    issues <- utils::capture.output(codetools::checkUsagePackage(package_name, 
                                                                 all = TRUE))
    return(issues)
}

