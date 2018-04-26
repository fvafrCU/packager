#' Use a Development Version in DESCRIPTION and NEWS.md
#'
#' @param path Path to your package's directory.
#' @return The return value of \code{\link{git_add_commit}}.
#' @export
use_dev_version <- function(path = ".") {
    devtools::use_dev_version(pkg = path)
    use_dev_news(path = path)
    status <- git_add_commit(path, message = "Use development version in NEWS")
    return(status)
}


#' Use a Development Version in NEWS.md
#'
#' @param path Path to your package's directory or the the NEWS file.
#' @return TRUE on success, FALSE otherwise.
#' @export
use_dev_news <- function(path = ".") {
    if (file.info(path)[["isdir"]]) {
        news_file <- file.path(path, "NEWS.md")
    } else {
        news_file <- path
    }
    if (! file.exists(news_file)) {
        status = FALSE
        warning(news_file, " does not exist!")
    } else {
        news <- readLines(news_file)
        dev_paragraph <- paste("#", desc::desc_get("Package"), 
                               desc::desc_get_version())
        news <- c(dev_paragraph, "", "* FIXME", "", news)
        writeLines(news, news_file)
        status  <- TRUE
    }
    return(status)
}
