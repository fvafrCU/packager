use_dev_version <- function(path) {
    devtools::use_dev_version(pkg = path)
    status <- use_dev_news(path = path)
    return(status)
}


use_dev_news <- function(path = ".") {
    news_file <- file.path(path, "NEWS.md")
    if (! file.exists(news_file)) {
        status = FALSE
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
