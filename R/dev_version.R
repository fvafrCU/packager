use_dev_news <- function(path = ".") {
    news_file <- file.path(path, "NEWS.md")
    news <- readLines(news_file)
    news <- c(paste("#", desc::desc_get("Package"), desc::desc_get_version()),
              "", "* FIXME", "",
              news)
    writeLines(news, news_file)
}
