read_dcf <- function (path) {
    fields <- colnames(read.dcf(path))
    as.list(read.dcf(path, keep.white = fields)[1, ])
}
write_dcf <- function (path, desc) {
    desc <- unlist(desc)
    desc <- gsub("\n[ \t]*\n", "\n .\n ", desc, perl = TRUE,
        useBytes = TRUE)
    desc <- gsub("\n \\.([^\n])", "\n  .\\1", desc, perl = TRUE,
        useBytes = TRUE)
    starts_with_whitespace <- grepl("^\\s", desc, perl = TRUE,
        useBytes = TRUE)
    delimiters <- ifelse(starts_with_whitespace, ":", ": ")
    text <- paste0(names(desc), delimiters, desc, collapse = "\n")
    if (substr(text, nchar(text), 1) != "\n") {
        text <- paste0(text, "\n")
    }
    cat(text, file = path)
}


