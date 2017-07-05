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
parse_check_results <- function (path) {
    lines <- paste(readLines(path, warn = FALSE), collapse = "\n")
    lines <- gsub("^NOTE: There was .*\n$", "", lines)
    lines <- gsub("^WARNING: There was .*\n$", "", lines)
    pieces <- strsplit(lines, "\n\\* ")[[1]]
    structure(list(errors = pieces[grepl("... ERROR", pieces, 
        fixed = TRUE)], warnings = pieces[grepl("... WARN", pieces, 
        fixed = TRUE)], notes = pieces[grepl("... NOTE", pieces, 
        fixed = TRUE)]), path = path, class = "check_results")
}


