use_bsd2clause_license <- function (path = ".") {
    pkg <- devtools::as.package(path)
    license = list(License = "BSD_2_clause + file LICENSE")
    res <- document::alter_description_file(path = path, substitution = license)
    author <- unlist(eval(parse(text = pkg["authors@r"])))
    copyright_holder <- paste(author[["given"]], author[["family"]])
    cat("YEAR: ", format(Sys.Date(), "%Y"), "\n", 
        "COPYRIGHT HOLDER: ", copyright_holder, 
        sep = "", file = file.path(pkg[["path"]], "LICENSE"))
    return(invisible(NULL))
}


