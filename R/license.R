use_bsd2clause_license <- function (path = ".") {
    pkg <- devtools::as.package(path)
    author <- unlist(eval(parse(text = pkg["authors@r"])))
    copyright_holder <- paste(author[["given"]], author[["family"]])
    message("* Updating license field in DESCRIPTION.")
    descPath <- file.path(pkg[["path"]], "DESCRIPTION")
    DESCRIPTION <- read_dcf(descPath)
    DESCRIPTION[["License"]] <- "BSD_2_clause + file LICENSE"
    write_dcf(descPath, DESCRIPTION)
    cat("YEAR: ", format(Sys.Date(), "%Y"), "\n", 
        "COPYRIGHT HOLDER: ", copyright_holder, 
        sep = "", file = file.path(pkg[["path"]], "LICENSE"))
    return(invisible(NULL))
}


