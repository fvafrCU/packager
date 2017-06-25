set_author <- function (path = ".", given, family, email) {
    pkg <- devtools::as.package(path)
    message("* Updating author field in DESCRIPTION.")
    descPath <- file.path(pkg[["path"]], "DESCRIPTION")
    DESCRIPTION <- read_dcf(descPath)
    authors_at_r <- paste0("person(given = \"", given, 
                           "\", family = \"", family, 
                           "\", email = \"", email, "\", 
                           role = c(\"aut\", \"cre\"))")
    DESCRIPTION[["Authors@R"]] <- authors_at_r
    write_dcf(descPath, DESCRIPTION)
    return(invisible(NULL))
}


