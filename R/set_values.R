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

set_title <- function (path = ".", title) {
    pkg <- devtools::as.package(path)
    message("* Updating title field in DESCRIPTION.")
    descPath <- file.path(pkg[["path"]], "DESCRIPTION")
    DESCRIPTION <- read_dcf(descPath)
    DESCRIPTION[["Title"]] <- title
    write_dcf(descPath, DESCRIPTION)
    message("* Updating title in package help.")
    cat(sep = "", "#' ", title, "\n#'\n", 
        file = file.path(pkg[["path"]], "R", 
                         paste0(pkg[["package"]], "-package.R")))
    return(invisible(NULL))
}

set_description <- function (path = ".", description) {
    pkg <- devtools::as.package(path)
    message("* Updating description field in DESCRIPTION.")
    descPath <- file.path(pkg[["path"]], "DESCRIPTION")
    DESCRIPTION <- read_dcf(descPath)
    desc_string <- paste(strwrap(description, prefix = "  ", initial = ""), 
                         collapse = "\n")
    DESCRIPTION[["Description"]] <- desc_string
    write_dcf(descPath, DESCRIPTION)
    message("* Updating description in package help.")
    desc_string <- paste(strwrap(description, prefix = "#' "), 
                         collapse = "\n")
    package_roxygen_file <- file.path(pkg[["path"]], "R",
                                      paste0(pkg[["package"]], "-package.R"))
    cat(desc_string, file = package_roxygen_file, append = TRUE, sep ="\n")
    package_roxygen_end <- c(paste0("#' @name ", pkg[["package"]], "-package"), 
                             paste0("#' @aliases ", pkg[["package"]], "-package"), 
                             "#' @docType package", 
                             "#' @keywords package", "NULL")
    cat(package_roxygen_end, file = package_roxygen_file, append = TRUE, 
        sep = "\n")

    return(invisible(NULL))
}



