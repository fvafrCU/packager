author_at_r <- function(given, family, email) {
    author_at_r <- paste0("person(given = \"", given, 
                          "\", family = \"", family, 
                          "\", email = \"", email, 
                          "\", role = c(\"aut\", \"cre\"))")
    return(author_at_r)
}

create_package_help <- function(path = ".", 
                                title = NULL,
                                description = NULL,
                                details = NULL
                                ) {
    if (is.null(title)) title  <- "Here Goes the Title"
    if (is.null(description)) {
        description  <- paste("A description is a paragraph consisting of one",
                              "or more sentences.")
        if (is.null(details)) 
            description <- paste(description, "You may add another paragraph",
                                 "for a 'Details' section.")
    }
    pkg <- devtools::as.package(path)
    package_roxygen_file <- file.path(pkg[["path"]], "R",
                                      paste0(pkg[["package"]], "-package.R"))
    package_roxygen_end <- c(paste0("@name ", pkg[["package"]], "-package"), 
                             paste0("@aliases ", pkg[["package"]], "-package"), 
                             "@docType package", 
                             "@keywords package")
    content <- c(strwrap(title, prefix = "#' "), "#'")
    content <- c(content, strwrap(description, prefix = "#' "), "#'")
    content <- c(content, strwrap(details, prefix = "#' "), "#'")
    content <- c(content, strwrap(package_roxygen_end, prefix = "#' "))
    content <- c(content, "NULL")
    writeLines(content, con = package_roxygen_file, sep = "\n")
    return(invisible(content))
}

update_description <- function(path = ".",
                               title = NULL,
                               description = NULL,
                               author_at_r = NULL
                               ) {
    if (is.null(author_at_r)) message("Argument 'author_at_r' is missing. ", 
                                 "Use\n\t", 
                                 'packager::sub("(email)", "\n\t\\1", author_at_r("Andreas Dominik", "Cullmann", "fvafrcu@arcor.de"))',
                                 "\nfor example")
    s <- list(Title = title, Description = description, 
              "Authors@R" = author_at_r)
    res <- document::alter_description_file(path = path, substitution = s)
    return(invisible(res))
}

set_package_info <- function(path, author_at_r = NULL, title = NULL, 
                             description = NULL, details = NULL) {
    r1 <- update_description(path = path, title = title, 
                             description = description, 
                             author_at_r = author_at_r)
    r2 <- create_package_help(path = path, title = title, 
                              description = description, details = details)
    return(invisible(list(r1, r2)))
}
