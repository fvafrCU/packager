create <- function(path, force = TRUE, ...) {
    if (isTRUE(force)) unlink(path, recursive = TRUE)
    devtools::create(path = path, ...)
    r <- git2r::init(path = path)
    paths <- unlist(git2r::status(r))
    git2r::add(r, paths)
    git2r::commit(r, "Initial Commit")
    infect(path = path)
    return(invisible(NULL))
}

infect <- function(path, ...) {
    r <- git2r::init(path = path)
    devtools::use_build_ignore("^.*\\.tar\\.gz$", pkg = path, escape = FALSE)
    devtools::use_build_ignore(paste0(devtools::as.package(path)[["package"]], 
                                      ".Rcheck"), pkg = path)
    use_makefile(path = path)
    use_intro(path = path, force = TRUE)
    use_devel(path = path)
    remove_Rproj(path = path)
    use_devtools(path = path)
    auth <- sub("(email)", "\n\t\\1", author_at_r("Andreas Dominik", "Cullmann", 
                                      "fvafrcu@arcor.de"))
    set_package_info(path = path, author_at_r = auth)
    use_bsd2clause_license(path = path)
    substitution <- list("Authors@R" = auth)
    document::alter_description_file(path = path, substitution = substitution)
    provide_throw(path = path)
    use_directory("log", pkg = path, ignore = TRUE)
    use_git_ignore("*.tar.gz", path = path)
    use_git_ignore(paste0(devtools::as.package(path)[["package"]], 
                                    ".Rcheck"), path = path)
    if (length(Sys.which("make")) != 0) {
        withr::with_dir(path, system2("make"))
    } else { # run at least roxygen
        roxygen2::roxygenize(package.dir = path)
    }
    paths <- unlist(git2r::status(r))
    git2r::add(r, paths)
    git2r::commit(r, "Packager Changes")
    return(invisible(NULL))
}

