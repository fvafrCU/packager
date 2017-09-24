create <- function(path, force = TRUE, ...) {
    if (isTRUE(force)) unlink(path, recursive = TRUE)
    devtools::create(path = path, rstudio = FALSE, check = FALSE)
    r <- git2r::init(path = path)
    paths <- unlist(git2r::status(r))
    git2r::add(r, paths)
    git2r::commit(r, "Initial Commit")
    infect(path = path, ...)
    return(invisible(NULL))
}

infect <- function(path, make = TRUE, git_add_and_commit = TRUE, ...) {
    r <- git2r::init(path = path)
    devtools::use_build_ignore("^.*\\.tar\\.gz$", pkg = path, escape = FALSE)
    devtools::use_build_ignore(paste0(devtools::as.package(path)[["package"]], 
                                      ".Rcheck"), pkg = path)
    use_makefile(path = path)
    use_intro(path = path, force = TRUE)
    use_devel(path = path)
    remove_Rproj(path = path)
    use_devtools(path = path)
    set_package_info(path = path, ...)
    use_bsd2clause_license(path = path)
    provide_throw(path = path)
    use_directory("log", pkg = path, ignore = TRUE)
    use_git_ignore("*.tar.gz", path = path)
    use_git_ignore(paste0(devtools::as.package(path)[["package"]], 
                                    ".Rcheck"), path = path)
    if (length(Sys.which("make")) != 0 && isTRUE(make)) {
        withr::with_dir(path, system2("make"))
    } else { # run at least roxygen
        roxygen2::roxygenize(package.dir = path)
    }
    paths <- unlist(git2r::status(r))
    if (isTRUE(git_add_and_commit)) {
            git2r::add(r, paths)
            git2r::commit(r, "Packager Changes")
    }
    return(invisible(NULL))
}

