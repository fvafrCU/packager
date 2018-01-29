#' Create a Package Template
#'
#' This is just a wrapper to create a package using
#' \code{\link[devtools:create]{devtools::create}} and
#' infect it using \code{\link{infect}}.
#'
#'
#' @param path See \code{\link[devtools:create]{devtools::create}}.
#' @param force Recursively \code{\link{unlink}} the path before calling
#' \code{\link[devtools:create]{devtools::create}(path)}?
#' @param ... Arguments to be passed to \code{\link{infect}}.
#' @return \code{\link[base:invisible]{Invisibly}}
#' \code{\link[base:NULL]{NULL}}.
#' condition of class c("error", "packager", "condition").
#' @export
#' @examples
#' path <- file.path(tempdir(), "myPackage")
#' packager::create(path = path)
#' list.files(path)
#' unlink(path, recursive = TRUE)
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

#' Adjust a Package
#'
#' Add a variety of extensions to a package (skeleton).
#'
#' This is \pkg{packager}'s core.
#'
#' @param path Path to the package to get infected.
#' @param make Try to use make on the infected package?
#' Option to be disabled for testing.
#' @param git_add_and_commit Add and commit changes in git?
#' Option to be disabled for testing.
#' @param ... Arguments to be passed to \code{\link{set_package_info}}.
#' @return \code{\link[base:invisible]{Invisibly}}
#' \code{\link[base:NULL]{NULL}}.
#' condition of class c("error", "packager", "condition").
#' @export
#' @examples
#' path <- file.path(tempdir(), "myPackage")
#' devtools::create(path = path)
#' l1 <- list.files(path, recursive = TRUE, full.names = TRUE)
#' packager::infect(path = path)
#' unlink(path, recursive = TRUE)
#' l2 <- list.files(path, recursive = TRUE, full.names = TRUE)
#' print(l1); print(l2)
infect <- function(path, make = FALSE, git_add_and_commit = TRUE, ...) {
    r <- git2r::init(path = path)
    devtools::use_build_ignore("^.*\\.tar\\.gz$", pkg = path, escape = FALSE)
    devtools::use_build_ignore(paste0(devtools::as.package(path)[["package"]],
                                      ".Rcheck"), pkg = path)
    devtools::use_build_ignore("cran-comments.md", pkg = path)
    devtools::use_build_ignore(".Rprofile", pkg = path)
    use_git_ignore(".Rprofile", path = path)
    use_git_ignore("*.tar.gz", path = path)
    use_git_ignore(paste0(devtools::as.package(path)[["package"]],
                                    ".Rcheck"), path = path)
    use_makefile(path = path)
    set_package_info(path = path, ...)
    use_devel(path = path)
    remove_Rproj(path = path)
    use_devtools(path = path)
    use_travis(path = path)
    add_github_url_to_desc(path = path)
    use_bsd2clause_license(path = path)
    provide_throw(path = path)
    provide_make(path = path)
    use_directory("log", pkg = path, ignore = TRUE)
    if (length(Sys.which("make")) != 0 && isTRUE(make)) {
        withr::with_dir(path, {
                            roxygen2::roxygenize(package.dir = ".")
                            devtools::load_all(".")
                            Sys.setenv("R_HOME" = Sys.which("R-devel"))
                            system("make")
                        }
        )
    } else {
        # run at least roxygen
        roxygen2::roxygenize(package.dir = path)
    }
    paths <- unlist(git2r::status(r))
    if (isTRUE(git_add_and_commit)) {
            git2r::add(r, paths)
            git2r::commit(r, "Packager Changes")
    }
    return(invisible(NULL))
}

#' Set a Package's Info
#'
#' Fill DESCRIPTION and R/xxx-package.R with the same Title and Description,
#' keeping the info given in both places identical.
#'
#' @param path Path to the package.
#' @param author_at_r A \code{\link[utils:person]{person}} object.
#' @param title A string giving the title.
#' @param description A string giving the description.
#' @param details A string giving the details. Defaults to NA in which case a
#' default details are inserted. Set to NULL to have no details at all.
#' @param ... Arguments to be passed to internal function
#' \code{packager:::use_intro}.
#' @return \code{\link[base:invisible]{Invisibly}}
#' a list of results of setting the xxx-package.R and the DESCRIPTION.
#' @export
#' @examples
#' path <- file.path(tempdir(), "myPackage")
#' devtools::create(path = path)
#' a  <- utils::person("Your", "Name", "some@whe.re", role = c("aut", "cre"))
#' set_package_info(path = path, author_at_r = a, title = "What Now?",
#'                  description = "This package does nothing.",
#'                  details = "Details do not show up in DESCRIPTION.")
#' package_desc <- file.path(path, "DESCRIPTION")
#' package_info_file <- file.path(path,
#'                                "R", paste0(basename(path), "-package.R"))
#' readLines(package_desc)
#' readLines(package_info_file)
#' unlink(path, recursive = TRUE)
set_package_info <- function(path, author_at_r = NULL,
                             title = "What it Does (One Line, Title Case)",
                             description = NULL, details = NA, ...) {
    r1 <- update_description(path = path, title = tools::toTitleCase(title),
                             description = description,
                             author_at_r = author_at_r)
    r2 <- create_package_help(path = path, title = tools::toTitleCase(title),
                              description = description, details = NA)
    r3 <- use_intro(path = path, details = details, ...)
    return(invisible(list(r1, r2, r3)))
}
