# infrastructure.R in devtools 1.13.3 states:
## #' @details
## #' Instead of the use_xyz functions from devtools use \link[usethis]{use_testthat}.
## #' @rdname devtools-deprecated
# and then heavily uses 
## #'@importsFrom usethis 

# Now devtools 1.13.3 was released on CRAN:
# # devtools_1.13.3.tar.gz	2017-08-02 09:05
# But the first release of usethis appeared on CRAN:
# # usethis_1.0.0.tar.gz	2017-10-22 19:36
# I do not know how they imported from a package not yet released, but that is 
# what they did.
# So I just got copies of the imported functions from usethis by calling the
# functions via devtools at the time. And then I modified most of them.

use_devtools <- function(path = ".") {
    pkg <- devtools::as.package(path)
    result <- NULL
    result <- c(result, use_news_md(pkg = path))
    result <- c(result, use_readme_rmd(path = path))
    # add imports to description
    if (pkg[["package"]] == "packager") {
        result <- c(result, devtools::use_package("devtools"), pkg = path)
        result <- c(result, devtools::use_package("git2r"), pkg = path)
        result <- c(result, devtools::use_package("withr"), pkg = path)
    }
    return(result)
}


# get rid of the interactive() part using yesno() to create the package. Blow if
# there is none!
load_pkg_description <- function(path, create) {
    path_desc <- file.path(path, "DESCRIPTION")
    if (!file.exists(path_desc)) {
        stop("No description at ", path_desc, call. = FALSE)
    }
    desc <- as.list(read.dcf(path_desc)[1, ])
    names(desc) <- tolower(names(desc))
    desc$path <- path
    structure(desc, class = "package")
}



# extending devtools' version to not hard code the source package of the
# template. And get rid of call to devtools:::open_in_rstudio().
# Remove dialog.
use_template <- function(template, save_as = template, data = list(),
                         ignore = FALSE, pkg = ".",
                         source_package = "packager",
                         force = isTRUE(getOption("packager")[["force"]])) {
    status <- FALSE
    pkg <- devtools::as.package(pkg)
    path <- file.path(pkg$path, save_as)
    if (! file.exists(path) || isTRUE(force)) {
        template_path <- system.file("templates", template,
                                     package = source_package, mustWork = TRUE)
        template_out <- whisker::whisker.render(readLines(template_path),
                                                data)
        message("* Creating `", save_as, "` from template.")
        writeLines(template_out, path)
        if (ignore) {
            message("* Adding `", save_as, "` to `.Rbuildignore`.")
            devtools::use_build_ignore(save_as, pkg = pkg)
        }
        status <- TRUE
    } else {
       warning("`", save_as, "` already exists.", call. = FALSE)
    }
    return(invisible(status))
}

# adjust use_readme_rmd to not pass the argument \code{open} with use_template()
# and fix the test on file.exists()
use_readme_rmd <- function (path = ".", ...) {
    pkg <- devtools::as.package(path)
    if (uses_github(pkg$path)) {
        pkg$github <- github_info(pkg$path)
    }
    pkg$Rmd <- TRUE
    use_template("omni-README", save_as = "README.Rmd", data = pkg,
                 ignore = TRUE, pkg = pkg, ...)
    devtools::use_build_ignore("^README-.*\\.png$", escape = FALSE, pkg = pkg)
    if (uses_git(pkg$path) && !file.exists(file.path(pkg$path, ".git",
                                                     "hooks", "pre-commit"))) {
        message("* Adding pre-commit hook")
        devtools::use_git_hook("pre-commit",
                               render_template("readme-rmd-pre-commit.sh"),
                               pkg = pkg)
    }
    return(invisible(NULL))
}

use_news_md <- function (pkg = ".", ...) {
    pkg <- devtools::as.package(pkg)
    use_template("NEWS.md", data = pkg, pkg = pkg, ...)
    invisible(NULL)
}

use_intro <- function (path = ".", ...) {
    pkg <- devtools::as.package(path)
    if (uses_github(pkg$path)) {
        pkg$github <- github_info(pkg$path)
    }
    pkg$date <- format(Sys.time(), "%Y-%m-%d, %H:%M:%S")
    vignette_name <- paste0("An_Introduction_to_",
                            pkg[["package"]], ".Rmd")
    check_suggested("rmarkdown")
    add_desc_package(pkg, "Suggests", "knitr")
    add_desc_package(pkg, "Suggests", "rmarkdown")
    add_desc_package(pkg, "VignetteBuilder", "knitr")
    use_directory("vignettes", pkg = pkg)
    path <- file.path("vignettes", vignette_name)
    use_template("vignette.Rmd", save_as = path, data = pkg,
                 ignore = FALSE, pkg = pkg, ...)
    return(invisible(NULL))
}

use_travis <- function (path = ".", ...) {
    pkg <- as.package(path)
    use_template("travis.yml", ".travis.yml", ignore = TRUE,
        pkg = pkg, ...)
    return(invisible(NULL))
}

# devtools' version does not pass ceiling to git2r::discover_repository,
# thus failing, if any .git found in the parents of the package path.
use_git <- function (message = "Initial commit", path = ".") {
    pkg <- devtools::as.package(path)
    if (!is.null(git2r::discover_repository(pkg[["path"]], ceiling = 0))) {
        message("* Git is already initialized")
        return(invisible())
    }
    message("* Initialising repo")
    r <- git2r::init(pkg[["path"]])
    use_git_ignore(c(".Rproj.user", ".Rhistory", ".RData"), path = path)
    message("* Adding files and committing")
    paths <- unlist(git2r::status(r))
    git2r::add(r, paths)
    git2r::commit(r, message)
    invisible(TRUE)
}

# Modified copy of devtools' unexported version
union_write <- function (path, new_lines) {
    if (file.exists(path)) {
        lines <- readLines(path, warn = FALSE)
    }
    else {
        lines <- character()
    }
    all <- union(lines, new_lines)
    writeLines(all, path)
}

# Modified copy of devtools' unexported version
use_git_ignore <- function (ignores, path = ".") {
    pkg <- devtools::as.package(path)
    paths <- paste0("`", ignores, "`", collapse = ", ")
    message("* Adding ", paths, " to ", file.path(pkg[["path"]],
        ".gitignore"))
    path <- file.path(pkg[["path"]], ".gitignore")
    union_write(path, ignores)
    invisible(TRUE)
}



