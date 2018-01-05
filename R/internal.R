is_null_or_true <- function(x) isTRUE(x) || is.null(x)
is_force <- function() return(is_null_or_true(getOption("packager")[["force"]]))

get_news <- function(path = ".") {
    root <- tryCatch(rprojroot::find_root(rprojroot::is_r_package,
                                          path = path),
                     error = function(e) return(FALSE)
                     )
    if (root == FALSE) throw("Can't find the R package")
    description <- read.dcf(file.path(root, "DESCRIPTION"))
    news <- unlist(strsplit(paste(readLines(file.path(root, "NEWS.md")), 
                                  collapse = "\n"), split = "# "))
    package_pattern <- paste0("^", description[1, "Package"], " ",
                            description[1, "Version"])
    news <- grep(package_pattern, news, value = TRUE)
    news <- sub(paste0(package_pattern, "\n"), "", news)
    return(news)
}

create_package_help <- function(path = ".",
                                title = NULL,
                                description = NULL,
                                details = NA
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
    if (is.na(details))
        details <- paste0("You will find the details in\\cr\n",
                         "\\code{vignette(\"An_Introduction_to_",
                         pkg[["package"]], "\", package = \"", pkg[["package"]],
                         "\")}.")
    package_roxygen_file <- file.path(pkg[["path"]], "R",
                                      paste0(pkg[["package"]], "-package.R"))
    package_roxygen_end <- c(paste0("@name ", pkg[["package"]], "-package"),
                             paste0("@aliases ", pkg[["package"]], "-package"),
                             "@docType package",
                             "@keywords package")
    content <- c(strwrap(title, prefix = "#' "), "#'")
    content <- c(content, strwrap(description, prefix = "#' "), "#'")
    content <- c(content, strwrap(details, prefix = "#' ", width = 80), "#'")
    content <- c(content, strwrap(package_roxygen_end, prefix = "#' "))
    content <- c(content, "NULL")
    writeLines(content, con = package_roxygen_file, sep = "\n")
    return(invisible(content))
}

update_description <- function(path = ".",
                               title = "A Fake Title",
                               description = "This is a fake package.",
                               author_at_r = NULL
                               ) {
    if (is.null(author_at_r)) {
        warning("Argument 'author_at_r' is missing, using default.")
        name <- whoami::fullname(fallback = "Foo Bar")
        name <- unlist(strsplit(name, split = " "))
        family <- name[length(name)]
        given <- setdiff(name, family)
        email <- whoami::email_address(fallback = "foobar@nowhere.com")
        author_at_r <- utils::person(family = family, given = given,
                                     email = email, role = c("aut", "cre"))
    }
    d <- desc::desc(path)
    if (! is.null(description))
        d$set(Title = title, Description = description)
    d$set_authors(author_at_r)
    d$write()
    return(invisible(NULL))
}

grep_directory <- function(path, pattern, exclude = NULL) {
    hits <- NULL
    files <- list.files(path, full.names = TRUE, recursive = TRUE)
    if (! is.null(exclude))
        files <- grep(exclude, files, value = TRUE, invert = TRUE)
    for (f in files) {
        l <- readLines(f)
        if (any(grepl(l, pattern = pattern, perl = TRUE))) {
            found <- paste(f, sep = ": ",
                         grep(l, pattern = pattern, perl = TRUE, value = TRUE))
            hits <- c(hits, found)
        }
    }
    return(hits)
}


remove_Rproj <- function(path = rprojroot::find_root(rprojroot::is_r_package)) {
                         file_name <- list.files(path,
                                                 pattern = ".*\\.Rproj$",
                                                 full.names = TRUE)
                         return(unlink(file_name))
}

is_git_uncommitted <- function(path = ".") {
    r <- git2r::repository(path, discover = TRUE)
    status <- vapply(git2r::status(r), length, integer(1))
    return(any(status != 0))
}

is_git_clone <- function(path = ".") {
    is_git_clone <- ! is.null(git2r::discover_repository(path, ceiling = 0))
    return(is_git_clone)
}

warn_and_stop <- function(...) {
    cat(...)
    throw(...)
}

provide_throw <- function(path = ".",
                          force = is_force(),
                          ...) {
    devtools::use_testthat(pkg = path)
    pkg <- devtools::as.package(path)

    file <- "throw.R"
    file_path <- file.path("R", file)
    use_template(file, save_as = file_path, data = pkg,
                 ignore = FALSE, pkg = pkg[["path"]], force = force, ...)

    suppressMessages(devtools::use_package("RUnit", type = "Suggests",
                                           pkg = path))
    suppressMessages(devtools::use_package("devtools", type = "Suggests",
                                           pkg = path))
    suppressMessages(devtools::use_package("rprojroot", type = "Suggests",
                                           pkg = path))
    file <- "test-throw.R"
    file_path <- file.path("tests", "testthat", file)
    use_template(file, save_as = file_path, data = pkg,
                 ignore = FALSE, pkg = pkg[["path"]], force = force, ...)

    file <- "runit.R"
    file_path <- file.path("tests", file)
    use_template(file, save_as = file_path, data = pkg,
                 ignore = FALSE, pkg = pkg[["path"]], force = force, ...)

    relative_path <- file.path("inst", "runit_tests")
    dir.create(file.path(pkg[["path"]], relative_path),
               showWarnings = FALSE, recursive = TRUE)
    file <- "runit-throw.R"
    file_path <- file.path(relative_path, file)
    use_template(file, save_as = file_path, data = pkg,
                 ignore = FALSE, pkg = pkg[["path"]], force = force, ...)


    return(NULL)
}

use_devel <- function(path = ".",
                      force = is_force(),
                      ignore = TRUE, ...) {
    pkg <- devtools::as.package(path)
    use_template("devel.R", data = pkg, pkg = pkg, force = force,
                 ignore = ignore)
    invisible(NULL)
}

use_makefile <- function(path = ".",
                         force = is_force(),
                         ignore = TRUE) {
    pkg <- devtools::as.package(path)
    use_template("nomakefile", "Makefile", data = pkg, pkg = pkg, force = force,
                 ignore = ignore)
    invisible(NULL)
}

use_devtools <- function(path = ".") {
    pkg <- devtools::as.package(path)
    result <- NULL
    result <- c(result, use_news_md(pkg = path))
    result <- c(result, use_readme_rmd(path = path))
    return(result)
}

get_remote_url <- function(path, discover = TRUE) {
    repo_failed <- "fail"
    repo <- tryCatch(git2r::repository(path, discover = discover),
                     error = function(e) return(repo_failed)
                     )
    if (identical(repo, repo_failed)) {
        res <- NULL
    } else {
        res <- git2r::remote_url(repo)
    }
    return(res)
}

get_github_url <- function(x,
                           force = is_force(),
                           return_only_one = FALSE) {
    if (length(x) == 0) {
        res <- NULL
    } else {
        index_github <- grep("^https://github.com", x)
        if (length(index_github) == 0) {
            res <- NULL
        } else {
            if (isTRUE(return_only_one) && length(index_github) > 1) {
                if (isTRUE(force)) {
                    warning("Found multiple github URL, ",
                            "using the first.")
                    res <- x[index_github[1]]
                } else {
                    throw("Found multiple github URL.")
                }
            } else {
                res <- x[index_github]
            }
        }
    }
    return(res)
}

travis_cli <- function(path) {
    r <- git2r::repository(path, discover = TRUE)
    travis_repo <- sub("https://github.com/", "",
                       grep("github",
                            value = TRUE, git2r::remote_url(r)))
    travis_log <- system2("sudo", paste("travis logs --repo",
                                        travis_repo),
                          stdout = TRUE)
    k <- grep("sessionInfo()", travis_log)
    travis_session_info <- travis_log[(k + 1): (k + 3)]
    return(travis_session_info)
}

#' Git Add All Changes and Commit
#'
#' The same as git commit -am"M", where M is the \code{message}.
#' @param repo The repository to commit.
#' @param message The commit message to use.
#' @return The return value of \code{\link[git2r:commit]{git2r::commit}}.
git_add_commit <- function(repo, message = "Uncommented Changes: Backing Up") {
    git2r::add(repo, unlist(git2r::status(repo)))
    return(git2r::commit(repo, message = message))

}
