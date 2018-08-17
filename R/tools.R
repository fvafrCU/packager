#' Use the BSD-2-Clause License
#'
#' It's my favorite and \pkg{devtools} provides
#' \code{\link[devtools:use_mit_license]{use_mit_license}} only.
#' @param path Path to the package.
#' @return Invisibly \code{\link{NULL}}.
#' @examples
#' withr::with_dir(tempdir(),
#'                 {
#'                     unlink("fakepack", recursive = TRUE)
#'                     devtools::create("fakepack")
#'                     use_bsd2clause_license("fakepack")
#'                     list.files("fakepack")
#'                     grep("^License", readLines(file.path("fakepack",
#'                                                          "DESCRIPTION")))
#'                     readLines(file.path("fakepack", "LICENSE"))
#'                 }
#' )
#' @export
use_bsd2clause_license <- function (path = ".") {
    pkg <- devtools::as.package(path)
    license  <- list(License = "BSD_2_clause + file LICENSE")
    d <- desc::desc(path)
    d$set(License = license)
    d$write()
    copyright_holder <- sub(" <.*$", "", d$get_author())
    cat("YEAR: ", format(Sys.Date(), "%Y"), "\n",
        "COPYRIGHT HOLDER: ", copyright_holder,
        sep = "", file = file.path(pkg[["path"]], "LICENSE"))
    return(invisible(NULL))
}


#' Add a \verb{github} \acronym{URL} to File \file{DESCRIPTION}
#'
#" When writing packages, I often forget to add the appropriate \verb{github}
#' \acronym{URL}.
#'
#' The \acronym{URL} is constructed by the package's name as read from it's file
#' \file{DESCRIPTION}, and the username returned by
#' \code{\link[whoami:gh_username]{whoami::gh_username}}.
#' \code{\link[whoami:gh_username]{whoami::gh_username}} allows for a fallback,
#' this is given by \code{default_gh_user}. 
#' You can specify \code{default_gh_user = NA}, to try to retrieve the username
#' by searching remotes on \verb{github} if the
#' package is a git repository. We do not use
#' \code{\link[git2r:config]{git2r::config}} since there's no way to make sure
#' the configured git user name, locally or globally, is a \verb{github}
#' username.
#'
#' @param path The path to the package.
#' @param default_gh_user See details.
#' @param normalize Passed to
#' \code{\link[desc:desc_set_urls]{desc::desc_set_urls}}.
#' @return \code{\link[base:invisible]{Invisibly} \link[base:logical]{TRUE}} if
#' adding a \verb{github} \acronym{URL}, \code{\link[base:logical]{FALSE}}
#' otherwise.
#' @export
#' @examples
#' tmp <- tempfile()
#' dir.create(tmp)
#' path <- file.path(tmp, "fakePackage")
#' devtools::create(path)
#' add_github_url_to_desc(path)
#' grep("^URL:", readLines(file.path(path, "DESCRIPTION")), value = TRUE)
add_github_url_to_desc <- function(path = ".", default_gh_user = NULL,
                                   normalize = TRUE) {
    status <- FALSE
    gh_username <- tryCatch(whoami::gh_username(fallback = default_gh_user),
                            error = function(e) return(default_gh_user))
    desc_url <- desc::desc_get_urls(path)

    package_dir <- basename(rprojroot::find_root(path = path,
                                                 rprojroot::is_r_package))
    package_name <- strip_off_attributes(desc::desc_get("Package", file = path))
    if (package_name != package_dir)
        warning("The package's name and root directory differ, ",
                "sticking with the name as retrieved from file DESCRIPTION.")
    if (! is.null(gh_username) && is.na(gh_username)) {
        git_url <- get_git_url(get_remote_url(path), type = "github")
        num_of_remotes <- length(grep(paste0(package_name, "$"), git_url))
        if (num_of_remotes == 1) {
            gh_username <- sub(paste0("^https://github.com/(.*)/", package_name,
                                      "$"),
                               "\\1", git_url)
        } else {
            warning("Found ", num_of_remotes,
                    " git remotes refering to `", package_name, "`.")
        }
    }
    if (is.null(gh_username) || is.na(gh_username)) {
        warning("Could not retrieve github user name. ",
                "Set the URL in DESCRIPTION manually!")
        manual_url <- NULL
    } else {
        manual_url <- paste("https://github.com", gh_username, package_name,
                            sep = "/")
    }
    if (! is.null(manual_url) && ! any(grepl(manual_url, desc_url))) {
        desc_url <- c(desc_url, manual_url)
        desc::desc_set_urls(desc_url, file = path, normalize = normalize)
        status <- TRUE
    }
    return(invisible(status))
}

#' Provide File \command{make.R}
#'
#' @param path Where to create the file.
#' @param Rbuildignore Add the file to \file{.Rbuildignore} under the given
#' \code{path}?
#' @return \code{\link[base:invisible]{Invisibly}}
#' \code{\link[base:logical]{TRUE}} on success, 
#' \code{\link[base:invisible]{Invisibly}}
#' \code{\link[base:logical]{FALSE}} otherwise.
#' @export
provide_make <- function(path, Rbuildignore = TRUE) {
    status <- use_template("make.R", pkg = path, ignore = Rbuildignore)
    return(status)
}

#' Print Lints by Name Suffix
#' 
#' I often use internals from other packages and save them in files 
#' named ..._internals..., ..._verbatim... of ..._modified... .
#' \cr I want these to be marked in \pkg{lintr}'s output.
#'
#' @param x A list of lints.
#' @param file_name_markers Parts of the file name which mark copied code.
#' @param sort Sort by file name suffix?
#' @param invert Invert the sorting?
#' @param ... Arguments passed down to print.
#' @return The list of lints with names marked.
#' @export
#' @examples
#' files <- list.files(system.file("files", package = "packager"), 
#'                     full.names = TRUE)
#' lints <- lintr:::flatten_lints(lapply(files, function(file) {
#'                                           if (interactive()) {
#'                                               message(".", appendLF = FALSE)
#'                                           }
#'                                           lintr::lint(file, 
#'                                                       parse_settings = FALSE)
#'                                      }))
#' 
#' print_lints(lints, invert = FALSE)
#' print_lints(lints, invert = TRUE)
print_lints <- function(x, sort = TRUE, invert = FALSE,
                 file_name_markers = c("_internals", "_verbatim", "_modified"),
                 ...
                 ) {

    mark <- "COPY:"
    nomark <- "NO COPY:"
    pattern <- paste0("(", 
                      paste0("^.*",file_name_markers , ".*$", 
                             collapse = "|"),
                      ")")
    set_mark <- function(x) {
        if (grepl(pattern, x$filename)) 
            x$mark <- mark
        else
            x$mark <- nomark
        return(x)
    }
    x <- lapply(x, function(x) {set_mark(x)})


    if (isTRUE(sort)) {
        marked <- sapply(x, function(x) return(x[["mark"]] == mark))
        if (isTRUE(invert))
            x <- c(x[marked], x[! marked])
        else
            x <- c(x[! marked], x[marked])

    }
    if (has_lints <- length(x) > 0) invisible(lapply(x, print_lint))
    return(invisible(x))
}

#' Is a Directory an R Package Directory?
#' 
#' Just a convenience wrapper to
#' \code{\link[rprojroot:is_r_package]{rprojroot::is_r_package}}.
#' @param path The path to the directory or one of its subdirectories.
#' @return TRUE if the directory is an \R package directory.
#' @export
is_r_package <- 
    rprojroot::as.root_criterion(rprojroot::is_r_package)[["testfun"]][[1]]

#' Provide a \verb{gitlab} \acronym{URL} for a Given Path
#'
#' @param path The path to the directory or one of its subdirectories.
#' @return a character string giving a \verb{github} \acronym{URL}.
#' @export
#' @examples
#' path <- file.path(tempdir(), "foo")
#' unlink(path, recursive = TRUE)
#' devtools::create(path)
#' try(provide_gitlab_url(path))
#' git2r::init(path)
#' provide_gitlab_url(path)
#' invisible(desc::desc_set(Package = "bar", file = path))
#' provide_gitlab_url(path)
provide_gitlab_url <- function(path = ".") {
    url <- get_git_url(get_remote_url(path))
    if (is.null(url)) {
        repository <- tryCatch(git2r::repository(path = path),
                               error = identity)
        # could use uses_git(path) as condition, but if TRUE, 
        # I would have to call gitr::repository in the TRUE suite of the if. 
        # So I do it above.
        if (inherits(repository, "error")) {
            throw(paste(path, "is not a git repository"))
        } else {
            directory <- basename(git2r::workdir(repository))
            if (is_r_package(path)) {
                package_name <- strip_off_attributes(desc::desc_get("Package", 
                                                                    file = path)
                )
                if (package_name != directory) {
                    warning("The package's name and root directory differ, ",
                            "sticking with the name as retrieved from file ", 
                            "DESCRIPTION.")
                    directory <- package_name
                }
            }
            git_signature <- tryCatch(git2r::default_signature(repository), 
                                    error = identity)
            if (inherits(git_signature, "error")) {
                user_name <- "foobar"
                user_email <- "foobar@nowhe.re"
                git2r::config(repository, 
                              user.name = user_name, user.email = user_email)
                git_signature <- git2r::default_signature(repository) 
            }
            name <- getElement(git_signature, "name")
            url <- paste("https://gitlab.com", name, directory, sep = "/")
        }
    }
    return(url)
}

#' Set a \file{DESCRIPTION} File's \acronym{URL} Field
#' 
#' I frequently forget to add an \acronym{URL} to my packages'
#' \file{DESCRIPTION} files,
#' and when I do not, I often forget to check that the \acronym{URL} is valid,
#' respectively the one I want. \cr
#' So this is a wrapper to functions from \pkg{desc} and \pkg{git2r} and i
#' messaging and/or adding
#' a reminder to file \code{TODO.md}.
#' @param url A character string giving the \acronym{URL} to set or add in
#' \file{DESCRIPTION}.
#' @param path Path to the \file{DESCRIPTION} file, see
#' \code{\link[desc:desc_get_urls]{desc::desc_get_urls}}.
#' @param normalize See \code{\link[desc:desc_set_urls]{desc::desc_set_urls}}.
#' @param do_commit Commit the updated \file{DESCRIPTION}?
#' @param do_remind Write a reminder into the package's \file{TODO.md}?
#' @param verbose Be verbose?
#' @param overwrite Set (overwrite) the \acronym{URL} field in
#' \file{DESCRIPTION}
#' instead
#' adding the \acronym{URL} given to the \acronym{URL} field in
#' \file{DESCRIPTION}?
#' @return \code{\link[base:invisible]{Invisibly} \link[base:logical]{TRUE}}
#' @export
#' @examples
#' path <- file.path(tempdir(), "foo")
#' unlink(path, recursive = TRUE)
#' devtools::create(path)
#' repo <- git2r::init(path)
#' git2r::config(repo, user.name = "foobar", user.email = "foobar@nowhe.re")
#' git2r::add(repo = repo, path = "*")
#' git2r::commit(repo = repo, message = "Initial commit")
#' url <- provide_gitlab_url(path = path)
#' set_desc_url(url, path = path)
#' git2r::commits(repo)
#' git2r::status(repo)
#' readLines(file.path(path, "TODO.md"))
set_desc_url <- function(url, path = ".", normalize = TRUE, 
                         overwrite = FALSE,
                         do_commit = is_force(), 
                         do_remind = !isTRUE(getOption("packager")[["force"]]),
                         verbose = getOption("packager")[["verbose"]]
                         ) {
    status <- FALSE
    if (isTRUE(overwrite)) {
        # do nothing
    } else {
        desc_url <- desc::desc_get_urls(path)
        url <- c(desc_url, url)
    }
    desc::desc_set_urls(url, file = path, normalize = normalize)
    if (isTRUE(do_commit) && uses_git(path)) {
        repository <- git2r::repository(path = path)
        git2r::add(repository, "DESCRIPTION")
        # File DESCRIPTION may not have changed, so try():
        invisible(tryCatch(git2r::commit(repository, 
                                         "Update URL in DESCRIPTION"),
                           error = identity))
    }
    m <- paste0("- make sure ", url, " exists!.")
    if (isTRUE(do_remind)) {
        union_write(file.path(path, "TODO.md"), m, prepend = TRUE)
    }
    if (!isTRUE(verbose)) message(m)
    status <- TRUE
    return(invisible(status))
}
