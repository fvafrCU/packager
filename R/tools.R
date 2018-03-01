#' Check Cyclomatic Complexity
#'
#' Run
#' \code{\link[cyclocomp:cyclocomp_package_dir]{cyclocomp_package_dir}} on the
#' package throwing an error when the maximum complexity is exceeded.
#' @param path The package's root directory.
#' @param max_complexity The maximum complexity (which should not be exceeded).
#' @return \code{\link[base:invisible]{Invisibly} \link[base:logical]{TRUE}} if
#' maximum cyclomatic complexity is not exceeded, throws an error otherwise.
#' @export
#' @examples
#' \dontrun{
#' # download and untar sources of some archived package
#' package  <- "document"
#' root <- paste0("http://cran.r-project.org/src/contrib/Archive/", package)
#' version <- "1.0.0"
#' tarball <- paste0(paste(package, version, sep = "_"), ".tar.gz")
#' remote_tarball <- paste(root, tarball, sep = "/")
#' local_tarball <- file.path(tempdir(), tarball)
#' utils::download.file(remote_tarball, local_tarball)
#' utils::untar(local_tarball, exdir = tempdir())
#' res <- tryCatch(check_cyclomatic_complexity(path = file.path(tempdir(),
#'                                                              package)),
#'                 error = identity)
#' print(res)
#' }
check_cyclomatic_complexity <- function(path = ".", max_complexity = 10) {
    cyclocomp <- cyclocomp::cyclocomp_package_dir(path)
    too_complex <- cyclocomp[["cyclocomp"]] > max_complexity
    if (any(too_complex)) {
        hits <- cyclocomp[too_complex, "name"]
        diff <- cyclocomp[too_complex, "cyclocomp"] - max_complexity
        msg <- paste0("Exceeding maximum cyclomatic complexity of ",
                     max_complexity, " for ", hits, " by ", diff, ".")
        throw(paste(msg, collapse = "\n"))
    }
    return(invisible(TRUE))
}

#' Check for NEWS.md Being Up to Date
#'
#' Compare your NEWS.md file to the 'Version' entry in DESCRIPTION.
#' @param path The directory to search.
#' @return \code{\link[base:invisible]{Invisibly} \link[base:logical]{TRUE}} if
#' NEWS.md matches DESCRIPTION, throws an error otherwise.
#' @export
check_news <- function(path = ".") {
    root <- rprojroot::find_root(path = path, rprojroot::is_r_package)
    description <- readLines(file.path(root, "DESCRIPTION"))
    version <- grep("^Version: ", description, value = TRUE)
    version_number <- trimws(strsplit(version, split = ":")[[1]][2])
    package <- grep("^Package: ", description, value = TRUE)
    package_name <- trimws(strsplit(package, split = ":")[[1]][2])
    news.md <- readLines(file.path(root, "NEWS.md"))
    devel_versions <- grep("[0-9]+\\.[0-9]+\\.[0-9]+\\.9000", news.md,
                           value = TRUE)
    if (length(devel_versions) > 0) {
        devel_numbers <- sapply(devel_versions,
                                function(x) strsplit(x, split = " ")[[1]][3])
        extra_devels <- setdiff(devel_numbers, version_number)
        if (length(extra_devels) > 0) {
            throw(paste("\nFound unmatched devel version: ", extra_devels))
        }
    }
    is_covered <- any(grepl(paste("^#", package_name, version_number), news.md))
    if (! is_covered) {
        throw(paste0("Version ", version_number, " not covered!"))
    } else {
        return(invisible(TRUE))
    }
}

#' Check for Code Tags
#'
#' You hopefully use code tags
#' (see \href{PEP 350}{https://www.python.org/dev/peps/pep-0350/} for example).
#' This functions searches for files under a directory containing such tags.
#' @param path The directory to search.
#' @param exclude_pattern A pattern for exlusions based on the file names.
#' Stronger than \code{include_pattern}.
#' @param include_pattern A pattern for inclusions based on the file names.
#' @param pattern The pattern to search for.
#' @return A character vector of hits.
#' @export
#' @examples
#' dir <- system.file("templates", package = "packager")
#' check_codetags(dir)
check_codetags <- function(path = ".", exclude_pattern = "\\.Rcheck/",
                           include_pattern = "\\.[Rr]$|\\.[Rr]md$",
                           pattern =  "XXX:|FIXME:|TODO:") {
    hits <- grep_directory(path = path, exclude_pattern = exclude_pattern,
                           include_pattern = include_pattern,
                           pattern =  pattern)
    return(hits)
}

#' Provide a Template for Your Comments To CRAN
#'
#' Devtools' \code{\link{release}} reads a file \emph{cran-comments.md}. This
#' function provides a template based on your R version your R CMD check log and
#' the package's NEWS.md.
#' @param path The path to the package.
#' @param initial Is this an initial release?
#' @param check_log Path to the check log relative to \code{path}. Typically
#' file.path("log", "dev_check.Rout").
#' @param travis_session_info Travis session info, search for\cr
#' "$ Rscript -e 'sessionInfo()'" \cr
#' in the raw log of the travis build and copy the
#' following three lines. This could read\cr
#' travis_session_info <- c("\cr
#'                     R version 3.4.0 (2017-04-21)\cr
#'                     Platform: x86_64-pc-linux-gnu (64-bit)\cr
#'                     Running under: Ubuntu precise (12.04.5 LTS) \cr
#'                     ")\cr
#' Set to `travis-cli` to retrieve Session info automatically if your system is
#' set up to use \url{https://github.com/travis-ci/travis.rb}.
#' @param name The name to sign with, if NA, the given name of the package
#' maintainer as stated in file DESCRIPTION is used.
#' @note This function writes to disk as side effect.
#' @return Character vector containing the cran comments, which are written to
#' cran-comments.md (see Note).
#' @export
provide_cran_comments <- function(check_log = NULL,
                                  path = ".",
                                  initial = FALSE,
                                  travis_session_info = NULL,
                                  name = NA) {
    if (is.na(name)) {
        name <- tryCatch({
            maintainer <- desc::desc_get_author(role = "cre", file = path)
            # NOTE: a person object is a strange thing, we seem to need `$`
            # here.
            paste(maintainer$given, collapse = " ")
        },
        error = function(e) return(name)
        )
    }
    pkg <- devtools::as.package(path)
    comments_file <- file.path(pkg[["path"]], "cran-comments.md")
    session <- utils::sessionInfo()
    here <- c("",
              session[["R.version"]][["version.string"]],
              paste0("Platform: ", session[["platform"]]),
              paste0("Running under: ", session[["running"]]))
    if (file.exists(file.path(pkg[["path"]], "NEWS.md"))) {
        news <- get_news(path = path)
    } else {
        news <- "\nXXX: State your changes and consider using a NEWS.md\n\n"
    }
    if (! is.null(check_log)) {
        check <- parse_check_results(file.path(path, check_log))
        check_output <- utils::capture.output(print.check_results(check),
                                               type = "message")[2]
    } else {
        check_output  <- "ERROR: No check log given!"
    }
    comments <- c("Dear CRAN Team,\n")
    if (isTRUE(initial)) {
        comments <- c(comments, "this is the initial commit of package '",
                      pkg$package, "'.\n\n", "XXX: Describe what it does.\n\n",
                      "Please consider uploading it to CRAN.\n")

    } else {
        comments <- c(comments, "this is a resubmission of package '",
                      pkg$package, "'. I have added the following changes:\n",
                      news,
                      "Please upload to CRAN.\n")
    }
    comments <- c(comments, "Best, ", name, "\n\n# Package ", pkg$package, " ",
                  pkg$version, "\n## Test  environments ", "\n",
                  "- ", paste(here[here != ""], collapse = "\n  "),
                  "\n")
    if (! is.null(travis_session_info)) {
        if (identical(travis_session_info, "travis-cli")) {
            travis_session_info <- tryCatch(travis_cli(path),
                                            error = function(e) return(NULL))
        }
        travis_session_info <- unlist(strsplit(travis_session_info, "\n"))
        comments <- c(comments, "- ",
                      paste(travis_session_info[travis_session_info != ""],
                            collapse = "\n  "), "\n")
    }
    comments <- c(comments, "- win-builder (devel)", "\n",
                  "\n## R CMD check results\n", check_output, "\n")
    if (! as.logical(file.access(".", mode = 2))) # see ?file.acces return/note
        writeLines(comments, con = comments_file, sep = "")
    return(invisible(comments))
}

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

#' Create a Git Tag Based on the Current Version Number
#'
#' This is basically the same as \command{git tag -a T -m M} where T is the
#' version read from the package's DESCRIPTION file and M is given by
#' \code{message} (see below).
#' @param path Path to the package.
#' @param tag_uncommited Tag if there are uncommitted changes?
#' @param message The tag message to be used.
#' @return \code{\link{FALSE}} or the value of
#' \code{\link[git2r:tag]{git2r::tag}}.
#' @export
git_tag <- function(path = ".", tag_uncommited = FALSE,
                    message = "CRAN release") {
    status <- FALSE
    root <- tryCatch(rprojroot::find_root(rprojroot::is_r_package, path = path),
                     error = function(e) return(path))
    version <- desc::desc_get_version(path)
    if (! is_git_clone(root))
        warn_and_stop("Not a git repository.")
    if (is_git_uncommitted(root) && ! isTRUE(tag_uncommited))
        warn_and_stop("Uncommited changes, aborting.")
    repo <- git2r::repository(root)
    tags <- git2r::tags(repo)
    is_first_tag <- length(tags) == 0
    if (! is_first_tag) {
        old_tag_names <- sapply(tags,
                                function(tag) return(methods::slot(tag,
                                                                   "name")))
        old_versions <- sub("^v", "", old_tag_names)
        description_version_is_newer <-
            vapply(strip_off_attributes(old_versions),
                   function(x)
                       utils::compareVersion(x,
                                             as.character(version)) < 0,
                   logical(1)
                   )
    }
    if (is_first_tag || all(description_version_is_newer)) {
        status <- git2r::tag(repo, as.character(version), message)
    } else {
        future_versions <- old_versions[! description_version_is_newer]
        warn_and_stop(paste0("File DESCRIPTION has version ", version,
                             ", but I found ",
                             strip_off_attributes(future_versions),
                             " in the git repository's tags."
                             ))

    }
    return(status)
}


#' Add a github URL to File DESCRIPTION
#'
#" When writing packages, I often forget to add the appropriate github URL.
#'
#' The URL is constructed by the package's name as read from it's file
#' DESCRIPTION, and the username returned by
#' \code{\link[whoami:gh_username]{whoami::gh_username}}.
#' \code{\link[whoami:gh_username]{whoami::gh_username}} allows for a fallback,
#' this is given by default_gh_user. You can specify \code{default_gh_user =
#' NA}, to try to retrieve the username by searching remotes on github if the
#' package is a git repository. We do not use
#' \code{\link[git2r:config]{git2r::config}} since there's no way to make sure
#' the configured git user name, locally or globally, is a github username.
#'
#' @param path The path to the package.
#' @param default_gh_user See details.
#' @param normalize Passed to
#' \code{\link[desc:desc_set_urls]{desc::desc_set_urls}}.
#' @return \code{\link[base:invisible]{Invisibly} \link[base:logical]{TRUE}} if
#' adding a github URL, \code{\link[base:logical]{FALSE}} otherwise.
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
        git_url <- get_github_url(get_remote_url(path))
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

#' Git Add All Changes and Commit
#'
#' The same as git commit -am"M", where M is the \code{message}.
#' @param path The path to the repository.
#' @param message The commit message to use.
#' @param untracked Add untracked files before commiting?
#' @param ... Arguments passed to \code{\link[git2r:status]{git2r::status}}.
#' @return The return value of \code{\link[git2r:commit]{git2r::commit}}.
#' @export
git_add_commit <- function(path, message = "Uncommented Changes: Backing Up",
                           untracked = FALSE, ...) {
    repository <- git2r::repository(path = path)
    # TODO: I get strange results for repositories created with devtools,
    # unstaged files disappear when passing 'untracked' = TRUE to
    # git2r::status().
    git_status <- git2r::status(repository, ...)
    files <- unlist(git_status[["unstaged"]])
    if (isTRUE(untracked)) files <- c(files, unlist(git_status[["untracked"]]))
    tryCatch(git2r::add(repository, files),
             error = function(e) warning("Nothing added."))
    res <- git_commit(repository = repository, commit_message = message)
    return(res)

}

#' Provide File \command{make.R}
#'
#' @param path Where to create the file.
#' @param Rbuildignore Add the file to .Rbuildignore under the given
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
#' \cr I want these to be marked in lintr's output.
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
#' print(lints, invert = FALSE)
#' print(lints, invert = TRUE)
print.lints <- function(x, sort = TRUE, invert = FALSE,
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
    if (has_lints <- length(x) > 0) invisible(lapply(x, print))
    return(invisible(x))
}
