#' Check for NEWS.md Being Up to Date
#'
#' Compare your NEWS.md file to the 'Version' entry in DESCRIPTION.
#' @param path The directory to search.
#' @return \code{TRUE} if NEWS.md matches DESCRIPTION, throws an error
#' otherwise.
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
            stop(paste("\nFound unmatched devel version: ", extra_devels))
        }
    }
    is_covered <- any(grepl(paste("^#", package_name, version_number), news.md))
    if (! is_covered) {
        stop("Version ", version_number, " not covered!")
    } else {
        return(TRUE)
    }
}

#' Check for Code Tags
#'
#' You hopefully use code tags
#' (see \href{PEP 350}{https://www.python.org/dev/peps/pep-0350/ for example}.
#' This functions searches for files under a directory containing such tags.
#' @param path The directory to search.
#' @param exclude Passed to \code{link{grep}}.
#' @param pattern The pattern to search for.
#' @return A character vector of hits.
#' @export
#' @examples
#' dir <- system.file("templates", package = "packager")
#' check_codetags(dir)
check_codetags <- function(path = ".", exclude = ".*\\.tar\\.gz$",
                           pattern =  "XXX:|FIXME:|TODO:") {
    return(grep_directory(path = path, exclude = exclude, pattern =  pattern))
}

#' Provide a Template for Your Comments To CRAN
#'
#' Devtools' \code{\link{release}} reads a file \emph{cran-comments.md}. This
#' function provides a template based on your R version and your check log.
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
#' @param name The name to sign with.
#' @note This function writes to disk as side effect.
#' @return Character vector containing the cran comments, which are written to
#' cran-comments.md (see Note).
#' @export
provide_cran_comments <- function(check_log = NULL,
                                  path = ".",
                                  initial = FALSE,
                                  travis_session_info = NULL,
                                  name = "Dominik") {
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
            travis_session_info <- travis_cli(path)
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
    root <- tryCatch(rprojroot::find_root(rprojroot::is_r_package),
                     error = function(e) return(path))
    version <- devtools::as.package(".")[["version"]]
    if (! is_git_clone(root))
        warn_and_stop("Not a git repository.")
    if (is_git_uncommitted(root) && ! isTRUE(tag_uncommited))
        warn_and_stop("Uncommited changes. Aborting")
    repo <- git2r::repository(root)
    tags <- git2r::tags(repo)
    is_first_tag <- length(tags) == 0
    if (! is_first_tag) {
        last_tag_number <- methods::slot(tags[[length(tags)]], "name")
        last_version_number <- sub("^v", "", last_tag_number)
    }
    if (is_first_tag || version != last_version_number)
        status <- git2r::tag(repo, version, message)
    return(status)
}


#' Add a github URL to File DESCRIPTION
#' 
#" When writing packages, I often forget to add the appropriate github URL.
#'
#' The URL is constructed by the package's name as read from it's file
#' DESCRIPTION, and the username returned by
#' \code{\link[whoami:gh_username]{whoami::gh_username}. 
#' \code{\link[whoami:gh_username]{whoami::gh_username} allows for a fallback,
#' this is given by default_gh_user. You can specify \code{default_gh_user =
#' NA}, to try to retrieve the username by searching remotes on github if the
#' package is a git repository. We do not use
#' \code{\link[git2r:config]{git2r::config} since there's no way to make sure
#' the configured git user name, locally or globally, is a github username.
#'
#' @param path The path to the package.
#' @param path default_gh_user See details.
#' @param normalize Passed to
#' \code{\link[desc:desc_set_urls]{desc::desc_set_urls}.
#' @return \code{\link[invisible]{Invsibly} \link[logical]{TRUE}} if adding a
#' github URL, \link[logical]{FALSE}} otherwise.
add_github_url_to_desc <- function(path = ".", default_gh_user = NULL, 
                                   normalize = TRUE) {
    status <- FALSE
    gh_username <- tryCatch(whoami::gh_username(fallback = default_gh_user),
                            error = function(e) return(NULL))
    desc_url <- desc::desc_get_urls(path)

    package_dir <- basename(devtools::as.package(path)[["path"]])
    package_name <- devtools::as.package(path)[["package"]]
    if (package_name != package_dir) 
        warning("The package's name and root directory differ.")
    if (is.na(gh_username)) {
        git_url <- get_github_url(get_remote_url(path))
        num_of_remotes <- length(grep(paste0(package_name, "$"), git_url))
        if (num_of_remotes == 1) {
            gh_username <- sub(paste0("^https://github.com/(.*)/", package_name, 
                                      "$"),
                               "\\1", git_url)
        } else {
            warning("Found ", num_of_remotes, 
                    " different git remotes refering to `", package_name, "`.")
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
