package <- "fvafrcu1"
root <- file.path("", "tmp")
path <- file.path(root, package)
devtools::create(path)
utils:::package.skeleton(package, path = root)


base::options(packager = list(could_force = TRUE, should_force = FALSE))
base::getOption("packager")
base::getOption("packager")[["could_force"]]
devtools::load_all(".")
packager::provide_throw()
# test logic
devtools::load_all(".")
d <- "/tmp/foo"
packager::create(path = d) 
l <- dir(d, recursive = TRUE, full.names = TRUE)
digest::sha1(c(l, sapply(l, readLines)))
#
if (FALSE) {
    packager::infect(path = ".",
                     title = "Helps Me Build Packages", 
                     author_at_r = sub("(email)", "\n\t\\1", author_at_r("Andreas Dominik", "Cullmann", "fvafrcu@arcor.de")),
                     description = "This is a set of wrappers around `devtools` and `MakefileR` and some sanity checks for developing packages.",
                     details = "I find devtools very helpful, but not helpful enough.\nSo this is my highly personalised extension." ,
                     make = FALSE, git_add_and_commit = FALSE)
}





# get github url
devtools::load_all(".")

get_remote_url <- function(path) {
    repo_failed <- "fail"
    repo <- tryCatch(git2r::repository(path),
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

url <- get_github_url(get_remote_url("~/document"))



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
add_github_url_to_desc()
