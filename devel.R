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

get_github_url(get_remote_url("~/tmp/bar"))
get_github_url(get_remote_url("~/document"))
get_github_url(get_remote_url("~/tmp/foo"))

# set github url
devtools::load_all(".")
path = "."
path = "~/document"

gh_username <- tryCatch(whoami::gh_username(fallback = gh_failed),
                        error = function(e) return(NULL))
package_dir <- basename(devtools::as.package(path)[["path"]])
package_name <- devtools::as.package(path)[["package"]]
gh_url <- get_github_url(get_remote_url(path))
desc_url <- get_github_url(desc::desc_get_urls(path))


if (is.null(gh_username)) {
    warning("Could not retrive github user name. ", 
            "Set the URL in DESCRIPTION manually!")
    manual_url <- NULL
} else {
    manual_url <- paste("https::github.com", gh_username, 
                        package_dir, 
                        sep = "/")
}

# desc_url NULL?
# desc_url NULL?

