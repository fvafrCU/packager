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


devtools::load_all(".")
# release
path = "."
stop_on_git = TRUE

if (uses_git(path) && isTRUE(stop_on_git)) {
        if (is_git_uncommitted(path = path) )
            throw("You have uncommitted changes.")
        if (! git_sync_status(path = path)) 
            throw("Your repository is not synced with it's upstream.")
}
csu <- "http://xmpalantir.wu.ac.at/cransubmit/index2.php"
built_path <- build_cran(path, args = NULL)
if (yesno("Ready to submit?")) {
    throw("User request.")
} else {
    upload_cran(pkg = path, built_path = built_path, cran_submission_url = csu)
}

