devtool::use_dev_version()
devtools::load_all(".")
packager::provide_cran_comments(check_log = "log/check.Rout", travis_session_info = "travis-cli")
if (FALSE) {
    set_package_info(path = ".", title = "Helps Me Build Packages", 
                     description = paste("Helper functions for a build system,",
                                         "heavily borrowing from and extending", 
                                         "package `devtools 1.13.3`."),
                     details = paste("I find devtools very helpful but it lacks",
                                     "some functionality I would want while", 
                                     "creating and building a package.\n", 
                                     "Maybe \\pkg{packager} should be two packages."),
                     force = is_force()
                     )
}

if (FALSE) {
    devtools::load_all(".")
    release <- function(path = ".", stop_on_git = TRUE, force = FALSE) {
    if (uses_git(path) && isTRUE(stop_on_git)) {
        if (is_git_uncommitted(path = path) )
            throw("You have uncommitted changes.")
        if (! git_sync_status(path = path)) 
            throw("Your repository is not synced with it's upstream.")
    }
    if (yesno("Ready to submit?")) { ## FIXME force
        throw("Aborting on user request.")
    } else {
        csu <- "http://xmpalantir.wu.ac.at/cransubmit/index2.php"
        built_path <- build_cran(path, args = NULL)
        upload_cran(pkg = path, built_path = built_path, 
                    cran_submission_url = csu)
        if (uses_git(path)) {
            message("Don't forget to tag commit ", git2r::reflog(r)[[1]]@sha, 
                    " as ", desc::desc_get_version(), 
                    ", once package is on CRAN.")
        }
        return(invisible(NULL))
    }
}

# make
fml <- fakemake::provide_make_list("package")
run <- fakemake::make("cleanr", fml, verbose = FALSE, force = TRUE)
run <- fakemake::make("check", fml, verbose = FALSE, force = TRUE)
fakemake::visualize(fml)
