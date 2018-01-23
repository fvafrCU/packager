devtools::use_dev_version()
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
    # release
    devtools::load_all(".")
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
}

# make
fml <- fakemake::provide_make_list("package")
fakemake::make("check", fml, verbose = FALSE)
# lintr and testthat did not work as expected!
