devtools::load_all(".")
devtools::use_dev_version()
if (FALSE) {
packager::provide_cran_comments(check_log = "log/check.Rout", travis_session_info = "travis-cli")
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


packager::check_codetags()
# make
ml <- provide_make_list()

fakemake::make(".log.Rout", ml)
fakemake::visualize(ml)


packager::create("../cuwintest")



# ============ cyclo deps
#' log_file = file.path("~", "git", "os", "fva", "bwibw", "log", "cyclocomp.Rout") 
update_cyclocomp_deps <- function(log) {
    log <- readLines(log_file)
    if (any(grepl("packages:", log))) {
        line <- grep("packages:", log, value = TRUE)
        packages <- trimws(unlist(strsplit(strsplit(line, split = ":")[[1]][2], 
                                           split = ",")))
        update.packages(oldPkgs = packages, ask = FALSE)
    }
}

