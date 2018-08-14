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


path <-"/tmp/test" 
packager::create(path)
packager::git_tag(path = path)

####
get_gitlab_info <- function(path = ".", private_token, ...) {
    if (missing(private_token)) {
        warning("You need a private token to access gitlab.")
        info <- NULL
    } else {
        url <- get_git_url(get_remote_url(path))
        print(url)
        project <- basename(url)
        user <- tolower(basename(dirname(url)))
        log <- get_gitlab_log(user = user, project = project, private_token, ...)
        info <- log
    }

    return(info)
}
get_gitlab_info()
gitlab_token <- readLines(file.path("~", ".gitlab_private_token.txt"))
get_gitlab_log(user = tolower(name), project = project, private_token = gitlab_token, httr::use_proxy("10.127.255.17", 8080))
if (Sys.info()[["nodename"]] == "fvafrdebianCU") {
    j <- get_gitlab_info(path = ".", private_token = gitlab_token, httr::use_proxy("10.127.255.17", 8080))
} else {
    j <- get_gitlab_info(private_token = gitlab_token) 
}

cat(j, sep = "\n")
