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
get_gitlab_log <- function(user, project, private_token) {
    url <- paste0("https://gitlab.com/api/v4/users/", user, "/projects/")
    names(private_token) <- "PRIVATE-TOKEN"
    r <- httr::GET(url, httr::add_headers(.headers = private_token))
    projects <- httr::content(r)
    which_names <- function(x, name) return(getElement(x, "name") == project)
    my_project <- projects[sapply(projects, which_names)][[1]]
    url <- paste0("https://gitlab.com/api/v4/projects/", 
                  my_project[["id"]], "/jobs/")
    r <- httr::GET(url, httr::add_headers(.headers = private_token))
    jobs <- httr::content(r)
    test_jobs <-  jobs[sapply(jobs, function(x) getElement(x, "name") == "test")]
    last_test_jobs_url <- test_jobs[[1]][["web_url"]]
    r <- httr::GET(paste0(last_test_jobs_url, "/raw"))
    job <- httr::content(r)
    job <- unlist(strsplit(job, split = "\n"))
    return(job)
}

j <- get_gitlab_log(user = "fvafrcu", project = "packager", 
                    private_token = "HbEgPMT5cG2tVe8MBvee")
cat(j, sep = "\n")
readLines("log/check.Rout")



info <- function() {
    s <- sessionInfo()
    info <- c(s$R.version$version.string,
              paste("Platform:", s$platform),
              paste("Running under:", s$running))
    return(info)
}
cat(info(), sep = "\n")

write_info <- function() {
 cat(paste("=== INFO:", info()), sep = "\n")
 return(invisible(NULL))
}
write_info()
