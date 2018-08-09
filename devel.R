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

user <- "fvafrcu"
project <- "packager"
url <- paste0("https://gitlab.com/api/v4/users/", user, "/projects/")
token <- "HbEgPMT5cG2tVe8MBvee" 




names(token) <- "PRIVATE-TOKEN"
library("httr")
r <- httr::GET(url, httr::add_headers(.headers = token))
projects <- httr::content(r)
my_project <- projects[sapply(projects, function(x) getElement(x, "name") == project)][[1]]


url <- paste0("https://gitlab.com/api/v4/projects/", my_project[["id"]], "/jobs/")
r <- httr::GET(url, httr::add_headers(.headers = token))
jobs <- httr::content(r)
test_jobs <-  jobs[sapply(jobs, function(x) getElement(x, "name") == "test")]
last_test_jobs_url <- test_jobs[[1]][["web_url"]]
r <- httr::GET(paste0(last_test_jobs_url, "/raw"))
job <- httr::content(r)
