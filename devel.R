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
is_check <- function(x) {
    is_check_stage <- identical(getElement(x, "stage"), "check")
    is_check_job <- identical(getElement(x, "name"), "check")
    is_check <- is_check_stage && is_check_job
    return(is_check)
}

is_my_name <- function(x, name) return(getElement(x, "name") == name)

#' Read a Gitlab Check Log
#' 
#' For a given user's project, the last log for jobs for name and stage "check"
#' will be read. This is assumed to be the output of R CMD check,
#' rcmdcheck::rcmdcheck(), devtools::check() or the like.
#' @param user The user's name on gitlab.
#' @param project The project's name on gitlab.
#' @param private_token The user's private token on gitlab.
#' @param ... Arguments passed to \code{\link[httr:GET]{httr::GET}}.
#' @return A character vector containing the lines of the gitlab log.
#' @examples
#' if (Sys.info()[["nodename"]] == "fvafrdebianCU") {
#'     j <- get_gitlab_log(user = "fvafrcu", project = "packager", 
#'                         private_token = "HbEgPMT5cG2tVe8MBvee", 
#'                         httr::use_proxy("10.127.255.17", 8080))
#' } else {
#'     j <- get_gitlab_log(user = "fvafrcu", project = "packager", 
#'                         private_token = "HbEgPMT5cG2tVe8MBvee")
#' }
#' 
#' cat(j, sep = "\n")
get_gitlab_log <- function(user, project, private_token, ...) {
    url <- paste0("https://gitlab.com/api/v4/users/", user, "/projects/")
    names(private_token) <- "PRIVATE-TOKEN"
    r <- httr::GET(url, httr::add_headers(.headers = private_token), ...)
    projects <- httr::content(r)
    my_project <- projects[sapply(projects, is_my_name, project)][[1]]
    url <- paste0("https://gitlab.com/api/v4/projects/", 
                  my_project[["id"]], "/jobs/")
    r <- httr::GET(url, httr::add_headers(.headers = private_token), ...)
    jobs <- httr::content(r)
    check_jobs <-  jobs[sapply(jobs, is_check)]
    last_check_jobs_url <- check_jobs[[1]][["web_url"]]
    r <- httr::GET(paste0(last_check_jobs_url, "/raw"), ...)
    job <- httr::content(r)
    job <- unlist(strsplit(job, split = "\n"))
    return(job)
}




write_info <- function(prefix = "=== packager info:") {
    obj <- Sys.info()
    dev_null <- capture.output(info <- deparse(dput(obj)))
    cat(paste0(prefix, info), sep = "\n")
    return(invisible(NULL))
}

write_rcmdcheck <- function(prefix = "=== packager rcmdcheck:") {
    obj <- rcmdcheck::rcmdcheck(".")
    dev_null <- capture.output(info <- deparse(dput(obj)))
    cat(paste0(prefix, info), sep = "\n")
    return(invisible(NULL))
}

log_check <- function() {
    write_info()
    write_rcmdcheck()
    return(invisible(NULL))
}

grep_log <- function(file, pattern) {
    lines <- readLines(file)
    matching_lines <- grep(pattern, lines, value = TRUE)
    stripped_lines <- sub(pattern, "", matching_lines)
    return(stripped_lines)
}
eval_from_log <- function(...) {
    return(eval(parse(text = grep_log(...))))
}


sink_file <- tempfile()
sink(sink_file)
print(mtcars)
log_check()
sink()
grep_log(sink_file, pattern = "=== packager info:")
info <- eval_from_log(sink_file, pattern = "=== packager info:")
rcmdcheck <- eval_from_log(sink_file, pattern = "=== packager rcmdcheck:")
