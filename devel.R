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
log_check()
sink()
grep_log(sink_file, pattern = "=== packager info:")
info <- eval_from_log(sink_file, pattern = "=== packager info:")
rcmdcheck <- eval_from_log(sink_file, pattern = "=== packager rcmdcheck:")
