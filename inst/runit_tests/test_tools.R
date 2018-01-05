if (interactive()) devtools::load_all()

provide_fake_package <- function() {
    tmp <- tempfile()
    dir.create(tmp)
    path <- file.path(tmp, "fakePackage")
    tryCatch(suppressMessages(devtools::create(path, quiet = TRUE)), 
                              error = identity
    )
    return(path)
}


notest_cyclocomp <- function() { # somehow fails on install...
    path <- file.path(tempdir(), "fakePackge")
    on.exit(unlink(path, recursive = TRUE))
    packager::create(path)
    result <- packager::check_cyclomatic_complexity(path)
    # news too new
    writeLines(c("# fakePackge 0.1.1", new), news_file)
    RUnit::checkTrue(packager::check_news(path))


    RUnit::checkTrue(result)
    pccc <- packager::check_cyclomatic_complexity
    RUnit::checkException(pccc(path, max_complexity = 0))
}

test_news <- function() {
    path <- file.path(tempdir(), "fakePackge")
    on.exit(unlink(path, recursive = TRUE))
    packager::create(path)
    result <- packager::check_news(path)
    RUnit::checkTrue(result)

    # Bump version
    desc::desc_bump_version(path, which = "minor")

    # unmachted devel version
    RUnit::checkException(packager::check_news(path))

    # add machting version, keep devel versoin
    news_file <- file.path(path, "NEWS.md")

    old <- readLines(news_file)
    new <- "# fakePackge 0.1.0"
    writeLines(c(new, old), news_file)
    RUnit::checkException(packager::check_news(path))

    # remove devel version
    writeLines(new, news_file)
    RUnit::checkTrue(packager::check_news(path))

    # actual version not covered in NEWS.md
    writeLines(c("# fakePackge 0.1.1"), news_file)
    RUnit::checkException(packager::check_news(path))
}

test_cran_comments <- function() {
    path <- provide_fake_package()
    on.exit(unlink(path, recursive = TRUE))
    expectation <- 
        structure(c("Dear CRAN Team,\n", "this is a resubmission of package '",
                    "fakePackage", "'. I have added the following changes:\n", 
                    "\nXXX: State your changes and consider using a NEWS.md\n\n",
                    "Please upload to CRAN.\n", "Best, ", "Andreas Dominik", 
                    "\n\n# Package ",
                    "fakePackage", " ", "0.0.0.9000", 
                    "\n## Test  environments ",
                    "\n", "- ", 
                    "R version 3.3.3 (2017-03-06)\n  Platform: x86_64-pc-linux-gnu (64-bit)\n  Running under: Debian GNU/Linux 9 (stretch)",
                    "\n", "- win-builder (devel)", "\n", "\n## R CMD check results\n",
                    "ERROR: No check log given!", "\n"), .Names = c("", "", "", "",
                    "", "", "", "given", "", "", "", "", "", "", "", "", "", "",
                    "", "", "", ""))
    expectation <- grep("^R ", expectation, value = TRUE, invert = TRUE)
    result <- packager::provide_cran_comments(path = path)
    result <- grep("^R ", result, value = TRUE, invert = TRUE)
    message(result)
    RUnit::checkIdentical(expectation, result)
}


test_git_tag <- function() {
    path <- file.path(tempdir(), "fake")
    on.exit(unlink(path, recursive = TRUE))
    devtools::create(path)

    # no repo
    RUnit::checkException(packager::git_tag(path = path))

    # initial repo
    packager:::use_git(path)
    result <- packager::git_tag(path = path)
    RUnit::checkIdentical("0.0.0.9000", methods::slot(result, "name"))
    RUnit::checkIdentical("CRAN release", methods::slot(result, "message"))
    desc::desc_bump_version("minor", file = path)
    
    # uncommitted changes
    RUnit::checkException(packager::git_tag(path = path))

    # commited changes
    r <- git2r::repository(path = path)
    packager:::git_add_commit(r)
    result <- packager::git_tag(path = path)
    RUnit::checkIdentical("0.1.0", methods::slot(result, "name"))
    RUnit::checkIdentical("CRAN release", methods::slot(result, "message"))

    # version number lower than in tags
    desc::desc_set(Version = "0.0.3", file = path)
    packager:::git_add_commit(r)
    RUnit::checkException(packager::git_tag(path = path))

}

test_githuburl <- function() {
    path <- file.path(tempdir(), "fake")
    on.exit(unlink(path, recursive = TRUE))
    devtools::create(path)
    user <- "foobar"


    #% gh_username defaults NULL, curl not working
    if (Sys.info()[["nodename"]] ==  "fvafrdebianCU") {
        # curl not working
        expectation <- FALSE
        result <- add_github_url_to_desc(path = path)
        RUnit::checkIdentical(expectation, result)
    } else {
        # FIXME: on travis??
    }
    
    #% No git remote set
    if (Sys.info()[["nodename"]] %in% c("h5", "h6")) {
        expectation <- TRUE
    } else {
        expectation <- FALSE
    }
    result <- add_github_url_to_desc(path = path, default_gh_user = NA)
    RUnit::checkIdentical(expectation, result)

    #% remote set 
    if (Sys.info()[["nodename"]] %in% c("h5", "h6")) {
        # whoami is better than remote url
        url <- paste0("https://github.com/fvafrCU/fake")
    } else {
        url <- paste0("https://github.com/", user, "/fake")
    }

    unlink(path, recursive = TRUE)
    devtools::create(path)
    repo <- git2r::init(path)  
    git2r::remote_add(repo, "github", url)
    result <- add_github_url_to_desc(path = path, default_gh_user = NA)
    RUnit::checkTrue(result)
    RUnit::checkIdentical(url, desc::desc_get_urls(path))

    #% user given
    if (Sys.info()[["nodename"]] %in% c("h5", "h6")) {
        # whoami is better than remote url
        expectation <- paste0("https://github.com/fvafrCU/fake")
    } else {
        expectation <- paste0("https://github.com/", user, "/fake")
    }
    add_github_url_to_desc(path = path, default_gh_user = user)
    result <- desc::desc_get_urls(file = path)
    RUnit::checkIdentical(expectation, result)
    readLines(file.path(path, "DESCRIPTION"))
}
