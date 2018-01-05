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

test_is_null_or_true <- function() {
    result <- packager:::is_null_or_true(NULL)
    RUnit::checkTrue(result)
    result <- packager:::is_null_or_true(TRUE)
    RUnit::checkTrue(result)
    result <- packager:::is_null_or_true("foobar")
    RUnit::checkTrue(! result)
}

test_is_force <- function() {
    result <- packager:::is_force()
    RUnit::checkTrue(result)
}

test_get_news <- function() {
    path <- provide_fake_package()
    on.exit(unlink(path, recursive = TRUE))
    devtools::use_news_md(path)
    result <- packager:::get_news(path)
    expectation <- "\n* Added a `NEWS.md` file to track changes to the package.\n\n\n"
    RUnit::checkIdentical(result, expectation)
}

test_grep_directory <- function() {
    path <- system.file("runit_tests", package = "packager")
    result <- unlist(strsplit(packager:::grep_directory(path = path, 
                                             pattern = "runit_tests.*packager"), 
                              split = ":"))[2]
    expectation <- "     path <- system.file(\"runit_tests\", package = \"packager\")"
    RUnit::checkIdentical(result, expectation)
}

test_git <- function() {
    path <- provide_fake_package()
    on.exit(unlink(path, recursive = TRUE))
    RUnit::checkTrue(! packager:::is_git_clone(path),
                          msg = "Not a git repo.")
    RUnit::checkException(packager:::is_git_uncommitted(path),
                          msg = "Not a git repo, no commits.")
    packager:::use_git(path = path)
    RUnit::checkTrue(packager:::is_git_clone(path))
    RUnit::checkTrue(! packager:::is_git_uncommitted(path),
                     msg = "All should be commited.")
    cat("foo", file = file.path(path, "DESCRIPTION"), append = TRUE)
    RUnit::checkTrue(packager:::is_git_uncommitted(path),
                     msg = "Uncommited changes.")
}

test_warn_and_stop <- function() 
    RUnit::checkException(packager:::warn_and_stop("foo"))


test_url <- function() {
    #% get_remote_url
    ##% no such url
    expectation <- NULL
    result <- packager:::get_remote_url()
    RUnit::checkTrue(identical(result, expectation))

    path <- file.path(tempdir(), "fakePackge")
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE))
    repo <- git2r::init(path)  
    git2r::remote_add(repo, "github", "https://github.com/fvafrCU/fakePackage")
    git2r::remote_add(repo, "local", "~/git/fakePackage")
    ##% package's url
    expectation <- "https://github.com/fvafrCU/fakePackage"
    result <- grep(value = TRUE, "github", packager:::get_remote_url(path))
    RUnit::checkIdentical(result, expectation)

    #% get_github_url
    ##% no such url
    expectation <- NULL
    url <- packager:::get_remote_url()
    result <- packager:::get_github_url(url)
    RUnit::checkIdentical(result, expectation)

    ##% no github url
    expectation <- NULL
    url <- packager:::get_remote_url(path)
    x <- grep(value = TRUE, "github", url, invert = TRUE)
    result <- packager:::get_github_url(x)
    RUnit::checkIdentical(result, expectation)

    ##% package's url
    expectation <- "https://github.com/fvafrCU/fakePackage"
    url <- packager:::get_remote_url(path)
    result <- packager:::get_github_url(url)
    RUnit::checkIdentical(result, expectation)

    ##% multiple url
    git2r::remote_add(repo, "github1", "https://github.com/fvafrCU/fakepackage")

    ###% return all url
    expectation <- c("https://github.com/fvafrCU/fakePackage", 
                     "https://github.com/fvafrCU/fakepackage")
    url <- packager:::get_remote_url(path)
    result <- packager:::get_github_url(url)
    RUnit::checkIdentical(result, expectation)

    ###% return first url
    expectation <- c("https://github.com/fvafrCU/fakePackage")
    url <- packager:::get_remote_url(path)
    result <- packager:::get_github_url(url, return_only_one = TRUE)
    RUnit::checkIdentical(result, expectation)

    ###% throw on multiple
    RUnit::checkException(packager:::get_github_url(url, return_only_one = TRUE, 
                                                    force = FALSE))
}


test_travis <- function() {
    path <- file.path(tempdir(), "fakePackge")
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE))
    repo <- git2r::init(path)  
    git2r::remote_add(repo, "github", "https://github.com/fvafrCU/packager")
    if (! Sys.info()[["nodename"]] %in% c("h5", "h6")) {
        RUnit::checkException(packager:::travis_cli(path))
    } else {
        RUnit::checkIdentical(class(packager:::travis_cli(path)), "character")
    }
}

