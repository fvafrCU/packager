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


notest_cyclocomp <- function() {
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

