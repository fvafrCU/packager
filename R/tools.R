#' Check for NEWS.md being up to date
#' 
#' Compare your NEWS.md file to the 'Version' entry in DESCRIPTION.
#' @param path The directory to search. 
#' @return \code{TRUE} if NEWS.md matches DESCRIPTION, throws an error
#' otherwise.
#' @export
check_news <- function(path = ".") {
    root <- rprojroot::find_root(path = path, rprojroot::is_r_package)
    description <- readLines(file.path(root, "DESCRIPTION"))
    version <- grep("^Version: ", description, value = TRUE)
    version_number <- trimws(strsplit(version, split = ":")[[1]][2])
    package <- grep("^Package: ", description, value = TRUE)
    package_name <- trimws(strsplit(package, split = ":")[[1]][2])
    news.md <- readLines(file.path(root, "NEWS.md"))
    devel_versions <- grep("[0-9]+\\.[0-9]+\\.[0-9]+\\.9000", news.md, 
                           value = TRUE)
    if (length(devel_versions) > 0) {
        devel_numbers <- sapply(devel_versions, 
                                function(x) strsplit(x, split = " ")[[1]][3])
        extra_devels <- setdiff(devel_numbers, version_number)
        if (length(extra_devels) > 0) {
            stop(paste("\nFound unmatched devel version: ", extra_devels)) 
        }
        
    }
    is_covered <- any(grepl(paste("^#", package_name, version_number), news.md)) 
    if (! is_covered) {
        stop("Version ", version_number, " not covered!")
    } else {
        return(TRUE)
    }
}

#' Check for Code Tags 
#' 
#' You hopefully use code tags
#' (see \href{PEP 350}{https://www.python.org/dev/peps/pep-0350/ for example}.
#' This functions searches for files under a directory containing such tags.
#' @param path The directory to search. 
#' @param exclude Passed to \code{link{grep}}.
#' @param pattern The pattern to search for. 
#' @return A character vector of hits.
#' @export
#' @examples
#' codes <- system.file("R", package = "packager")
#' check_codetags(codes)
check_codetags <- function(path = ".", exclude = ".*\\.tar\\.gz$", 
                           pattern =  "XXX:|FIXME:|TODO:") {
    return(grep_directory(path = path, exclude = exclude, pattern =  pattern))
}

#' Provide a Template For Your Comments To CRAN
#' 
#' Devtools' \code{\link{release}} reads a file \emph{cran-comments.md}. This
#' function provides a template based on your R version and your check log.
#' @param path The path to the package.
#' @param initial Is this an initial release?
#' @param check_log Path to the check log relative to \code{path}. Typically 
#' file.path("log", "dev_check.Rout").
#' @param travis_session_info Travis session info, search for\cr   
#' "$ Rscript -e 'sessionInfo()'" \cr 
#' in the raw log of the travis build and copy the
#' following three lines. This could read\cr 
#' travis_session_info <- c("\cr 
#'                     R version 3.4.0 (2017-04-21)\cr 
#'                     Platform: x86_64-pc-linux-gnu (64-bit)\cr 
#'                     Running under: Ubuntu precise (12.04.5 LTS) \cr 
#'                     ")\cr 
#' Set to `travis-cli` to retrieve Session info automatically if your system is 
#' set up to use \url{https://github.com/travis-ci/travis.rb}.
#' @param name The name to sign with.
#' @note This function writes to disk as side effect.
#' @return Character vector containing the cran comments, which are written to
#' cran-comments.md (see Note).
#' @export
provide_cran_comments <- function(check_log = NULL,
                                  path = ".",
                                  initial = FALSE,
                                  travis_session_info = NULL,
                                  name = "Dominik") {
    pkg <- devtools::as.package(path)
    comments_file = file.path(pkg[["path"]], "cran-comments.md")
    session <- utils::sessionInfo()
    here <- c("",
              session[["R.version"]][["version.string"]],
              paste0("Platform: ", session[["platform"]]),
              paste0("Running under: ", session[["running"]]))
    if (file.exists(file.path(pkg[["path"]], "NEWS.md"))) {
        news <- get_news(path = path)
    } else {
        news <- "\nXXX: State your changes and consider using a NEWS.md\n\n"
    }
    if (! is.null(check_log)) {
        check <- parse_check_results(file.path(path, check_log))
        check_output <- utils::capture.output(print.check_results(check), 
                                               type = "message")[2]
    } else {
        check_output  <- "ERROR: No check log given!"
    }
    comments <- c("Dear CRAN Team,\n")
    if (isTRUE(initial)) {
        comments <- c(comments, "this is the initial commit of package '", 
                      pkg$package, "'.\n\n", "XXX: Describe what it does.\n\n",
                      "Please consider uploading it to CRAN.\n")

    } else {
        comments <- c(comments, "this is a resubmission of package '", 
                      pkg$package, "'. I have added the following changes:\n", 
                      news,
                      "Please upload to CRAN.\n")
    }
    comments <- c(comments, "Best, ", name, "\n\n# Package ", pkg$package," ", 
                  pkg$version, "\n## Test  environments ", "\n",
                  "- ", paste(here[here != ""], collapse = "\n  "), 
                  "\n")
    if (! is.null(travis_session_info)) {
        if (length(travis_session_info) == 1 && 
            travis_session_info == "travis-cli") {
            r <- git2r::repository(path, discover = TRUE)
            travis_repo <- sub("https://github.com/", "", 
                               grep("github", value = TRUE, git2r::remote_url(r)))
            travis_log <- system2("sudo", paste("travis logs --repo", travis_repo), 
                                  stdout = TRUE)
            k <- grep("sessionInfo()", travis_log)
            travis_session_info <- travis_log[(k + 1): (k + 3)]
        }
        travis_session_info <- unlist(strsplit(travis_session_info, "\n"))
        comments <- c(comments, "- ", 
                      paste(travis_session_info[travis_session_info != ""], 
                            collapse = "\n  "), "\n")
    }
    comments <- c(comments, "- win-builder (devel)", "\n", 
                  "\n## R CMD check results\n", check_output, "\n")
    if (! as.logical(file.access(".", mode = 2))) # see ?file.acces return/note
        writeLines(comments, con = comments_file, sep = "")
    return(invisible(comments))
}

use_bsd2clause_license <- function (path = ".") {
    pkg <- devtools::as.package(path)
    license = list(License = "BSD_2_clause + file LICENSE")
    res <- document::alter_description_file(path = path, substitution = license)
    author <- unlist(eval(parse(text = pkg["authors@r"])))
    copyright_holder <- paste(author[["given"]], author[["family"]])
    cat("YEAR: ", format(Sys.Date(), "%Y"), "\n", 
        "COPYRIGHT HOLDER: ", copyright_holder, 
        sep = "", file = file.path(pkg[["path"]], "LICENSE"))
    return(invisible(NULL))
}


##   if (interactive()) setwd(dirname(getwd()))
##   options(warn = 1) # make warnings appear immediately
##   
##   devtools::load_all(quiet = TRUE)
##   
##   
##   #% lintr
##   lints <- lintr::lint_package(path = ".")
##   if(interactive()) {
##       print(lints)
##   } else {
##       output_directory <- "log/lintr_output"
##       unlink(output_directory, recursive = TRUE)
##       dir.create(output_directory, recursive = TRUE)
##       lint_file <- file.path(output_directory, "lint_package.out")
##       if (length(lints) > 0) {
##           warning("found lints, see ", lint_file)
##           writeLines(unlist(lapply(lints, paste, collapse = " ")), con = lint_file)
##       } else {
##           m <- "Congratulations: no lints found."
##           message(m)
##           writeLines(m, con = lint_file)
##       }
##   }

author_at_r <- function(given, family, email) {
    author_at_r <- paste0("person(given = \"", given, 
                          "\", family = \"", family, 
                          "\", email = \"", email, 
                          "\", role = c(\"aut\", \"cre\"))")
    return(author_at_r)
}

create_package_help <- function(path = ".", 
                                title = NULL,
                                description = NULL,
                                details = NULL
                                ) {
    if (is.null(title)) title  <- "Here Goes the Title"
    if (is.null(description)) {
        description  <- paste("A description is a paragraph consisting of one",
                              "or more sentences.")
        if (is.null(details)) 
            description <- paste(description, "You may add another paragraph",
                                 "for a 'Details' section.")
    }


get_news <- function(path = ".") {
    root <- tryCatch(rprojroot::find_root(rprojroot::is_r_package, 
                                          path = path),
                     error = function(e) return(FALSE)
                     )
    if(root == FALSE) stop("Can't find the R package")
    description <- read.dcf(file.path(root, "DESCRIPTION"))
    news <- unlist(strsplit(paste(readLines("NEWS.md"), collapse = "\n"), split = "# "))
    package_pattern <- paste0("^", description[1, "Package"], " ", 
                            description[1, "Version"])
    news <- grep(package_pattern, news, value = TRUE) 
    news <- sub(paste0(package_pattern, "\n"), "", news)
    return(news)
}
    pkg <- devtools::as.package(path)
    package_roxygen_file <- file.path(pkg[["path"]], "R",
                                      paste0(pkg[["package"]], "-package.R"))
    package_roxygen_end <- c(paste0("@name ", pkg[["package"]], "-package"), 
                             paste0("@aliases ", pkg[["package"]], "-package"), 
                             "@docType package", 
                             "@keywords package")
    content <- c(strwrap(title, prefix = "#' "), "#'")
    content <- c(content, strwrap(description, prefix = "#' "), "#'")
    content <- c(content, strwrap(details, prefix = "#' "), "#'")
    content <- c(content, strwrap(package_roxygen_end, prefix = "#' "))
    content <- c(content, "NULL")
    writeLines(content, con = package_roxygen_file, sep = "\n")
    return(invisible(content))
}

update_description <- function(path = ".",
                               title = NULL,
                               description = NULL,
                               author_at_r = NULL
                               ) {
    if (is.null(author_at_r)) message("Argument 'author_at_r' is missing. ", 
                                 "Use\n\t", 
                                 'packager::sub("(email)", "\n\t\\1", author_at_r("Andreas Dominik", "Cullmann", "fvafrcu@arcor.de"))',
                                 "\nfor example")
    s <- list(Title = title, Description = description, 
              "Authors@R" = author_at_r)
    res <- document::alter_description_file(path = path, substitution = s)
    return(invisible(res))
}

set_package_info <- function(path, author_at_r = NULL, title = NULL, 
                             description = NULL, details = NULL) {
    r1 <- update_description(path = path, title = title, 
                             description = description, 
                             author_at_r = author_at_r)
    r2 <- create_package_help(path = path, title = title, 
                              description = description, details = details)
    return(invisible(list(r1, r2)))
}

grep_directory <- function(path, pattern, exclude = NULL) {
    hits <- NULL
    files <- list.files(path, full.names = TRUE, recursive = TRUE)
    if (! is.null(exclude))
        files <- grep(exclude, files, value = TRUE, invert = TRUE)
    for (f in files) {
        l <- readLines(f)
        if (any(grepl(l, pattern = pattern, perl = TRUE))) {
            found <- paste(f, sep = ": ",
                         grep(l, pattern = pattern, perl = TRUE, value = TRUE))
            hits <- c(hits, found)
        }
    }
    return(hits)
}


remove_Rproj <- function(path = rprojroot::find_root(rprojroot::is_r_package)) {
                         file_name <- list.files(path, 
                                                 pattern = ".*\\.Rproj$", 
                                                 full.names = TRUE)
                         return(unlink(file_name))
}

is_git_uncommitted <- function(path = ".") {
    r <- git2r::repository(path, discover = TRUE)
    status <- vapply(git2r::status(r), length, integer(1))
    return(any(status != 0))
}

is_git_clone <- function(path = ".") {
    is_git_clone <- ! is.null(git2r::discover_repository(path, ceiling = 0))
    return(is_git_clone)
}

warn_and_stop <- function(...) {
    cat(...)
    stop(...)
} 

git_tag <- function(path = ".", tag_uncommited = FALSE) {
    status <- TRUE
    root <- tryCatch(rprojroot::find_root(rprojroot::is_r_package),
                     error = function(e) return(path))
    if (! is_git_clone(root))
        warn_and_stop("Not a git repository.")
    if (is_git_uncommitted(root) && ! isTRUE(tag_uncommited))
        warn_and_stop("Uncommited changes. Aborting")
    repo <- git2r::repository(root)
    tags <- git2r::tags(repo)
    last_tag_number <- methods::slot(tags[[length(tags)]], "name")
    last_version_number <- sub("^v", "", last_tag_number)
    d <- readLines(file.path(root, "DESCRIPTION"))
    version <- sub("^Version: ", "", grep("^Version: ", d, value = TRUE))
    if (version != last_version_number)
        cmd <- paste("git tag -a", version)
    if (interactive()) {
        status <- system2("git", sub("^git ", "", cmd))
    } else {
        warn_and_stop("Run\n\t", cmd, "\non your system.")
    }
    return(status)
}

create_devel <- function(path = rprojroot::find_root(rprojroot::is_r_package)) {
    file_name <- file.path(path, "devel.R")
    devtools::use_build_ignore("devel.R", pkg = path)
    return(cat("devtools::load_all()\n", file = file_name))
}

provide_throw <- function(path, 
                          force = is_null_or_true(getOption("packager")[["force"]]),
                          ...) {
    devtools::use_testthat(pkg = path)
    pkg <- as.package(path)

    file <- "throw.R"
    file_path <- file.path("R", file)
    use_template(file, save_as = file_path, data = pkg, 
                 ignore = FALSE, pkg = pkg[["path"]], force = force, ...)

    suppressMessages(devtools::use_package("RUnit", type = "Suggests", 
                                           pkg = path))
    file <- "runit.R"
    file_path <- file.path("tests", file)
    use_template(file, save_as = file_path, data = pkg, 
                 ignore = FALSE, pkg = pkg[["path"]], force = force, ...)

    dir.create(file.path(pkg[["path"]], "tests", "runit"), showWarnings = FALSE)
    file <- "runit-throw.R"
    file_path <- file.path("tests", "runit", file)
    use_template(file, save_as = file_path, data = pkg, 
                 ignore = FALSE, pkg = pkg[["path"]], force = force, ...)

    file <- "test-throw.R"
    file_path <- file.path("tests", "testthat", file)
    use_template(file, save_as = file_path, data = pkg, 
                 ignore = FALSE, pkg = pkg[["path"]], force = force, ...)

    return(NULL)
}

