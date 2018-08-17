#' Check Usage with \pkg{codetools}' \code{\link{checkUsagePackage}}
#'
#' This is just a convenience wrapper to \code{\link{checkUsagePackage}} (which
#' needs loading of the [development version of the] package).
#' @param path Path to the package directory.
#' @return A character vector of issues found by
#' \code{\link{checkUsagePackage}}.
#' @export
check_usage <- function(path = ".") {
    root <- rprojroot::find_root(path = path, rprojroot::is_r_package)
    devtools::load_all(root)
    package_name <- as.character(desc::desc_get("Package", path))
    issues <- utils::capture.output(codetools::checkUsagePackage(package_name,
                                                                 all = TRUE))
    return(issues)
}

#' Check \verb{Cyclomatic Complexity}
#'
#' Run
#' \code{\link[cyclocomp:cyclocomp_package_dir]{cyclocomp_package_dir}} on the
#' package throwing an error when the maximum complexity is exceeded.
#' @param path The package's root directory.
#' @param max_complexity The maximum \verb{cyclomatic complexity}
#'        (which should not be exceeded).
#' @return \code{\link[base:invisible]{Invisibly} \link[base:logical]{TRUE}} if
#' maximum \verb{cyclomatic complexity} is not exceeded, throws an error
#' otherwise.
#' @export
#' @examples
#' \dontrun{
#' # download and untar sources of some archived package
#' package  <- "document"
#' root <- paste0("http://cran.r-project.org/src/contrib/Archive/", package)
#' version <- "1.0.0"
#' tarball <- paste0(paste(package, version, sep = "_"), ".tar.gz")
#' remote_tarball <- paste(root, tarball, sep = "/")
#' local_tarball <- file.path(tempdir(), tarball)
#' utils::download.file(remote_tarball, local_tarball)
#' utils::untar(local_tarball, exdir = tempdir())
#' res <- tryCatch(check_cyclomatic_complexity(path = file.path(tempdir(),
#'                                                              package)),
#'                 error = identity)
#' print(res)
#' }
check_cyclomatic_complexity <- function(path = ".", max_complexity = 10) {
    cyclocomp <- cyclocomp::cyclocomp_package_dir(path)
    too_complex <- cyclocomp[["cyclocomp"]] > max_complexity
    if (any(too_complex)) {
        hits <- cyclocomp[too_complex, "name"]
        diff <- cyclocomp[too_complex, "cyclocomp"] - max_complexity
        msg <- paste0("Exceeding maximum cyclomatic complexity of ",
                     max_complexity, " for ", hits, " by ", diff, ".")
        throw(paste(msg, collapse = "\n"))
    }
    return(invisible(TRUE))
}

#' Check for \file{NEWS.md} Being Up to Date
#'
#' Compare your \file{NEWS.md} file to the 'Version' entry in DESCRIPTION.
#' @param path The directory to search.
#' @return \code{\link[base:invisible]{Invisibly} \link[base:logical]{TRUE}} if
#' \file{NEWS.md} matches DESCRIPTION, throws an error otherwise.
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
            throw(paste("\nFound unmatched devel version: ", extra_devels))
        }
    }
    is_covered <- any(grepl(paste("^#", package_name, version_number), news.md))
    if (! is_covered) {
        throw(paste0("Version ", version_number, " not covered!"))
    } else {
        return(invisible(TRUE))
    }
}

#' Check for Code Tags
#'
#' You hopefully use code tags
#' (see \href{https://www.python.org/dev/peps/pep-0350/}{PEP 350} for example).
#' This functions searches for files under a directory containing such tags.
#' @param path The directory to search.
#' @param exclude_pattern A pattern for exclusions based on the file names.
#' Stronger than \code{include_pattern}.
#' @param include_pattern A pattern for inclusions based on the file names.
#' @param pattern The pattern to search for.
#' @return A character vector of hits.
#' @export
#' @examples
#' dir <- system.file("templates", package = "packager")
#' check_codetags(dir)
check_codetags <- function(path = ".", exclude_pattern = "\\.Rcheck/",
                           include_pattern = "\\.[Rr]$|\\.[Rr]md$",
                           pattern =  "XXX:|FIXME:|TODO:") {
    hits <- grep_directory(path = path, exclude_pattern = exclude_pattern,
                           include_pattern = include_pattern,
                           pattern =  pattern)
    return(hits)
}
