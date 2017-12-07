# verbatim copies of internals and dependencies of internals from devtools
# 1.13.3 but with "#' @export" tags removed.
has_description <- function(path) {
  file.exists(file.path(path, 'DESCRIPTION'))
}

strip_slashes <- function(x) {
  x <- sub("/*$", "", x)
  x
}

is_root <- function(path) {
  identical(path, dirname(path))
}

is_dir <- function(x) file.info(x)$isdir

#' Is the object a package?
#'
#' @keywords internal
is.package <- function(x) inherits(x, "package")


#' Coerce input to a package.
#'
#' Possible specifications of package:
#' \itemize{
#'   \item path
#'   \item package object
#' }
#' @param x object to coerce to a package
#' @param create only relevant if a package structure does not exist yet: if
#'   \code{TRUE}, create a package structure; if \code{NA}, ask the user
#'   (in interactive mode only)
#' @keywords internal
as.package <- function(x = NULL, create = NA) {
  if (is.package(x)) return(x)

  x <- package_file(path = x)
  load_pkg_description(x, create = create)
}

#' Add a file to \code{.Rbuildignore}
#'
#' \code{.Rbuildignore} has a regular expression on each line, but it's
#' usually easier to work with specific file names. By default, will (crudely)
#' turn a filename into a regular expression that will only match that
#' path. Repeated entries will be silently removed.
#'
#' @param pkg package description, can be path or package name.  See
#'   \code{\link{as.package}} for more information
#' @param files Name of file.
#' @param escape If \code{TRUE}, the default, will escape \code{.} to
#'   \code{\\.} and surround with \code{^} and \code{$}.
#' @return Nothing, called for its side effect.
#' @aliases add_build_ignore
#' @family infrastructure
#' @keywords internal
use_build_ignore <- function(files, escape = TRUE, pkg = ".") {
  pkg <- as.package(pkg)

  if (escape) {
    files <- paste0("^", gsub("\\.", "\\\\.", files), "$")
  }

  path <- file.path(pkg$path, ".Rbuildignore")
  union_write(path, files)

  invisible(TRUE)
}

use_directory <- function(path, ignore = FALSE, pkg = ".") {
  pkg <- as.package(pkg)
  pkg_path <- file.path(pkg$path, path)

  if (file.exists(pkg_path)) {
    if (!is_dir(pkg_path)) {
      stop("`", path, "` exists but is not a directory.", call. = FALSE)
    }
  } else {
    message("* Creating `", path, "`.")
    dir.create(pkg_path, showWarnings = FALSE, recursive = TRUE)
  }

  if (ignore) {
    message("* Adding `", path, "` to `.Rbuildignore`.")
    use_build_ignore(path, pkg = pkg)
  }

  invisible(TRUE)
}

#' Check that the version of an imported package satisfies the requirements
#'
#' @param dep_name The name of the package with objects to import
#' @param dep_ver The version of the package
#' @param dep_compare The comparison operator to use to check the version
#' @keywords internal
check_dep_version <- function(dep_name, dep_ver = NA, dep_compare = NA) {
  if (!requireNamespace(dep_name, quietly = TRUE)) {
    stop("Dependency package ", dep_name, " not available.")
  }

  if (xor(is.na(dep_ver), is.na(dep_compare))) {
    stop("dep_ver and dep_compare must be both NA or both non-NA")

  } else if(!is.na(dep_ver) && !is.na(dep_compare)) {

    compare <- match.fun(dep_compare)
    if (!compare(
      as.numeric_version(getNamespaceVersion(dep_name)),
      as.numeric_version(dep_ver))) {

      warning("Need ", dep_name, " ", dep_compare,
        " ", dep_ver,
        " but loaded version is ", getNamespaceVersion(dep_name))
    }
  }
  return(TRUE)
}

is_installed <- function(pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

#' Parse package dependency strings.
#'
#' @param string to parse. Should look like \code{"R (>= 3.0), ggplot2"} etc.
#' @return list of two character vectors: \code{name} package names,
#'   and \code{version} package versions. If version is not specified,
#'   it will be stored as NA.
#' @keywords internal
parse_deps <- function(string) {
  if (is.null(string)) return()
  stopifnot(is.character(string), length(string) == 1)
  if (grepl("^\\s*$", string)) return()

  pieces <- strsplit(string, "[[:space:]]*,[[:space:]]*")[[1]]

  # Get the names
  names <- gsub("\\s*\\(.*?\\)", "", pieces)
  names <- gsub("^\\s+|\\s+$", "", names)

  # Get the versions and comparison operators
  versions_str <- pieces
  have_version <- grepl("\\(.*\\)", versions_str)
  versions_str[!have_version] <- NA

  compare  <- sub(".*\\((\\S+)\\s+.*\\)", "\\1", versions_str)
  versions <- sub(".*\\(\\S+\\s+(.*)\\)", "\\1", versions_str)

  # Check that non-NA comparison operators are valid
  compare_nna   <- compare[!is.na(compare)]
  compare_valid <- compare_nna %in% c(">", ">=", "==", "<=", "<")
  if(!all(compare_valid)) {
    stop("Invalid comparison operator in dependency: ",
      paste(compare_nna[!compare_valid], collapse = ", "))
  }

  deps <- data.frame(name = names, compare = compare,
    version = versions, stringsAsFactors = FALSE)

  # Remove R dependency
  deps[names != "R", ]
}

suggests_dep <- function(pkg) {

  suggests <- read_dcf(system.file("DESCRIPTION", package = "devtools"))$Suggests
  deps <- parse_deps(suggests)

  found <- which(deps$name == pkg)[1L]

  if (!length(found)) {
     stop(sQuote(pkg), " is not in Suggests: for devtools!", call. = FALSE)
  }
  deps[found, ]
}


can_overwrite <- function(path) {
  name <- basename(path)

  if (!file.exists(path)) {
    TRUE
  } else if (interactive() && !yesno("Overwrite `", name, "`?")) {
    TRUE
  } else {
    FALSE
  }
}

render_template <- function(name, data = list()) {
  path <- system.file("templates", name, package = "devtools")
  template <- readLines(path)
  whisker::whisker.render(template, data)
}

github_info <- function(path = ".", remote_name = NULL) {
  if (!uses_github(path))
    return(github_dummy)

  r <- git2r::repository(path, discover = TRUE)
  r_remote_urls <- grep("github", remote_urls(r), value = TRUE)

  if (!is.null(remote_name) && !remote_name %in% names(r_remote_urls))
    stop("no github-related remote named ", remote_name, " found")

  remote_name <- c(remote_name, "origin", names(r_remote_urls))
  x <- r_remote_urls[remote_name]
  x <- x[!is.na(x)][1]

  github_remote_parse(x)
}

add_desc_package <- function(pkg = ".", field, name) {
  pkg <- as.package(pkg)
  desc_path <- file.path(pkg$path, "DESCRIPTION")

  desc <- read_dcf(desc_path)
  old <- desc[[field]]
  if (is.null(old)) {
    new <- name
    changed <- TRUE
  } else {
    if (!grepl(paste0('\\b', name, '\\b'), old)) {
      new <- paste0(old, ",\n    ", name)
      changed <- TRUE
    } else {
      changed <- FALSE
    }
  }
  if (changed) {
    desc[[field]] <- new
    write_dcf(desc_path, desc)
  }
  invisible(changed)
}

github_remote_parse <- function(x) {
  if (length(x) == 0) return(github_dummy)
  if (!grepl("github", x)) return(github_dummy)

  if (grepl("^(https|git)", x)) {
    # https://github.com/hadley/devtools.git
    # https://github.com/hadley/devtools
    # git@github.com:hadley/devtools.git
    re <- "github[^/:]*[/:]([^/]+)/(.*?)(?:\\.git)?$"
  } else {
    stop("Unknown GitHub repo format", call. = FALSE)
  }

  m <- regexec(re, x)
  match <- regmatches(x, m)[[1]]
  list(
    username = match[2],
    repo = match[3],
    fullname = paste0(match[2], "/", match[3])
  )
}

uses_git <- function(path = ".") {
  !is.null(git2r::discover_repository(path, ceiling = 0))
}

github_dummy <- list(username = "<USERNAME>", repo = "<REPO>", fullname = "<USERNAME>/<REPO>")

remote_urls <- function(r) {
  remotes <- git2r::remotes(r)
  stats::setNames(git2r::remote_url(r, remotes), remotes)
}

uses_github <- function(path = ".") {
  if (!uses_git(path))
    return(FALSE)

  r <- git2r::repository(path, discover = TRUE)
  r_remote_urls <- git2r::remote_url(r)

  any(grepl("github", r_remote_urls))
}

read_dcf <- function(path) {
  fields <- colnames(read.dcf(path))
  as.list(read.dcf(path, keep.white = fields)[1, ])
}

write_dcf <- function(path, desc) {
  desc <- unlist(desc)
  # Add back in continuation characters
  desc <- gsub("\n[ \t]*\n", "\n .\n ", desc, perl = TRUE, useBytes = TRUE)
  desc <- gsub("\n \\.([^\n])", "\n  .\\1", desc, perl = TRUE, useBytes = TRUE)

  starts_with_whitespace <- grepl("^\\s", desc, perl = TRUE, useBytes = TRUE)
  delimiters <- ifelse(starts_with_whitespace, ":", ": ")
  text <- paste0(names(desc), delimiters, desc, collapse = "\n")

  # If the description file has a declared encoding, set it so nchar() works
  # properly.
  if ("Encoding" %in% names(desc)) {
    Encoding(text) <- desc[["Encoding"]]
  }

  if (substr(text, nchar(text), 1) != "\n") {
    text <- paste0(text, "\n")
  }

  cat(text, file = path)
}

parse_check_results <- function(path) {
  lines <- paste(readLines(path, warn = FALSE), collapse = "\n")

  # Strip off trailing NOTE and WARNING messages
  lines <- gsub("^NOTE: There was .*\n$", "", lines)
  lines <- gsub("^WARNING: There was .*\n$", "", lines)

  pieces <- strsplit(lines, "\n\\* ")[[1]]

  structure(
    list(
      errors =   pieces[grepl("... ERROR", pieces, fixed = TRUE)],
      warnings = pieces[grepl("... WARN", pieces, fixed = TRUE)],
      notes =    pieces[grepl("... NOTE", pieces, fixed = TRUE)]
    ),
    path = path,
    class = "check_results"
  )
}

print.check_results <- function(x, ...) {
  message("R CMD check results")
  message(summarise_check_results(x))

  cat(format(x), "\n", sep = "")
  invisible(x)
}

summarise_check_results <- function(x, colour = FALSE) {
  n <- lapply(x, length)
  paste0(
    show_count(n$errors, "error ", "errors", colour && n$errors > 0), " | ",
    show_count(n$warnings, "warning ", "warnings", colour && n$warnings > 0), " | ",
    show_count(n$notes, "note ", "notes")
  )
}

show_count <- function(n, singular, plural, is_error = FALSE) {
  out <- paste0(n, " ", ngettext(n, singular, plural))
  if (is_error && requireNamespace("crayon", quietly = TRUE)) {
    out <- crayon::red(out)
  }
  out
}

package_file <- function(..., path = ".") {
  if (!is.character(path) || length(path) != 1) {
    stop("`path` must be a string.", call. = FALSE)
  }
  path <- strip_slashes(normalizePath(path, mustWork = FALSE))

  if (!file.exists(path)) {
    stop("Can't find '", path, "'.", call. = FALSE)
  }
  if (!file.info(path)$isdir) {
    stop("'", path, "' is not a directory.", call. = FALSE)
  }

  # Walk up to root directory
  while (!has_description(path)) {
    path <- dirname(path)

    if (is_root(path)) {
      stop("Could not find package root.", call. = FALSE)
    }
  }

  file.path(path, ...)
}
