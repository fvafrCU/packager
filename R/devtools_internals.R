# a collection of devtools' internals from devtools 1.13.3
has_description <- function (path) {
    file.exists(file.path(path, "DESCRIPTION"))
}

strip_slashes <- function (x) {
    x <- sub("/*$", "", x)
    x
}

is_root <- function (path) {
    identical(path, dirname(path))
}

is_dir <- function (x) file.info(x)$isdir

is.package <- function (x) inherits(x, "package")

as.package <- function (x = NULL, create = NA) {
    if (is.package(x)) 
        return(x)
    x <- package_file(path = x)
    load_pkg_description(x, create = create)
}

use_build_ignore <- function (files, escape = TRUE, pkg = ".") {
    pkg <- as.package(pkg)
    if (escape) {
        files <- paste0("^", gsub("\\.", "\\\\.", files), "$")
    }
    path <- file.path(pkg$path, ".Rbuildignore")
    union_write(path, files)
    invisible(TRUE)
}

use_directory <- function (path, ignore = FALSE, pkg = ".") {
    pkg <- as.package(pkg)
    pkg_path <- file.path(pkg$path, path)
    if (file.exists(pkg_path)) {
        if (!is_dir(pkg_path)) {
            stop("`", path, "` exists but is not a directory.", 
                call. = FALSE)
        }
    }
    else {
        message("* Creating `", path, "`.")
        dir.create(pkg_path, showWarnings = FALSE, recursive = TRUE)
    }
    if (ignore) {
        message("* Adding `", path, "` to `.Rbuildignore`.")
        use_build_ignore(path, pkg = pkg)
    }
    invisible(TRUE)
}


check_dep_version <- function (dep_name, dep_ver = NA, dep_compare = NA) {
    if (!requireNamespace(dep_name, quietly = TRUE)) {
        stop("Dependency package ", dep_name, " not available.")
    }
    if (xor(is.na(dep_ver), is.na(dep_compare))) {
        stop("dep_ver and dep_compare must be both NA or both non-NA")
    }
    else if (!is.na(dep_ver) && !is.na(dep_compare)) {
        compare <- match.fun(dep_compare)
        if (!compare(as.numeric_version(getNamespaceVersion(dep_name)), 
            as.numeric_version(dep_ver))) {
            warning("Need ", dep_name, " ", dep_compare, " ", 
                dep_ver, " but loaded version is ", getNamespaceVersion(dep_name))
        }
    }
    return(TRUE)
}

is_installed <- function (pkg, version = 0) {
    installed_version <- tryCatch(utils::packageVersion(pkg), 
        error = function(e) NA)
    !is.na(installed_version) && installed_version >= version
}

parse_deps <- function (string) {
    if (is.null(string)) 
        return()
    stopifnot(is.character(string), length(string) == 1)
    if (grepl("^\\s*$", string)) 
        return()
    pieces <- strsplit(string, "[[:space:]]*,[[:space:]]*")[[1]]
    names <- gsub("\\s*\\(.*?\\)", "", pieces)
    names <- gsub("^\\s+|\\s+$", "", names)
    versions_str <- pieces
    have_version <- grepl("\\(.*\\)", versions_str)
    versions_str[!have_version] <- NA
    compare <- sub(".*\\((\\S+)\\s+.*\\)", "\\1", versions_str)
    versions <- sub(".*\\(\\S+\\s+(.*)\\)", "\\1", versions_str)
    compare_nna <- compare[!is.na(compare)]
    compare_valid <- compare_nna %in% c(">", ">=", "==", "<=", 
        "<")
    if (!all(compare_valid)) {
        stop("Invalid comparison operator in dependency: ", paste(compare_nna[!compare_valid], 
            collapse = ", "))
    }
    deps <- data.frame(name = names, compare = compare, version = versions, 
        stringsAsFactors = FALSE)
    deps[names != "R", ]
}

suggests_dep <- function (pkg) {
    suggests <- read_dcf(system.file("DESCRIPTION", package = "devtools"))$Suggests
    deps <- parse_deps(suggests)
    found <- which(deps$name == pkg)[1L]
    if (!length(found)) {
        stop(sQuote(pkg), " is not in Suggests: for devtools!", 
            call. = FALSE)
    }
    deps[found, ]
}

check_suggested <- function (pkg, version = NULL, compare = NA) {
    if (is.null(version)) {
        if (!is.na(compare)) {
            stop("Cannot set ", sQuote(compare), " without setting ", 
                sQuote(version), call. = FALSE)
        }
        dep <- suggests_dep(pkg)
        version <- dep$version
        compare <- dep$compare
    }
    if (!is_installed(pkg) || !check_dep_version(pkg, version, 
        compare)) {
        msg <- paste0(sQuote(pkg), if (is.na(version)) 
            ""
        else paste0(" >= ", version), " must be installed for this functionality.")
        if (interactive()) {
            message(msg, "\nWould you like to install it?")
            if (utils::menu(c("Yes", "No")) == 1) {
                utils::install.packages(pkg)
            }
            else {
                stop(msg, call. = FALSE)
            }
        }
        else {
            stop(msg, call. = FALSE)
        }
    }
}

yesno <- function (...) {
    yeses <- c("Yes", "Definitely", "For sure", "Yup", "Yeah", 
        "I agree", "Absolutely")
    nos <- c("No way", "Not yet", "I forget", "No", "Nope", "Uhhhh... Maybe?")
    cat(paste0(..., collapse = ""))
    qs <- c(sample(yeses, 1), sample(nos, 2))
    rand <- sample(length(qs))
    utils::menu(qs[rand]) != which(rand == 1)
}

can_overwrite <- function (path) {
    name <- basename(path)
    if (!file.exists(path)) {
        TRUE
    }
    else if (interactive() && !yesno("Overwrite `", name, "`?")) {
        TRUE
    }
    else {
        FALSE
    }
}

render_template  <- function (name, data = list()) {
    path <- system.file("templates", name, package = "devtools")
    template <- readLines(path)
    whisker::whisker.render(template, data)
}

github_dummy <- list(username = "<USERNAME>", repo = "<REPO>", fullname = "<USERNAME>/<REPO>")

github_info <- function (path = ".", remote_name = NULL) {
    if (!uses_github(path)) 
        return(github_dummy)
    r <- git2r::repository(path, discover = TRUE)
    r_remote_urls <- grep("github", remote_urls(r), value = TRUE)
    if (!is.null(remote_name) && !remote_name %in% names(r_remote_urls)) 
        stop("no github-related remote named ", remote_name, 
            " found")
    remote_name <- c(remote_name, "origin", names(r_remote_urls))
    x <- r_remote_urls[remote_name]
    x <- x[!is.na(x)][1]
    github_remote_parse(x)
}
add_desc_package <- function (pkg = ".", field, name) {
    pkg <- as.package(pkg)
    desc_path <- file.path(pkg$path, "DESCRIPTION")
    desc <- read_dcf(desc_path)
    old <- desc[[field]]
    if (is.null(old)) {
        new <- name
        changed <- TRUE
    }
    else {
        if (!grepl(paste0("\\b", name, "\\b"), old)) {
            new <- paste0(old, ",\n    ", name)
            changed <- TRUE
        }
        else {
            changed <- FALSE
        }
    }
    if (changed) {
        desc[[field]] <- new
        write_dcf(desc_path, desc)
    }
    invisible(changed)
}

github_remote_parse  <- function (x) {
    if (length(x) == 0) 
        return(github_dummy)
    if (!grepl("github", x)) 
        return(github_dummy)
    if (grepl("^(https|git)", x)) {
        re <- "github[^/:]*[/:]([^/]+)/(.*?)(?:\\.git)?$"
    }
    else {
        stop("Unknown GitHub repo format", call. = FALSE)
    }
    m <- regexec(re, x)
    match <- regmatches(x, m)[[1]]
    list(username = match[2], repo = match[3], fullname = paste0(match[2], 
        "/", match[3]))
}

uses_git <- function (path = ".") {
    !is.null(git2r::discover_repository(path, ceiling = 0))
}

remote_urls <- function (r) {
    remotes <- git2r::remotes(r)
    stats::setNames(git2r::remote_url(r, remotes), remotes)
}

uses_github <- function (path = ".") {
    if (!uses_git(path)) 
        return(FALSE)
    r <- git2r::repository(path, discover = TRUE)
    r_remote_urls <- git2r::remote_url(r)
    any(grepl("github", r_remote_urls))
}

read_dcf <- function (path) {
    fields <- colnames(read.dcf(path))
    as.list(read.dcf(path, keep.white = fields)[1, ])
}

write_dcf <- function (path, desc) {
    desc <- unlist(desc)
    desc <- gsub("\n[ \t]*\n", "\n .\n ", desc, perl = TRUE,
        useBytes = TRUE)
    desc <- gsub("\n \\.([^\n])", "\n  .\\1", desc, perl = TRUE,
        useBytes = TRUE)
    starts_with_whitespace <- grepl("^\\s", desc, perl = TRUE,
        useBytes = TRUE)
    delimiters <- ifelse(starts_with_whitespace, ":", ": ")
    text <- paste0(names(desc), delimiters, desc, collapse = "\n")
    if (substr(text, nchar(text), 1) != "\n") {
        text <- paste0(text, "\n")
    }
    cat(text, file = path)
}

parse_check_results <- function (path) {
    lines <- paste(readLines(path, warn = FALSE), collapse = "\n")
    lines <- gsub("^NOTE: There was .*\n$", "", lines)
    lines <- gsub("^WARNING: There was .*\n$", "", lines)
    pieces <- strsplit(lines, "\n\\* ")[[1]]
    structure(list(errors = pieces[grepl("... ERROR", pieces, 
        fixed = TRUE)], warnings = pieces[grepl("... WARN", pieces, 
        fixed = TRUE)], notes = pieces[grepl("... NOTE", pieces, 
        fixed = TRUE)]), path = path, class = "check_results")
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

