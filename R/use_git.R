# devtools' version does not pass ceiling to git2r::discover_repository, 
# thus failing, if any .git found in the parents of the package path. 
use_git <- function (message = "Initial commit", path = ".") {
    pkg <- devtools::as.package(path)
    if (!is.null(git2r::discover_repository(pkg[["path"]], ceiling = 0))) {
        message("* Git is already initialized")
        return(invisible())
    }
    message("* Initialising repo")
    r <- git2r::init(pkg[["path"]])
    use_git_ignore(c(".Rproj.user", ".Rhistory", ".RData"), path = path)
    message("* Adding files and committing")
    paths <- unlist(git2r::status(r))
    git2r::add(r, paths)
    git2r::commit(r, message)
    invisible(TRUE)
}

# Modified copy of devtools' unexported version
union_write <- function (path, new_lines) {
    if (file.exists(path)) {
        lines <- readLines(path, warn = FALSE)
    }
    else {
        lines <- character()
    }
    all <- union(lines, new_lines)
    writeLines(all, path)
}

# Modified copy of devtools' unexported version
use_git_ignore <- function (ignores, path = ".") {
    pkg <- devtools::as.package(path)
    paths <- paste0("`", ignores, "`", collapse = ", ")
    message("* Adding ", paths, " to ", file.path(pkg[["path"]],
        ".gitignore"))
    path <- file.path(pkg[["path"]], ".gitignore")
    union_write(path, ignores)
    invisible(TRUE)
}



