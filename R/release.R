#' Release a Package to CRAN
#'
#' This is a stripped version of \pkg{devtools}'
#' \code{\link[devtools:release]{release function}, it does not do all the
#' interactive checks. 
#' @param path The package's root directory.
#' @param stop_on_git Stop if git has uncommited changes or is not synced with
#' the origin?
#' @param force Skip user interaction?
#' @return \code{\link[base:invisible]{Invisibly} \link{NULL}} 
#' @export
release <- function(path = ".", stop_on_git = TRUE, force = FALSE) {
    if (uses_git(path) && isTRUE(stop_on_git)) {
        if (is_git_uncommitted(path = path) )
            throw("You have uncommitted changes.")
        if (! git_sync_status(path = path)) 
            throw("Your repository is not synced with it's upstream.")
    }
    if (yesno("Ready to submit?") && ! isTRUE(force)) {
        throw("Aborting on user request.")
    } else {
        csu <- "http://xmpalantir.wu.ac.at/cransubmit/index2.php"
        built_path <- build_cran(path, args = NULL)
        upload_cran(pkg = path, built_path = built_path, 
                    cran_submission_url = csu)
        if (uses_git(path)) {
            r <- git2r::repository(path = path) 
            message("Don't forget to tag commit ", git2r::reflog(r)[[1]]@sha, 
                    " as ", desc::desc_get_version(), 
                    ", once package is on CRAN.")
        }
        return(invisible(NULL))
    }
}

