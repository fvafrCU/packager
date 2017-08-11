# extending devtools' version to not hard code the source package of the
# template. And get rid of call to devtools:::open_in_rstudio().
use_template <- function (template, save_as = template, data = list(), 
                          ignore = FALSE, pkg = ".", 
                          source_package = "packager", force = FALSE) {
    pkg <- devtools::as.package(pkg)
    path <- file.path(pkg$path, save_as)
    if (! isTRUE(force)) {
        if (! can_overwrite(path)) {
            stop("`", save_as, "` already exists.", call. = FALSE)
        }
    }
    template_path <- system.file("templates", template, 
                                 package = source_package, mustWork = TRUE)
    template_out <- whisker::whisker.render(readLines(template_path), 
        data)
    message("* Creating `", save_as, "` from template.")
    writeLines(template_out, path)
    if (ignore) {
        message("* Adding `", save_as, "` to `.Rbuildignore`.")
        devtools::use_build_ignore(save_as, pkg = pkg)
    }
    invisible(TRUE)
}

# adjust use_readme_rmd to not pass the argument \code{open} with use_template()
# and fix the test on file.exists()
use_readme_rmd <- function (pkg = ".", ...) {
    pkg <- devtools::as.package(pkg)
    if (uses_github(pkg$path)) {
        pkg$github <- github_info(pkg$path)
    }
    pkg$Rmd <- TRUE
    use_template("omni-README", save_as = "README.Rmd", data = pkg, 
        ignore = TRUE, pkg = pkg, ...)
    devtools::use_build_ignore("^README-.*\\.png$", escape = FALSE, pkg = pkg)
    if (uses_git(pkg$path) && !file.exists(file.path(pkg$path, ".git", 
        "hooks", "pre-commit"))) {
        message("* Adding pre-commit hook")
        devtools::use_git_hook("pre-commit", render_template("readme-rmd-pre-commit.sh"), 
            pkg = pkg)
    }
    invisible(TRUE)
}

use_intro <- function (pkg = ".", ...) {
    pkg <- devtools::as.package(pkg)
    if (uses_github(pkg$path)) {
        pkg$github <- github_info(pkg$path)
    }
    vignette_name <- paste0("An_Introduction_to_", 
                                          pkg[["package"]], ".Rmd")
    # use the original to make checks and create the directory
    #devtools::use_vignette(vignette_name, pkg = path)
    check_suggested("rmarkdown")
    add_desc_package(pkg, "Suggests", "knitr")
    add_desc_package(pkg, "Suggests", "rmarkdown")
    add_desc_package(pkg, "VignetteBuilder", "knitr")
    use_directory("vignettes", pkg = pkg)
    path <- file.path("vignettes", vignette_name)
    use_template("vignette.Rmd", save_as = path, data = pkg, 
        ignore = FALSE, pkg = pkg, ...)
    invisible(TRUE)
}

use_devtools <- function(path = ".") {
    pkg <- devtools::as.package(path)
    result <- NULL
    result <- c(result, devtools::use_news_md(pkg = path))
    result <- c(result, devtools::use_build_ignore("devel.R", pkg = path))
    result <- c(result, use_readme_rmd(pkg = path))
    result <- c(result, devtools::use_vignette(paste0("An_Introduction_to_", 
                                                     pkg[["package"]]), pkg = path))
    result <- c(result, devtools::use_cran_comments(pkg = path))
    result <- c(result, devtools::use_test("basic.R", pkg = path))
    result <- c(result, devtools::use_travis(pkg = path))
    result <- c(result, devtools::use_coverage(type = c("codecov"), pkg = path))
    cat("after_success:", "  - Rscript -e 'covr::codecov()'", sep = "\n", 
        file = file.path(path, ".travis.yml"), append = TRUE)
    result <- c(result, devtools::use_package("roxygen2"), pkg = path)
    result <- c(result, devtools::use_package("devtools"), pkg = path)
    result <- c(result, devtools::use_package("git2r"), pkg = path)
    return(result)
}

