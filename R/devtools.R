use_devtools <- function(path = ".") {
    pkg <- devtools::as.package(path)
    result <- NULL
    result <- c(result, devtools::use_news_md(pkg = path))
    result <- c(result, devtools::use_build_ignore("devel.R", pkg = path))
    result <- c(result, devtools::use_readme_rmd(pkg = path))
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

