use_devtools <- function(path = ".") {
    pkg <- devtools::as.package(path)
    result <- NULL
    result <- c(result, devtools::use_news_md())
    result <- c(result, devtools::use_build_ignore("devel.R"))
    result <- c(result, devtools::use_readme_rmd())
    result <- c(result, devtools::use_vignette(paste0("An_Introduction_to_", 
                                                     pkg[["package"]])))
    result <- c(result, devtools::use_cran_comments())
    result <- c(result, devtools::use_test("test-basic.R"))
    result <- c(result, devtools::use_travis())
    result <- c(result, devtools::use_coverage(type = c("codecov")))
    cat("after_success:", "  - Rscript -e 'covr::codecov()'", sep = "\n", 
        file = file.path(path, ".travis.yml"), append = TRUE)
    result <- c(result, devtools::use_package("roxygen2"))
    result <- c(result, devtools::use_package("devtools"))
    result <- c(result, devtools::use_package("git2r"))
    return(result)
}

