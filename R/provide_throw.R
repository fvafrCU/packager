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

