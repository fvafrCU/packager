package <- "fvafrcu1"
root <- file.path("", "tmp")
path <- file.path(root, package)
devtools::create(path)
utils:::package.skeleton(package, path = root)


base::options(packager = list(could_force = TRUE, should_force = FALSE))
base::getOption("packager")
base::getOption("packager")[["could_force"]]
path <- "."
devtools::load_all(".")
#
packager::use_makefile(path = ".")
roxygen2::roxygenize(packager.dir = path)
packager::use_intro(path = path)
packager::use_devel(path = path)
packager::remove_Rproj(path = path)
packager::use_git(path = path)
packager::use_devtools(path = path)
devtools::build_vignettes(pkg = path)
# these functions should use documenation::alter_description_file
packager::set_package_info(path = path,
                           title = "Helps Me Build Packages", 
                           author_at_r = sub("(email)", "\n\t\\1", author_at_r("Andreas Dominik", "Cullmann", "fvafrcu@arcor.de")),
                           description = "This is a set of wrappers around `devtools` and `MakefileR` and some sanity checks for developing packages.",
                           details = "I find devtools very helpful, but not helpful enough.\nSo this is my highly personalised extension.")
packager::use_readme_rmd()
knitr::knit(input = file.path(path, "README.Rmd"), output = file.path(path, "README.md"))
package::use_bsd2clause_license(path = path)
#
# TODO: create .lintr file excluding vignette codes
r <- git2r::init(path = path)
paths <- unlist(git2r::status(r))
git2r::add(r, paths)
git2r::commit(r, "packager changes")

packager::provide_cran_comments(path = path, travis_session_info = "travis-cli")
auth <- sub("(email)", "\n\t\\1", author_at_r("Andreas Dominik", "Cullmann", "<fvafrcu@arcor.de>"))
substitution <- list("Authors@R" = auth)
document::alter_description_file(path = path, s = substitution)
packager::provide_throw(path = ".")

packager::use_directory("log", pkg = path, ignore = TRUE)

devtools::use_build_ignore("^.*\\.tar\\.gz$", pkg = path, escape = FALSE)
devtools::use_build_ignore(paste0(devtools::as.package(path)$package, ".Rcheck"), pkg = path)
packager::use_git_ignore("*.tar.gz", path = path)
packager::use_git_ignore(paste0(devtools::as.package(path)$package, ".Rcheck"), path = path)
packager::provide_makefile(path = path)
