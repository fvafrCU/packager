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
roxygen2::roxygenize(path)
packager::use_intro()
#create_devel(path = path)
packager::remove_Rproj(path = path)
packager::use_git(path = path)
packager::use_devtools(path = path)
devtools::build_vignettes(pkg = path)
# these functions should use documenation::alter_description_file
packager::set_package_info(path = ".",
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

devtools::check(path)
devtools::install(path)


devtools::load_all(".")
packager::provide_cran_comments("~/document/log/dev_check.Rout")
auth <- sub("(email)", "\n\t\\1", author_at_r("Andreas Dominik", "Cullmann", "<fvafrcu@arcor.de>"))
substitution <- list("Authors@R" = auth)
document::alter_description_file(path = path, s = substitution)
packager::provide_throw(".")


devtools::use_build_ignore("Makefile", pkg = path)
MakefileR::make_rule("all", c("foo", "bar"))
