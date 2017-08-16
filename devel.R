devtools::install_github("fvafrcu/document")
package <- "fvafrcu"
path <- file.path(dirname(getwd()), package)
devtools::create(path)


path <- "."
devtools::load_all(".")
roxygen2::roxygenize(path)
use_intro(force = TRUE)
devtools::build_vignettes(pkg = path)
packager::use_readme_rmd(force = TRUE)
knitr::knit(input = file.path(path, "README.Rmd"), output = file.path(path, "README.md"))
#create_devel(path = path)
remove_Rproj(path = path)
use_git(path = path)
use_devtools(path = path)
# these functions should use documenation::alter_description_file
set_title(path = path, "Helps Me Build Packages")
set_description(path = path, "This is a set of wrappers around `devtools` and `MakefileR` and some sanity checks for developing packages.")
set_author(path = path, given = "Andreas Dominik", family = "Cullmann", email = "fvafrcu@arcor.de")
use_bsd2clause_license(path = path)
#
# TODO: create .lintr file excluding vignette codes
r <- git2r::init(path = path)
paths <- unlist(git2r::status(r))
git2r::add(r, paths)
git2r::commit(r, "packager changes")

devtools::check(path)
devtools::install(path)
