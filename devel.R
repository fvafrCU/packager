package <- "fvafrcu1"
root <- file.path("", "tmp")
path <- file.path(root, package)
devtools::create(path)
utils:::package.skeleton(package, path = root)



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
set_package_info(path = ".",
                 title = "Helps Me Build Packages", 
                 author_at_r = sub("(email)", "\n\t\\1", author_at_r("Andreas Dominik", "Cullmann", "fvafrcu@arcor.de")),
                 description = "This is a set of wrappers around `devtools` and `MakefileR` and some sanity checks for developing packages.",
                 details = "I find devtools very helpful, but not helpful enough.\nSo this very personalised is my extension.")
use_bsd2clause_license(path = path)
#
# TODO: create .lintr file excluding vignette codes
r <- git2r::init(path = path)
paths <- unlist(git2r::status(r))
git2r::add(r, paths)
git2r::commit(r, "packager changes")

devtools::check(path)
devtools::install(path)


devtools::load_all(".")
provide_cran_comments("~/document/log/dev_check.Rout")
auth = sub("(email)", "\n\t\\1", author_at_r("Andreas Dominik", "Cullmann", "<fvafrcu@arcor.de>"))
substitution = list("Authors@R" = auth)
alter_description_file(path = path, s = substitution)
 
