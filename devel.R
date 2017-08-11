package <- "fvafrcu"
path <- file.path(dirname(getwd()), package)
devtools::create(path)


path <- "."
devtools::load_all(".")
use_intro(force = TRUE)
use_readme_rmd(force = TRUE)
#create_devel(path = path)
remove_Rproj(path = path)
use_git(path = path)
use_devtools(path = path)
set_author(path = path, given = "Andreas Dominik", family = "Cullmann", email = "fvafrcu@arcor.de")
use_bsd2clause_license(path = path)
r <- git2r::init(path = path)
paths <- unlist(git2r::status(r))
git2r::add(r, paths)
roxygen2::roxygenize(".")
git2r::commit(r, "packager changes")

devtools::check(path)
devtools::install(path)
