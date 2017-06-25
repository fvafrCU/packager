devtools::load_all()
path <- "."
# create_devel(path = path)
remove_Rproj(path = path)
use_git(path = path)
use_devtools(path = path)
set_author(path = path, given = "Andreas Dominik", family = "Cullmann", email = "fvafrcu@arcor.de")
use_bsd2clause_license(path = path)
r <- git2r::init(path = path)

devtools::check()
