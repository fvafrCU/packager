package <- "fvafrcu1"
root <- file.path("", "tmp")
path <- file.path(root, package)
devtools::create(path)
utils:::package.skeleton(package, path = root)


base::options(packager = list(could_force = TRUE, should_force = FALSE))
base::getOption("packager")
base::getOption("packager")[["could_force"]]
devtools::load_all(".")
packager::provide_throw()
# test logic
devtools::load_all(".")
d <- "/tmp/foo"
packager::create(path = d) 
l <- dir(d, recursive = TRUE, full.names = TRUE)
digest::sha1(c(l, sapply(l, readLines)))
#
if (FALSE) {
packager::infect(path = ".",
                 title = "Helps Me Build Packages", 
                 author_at_r = sub("(email)", "\n\t\\1", author_at_r("Andreas Dominik", "Cullmann", "fvafrcu@arcor.de")),
                 description = "This is a set of wrappers around `devtools` and `MakefileR` and some sanity checks for developing packages.",
                 details = "I find devtools very helpful, but not helpful enough.\nSo this is my highly personalised extension." ,
                 make = FALSE, git_add_and_commit = FALSE)
}



# author_at_r
path <- file.path(tempdir(), "prutp")
force = TRUE
if (isTRUE(force)) unlink(path, recursive = TRUE)
    devtools::create(path = path, rstudio = FALSE, check = FALSE)
    r <- git2r::init(path = path)
    paths <- unlist(git2r::status(r))
    git2r::add(r, paths)
    git2r::commit(r, "Initial Commit")

    r <- git2r::init(path = path)
    devtools::use_build_ignore("^.*\\.tar\\.gz$", pkg = path, escape = FALSE)
    devtools::use_build_ignore(paste0(devtools::as.package(path)[["package"]],
                                      ".Rcheck"), pkg = path)
    use_makefile(path = path)
    use_intro(path = path, force = TRUE)
    use_devel(path = path)
    remove_Rproj(path = path)
    use_devtools(path = path)
    use_travis(path = path)
    set_package_info(path = path)
    use_bsd2clause_license(path = path)
