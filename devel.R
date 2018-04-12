devtools::load_all(".")
devtools::use_dev_version()
if (FALSE) {
    packager::provide_cran_comments(check_log = "log/check.Rout", travis_session_info = "travis-cli")
    set_package_info(path = ".", title = "Helps Me Build Packages", 
                     description = paste("Helper functions for a build system,",
                                         "heavily borrowing from and extending", 
                                         "package `devtools 1.13.3`."),
                     details = paste("I find devtools very helpful but it lacks",
                                     "some functionality I would want while", 
                                     "creating and building a package.\n", 
                                     "Maybe \\pkg{packager} should be two packages."),
                     force = is_force()
                     )
}


packager::check_codetags()
# make
ml <- provide_make_list()

fakemake::make(".log.Rout", ml)
fakemake::visualize(ml)


packager::create("../cuwintest")



# ============ cyclo deps

# from remotes package version 1.1.1
has_package <- function(pkg) {
    if (pkg %in% loadedNamespaces()) {
        TRUE
    }
    else {
        requireNamespace(pkg, quietly = TRUE)
    }
}

safe_install_packages <- function(...) {
    lib <- paste(.libPaths(), collapse = ":")
    if (has_package("crancache") && has_package("callr")) {
        i.p <- "crancache" %::% "install_packages"
    } else {
        i.p <- utils::install.packages
    }
    withr::with_envvar(c(R_LIBS = lib, R_LIBS_USER = lib, R_LIBS_SITE = lib, 
                         R_PROFILE_USER = tempfile()), i.p(...))
}

install_packages <- function(packages, repos = getOption("repos"), 
                             type = getOption("pkgType"), 
                             ..., dependencies = FALSE, quiet = NULL) {
    if (identical(type, "both")) 
        type <- "binary"
    if (is.null(quiet)) 
        quiet <- !identical(type, "source")
    message("Installing ", length(packages), " packages: ", paste(packages, 
                                                                  collapse = ", "))
    safe_install_packages(packages, repos = repos, type = type, 
                          ..., dependencies = dependencies, quiet = quiet)
}
update <- function(object, ..., quiet = FALSE, upgrade = TRUE) {
    ahead <- object$package[object$diff == 2L]
    if (length(ahead) > 0 && !quiet) {
        message("Skipping ", length(ahead), " packages not available: ", 
                paste(ahead, collapse = ", "))
    }
    missing <- object$package[object$diff == 1L]
    if (length(missing) > 0 && !quiet) {
        message("Skipping ", length(missing), " packages ahead of CRAN: ", 
                paste(missing, collapse = ", "))
    }
    if (upgrade) {
        behind <- object$package[object$diff < 0L]
    } else {
        behind <- object$package[is.na(object$installed)]
    }
    if (length(behind) > 0L) {
        install_packages(behind, repos = attr(object, "repos"), 
                         type = attr(object, "type"), ...)
    }
}

update_deps <- function(path) {
    deps <- remotes::dev_package_deps(path)
    update(deps)
}
update_deps(".")
