# infrastructure.R in devtools 1.13.3 states:
## #' @details
## #' Instead of the use_xyz functions from devtools use \link[usethis]{use_testthat}.
## #' @rdname devtools-deprecated
# and then heavily uses
## #'@importsFrom usethis

# Now devtools 1.13.3 was released on CRAN:
# # devtools_1.13.3.tar.gz	2017-08-02 09:05
# But the first release of usethis appeared on CRAN:
# # usethis_1.0.0.tar.gz	2017-10-22 19:36
# I do not know how they imported from a package not yet released, but that is
# what they did.
# So I just got copies of the imported functions from usethis by calling the
# functions via devtools at the time. And then I modified most of them.


# add devtools:: to the call for build():
build_cran <- function(pkg, args = NULL) {
  message("Building")
  built_path <- devtools::build(pkg, tempdir(), manual = TRUE, args = args)
  message("Submitting file: ", built_path)
  message(
    "File size: ",
    format(as.object_size(file.info(built_path)$size), units = "auto")
  )
  built_path
}


# get rid of the interactive() part using yesno() to create the package.
# Blow if there is none!
load_pkg_description <- function(path, create) {
  never_used <- create # this was part of the interaction.
  path_desc <- file.path(path, "DESCRIPTION")
  if (!file.exists(path_desc)) {
    stop("No description at ", path_desc, call. = FALSE)
  }
  desc <- as.list(read.dcf(path_desc)[1, ])
  names(desc) <- tolower(names(desc))
  desc$path <- path
  structure(desc, class = "package")
}



# extending devtools' version to not hard code the source package of the
# template. And get rid of call to devtools:::open_in_rstudio().
# Remove dialog.
use_template <- function(template, save_as = template, data = list(),
                         ignore = FALSE, pkg = ".",
                         source_package = "packager",
                         force = isTRUE(getOption("packager")[["force"]])) {
  status <- FALSE
  pkg <- devtools::as.package(pkg)
  path <- file.path(pkg$path, save_as)
  if (!file.exists(path) || isTRUE(force)) {
    template_path <- system.file("templates", template,
      package = source_package, mustWork = TRUE
    )
    template_out <- whisker::whisker.render(
      readLines(template_path),
      data
    )
    message("* Creating `", save_as, "` from template.")
    writeLines(template_out, path)
    if (ignore) {
      message("* Adding `", save_as, "` to `.Rbuildignore`.")
      use_build_ignore(save_as, pkg = pkg)
    }
    status <- TRUE
  } else {
    warning("`", save_as, "` already exists.", call. = FALSE)
  }
  return(invisible(status))
}

# adjust use_readme_rmd to not pass the argument \code{open} with use_template()
# and fix the test on file.exists()
# added git_user to pkg
use_readme_rmd <- function(path = ".", ...) {
  pkg <- devtools::as.package(path)
  if (uses_git(path)) {
    pkg[["git_user"]] <- tryCatch(git2r::default_signature(path)[["name"]],
      error = function(e) return(NULL)
    )
  }

  if (uses_github(pkg$path)) {
    pkg$github <- github_info(pkg$path)
  }
  pkg$Rmd <- TRUE
  use_template("README.Rmd",
    save_as = "README.Rmd", data = pkg,
    ignore = TRUE, pkg = pkg, ...
  )
  use_build_ignore("^README-.*\\.png$", escape = FALSE, pkg = pkg)
  if (uses_git(pkg$path) && !file.exists(file.path(
    pkg$path, ".git",
    "hooks", "pre-commit"
  ))) {
    message("* Adding pre-commit hook")
    devtools::use_git_hook("pre-commit",
      render_template("readme-rmd-pre-commit.sh"),
      pkg = pkg
    )
  }
  return(invisible(NULL))
}

use_news_md <- function(pkg = ".", ...) {
  pkg <- devtools::as.package(pkg)
  use_template("NEWS.md", data = pkg, pkg = pkg, ...)
  invisible(NULL)
}

use_intro <- function(path = ".", ..., details = NULL) {
  if (is.na(details)) details <- NULL # NA would get printed into vignette.
  pkg <- devtools::as.package(path)
  pkg$details <- details
  if (uses_github(pkg$path)) {
    pkg$github <- github_info(pkg$path)
  }
  pkg$date <- format(Sys.time(), "%Y-%m-%d, %H:%M:%S")
  vignette_name <- paste0(
    "An_Introduction_to_",
    pkg[["package"]], ".Rmd"
  )
  check_suggested("rmarkdown")
  add_desc_package(pkg, "Suggests", "knitr")
  add_desc_package(pkg, "Suggests", "rmarkdown")
  add_desc_package(pkg, "VignetteBuilder", "knitr")
  use_directory("vignettes", pkg = pkg)
  path <- file.path("vignettes", vignette_name)
  use_template("vignette.Rmd",
    save_as = path, data = pkg,
    ignore = FALSE, pkg = pkg, ...
  )
  return(invisible(NULL))
}

use_travis <- function(path = ".", ...) {
  pkg <- as.package(path)
  use_template("travis.yml", ".travis.yml",
    ignore = TRUE,
    pkg = pkg, ...
  )
  return(invisible(NULL))
}

# devtools' version does not pass ceiling to git2r::discover_repository,
# thus failing, if any .git found in the parents of the package path.
# It also works only for R packages, which does not make much sense to me.
use_git <- function(path = ".", message = "Initial commit") {
  if (!is.null(git2r::discover_repository(path, ceiling = 0))) {
    message("* Git is already initialized")
    return(invisible())
  }
  message("* Initialising repo")
  r <- git2r::init(path)
  use_git_ignore(c(".Rproj.user", ".Rhistory", ".RData"), path = path)
  message("* Adding files and committing")
  paths <- unlist(git2r::status(r))
  git2r::add(r, paths)
  git_commit(r, message)
  invisible(TRUE)
}

# Modified copy of devtools' unexported version
# added argument 'prepend'
union_write <- function(path, new_lines, prepend = FALSE) {
  if (file.exists(path)) {
    lines <- readLines(path, warn = FALSE)
  }
  else {
    lines <- character()
  }
  if (isTRUE(prepend)) {
      all <- union(new_lines, lines)
  } else {
      all <- union(lines, new_lines)
  }
  writeLines(all, path)
}

# Modified copy of devtools' unexported version
use_git_ignore <- function(ignores, path = ".") {
  paths <- paste0("`", ignores, "`", collapse = ", ")
  message("* Adding ", paths, " to ", file.path(
    path,
    ".gitignore"
  ))
  path <- file.path(path, ".gitignore")
  union_write(path, ignores)
  invisible(TRUE)
}


# call utils::
check_suggested <- function(pkg, version = NULL, compare = NA) {
  if (is.null(version)) {
    if (!is.na(compare)) {
      stop("Cannot set ", sQuote(compare), " without setting ",
        sQuote(version),
        call. = FALSE
      )
    }
    dep <- suggests_dep(pkg)
    version <- dep$version
    compare <- dep$compare
  }
  if (!is_installed(pkg) || !check_dep_version(
    pkg, version,
    compare
  )) {
    msg <- paste0(
      sQuote(pkg), if (is.na(version)) {
        ""
      } else {
        paste0(" >= ", version)
      } ,
      " must be installed for this functionality."
    )
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::install.packages(pkg)
      }
      else {
        stop(msg, call. = FALSE)
      }
    }
    else {
      stop(msg, call. = FALSE)
    }
  }
}

# call utils::
yesno <- function(...) {
  yeses <- c(
    "Yes", "Definitely", "For sure", "Yup", "Yeah",
    "I agree", "Absolutely"
  )
  nos <- c("No way", "Not yet", "I forget", "No", "Nope", "Uhhhh... Maybe?")
  cat(paste0(..., collapse = ""))
  qs <- c(sample(yeses, 1), sample(nos, 2))
  rand <- sample(length(qs))
  utils::menu(qs[rand]) != which(rand == 1)
}

# add cran_submission_url as argument
upload_cran <- function(pkg, built_path, cran_submission_url = NULL) {
  pkg <- as.package(pkg)
  maint <- maintainer(pkg)
  comments <- cran_comments(pkg)

  # Initial upload ---------
  message("Uploading package & comments")
  body <- list(
    pkg_id = "",
    name = maint$name,
    email = maint$email,
    uploaded_file = httr::upload_file(built_path, "application/x-gzip"),
    comment = comments,
    upload = "Upload package"
  )
  r <- httr::POST(cran_submission_url, body = body)
  httr::stop_for_status(r)
  new_url <- httr::parse_url(r$url)
  new_url$query$strErr

  # Confirmation -----------
  message("Confirming submission")
  body <- list(
    pkg_id = new_url$query$pkg_id,
    name = maint$name,
    email = maint$email,
    policy_check = "1/",
    submit = "Submit package"
  )
  r <- httr::POST(cran_submission_url, body = body)
  httr::stop_for_status(r)
  new_url <- httr::parse_url(r$url)
  if (new_url$query$submit == "1") {
    message(
      "Package submission successful.\n",
      "Check your email for confirmation link."
    )
  } else {
    stop("Package failed to upload.", call. = FALSE)
  }

  invisible(TRUE)
}

# sanitize the return value. Should be TRUE if in sync.
# In devtools, there's a wrapper, `git_check_sync_status` that just does this.
git_sync_status <- function(path = ".", check_ahead = TRUE, check_behind = TRUE) {
  r <- git2r::repository(path, discover = TRUE)

  r_head <- git2r::head(r)
  if (!methods::is(r_head, "git_branch")) {
    stop("HEAD is not a branch", call. = FALSE)
  }

  upstream <- git2r::branch_get_upstream(r_head)
  if (is.null(upstream)) {
    stop("No upstream branch", call. = FALSE)
  }

  git2r::fetch(r, git2r::branch_remote_name(upstream))

  c1 <- git2r::lookup(r, git2r::branch_target(r_head))
  c2 <- git2r::lookup(r, git2r::branch_target(upstream))
  ab <- git2r::ahead_behind(c1, c2)

  #   if (ab[1] > 0)
  #     message(ab[1], " ahead of remote")
  #   if (ab[2] > 0)
  #     message(ab[2], " behind remote")

  is_ahead <- ab[[1]] != 0
  is_behind <- ab[[2]] != 0
  check <- (check_ahead && is_ahead) || (check_behind && is_behind)
  return(!check)
}
