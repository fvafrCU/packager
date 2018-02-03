git_commit <- function(repository, commit_message,
                       verbose = isTRUE(getOption("packager")[["verbose"]])) {
    repo_config <- tryCatch(git2r::default_signature(repository), 
                            error = identity)
    if (inherits(repo_config, "error")) {
        user_name <- "foobar"
        user_email <- "foobar@nowhe.re"
        if (isTRUE(verbose)) message("Could not find user and email for git. ",
                                     "Setting local git config user.name to ",
                                     user_name, " and user.email to ",
                                     user_email, ". Change as apropriate.")
        git2r::config(repository, 
                      user.name = user_name, user.email = user_email)
    }
    res <- git2r::commit(repository, commit_message)
    return(res)
}

