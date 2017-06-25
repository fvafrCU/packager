create_devel <- function(path = rprojroot::find_root(rprojroot::is_r_package)) {
                         file_name <- file.path(path, "devel.R")
                         return(cat("devtools::load_all()\n", file = file_name))
}

