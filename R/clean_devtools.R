remove_Rproj <- function(path = rprojroot::find_root(rprojroot::is_r_package)) {
                         file_name <- list.files(path, 
                                                 pattern = ".*\\.Rproj$", 
                                                 full.names = TRUE)
                         return(unlink(file_name))
}
