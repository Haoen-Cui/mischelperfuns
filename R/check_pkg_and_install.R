InstallPackagesIfNotAlready <- function(pkgs, 
                                        install_if_not = TRUE, 
                                        return_status = FALSE, 
                                        verbose = getOption("verbose")) { 
  # get installation status 
  isInstalled <- as.logical(sapply(pkgs, function(pkg) 
    length( find.package(pkg, quiet = TRUE, verbose = FALSE) ) )) 
  # install missing packages 
  if ( install_if_not & !all(isInstalled) ) {
    tryCatch(install.packages(pkgs[!isInstalled]), 
             error = function(e) e)
  }
  # return and/or print installation status 
  if ( return_status | verbose ) {
    status_df <- data.frame("Before" = isInstalled, "After" = isInstalled)
    rownames(status_df) <- as.character(pkgs) 
    status_df[!isInstalled, "After"] <- 
      as.logical(sapply(pkgs[!isInstalled], function(pkg) 
        length( find.package(pkg, quiet = TRUE, verbose = FALSE) ) )) 
  }
  if ( verbose ) print(status_df) 
  if ( return_status ) return(status_df) 
}
