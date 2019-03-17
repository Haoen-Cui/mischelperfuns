#' Check Package Installation Status and Install If Missing Upon Request 
#'
#' @param pkgs (character vector) names of packages to check installation status. 
#' @param install_if_not (logical) whether to install missing packages or not. 
#' @param return_status (logical) whether to return package installation status or not. 
#' @param verbose (logical) whether to print package installation status or not. 
#'
#' @return if \code{install_if_not} is \code{TRUE}, then return a \code{data.frame} 
#'     consisting of the installation status. The \code{data.frame} has two columns: 
#'     \code{"Before"} and \code{"After"} indicating the status before and after the 
#'     function \code{InstallPackagesIfNotAlready} is executed. Further, its rows are 
#'     named after the packages names as specified in \code{pkgs}. 
#' 
#' @examples 
#' InstallPackagesIfNotAlready(c("MASS", "knitr"), return_status = TRUE)
InstallPackagesIfNotAlready <- function(pkgs, 
                                        install_if_not = TRUE, 
                                        return_status = FALSE, 
                                        verbose = FALSE) { 
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
