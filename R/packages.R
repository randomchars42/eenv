#'
#' Update \pkg{bioset} and \pkg{eenv} from github.
#'
#' @export
#'
update_packages <- function() {
  if (package_available(package = "devtools")) {
    devtools::install_github("randomchars42/bioset")
    devtools::install_github("randomchars42/eenv")
  }
}

#'
#' Load a package.
#'
#' @description
#' If the package is not installed. Try to install it before loading.
#'
#' @export
#' @family package functions
#' @param package The name of the package as a string.
#' @return TRUE if the package could be (installed and) loaded.
package_load <- function(package) {
  # find.package returns a string of length 0 if the package is not installed
  if (package_available(package)) {
    throw_message("Install missing package: ", package)
    install.packages(pkgs = c(package))
  }

  if (! require(
    package = package,
    quietly = TRUE,
    warn.conflicts = TRUE,
    character.only = TRUE)) {
    stop(paste0("Could not load package: ", package))
    return(FALSE)
  }
  return(TRUE)
}

#'
#' Test if a package is installed.
#'
#' @export
#' @family package functions
#' @param package The name of the package as a string.
#' @return TRUE if the package is installed.
#'
package_available <- function(package) {
  # find.package returns a string of length 0 if the package is not installed
  return(length(find.package(package = package, quiet = TRUE)) != 0)
}
