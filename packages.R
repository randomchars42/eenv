packages_base <- c(
  "readODS",
  "gmodels", # CrossTable 
  "tidyverse",
  "cowplot", # combine ggplots into panels
  "knitr", # fancy tables and output specific for word / html / pdf
  "kableExtra" # even fancier tables
)

if (exists("packages")) {
  packages <- c(packages_base, packages)
} else {
  packages <- packages
}


package_load <- function(package) {
  # find.package returns a string of length 0 if the package is not installed
  if (length(find.package(package = package, quiet = TRUE)) == 0) {
    message(paste0("Install missing package: ", package))
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

packages_loaded <- sapply(packages, package_load)

rm(packages, package_load, packages_loaded, packages_base)