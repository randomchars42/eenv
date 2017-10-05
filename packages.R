packages_base <- c(
  "readODS",
  "gmodels", # CrossTable 
  "tidyverse",
  "cowplot", # combine ggplots into panels
  "knitr", # fancy tables and output specific for word / html / pdf
  "kableExtra", # even fancier tables
  "devtools" # to install biosets
)

if (exists("packages")) {
  packages <- c(packages_base, packages)
} else {
  packages <- packages_base
}

packages_loaded <- sapply(packages, package_load)

if (length(find.package(package = "biosets", quiet = TRUE)) == 0) {
  message(paste0("Install missing package: bioset"))
  devtools::install_github("randomchars42/bioset")
  #install.packages("../bioset_0.1.0.9000.tar.gz", repos=NULL) 
}

package_load("bioset")

rm(packages, package_load, packages_loaded, packages_base)
