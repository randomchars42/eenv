core_packages <- c("tidyverse", "gmodels", "cowplot", "knitr", "rmarkdown", "bioset")

.onAttach <- function(libname, pkgname) {
  needed <- core_packages[!is_attached(core_packages)]
  if (length(needed) == 0) {
    return()
  }
  sapply(core_packages, library, character.only = TRUE, warn.conflicts = FALSE)

  # re-create theme to updaet ggplot2-defaults
  create_theme()

  return(invisible())
}

is_attached <- function(x) {
  attached <- paste0("package:", x) %in% search()
  return(attached)
}
