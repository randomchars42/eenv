core_packages <- c("ggplot2", "cowplot", "knitr", "rmarkdown", "bioset", "magrittr", "tibble", "dplyr", "gmodels", "readr")

.onAttach <- function(libname, pkgname) {
  needed <- core_packages[!is_attached(core_packages)]
  if (length(needed) == 0) {
    return()
  }
  sapply(core_packages, library, character.only = TRUE, warn.conflicts = FALSE)

  # re-create theme to update ggplot2-defaults
  ggplot2::theme_set(eenv::create_eenv_theme())

  options(scipen = 18, digits = 2)

  return(invisible())
}

is_attached <- function(x) {
  attached <- paste0("package:", x) %in% search()
  return(attached)
}
