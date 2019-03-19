args_to_text <- function(...) {
  parts <- list(...)
  return(paste0(parts, collapse = ''))
}

release_questions <- function() {
  c(
    "devtools::revdep_check()?",
    "devtools::build_win()?",
    "Are you feeling good?"
  )
}

#
#  1. Check
#       * if `Check` shows fixed errors remove ../eenv.Rcheck
#  2. Build source
#
