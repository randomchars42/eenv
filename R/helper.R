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
