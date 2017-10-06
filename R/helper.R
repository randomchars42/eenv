#'
#' Check if `x` is a number.
#'
#' @description
#' Check that `x` is neither `NULL`, nor `NA`` and is of type `numeric` or
#' `integer`.
#'
#' @export
#' @param x The variable to check.
#' @return `TRUE` / `FALSE``
#'
is_number <- function(x) {
  if(! is.null(x) && ! is.na(x) && (is.numeric(x) || is.integer(x))) {
    return(TRUE)
  }
  return(FALSE)
}

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
