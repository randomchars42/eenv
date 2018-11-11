#'
#' Check if `x` is a number.
#'
#' @description
#' Check that `x` is neither `NULL`, nor `NA`` and is of type `numeric` or
#' `integer`.
#'
#' @export
#' @param x The variable to check.
#' @return `TRUE` / `FALSE`
#'
is_number <- function(x) {
  if(! is.null(x) && ! is.na(x) && (is.numeric(x) || is.integer(x))) {
    return(TRUE)
  }
  return(FALSE)
}

#'
#' Calculate the coefficient of variation of `x`.
#'
#' @description
#' Calculate the coefficient (cv) of variation of the values in vector `x`.
#'
#' @export
#' @param x The values to use.
#' @return The cv.
#'
calc_cv <- function(x) {
  result <- stats::sd(x) / mean(x)
  return(result)
}
