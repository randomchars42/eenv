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

#'
#' Calculate the difference between sequential rows while grouping.
#'
#' @export
#' @param data A tibble.
#' @param values The column with the values to calculate the difference with.
#' @param arrange The column to arrange the data by.
#' @param ... The columns to group by.
#' @param signif_above An difference above this cut off is considered significant.
#' @return A tibble.
#'
calc_sequential_difference <- function(
  data,
  values,
  arrange,
  ...,
  signif_above = 0.2) {
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  stopifnot(
    tibble::is_tibble(data))

  group <- rlang::enquos(...)
  values <- rlang::enquo(values)
  arrange <- rlang::enquo(arrange)

  base_name <- rlang::quo_name(values)
  value_diff = rlang::sym(paste0(base_name, "_diff"))
  value_diff_abs = rlang::sym(paste0(base_name, "_diff_abs"))
  value_diff_perc = rlang::sym(paste0(base_name, "_diff_perc"))
  value_diff_signif = rlang::sym(paste0(base_name, "_diff_signif"))
  value_diff_cat = rlang::sym(paste0(base_name, "_diff_cat"))
  value_diff_max = rlang::sym(paste0(base_name, "_diff_max"))
  value_diff_perc_max = rlang::sym(paste0(base_name, "_diff_perc_max"))
  value_diff_in_sequence = rlang::sym(paste0(base_name, "_diff_in_sequence"))

  data_result <- data %>%
    dplyr::arrange(!! arrange) %>%
    dplyr::group_by(!!! group) %>%
    dplyr::mutate(
      !! value_diff_in_sequence := ifelse(!! arrange - dplyr::lag(!! arrange) == 1, TRUE, FALSE),
      !! value_diff := !! values - dplyr::lag(!! values),
      !! value_diff_abs := abs(!! value_diff),
      !! value_diff_perc := !! value_diff_abs / dplyr::lag(!! values),
      !! value_diff_signif := !! value_diff_perc > signif_above,
      !! value_diff_cat := ifelse(!! value_diff > 0, "rise",  "fall"),
      !! value_diff_cat := ifelse(!! value_diff_signif, !! value_diff_cat,  "insignificant"),
      !! value_diff_max := max(!! value_diff, na.rm = TRUE),
      !! value_diff_perc_max := max(!! value_diff_perc, na.rm = TRUE)) %>%
    dplyr::ungroup()

  return(data_result)
}
