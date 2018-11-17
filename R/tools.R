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
#' Returns the data set with following columns added:
#'
#'  * COLUMN_diff: The difference to the row before.
#'  * COLUMN_diff_rel: The difference to the row before as relative difference of the row before.
#'    Either positive or negative.
#'  * COLUMN_diff_signif: Is the relative difference >= signif_above.
#'  * COLUMN_diff_cat: "rise", "fall" or "insignificant"
#'  * COLUMN_diff_risen: TRUE if "rise" at least once per group.
#'  * COLUMN_diff_fallen: TRUE if "fall" at least once per group.
#'  * COLUMN_diff_rise_max: The maximum increase per group.
#'  * COLUMN_diff_rise_rel_max: The maximum relative increase per group.
#'  * COLUMN_diff_fall_max: The maximum decrease per group.
#'  * COLUMN_diff_fall_rel_max: The maximum relative decrease per group
#'  * COLUMN_diff_max: The maximum increase or decrease per group.
#'  * COLUMN_diff_rel_max: The maximum relative increase or decrease per group.
#'  * COLUMN_diff_in_sequence: Is this row the next in sequence or was >= 1 skipped?
#'  * COLUMN_diff_rise_first: The first rise.
#'  * COLUMN_diff_rise_first_sequence: The first rise ignoring rows with COLUMN_diff_in_sequence = FALSE.
#'  * COLUMN_diff_fall_first: The first decrease.
#'  * COLUMN_diff_fall_first_sequence: The first decrease ignoring rows with COLUMN_diff_in_sequence = FALSE.
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
  value_diff_rel = rlang::sym(paste0(base_name, "_diff_rel"))
  value_diff_signif = rlang::sym(paste0(base_name, "_diff_signif"))
  value_diff_cat = rlang::sym(paste0(base_name, "_diff_cat"))
  value_diff_risen = rlang::sym(paste0(base_name, "_diff_risen"))
  value_diff_fallen = rlang::sym(paste0(base_name, "_diff_fallen"))
  value_diff_rise_max = rlang::sym(paste0(base_name, "_diff_rise_max"))
  value_diff_rise_rel_max = rlang::sym(paste0(base_name, "_diff_rise_rel_max"))
  value_diff_fall_max = rlang::sym(paste0(base_name, "_diff_fall_max"))
  value_diff_fall_rel_max = rlang::sym(paste0(base_name, "_diff_fall_rel_max"))
  value_diff_max = rlang::sym(paste0(base_name, "_diff_max"))
  value_diff_rel_max = rlang::sym(paste0(base_name, "_diff_rel_max"))
  value_diff_in_sequence = rlang::sym(paste0(base_name, "_diff_in_sequence"))
  value_diff_rise_first = rlang::sym(paste0(base_name, "_diff_rise_first"))
  value_diff_rise_first_sequence = rlang::sym(paste0(base_name, "_diff_rise_first_sequence"))
  value_diff_fall_first = rlang::sym(paste0(base_name, "_diff_fall_first"))
  value_diff_fall_first_sequence = rlang::sym(paste0(base_name, "_diff_fall_first_sequence"))

  data_result <- data %>%
    dplyr::arrange(!! arrange) %>%
    dplyr::group_by(!!! group) %>%
    dplyr::mutate(
      !! value_diff_in_sequence := ifelse(!! arrange - dplyr::lag(!! arrange) == 1, TRUE, FALSE),
      !! value_diff := !! values - dplyr::lag(!! values),
      !! value_diff_rel := !! value_diff / dplyr::lag(!! values),
      !! value_diff_signif := abs(!! value_diff_rel) > signif_above,
      !! value_diff_cat := ifelse(!! value_diff > 0, "rise",  "fall"),
      !! value_diff_cat := ifelse(!! value_diff_signif, !! value_diff_cat,  "insignificant"),
      !! value_diff_risen := "rise" %in% !! value_diff_cat,
      !! value_diff_rise_rel_max := max(!! value_diff_rel, na.rm = TRUE),
      !! value_diff_rise_rel_max := ifelse(is.infinite(!! value_diff_rise_rel_max), NA, !! value_diff_rise_rel_max),
      !! value_diff_rise_rel_max := ifelse(!! value_diff_rise_rel_max > signif_above, !! value_diff_rise_rel_max, NA),
      !! value_diff_rise_max := max(!! value_diff, na.rm = TRUE),
      !! value_diff_rise_max := ifelse(is.infinite(!! value_diff_rise_max), NA, !! value_diff_rise_max),
      !! value_diff_rise_max := ifelse(!! value_diff_rise_rel_max > signif_above, !! value_diff_rise_max, NA),
      !! value_diff_rise_first := min(ifelse(!! value_diff_cat == "rise", !! arrange, NA), na.rm = TRUE),
      !! value_diff_rise_first := ifelse(is.infinite(!! value_diff_rise_first), NA, !! value_diff_rise_first),
      !! value_diff_rise_first_sequence := min(ifelse(!! value_diff_cat == "rise" & !! value_diff_in_sequence, !! arrange, NA), na.rm = TRUE),
      !! value_diff_rise_first_sequence := ifelse(is.infinite(!! value_diff_rise_first_sequence), NA, !! value_diff_rise_first_sequence),
      !! value_diff_fallen := "fall" %in% !! value_diff_cat,
      !! value_diff_fall_rel_max := min(!! value_diff_rel, na.rm = TRUE),
      !! value_diff_fall_rel_max := ifelse(is.infinite(!! value_diff_fall_rel_max), NA, !! value_diff_fall_rel_max),
      !! value_diff_fall_rel_max := ifelse(!! value_diff_fall_rel_max < -1 * signif_above, !! value_diff_fall_rel_max, NA),
      !! value_diff_fall_max := min(!! value_diff, na.rm = TRUE),
      !! value_diff_fall_max := ifelse(is.infinite(!! value_diff_fall_max), NA, !! value_diff_fall_max),
      !! value_diff_fall_max := ifelse(!! value_diff_fall_rel_max < -1 * signif_above, !! value_diff_fall_max, NA),
      !! value_diff_fall_first := min(ifelse(!! value_diff_cat == "fall", !! arrange, NA), na.rm = TRUE),
      !! value_diff_fall_first := ifelse(is.infinite(!! value_diff_fall_first), NA, !! value_diff_fall_first),
      !! value_diff_fall_first_sequence := min(ifelse(!! value_diff_cat == "fall" & !! value_diff_in_sequence, !! arrange, NA), na.rm = TRUE),
      !! value_diff_fall_first_sequence := ifelse(is.infinite(!! value_diff_fall_first_sequence), NA, !! value_diff_fall_first_sequence),
      !! value_diff_max := ifelse(!! value_diff_fall_max * -1 > !! value_diff_rise_max, !! value_diff_fall_max, !! value_diff_rise_max),
      !! value_diff_rel_max := ifelse(!! value_diff_fall_rel_max * -1 > !! value_diff_rise_rel_max, !! value_diff_fall_rel_max, !! value_diff_rise_rel_max)) %>%
    dplyr::ungroup()

  return(data_result)
}
