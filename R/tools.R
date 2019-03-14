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
#'  * COLUMN_diff_signif: Is the relative difference >= rel_diff_signif.
#'  * COLUMN_diff_cat: "rise", "fall" or "insignificant"
#'  * COLUMN_diff_risen: TRUE if "rise" at least once per group.
#'  * COLUMN_diff_rise_max: The maximal increase per group.
#'  * COLUMN_diff_rise_rel_max: The maximal relative increase per group.
#'  * COLUMN_diff_rise_first: The first rise.
#'  * COLUMN_diff_rise_first_seq: The first rise ignoring rows with COLUMN_diff_in_seq == FALSE.
#'  * COLUMN_diff_diff_rise_signif_above: TRUE if a rise was at least >= rise_above or
#'    if the previous value was >= rise_above an increase of >= rel_diff_signif.
#'  * COLUMN_diff_rise_signif_above_first: The first rise in COLUMN_diff_rise_signif_above_first per group.
#'  * COLUMN_diff_risen_signif_above: TRUE if there was at least one per group with COLUMN_diff_rise_signif_above == TRUE.
#'  * COLUMN_diff_fallen: TRUE if "fall" at least once per group.
#'  * COLUMN_diff_fall_max: The maximal decrease per group.
#'  * COLUMN_diff_fall_rel_max: The maximal relative decrease per group
#'  * COLUMN_diff_fall_first: The first decrease.
#'  * COLUMN_diff_fall_first_seq: The first decrease ignoring rows with COLUMN_diff_in_seq == FALSE.
#'  * COLUMN_diff_diff_fall_signif_below: TRUE if a fall was at least <= fall_below or
#'    if the previous value was <= fall_below an increase of <= rel_diff_signif.
#'  * COLUMN_diff_fall_signif_below_first: The first fall in COLUMN_diff_fall_signif_below_first per group.
#'  * COLUMN_diff_falln_signif_below: TRUE if there was at least one per group with COLUMN_diff_fall_signif_below == TRUE.
#'  * COLUMN_diff_max: The maximal increase or decrease per group.
#'  * COLUMN_diff_rel_max: The maximal relative increase or decrease per group.
#'  * COLUMN_diff_in_seq: Is this row the next in sequqnece or was >= 1 skipped?
#' @export
#' @param data A tibble.
#' @param values The column with the values to calculate the difference with.
#' @param arrange The column to arrange the data by.
#' @param ... The columns to group by.
#' @param rel_diff_signif A relative difference above this cut-off is considered significant.
#' @param rise_above A rise above this cut-off is required before it is considered significant.
#' @param fall_below A fall below this cut-off is required before it is considered significant.
#' @return A tibble.
#'
calc_sequential_difference <- function(
  data,
  values,
  arrange,
  ...,
  rel_diff_signif = 0.2,
  rise_above = 0,
  fall_below = 0) {
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  stopifnot(
    tibble::is_tibble(data))

  group <- rlang::enquos(...)
  value <- rlang::enquo(values)
  arrange <- rlang::enquo(arrange)

  base_name <- rlang::quo_name(value)
  d_in_seq = rlang::sym(paste0(base_name, "_diff_in_seq"))
  d = rlang::sym(paste0(base_name, "_diff"))
  d_rel = rlang::sym(paste0(base_name, "_diff_rel"))
  d_signif = rlang::sym(paste0(base_name, "_diff_signif"))
  d_cat = rlang::sym(paste0(base_name, "_diff_cat"))
  d_risen = rlang::sym(paste0(base_name, "_diff_risen"))
  d_rise_max = rlang::sym(paste0(base_name, "_diff_rise_max"))
  d_rise_rel_max = rlang::sym(paste0(base_name, "_diff_rise_rel_max"))
  d_rise_first = rlang::sym(paste0(base_name, "_diff_rise_first"))
  d_rise_first_seq = rlang::sym(paste0(base_name, "_diff_rise_first_seq"))
  d_rise_signif_above = rlang::sym(paste0(base_name, "_diff_rise_signif_above"))
  d_rise_signif_above_first = rlang::sym(paste0(base_name, "_diff_rise_signif_above_first"))
  d_risen_signif_above = rlang::sym(paste0(base_name, "_diff_risen_signif_above"))
  d_fallen = rlang::sym(paste0(base_name, "_diff_fallen"))
  d_fall_max = rlang::sym(paste0(base_name, "_diff_fall_max"))
  d_fall_rel_max = rlang::sym(paste0(base_name, "_diff_fall_rel_max"))
  d_fall_first = rlang::sym(paste0(base_name, "_diff_fall_first"))
  d_fall_first_seq = rlang::sym(paste0(base_name, "_diff_fall_first_seq"))
  d_fall_signif_below = rlang::sym(paste0(base_name, "_diff_fall_signif_below"))
  d_fall_signif_below_first = rlang::sym(paste0(base_name, "_diff_fall_signif_below_first"))
  d_fallen_signif_below = rlang::sym(paste0(base_name, "_diff_fallen_signif_below"))
  d_max = rlang::sym(paste0(base_name, "_diff_max"))
  d_rel_max = rlang::sym(paste0(base_name, "_diff_rel_max"))

  data_result <- data %>%
    dplyr::arrange(!! arrange) %>%
    dplyr::group_by(!!! group) %>%
    dplyr::mutate(
      !! d_in_seq := ifelse(!! arrange - dplyr::lag(!! arrange) == 1, TRUE, FALSE),
      # calc difference
      !! d := !! value - dplyr::lag(!! value),
      # in relation to prev value
      !! d_rel := !! d / dplyr::lag(!! value),
      # is significant?
      !! d_signif := abs(!! d_rel) > rel_diff_signif,
      !! d_cat := ifelse(!! d > 0, "rise",  "fall"),
      !! d_cat := ifelse(!! d_signif, !! d_cat,  "insignificant"),
      # rise
      !! d_risen := "rise" %in% !! d_cat,
      # maximal rise
      !! d_rise_rel_max := max(!! d_rel, na.rm = TRUE),
      !! d_rise_rel_max := ifelse(is.infinite(!! d_rise_rel_max), NA, !! d_rise_rel_max),
      !! d_rise_rel_max := ifelse(!! d_rise_rel_max > rel_diff_signif, !! d_rise_rel_max, NA),
      !! d_rise_max := max(!! d, na.rm = TRUE),
      !! d_rise_max := ifelse(is.infinite(!! d_rise_max), NA, !! d_rise_max),
      !! d_rise_max := ifelse(!! d_rise_rel_max > rel_diff_signif, !! d_rise_max, NA),
      # first rise
      !! d_rise_first := min(ifelse(!! d_cat == "rise", !! arrange, NA), na.rm = TRUE),
      !! d_rise_first := ifelse(is.infinite(!! d_rise_first), NA, !! d_rise_first),
      !! d_rise_first_seq := min(ifelse(!! d_cat == "rise" & !! d_in_seq, !! arrange, NA), na.rm = TRUE),
      !! d_rise_first_seq := ifelse(is.infinite(!! d_rise_first_seq), NA, !! d_rise_first_seq),
      # rise above or if already above rise significantly in relation to before
      !! d_rise_signif_above := ifelse(dplyr::lag(!! value) >= rise_above, !! d_cat == "rise", !! value >= rise_above),
      !! d_rise_signif_above_first := min(ifelse(!! d_rise_signif_above, !! arrange, NA), na.rm = TRUE),
      !! d_rise_signif_above_first := ifelse(is.infinite(!! d_rise_signif_above_first), NA, !! d_rise_signif_above_first),
      !! d_risen_signif_above := any(!! d_rise_signif_above),
      !! d_risen_signif_above := ifelse(is.na(!! d_risen_signif_above), FALSE, !! d_risen_signif_above),
      # fall
      !! d_fallen := "fall" %in% !! d_cat,
      # maximal fall
      !! d_fall_rel_max := min(!! d_rel, na.rm = TRUE),
      !! d_fall_rel_max := ifelse(is.infinite(!! d_fall_rel_max), NA, !! d_fall_rel_max),
      !! d_fall_rel_max := ifelse(!! d_fall_rel_max < -1 * rel_diff_signif, !! d_fall_rel_max, NA),
      !! d_fall_max := min(!! d, na.rm = TRUE),
      !! d_fall_max := ifelse(is.infinite(!! d_fall_max), NA, !! d_fall_max),
      !! d_fall_max := ifelse(!! d_fall_rel_max < -1 * rel_diff_signif, !! d_fall_max, NA),
      # first fall
      !! d_fall_first := min(ifelse(!! d_cat == "fall", !! arrange, NA), na.rm = TRUE),
      !! d_fall_first := ifelse(is.infinite(!! d_fall_first), NA, !! d_fall_first),
      !! d_fall_first_seq := min(ifelse(!! d_cat == "fall" & !! d_in_seq, !! arrange, NA), na.rm = TRUE),
      !! d_fall_first_seq := ifelse(is.infinite(!! d_fall_first_seq), NA, !! d_fall_first_seq),
      # fall below or if already below fall significantly in relation to before
      !! d_fall_signif_below := ifelse(dplyr::lag(!! value) <= fall_below, !! d_cat == "fall", !! value <= fall_below),
      !! d_fall_signif_below_first := min(ifelse(!! d_fall_signif_below, !! arrange, NA), na.rm = TRUE),
      !! d_fall_signif_below_first := ifelse(is.infinite(!! d_fall_signif_below_first), NA, !! d_fall_signif_below_first),
      !! d_fallen_signif_below := any(!! d_fall_signif_below),
      !! d_fallen_signif_below := ifelse(is.na(!! d_fallen_signif_below), FALSE, !! d_fallen_signif_below),
      # maximal absolute difference
      !! d_max := ifelse(!! d_fall_max * -1 > !! d_rise_max, !! d_fall_max, !! d_rise_max),
      !! d_rel_max := ifelse(!! d_fall_rel_max * -1 > !! d_rise_rel_max, !! d_fall_rel_max, !! d_rise_rel_max)) %>%
    dplyr::ungroup()

  return(data_result)
}

#'
#' Turn long data with subgroups into wide data.
#'
#' @description
#' Turns data in a column "column" which is divided into groups by a column
#' "group" into a tibble with a column for each group (each unique value in
#' "group").
#'
#' @export
#' @param data A tibble containing the data.
#' @param column Which column to use.
#' @param group The column that parts the variable into subgroups.
#' @param id A column holding ids to identify the same entity in each subgroup.
#' @param ... Further ids.
#' @return list
#'
get_variable_by_groups <- function(data, column, group, id, ...) {
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  id <- rlang::enquo(id)
  column <- rlang::enquo(column)
  group <- rlang::enquo(group)
  keep <- rlang::quos(...)
  result <- list()

  # remove all columns we don't need
  # and convert from long to wide
  return(data %>%
    dplyr::select(!! id, !! column, !! group, !!! keep) %>%
    dplyr::arrange(!! group) %>%
    tidyr::spread(!! group, !! column))
}
