#'
#' Calculate and plot a qq-plot for all groups.
#'
#' @description
#' Calculate and plot a qq-plot with actual values as axes instead of z-values
#' for all groups. Useful when checking series of measurements.
#'
#' @export
#' @param data A tibble containing the data.
#' @param column Which column to use.
#' @param group The column that parts the variable into subgroups.
#' @param id A column holding ids to identify the same entity in each subgroup.
#' @param method How to calculate ranks.
#' @return list
#'
plot_qq_for_groups <- function(data, column, group, id, method = "middle") {
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  column <- rlang::enquo(column)
  group <- rlang::enquo(group)
  id <- rlang::enquo(id)
  result <- list()

  data <- get_variable_by_groups(data, !! column, !! group, !! id)
  # the remaining columns are the groups and !! id
  levels <- dimnames(dplyr::select(data, - !! id))[[2]]
  result[["data"]] <- data
  result[["levels"]] <- levels

  for (group in levels) {
    result[["groups"]][[group]] <- plot_qq(data, !! rlang::sym(group), method)
  }
  return(result)
}

#'
#' Friedman's test for a series of data.
#'
#' @description
#' Calculate Friedman's test for a series of data. If it shows that the
#' groups in the series are not equal it will calculate a Wilcoxon's signed-
#' rank test comparing each group to the reference group.
#'
#' @export
#' @param data A tibble containing the data.
#' @param column Which column to use.
#' @param group The column that parts the variable into subgroups.
#' @param id A column holding ids to identify the same entity in each subgroup.
#' @param reference_group The value in column `group` that indicates the
#' reference group.
#' @param ignore_friedman Ignore the result of Friedman's test and compare all
#' subgroups.
#' @return list
#'
test_friedman <- function(data, column, group, id, reference_group,
  ignore_friedman = FALSE) {
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  id <- rlang::enquo(id)
  variable <- rlang::enquo(variable)
  group <- rlang::enquo(group)
  result <- list()

  data <- get_variable_by_groups(data, !! variable, !! group, !! id)
  # the remaining columns are the groups and !! id
  levels <- dimnames(data %>% dplyr::select(- !! id))[[2]]

  if (! reference_group %in% levels) {
    stop(paste0("Reference group (\"", reference_group, "\") not in column \"",
      rlang::quo_name(group), "\""))
  }

  result[["data"]] <- data
  result[["levels"]] <- levels

  # friedman.test works on matrices and does not like NAs
  data_friedman <- data %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::select(-!! id) %>%
    as.matrix(.)

  n <- nrow(data)
  n_friedman <- nrow(data_friedman)
  if (n_friedman / n < 0.95) {
    warning(paste0("Dataset used for Friedman's test is smaller than ",
                   "what was given (", format_perc(n_friedman / n), ")!"))
  }

  friedman <- stats::friedman.test(data_friedman)
  result[["friedman"]] <- friedman
  message(paste0("Friedman's Test: p = ", format_p(friedman$p.value)))

  # if Friedman's test indicated the median is not the same we have to
  # test which group differed from the baseline
  if (friedman$p.value <= 0.05 | ignore_friedman) {
    reference_group <- as.character(reference_group)
    reference_group_sym <- rlang::sym(reference_group)

    # loop over every group and compare it to the reference_group
    for (group in levels) {
      group <- as.character(group)
      if (! group == reference_group) {
        group_sym <- rlang::sym(group)
        # Wilcoxon's signed-rank test only accepts complete data
        # this may differ considerably from the data used by Friedman's test
        # because its more likely for both values to be available than for all
        # values
        data_wilcoxon <- data %>%
          dplyr::select(!! reference_group_sym, !! group_sym) %>%
          dplyr::filter(stats::complete.cases(.))
        wilcoxon <- stats::wilcox.test(
          dplyr::pull(data_wilcoxon, !! reference_group_sym),
          dplyr::pull(data_wilcoxon, !! group_sym),
          paired = TRUE)
        result[["groups"]][[group]] <- wilcoxon
        p <- ifelse(is.na(wilcoxon$p.value), "NA", format_p(wilcoxon$p.value))
        message("Wilcoxon's signed-rank test on \"", reference_group, "\" ~ \"",
          group, "\": p = ", p)
      }
    }
  }

  return(result)
}
