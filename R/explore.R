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
#' Calculate Wilcoxon's test (paired or not) for multiple groups.
#'
#' @description
#' Calculate Wilcoxon's test (paired or not) between two sets for multiple
#' groups. The groups are specified by column `group`, the sets by column
#' `set`, e.g.:
#'
#' Suppose you had two sets of plants comprising three plants each. One set will
#' be placed by the window, the other in the corner. You measure height once a
#' week for ten weeks. Your data looks like:
#'
#'      ID  set  week  height
#'   1   1    1     1       6
#'   2   2    1     1       5
#'   3   3    1     1       4
#'   4   4    2     1       5
#'   5   5    2     1       4
#'   6   6    2     1       6
#'   7   1    1     2       7
#'   8   2    1     2       7
#'   9   3    1     2       6
#'  10   4    2     2       6
#'  11   5    2     2       4
#'  12   6    2     2       6
#'  13   1    1     3       8
#'  14   2    1     3       9
#'  15   3    1     3       8
#'  ...
#'
#'  You would specify: `id = ID`, `set = set`,  `group = week`,
#'  `column = height` to compare height between both sets (`set``) for each week
#'  (`group`).
#' @export
#' @param data A tibble containing the data.
#' @param column Which column to use.
#' @param group The column that parts the variable into groups.
#' @param id A column holding ids to identify the same entity in each group.
#' @param set A column indicating the two sets to compare values in groups with.
#' @param paired Calculate a paired (signed-rank) test or not (rank-sum).
#' @return list
#'
test_wilcoxon_for_groups <- function(data, column, group, id, set, paired) {
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  column <- rlang::enquo(column)
  group <- rlang::enquo(group)
  id <- rlang::enquo(id)
  set <- rlang::enquo(set)
  result <- list()

  sets <- data %>%
    dplyr::distinct(!! set) %>%
    dplyr::pull(!! set)

  if (length(sets) != 2) {
    stop("This function needs exactly two sets.")
  }

  data_set_1 <- get_variable_by_groups(
    dplyr::filter(data, !! set == sets[[1]]), !! column, !! group, !! id)
  data_set_2 <- get_variable_by_groups(
    dplyr::filter(data, !! set == sets[[2]]), !! column, !! group, !! id)
  # the remaining columns are the groups and !! id
  levels_1 <- dimnames(dplyr::select(data_set_1, - !! id))[[2]]
  levels_2 <- dimnames(dplyr::select(data_set_2, - !! id))[[2]]
  if (! setequal(levels_1, levels_2)) {
    stop("The sets contain different groups")
  } else {
    levels <- levels_1
  }

  result[[paste0("data_set_", quo_name(set), "_", sets[[1]])]] <- data_set_1
  result[[paste0("data_set_", quo_name(set), "_", sets[[2]])]] <- data_set_2
  result[["levels"]] <- levels

  test <- ifelse(paired, "signed_rank", "rank-sum")

  data_wilcoxon <- tibble::as.tibble(merge(
    data_set_1,
    data_set_2,
    by = quo_name(id),
    suffixes = c("_x", "_y"),
    all = TRUE
  ))

  if (paired) {
    data_wilcoxon <- data_wilcoxon %>%
      filter(stats::complete.cases(.))
  }
  result[["data_wilcoxon"]] <- data_wilcoxon

  for (group in levels) {
    group <- as.character(group)
    #return(dplyr::pull(data_wilcoxon, !! rlang::sym(paste0(group, "_x"))))

    wilcoxon <- wilcox.test(
      dplyr::pull(data_wilcoxon, !! rlang::sym(paste0(group, "_x"))),
      dplyr::pull(data_wilcoxon, !! rlang::sym(paste0(group, "_y"))),
      paired = paired)

    result[["groups"]][[group]] <- wilcoxon
    message(paste0("Wilcoxon's ", test," test between both sets for group \"",
                   group, "\": p = ", format_p(wilcoxon$p.value)))
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
  column <- rlang::enquo(column)
  group <- rlang::enquo(group)
  result <- list()

  data <- get_variable_by_groups(data, !! column, !! group, !! id)
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
        message(paste0("Wilcoxon's signed-rank test on \"", reference_group,
          "\" ~ \"", group, "\": p = ", wilcoxon$p.value))
      }
    }
  }

  return(result)
}

#'
#' Calculate all CrossTables for the given variables.
#'
#' @description
#' Give an overview of possible correlations by calculating all CrossTables for
#' the given variables.
#'
#' @export
#' @param data A tibble containing the data.
#' @param variables_x A list of variable names as strings.
#' @param variables_y A list of variable names as strings.
#' @return list
#'
scan_crosstables <- function(data, variables_x, variables_y) {
  for (y in variables_y) {
    message(y)
    for (x in variables_x) {
      utils::capture.output(
        gmodels::CrossTable(
          data[[x]], data[[y]], fisher = TRUE, expected = TRUE),
        file="/dev/null", type="output")
      message(sprintf("%s, %s - Chi-Squared: %s; Fisher: %s", x, y,
        format_p(res$chisq$p.value), format_p(res$fisher.ts$p.value)))
    }
    invisible(readline(prompt="Press [enter] to continue"))
  }
}

#'
#' Calculate the CrossTable for the given variables.
#'
#' @description
#' Does not do much more than CrossTable does but saves some keystrokes when
#' doing a lot of CrossTables.
#'
#' It takes sets of the form c(ID, X, Y, SHOW), where SHOW specifies whether the
#' crosstable should be shown.
#' The output of `gmodels::CrossTable(data[[X]], data[[Y]], fisher = TRUE,
#' expected = TRUE)` will be saved in the returned list `list[[ID]]`.
#' @export
#' @param data A tibble containing the data.
#' @param ... Sets of variables to analyse (see above).
#' @return list
#'
calc_crosstables <- function(data, ...) {
  result = list()
  for (param_list in list(...)) {
    id <- param_list[[1]]
    x <- param_list[[2]]
    y <- param_list[[3]]
    show <- param_list[[4]]
    if (show) {
      res <- gmodels::CrossTable(
        data[[x]], data[[y]], fisher = TRUE, expected = TRUE)
    } else {
      utils::capture.output(
        res <- gmodels::CrossTable(
          data[[x]], data[[y]], fisher = TRUE, expected = TRUE),
        file="/dev/null", type="output")
    }
    message(sprintf("%s: %s, %s - Chi-Squared: %s; Fisher: %s",
      as.character(id), x, y, format_p(res$chisq$p.value),
      format_p(res$fisher.ts$p.value)))
    if (show) {
      invisible(readline(prompt="Press [enter] to continue"))
    }
    result[[id]] <- res
  }
  return(result)
}

#'
#' Calculate all Reciever-Operator-Curves (ROCs).
#'
#' @description
#' Calculate all Reciever-Operator-Curves (ROCs) and the corresponding area
#' under the curve (AUC).
#'
#' @export
#' @param data A tibble containing the data.
#' @param predictors A list of variable names.
#' @param responses A list of variable names.
#' @param response_pos The response that will be interpreted as an event.
#' @return NULL
#'
scan_rocs <- function(data, predictors, responses, response_pos = TRUE) {
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  for (response in responses) {
    message(rlang::quo_name(response))

    data_tmp <- data %>%
      dplyr::mutate(
        !! response := ifelse(!! response == response_pos, TRUE, FALSE)
      )

    for (predictor in predictors) {
      data_tmp2 <- data_tmp %>%
        dplyr::select(!! predictor, !! response) %>%
        dplyr::filter(stats::complete.cases(.))
      res <- test_roc_empiric(data_tmp2, !! predictor, !! response, TRUE)
      print(res$plot)

      steps <- res$steps %>%
        dplyr::group_by(truepositives) %>%
        dplyr::summarize(
          threshold = max(predictor),
          sensitivity = first(sensitivity),
          specificity = first(specificity),
          fpr = max(fpr),
          tpr = max(tpr))
      print(steps)
    }
  }
}
