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

  result[[paste0("data_set_", rlang::quo_name(set), "_", sets[[1]])]] <-
    data_set_1
  result[[paste0("data_set_", rlang::quo_name(set), "_", sets[[2]])]] <-
    data_set_2
  result[["levels"]] <- levels

  test <- ifelse(paired, "signed_rank", "rank-sum")

  data_wilcoxon <- tibble::as.tibble(merge(
    data_set_1,
    data_set_2,
    by = rlang::quo_name(id),
    suffixes = c("_x", "_y"),
    all = TRUE
  ))

  if (paired) {
    data_wilcoxon <- data_wilcoxon %>%
      dplyr::filter(stats::complete.cases(.))
  }
  result[["data_wilcoxon"]] <- data_wilcoxon

  for (group in levels) {
    group <- as.character(group)
    #return(dplyr::pull(data_wilcoxon, !! rlang::sym(paste0(group, "_x"))))

    wilcoxon <- stats::wilcox.test(
      dplyr::pull(data_wilcoxon, !! rlang::sym(paste0(group, "_x"))),
      dplyr::pull(data_wilcoxon, !! rlang::sym(paste0(group, "_y"))),
      paired = paired)

    result[["groups"]][[group]] <- wilcoxon
    print(paste0("Wilcoxon's ", test," test between both sets for group \"",
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
  print(paste0("Friedman's Test: p = ", format_p(friedman$p.value)))

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
        print(paste0("Wilcoxon's signed-rank test on \"", reference_group,
          "\" ~ \"", group, "\": p = ", wilcoxon$p.value))
      }
    }
  }

  return(result)
}
#'
#' Extract p-values from a result list.
#'
#' @description
#' Convenience function for extracting p-values from result lists as returned by
#' `test_wilcoxon_for_groups` or `test_friedman`. The result is vector with the
#' group names as names and can be used by `ggplot_annotate_signif`.
#'
#' @export
#' @param result_list The result list.
#' @param names_num Convert the group names to numeric.
#' @return vector
#'
extract_p_values_from_result_list <- function(result_list, names_num = FALSE) {
  p_values <- c()

  for (group in names(result_list[["groups"]])) {
    tmp <- names(p_values)
    p_values <- c(p_values, result_list[["groups"]][[group]]$p.value)
    names(p_values) <- c(tmp, group)
  }

  rm(tmp)
  return(p_values)
}

#'
#' Extract p-values from a result list.
#'
#' @description
#' Convenience function for extracting p-values from result lists as returned by
#' `test_wilcoxon_for_groups` or `test_friedman`. The result is vector with the
#' group names as names and can be used by `ggplot_annotate_signif`.
#'
#' @export
#' @param data A tibble containing the data.
#' @param x The column used for the x-axis.
#' @param y The column used for the y-axis.
#' @param p_values A vector containing p-values, the name marks the
#' corresponding x.
#' @param label Either "stars" or "p-values".
#' @param margin The distance above the highest y at x in units of y.
#' @param static_y Use the maximum y + margin for all x.
#' @param steps If `label` is "stars" add an "*" for each step teh p_value is
#' below.
#' @param font_face The font face used for the labels ("plain"|"bold"|"italic"|
#' "bold.italic")
#' @param font_size The font size in points.
#' @return list
#'
ggplot_annotate_signif <- function(data, x, y, p_values, label = "stars",
  margin = 1, static_y = TRUE, steps = eenv_signif_steps,
  font_face = "plain", font_size = eenv_theme[[1]]$text$size) {
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  steps <- sort(steps, decreasing = TRUE)

  result <- list()

  # get maximal y at x
  data <- data %>%
    dplyr::group_by(!! x) %>%
    dplyr::summarize(
      !! y := max(!! y) + margin)

  y_values <- dplyr::pull(data, !! y)
  names(y_values) <- dplyr::pull(data, !! x)
  i = 1

  for (x_current in names(p_values)) {
    p_value = p_values[[x_current]]
    label_current = ""
    if (x_current %in% names(y_values) & p_value <= max(steps) ) {
      # dynamic y means max y at this x + margin
      if (! static_y) {
        y_current = y_values[[x_current]]
      } else {
        y_current = max(y_values)
      }

      if (label == "stars") {
        for (step in steps) {
          if (p_value >= step) {
            break
          }
          label_current = paste0(label_current, "*")
        }
      } else {
        label_current = format_p(p_value)
      }

      result[[i]] = ggplot2::annotate(
        geom = "text",
        x = as.numeric(x_current),
        y = y_current,
        label = label_current,
        vjust = "center",
        hjust = "middle",
        fontface = font_face,
        size = font_size / ggplot2::.pt)
      i = i + 1
    }
  }

  return(result)
}

calc_crosstable_int <- function(data, x, y, show = FALSE, id = 1) {
  x = rlang::quo_name(x)
  y = rlang::quo_name(y)
  if (show) {
    result <- gmodels::CrossTable(
      data[[x]], data[[y]], fisher = TRUE, expected = TRUE)
  } else {
    utils::capture.output(
      result <- gmodels::CrossTable(
        data[[x]], data[[y]], fisher = TRUE, expected = TRUE),
      file="/dev/null", type="output")
  }
  print(sprintf("%s) %s by %s : Chi-Squared: %s; Fisher: %s",
                  as.character(id), x, y, format_p(result$chisq$p.value),
                  format_p(result$fisher.ts$p.value)))
  return(result)
}

calc_roc_int <- function(data, predictor, response, threshold = NULL,
  show = FALSE, id = 1, print_auc = FALSE, print_points = FALSE,
  print_steps = FALSE, print_threshold) {
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  result <- test_roc_empiric(
    data = data,
    predictor = !! predictor,
    response = !! response,
    print_auc = print_auc,
    print_points = print_points,
    print_steps = print_steps)

  if (is.numeric(threshold) & print_threshold) {
    # find the point in the dataset which is closest to the given threshold
    # as it is unlikely that the exact threshold will be given as a parameter
    data_point <- result$steps_summarised %>%
      dplyr::mutate(
        diff = abs(threshold - predictor)) %>%
      dplyr::slice(which.min(diff))
    if (nrow(data_point) > 0) {
      point_x <- data_point[[1, "fpr"]]
      point_y <- data_point[[1, "tpr"]]
      result$plot <- result$plot +
        ggplot2::annotate("point", x = point_x, y = point_y)
    }
  }

  if (show) {
    print(result$plot)
    print(result$steps_summarised)
  }
  print(sprintf("%s) %s by %s: AUC: %s",
                as.character(id), rlang::quo_name(response),
                rlang::quo_name(predictor),
                format_number(result$AUC, decimals = 2)))
  return(result)
}

#'
#' Calculate a crosstable or ROC and test metrics for the given variables.
#'
#' @description
#' Calculate a crosstable or ROC and test metrics (if a cutoff is given) for the
#' given variables.
#'
#' It takes sets of the form c(ID, PREDICTOR, RESPONSE, RESPONSE_POS, CUTOFF).
#' @export
#' @param data A tibble containing the data.
#' @param ... Sets of variables to analyse (see above).
#' @param alpha The alpha used for testing.
#' @param show Show results (tibbles / plots).
#' @param force_show A list of ids of results that should be shown regardless of
#' `show`.
#' @param print_auc Print the AUC onto the ROC plot?
#' @param print_points Print all data points onto the ROC plot?
#' @param print_steps Print only the steps onto the ROC plot?
#' @param print_threshold Mark the given threshold in the ROC plot?
#' @return list
#'
test_simple_predictions <- function(data, ..., alpha = eenv_alpha, show = FALSE,
                                    force_show = c(), print_auc = FALSE,
                                    print_points = FALSE, print_steps = FALSE,
                                    print_threshold = FALSE) {
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`
  result = list()

  for (param_list in list(...)) {
    if (length(param_list) < 3) {
      warning("Encountered malformed set.")
    }
    response_pos = NULL
    threshold = NULL

    id <- param_list[[1]]
    predictor <- rlang::sym(param_list[[2]])
    response <- rlang::sym(param_list[[3]])

    if (length(param_list) > 3) {
      response_pos <- param_list[[4]]
    }

    if (length(param_list) > 4){
      threshold <- as.numeric(param_list[[5]])
    }

    show_item = FALSE
    if (show | id %in% force_show) {
      show_item = TRUE
    }

    if (is.null(response_pos)) {
      # crosstables are only sensible if the predictor is categorical
      # this excludes all other tests as used here
      result[[id]][["crosstable"]] <- calc_crosstable_int(
        data = data,
        x = predictor,
        y = response,
        show = show_item,
        id = id)
    } else {
      data_tmp <- data %>%
        dplyr::mutate(
          !! response := !! response == response_pos) %>%
        dplyr::select(!! predictor, !! response) %>%
        dplyr::filter(stats::complete.cases(.))

      result[[id]][["roc"]] <- calc_roc_int(
        data = data_tmp,
        predictor = predictor,
        response = response,
        threshold = threshold,
        show = show_item,
        id = id,
        print_auc = print_auc,
        print_points = print_points,
        print_steps = print_steps,
        print_threshold )

      if (is.numeric(threshold)) {
        data_tmp <- data_tmp %>%
          dplyr::mutate(!! predictor := !! predictor >= threshold)

        result[[id]][["metrics"]] <- test_get_metrics(
          data = data_tmp,
          pred_cond = !! predictor,
          pred_cond_targ = TRUE,
          act_cond = !! response,
          act_cond_targ = TRUE)

        result[[id]][["relation"]] <- test_get_relation(
          data = data_tmp,
          pred_cond = !! predictor,
          pred_cond_targ = TRUE,
          act_cond = !! response,
          act_cond_targ = TRUE,
          alpha = alpha)

        if (show_item) {
          print(result[[id]][["relation"]])
        }

        # result[[id]][["relation_formatted"]] <- test_format_relations(
        #   raw_relations = result[[id]][["relation"]],
        #   relations = relations)
      }
    }

    if (show_item) {
      invisible(readline(prompt="Press [enter] to continue"))
    }
  }
  return(result)
}

#'
#' Calculate a crosstable, ROC and metrics for each combination of variables.
#'
#' @description
#' Calculate a crosstable, a ROC and test metrics (if a cutoff is given) for
#' each combination of the given variables.
#'
#' @export
#' @param data A tibble containing the data.
#' @param predictors A list of predictors (strings).
#' @param responses A list of responses (strings).
#' @param response_pos The value of `responses` considered positive.
#' @param alpha The alpha used for testing.
#' @return list
#'
scan_simple_predictions <- function(data, predictors, responses, response_pos,
                                    alpha = eenv_alpha) {
  result = list()

  for (response in responses) {
    for (predictor in predictors) {
      result[[paste0(predictor,"~",response)]] <- test_simple_predictions(
        data = data,
        c(1, predictor, response, response_pos, NULL),
        alpha = alpha,
        show = TRUE,
        print_auc = TRUE,
        print_points = FALSE,
        print_steps = FALSE,
        print_threshold = FALSE
      )
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
#' @param data A tibble containing the data.
#' @param variables_x A list of variable names as strings.
#' @param variables_y A list of variable names as strings.
#' @return list
#'
#' @name scan_crosstables-deprecated
#' @seealso `eenv_deprecated`
#' @keywords internal
#'
NULL

#' @rdname eenv-deprecated
#' @section `scan_crosstables`:
#' For `scan_crosstables` use `scan_simple_predictions`.
#'
#' @export
#'
scan_crosstables <- function(data, variables_x, variables_y) {
  .Deprecated("scan_simple_prediction")
  return(scan_simple_predictions(
    data = data,
    predictors = variables_x,
    responses = variables_y,
    response_pos = NULL))
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
#' @param data A tibble containing the data.
#' @param ... Sets of variables to analyse (see above).
#' @return list
#'
#' @name calc_crosstables-deprecated
#' @seealso `eenv_deprecated`
#' @keywords internal
#'
NULL

#' @rdname eenv-deprecated
#' @section `calc_crosstables`:
#' For `calc_crosstables` use `test_simple_predictions`.
#'
#' @export
#'
calc_crosstables <- function(data, ...) {
  .Deprecated("test_simple_prediction")
  return(test_simple_predictions(
    data = data,
    ...,
    alpha = eenv_alpha,
    show = TRUE))
}

#'
#' Calculate all Reciever-Operator-Curves (ROCs).
#'
#' @description
#' Calculate all Reciever-Operator-Curves (ROCs) and the corresponding area
#' under the curve (AUC).
#'
#' @param data A tibble containing the data.
#' @param predictors A list of variable names.
#' @param responses A list of variable names.
#' @param response_pos The response that will be interpreted as an event.
#' @return NULL
#'
#' @name scan_rocs-deprecated
#' @seealso `eenv_deprecated`
#' @keywords internal
#'
NULL

#' @rdname eenv-deprecated
#' @section `scan_rocs`:
#' For `scan_rocs` use `test_simple_predictions`.
#'
#' @export
#'
scan_rocs <- function(data, predictors, responses, response_pos = TRUE) {
  .Deprecated("scan_simple_prediction")
  return(scan_simple_predictions(
    data = data,
    predictors = predictors,
    responses = responses,
    response_pos = response_pos))
}
