#'
#' Work with test results vs. actual conditions.
#'
#' @description
#' Functions to retrieve true positives, true negatives, ...
#'
#' Structure: Suppose you designed a test to predict if a person lied. Your
#' tibble might look like this:
#'
#' |   | test_result | person_lied | comment           |
#' |---|-------------|-------------|-------------------|
#' | 1 | pos         | yes         | <- true positive  |
#' | 2 | pos         | no          | <- false positive |
#' | 3 | neg         | no          | <- true negative  |
#' | 4 | pos         | yes         | ...               |
#' | 5 | neg         | no          | ...               |
#'
#' Your predicted condition (`pred_cond`) would be `test_result` and the
#' targeted condition (`pred_cond_targ`) would be `pos`. The actual condition
#' (`act_cond`) would be `person_lied` with targeted condition
#' (`act_cond_targ`) `yes`.
#'
#' @name test_get_rows
#' @param data A tibble holding the data
#' @param pred_cond The column holding the predicted conditions / test results.
#' @param act_cond The column holding the true conditions.
#' @param pred_cond_targ The value that signifies the targeted predicted
#' condition.
#' @param act_cond_targ The value that signifies the targeted true condition.
#' @return A tibble containing the true positives / negatives / ...
#'
NULL

test_get_rows_int <- function(data, pred_cond, act_cond, ...) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  expression <- rlang::enexprs(...)
  return(data %>% dplyr::filter(
    ! is.na(!! pred_cond) & ! is.na(!! act_cond), !!! expression)
  )
}

#' @export
#' @rdname test_get_rows
test_get_true_positives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_rows_int(
    data = data,
    pred_cond = !! pred_cond,
    act_cond = !! act_cond,
    !! pred_cond == !! pred_cond_targ, !! act_cond == !! act_cond_targ))
}

#' @export
#' @rdname test_get_rows
test_get_false_positives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_rows_int(
    data = data,
    pred_cond = !! pred_cond,
    act_cond = !! act_cond,
    !! pred_cond == !! pred_cond_targ, !! act_cond != !! act_cond_targ))
}

#' @export
#' @rdname test_get_rows
test_get_actual_positives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_rows_int(
    data = data,
    pred_cond = !! pred_cond,
    act_cond = !! act_cond,
    !! act_cond == !! act_cond_targ))
}

#' @export
#' @rdname test_get_rows
test_get_predicted_positives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_rows_int(
    data = data,
    pred_cond = !! pred_cond,
    act_cond = !! act_cond,
    !! pred_cond == !! pred_cond_targ))
}

#' @export
#' @rdname test_get_rows
test_get_true_negatives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_rows_int(
    data = data,
    pred_cond = !! pred_cond,
    act_cond = !! act_cond,
    !! pred_cond != !! pred_cond_targ, !! act_cond != !! act_cond_targ))
}

#' @export
#' @rdname test_get_rows
test_get_false_negatives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_rows_int(
    data = data,
    pred_cond = !! pred_cond,
    act_cond = !! act_cond,
    !! pred_cond != !! pred_cond_targ, !! act_cond == !! act_cond_targ))
}

#' @export
#' @rdname test_get_rows
test_get_actual_negatives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_rows_int(
    data = data,
    pred_cond = !! pred_cond,
    act_cond = !! act_cond,
    !! act_cond != !! act_cond_targ))
}

#' @export
#' @rdname test_get_rows
test_get_predicted_negatives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_rows_int(
    data = data,
    pred_cond = !! pred_cond,
    act_cond = !! act_cond,
    !! pred_cond != !! pred_cond_targ))
}

#'
#' Work with test results vs. actual conditions.
#'
#' @description
#' Functions to retrieve true positives, prevalence, ...
#'
#' Structure: Suppose you designed a test to predict if a person lied. Your
#' tibble might look like this:
#'
#' |   | test_result | person_lied | comment           |
#' |---|-------------|-------------|-------------------|
#' | 1 | pos         | yes         | <- true positive  |
#' | 2 | pos         | no          | <- false positive |
#' | 3 | neg         | no          | <- true negative  |
#' | 4 | pos         | yes         | ...               |
#' | 5 | neg         | no          | ...               |
#'
#' Your predicted condition (`pred_cond`) would be `test_result` and the
#' targeted condition (`pred_cond_targ`) would be `pos`. The actual condition
#' (`act_cond`) would be `person_lied` with targeted condition
#' (`act_cond_targ`) `yes`.
#'
#' @name test_get_values
#' @param data A tibble holding the data
#' @param pred_cond The column holding the predicted conditions / test results.
#' @param act_cond The column holding the true conditions.
#' @param pred_cond_targ The value that signifies the targeted predicted
#' condition.
#' @param act_cond_targ The value that signifies the targeted true condition.
#' @param prevalence The prevalence in the reference population.
#' @param alpha The alpha level.
#' @param confusion_matrix The confusion matrix as returned by
#' `test_get_confusion_list`, `data` will be ignored.
#' @param haldane_anscombe_correction Apply Haldane Anscombe correction if
#' necessary.
#' @return variable
#'
NULL

test_get_confusion_matrix <- function(data, pred_cond, act_cond,
                                      pred_cond_targ = TRUE,
                                      act_cond_targ = TRUE,
                                      prevalence = NULL,
                                      confusion_matrix = NULL) {
  if (!is.null(confusion_matrix)) {
    return(confusion_matrix)
  }

  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)

  data <- data %>%
    dplyr::select(!! pred_cond, !! act_cond) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::mutate(
      tp = !! pred_cond == pred_cond_targ & !! act_cond == act_cond_targ,
      fp = !! pred_cond == pred_cond_targ & !! act_cond != act_cond_targ,
      tn = !! pred_cond != pred_cond_targ & !! act_cond != act_cond_targ,
      fn = !! pred_cond != pred_cond_targ & !! act_cond == act_cond_targ)

  total <- nrow(data)
  # the confusion matrix, TRUE will be counted and summed up
  mat <- matrix(c(
    sum(dplyr::pull(data, tp)),
    sum(dplyr::pull(data, fp)),
    sum(dplyr::pull(data, fn)),
    sum(dplyr::pull(data, tn))),
    ncol = 2,
    byrow = TRUE)

  colnames(mat) <- c("CP", "CN")
  rownames(mat) <- c("PP", "PN")

  return(mat)
}

#' @export
#' @rdname test_get_values
test_get_sensitivity <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE, prevalence = NULL, confusion_matrix = NULL) {
	`%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
	pred_cond <- rlang::enquo(pred_cond)
	act_cond <- rlang::enquo(act_cond)
  mat <- test_get_confusion_matrix(data = data, pred_cond = !! pred_cond,
    act_cond = !! act_cond, pred_cond_targ = pred_cond_targ,
    act_cond_targ = act_cond_targ, prevalence = prevalence,
    confusion_matrix = confusion_matrix)
  tp <- mat[["PP", "CP"]]
  fn <- mat[["PN", "CP"]]
  return(tp / (tp + fn))
}

#' @export
#' @rdname test_get_values
test_get_specificity <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE, prevalence = NULL, confusion_matrix = NULL) {
	`%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
	pred_cond <- rlang::enquo(pred_cond)
	act_cond <- rlang::enquo(act_cond)
  mat <- test_get_confusion_matrix(data = data, pred_cond = !! pred_cond,
    act_cond = !! act_cond, pred_cond_targ = pred_cond_targ,
    act_cond_targ = act_cond_targ, prevalence = prevalence,
    confusion_matrix = confusion_matrix)
  tn <- mat[["PN", "CN"]]
  fp <- mat[["PP", "CN"]]
  return(tn / (tn + fp))
}

#' @export
#' @rdname test_get_values
test_get_prevalence <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE, prevalence = NULL, confusion_matrix = NULL) {
  if (!is.null(prevalence)) return(prevalence)
	`%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
	pred_cond <- rlang::enquo(pred_cond)
	act_cond <- rlang::enquo(act_cond)
  mat <- test_get_confusion_matrix(data = data, pred_cond = !! pred_cond,
    act_cond = !! act_cond, pred_cond_targ = pred_cond_targ,
    act_cond_targ = act_cond_targ, prevalence = prevalence,
    confusion_matrix = confusion_matrix)
  tp <- mat[["PP", "CP"]]
  fn <- mat[["PN", "CP"]]
  return((tp + fn) / sum(mat))
}

#' @export
#' @rdname test_get_values
test_get_positive_predictive_value <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE, prevalence = NULL, confusion_matrix = NULL) {
	`%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
	pred_cond <- rlang::enquo(pred_cond)
	act_cond <- rlang::enquo(act_cond)
  mat <- test_get_confusion_matrix(data = data, pred_cond = !! pred_cond,
    act_cond = !! act_cond, pred_cond_targ = pred_cond_targ,
    act_cond_targ = act_cond_targ, prevalence = prevalence,
    confusion_matrix = confusion_matrix)

  prev <- test_get_prevalence(NULL, NULL, NULL, confusion_matrix = mat, prevalence = prevalence)
  spec <- test_get_specificity(NULL, NULL, NULL, confusion_matrix = mat)
  sens <- test_get_sensitivity(NULL, NULL, NULL, confusion_matrix = mat)
  return((sens * prev) / ((sens * prev) + ((1-spec) * (1-prev))))
}

#' @export
#' @rdname test_get_values
test_get_negative_predictive_value <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE, prevalence = NULL, confusion_matrix = NULL) {
	`%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
	pred_cond <- rlang::enquo(pred_cond)
	act_cond <- rlang::enquo(act_cond)
  mat <- test_get_confusion_matrix(data = data, pred_cond = !! pred_cond,
    act_cond = !! act_cond, pred_cond_targ = pred_cond_targ,
    act_cond_targ = act_cond_targ, prevalence = prevalence,
    confusion_matrix = confusion_matrix)

  prev <- test_get_prevalence(NULL, NULL, NULL, confusion_matrix = mat, prevalence = prevalence)
  spec <- test_get_specificity(NULL, NULL, NULL, confusion_matrix = mat)
  sens <- test_get_sensitivity(NULL, NULL, NULL, confusion_matrix = mat)
  return((spec * (1-prev)) / ((spec * (1-prev)) + ((1-sens) * prev)))
}

#' @export
#' @rdname test_get_values
test_get_metrics <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE, prevalence = NULL, confusion_matrix = NULL) {
	`%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
	pred_cond <- rlang::enquo(pred_cond)
	act_cond <- rlang::enquo(act_cond)
  mat <- test_get_confusion_matrix(data = data, pred_cond = !! pred_cond,
    act_cond = !! act_cond, pred_cond_targ = pred_cond_targ,
    act_cond_targ = act_cond_targ, prevalence = prevalence,
    confusion_matrix = confusion_matrix)

  prev <- test_get_prevalence(NULL, NULL, NULL, confusion_matrix = mat, prevalence = prevalence)

  result <- list(
    "Total" = sum(mat),
    "Reference Positives" = mat[["PP", "CP"]] + mat[["PN", "CP"]],
    "Test Positives" = mat[["PP", "CP"]] + mat [["PP", "CN"]],
    "True Positives" = mat[["PP", "CP"]],
    "False Positives" = mat[["PP", "CN"]],
    "Reference Negatives" = mat[["PP", "CN"]] + mat [["PN", "CN"]],
    "Test Negatives" = mat[["PN", "CP"]] + mat [["PN", "CN"]],
    "True Negatives" = mat[["PN", "CN"]],
    "False Negatives" = mat[["PN", "CP"]],
    "Sensitivity" = test_get_sensitivity(NULL, NULL, NULL, confusion_matrix = mat),
    "Specificity" = test_get_specificity(NULL, NULL, NULL, confusion_matrix = mat),
    "Prevalence in the Given Cohort" = test_get_prevalence(NULL, NULL, NULL, confusion_matrix = mat),
    "Prevalence used for PPV / NPV" = prev,
    "PPV" = test_get_positive_predictive_value(NULL, NULL, NULL, confusion_matrix = mat, prevalence = prevalence),
    "NPV" = test_get_negative_predictive_value(NULL, NULL, NULL, confusion_matrix = mat, prevalence = prevalence)
  )

  return(unlist(result))
}

#' @export
#' @rdname test_get_values
test_get_relation <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE, alpha = eenv_alpha, haldane_anscombe_correction = TRUE) {
	`%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
	pred_cond <- rlang::enquo(pred_cond)
	act_cond <- rlang::enquo(act_cond)
  mat <- test_get_confusion_matrix(data = data, pred_cond = !! pred_cond,
    act_cond = !! act_cond, pred_cond_targ = pred_cond_targ,
    act_cond_targ = act_cond_targ, prevalence = prevalence,
    confusion_matrix = confusion_matrix)
	return(exposure_get_relation(
		data = data,
		exposure = !! pred_cond,
		condition = !! act_cond,
		confusion_matrix = mat,
		alpha = alpha,
		haldane_anscombe_correction = haldane_anscombe_correction))
}

#'
#' Calculate an empiric ROC.
#'
#' @description
#' Calculate an empiric ROC.
#'
#' @export
#'
#' @param data The tibble containing the data.
#' @param predictor The column holding the predictors.
#' @param response The column holding the responses.
#' @param print_auc Print the AUC when creating the plot?
#' @param print_points Plot the single points?
#' @param print_steps Plot points that define steps only?
#' @return list
#'
test_roc_empiric <- function(data, predictor, response, print_auc = TRUE,
                             print_points = FALSE, print_steps = FALSE) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
  predictor <- rlang::enquo(predictor)
  response <- rlang::enquo(response)
  predictors <- dplyr::pull(data, !! predictor)
  responses <- dplyr::pull(data, !! response)
  length_fill <- length(predictors)

  data_roc <- tibble::tibble(
    predictor = predictors,
    response = responses,
    truepositives = rep(NA, each = length_fill),
    falsepositives = rep(NA, each = length_fill),
    truenegatives = rep(NA, each = length_fill),
    falsenegatives = rep(NA, each = length_fill),
    sensitivity = rep(NA, each = length_fill),
    specificity = rep(NA, each = length_fill),
    ppv = rep(NA, each = length_fill),
    npv = rep(NA, each = length_fill),
    tpr = rep(NA, each = length_fill),
    fpr = rep(NA, each = length_fill))

  for (i in 1:nrow(data_roc)) {
    threshold <- as.numeric(data_roc[i, "predictor"])

    data_roc <- data_roc %>%
      dplyr::mutate(predictor_pos = predictor >= threshold)

    metrics <- test_get_metrics(
      data_roc,
      pred_cond = predictor_pos,
      act_cond = responses,
      pred_cond_targ = TRUE,
      act_cond_targ = TRUE,
      prevalence = NULL)

    data_roc[i, "truepositives"] <- as.numeric(metrics[["True Positives"]])
    data_roc[i, "falsepositives"] <- as.numeric(metrics[["False Positives"]])
    data_roc[i, "truenegatives"] <- as.numeric(metrics[["True Negatives"]])
    data_roc[i, "falsenegatives"] <- as.numeric(metrics[["False Negatives"]])
    data_roc[i, "sensitivity"] <- as.numeric(metrics[["Sensitivity"]])
    data_roc[i, "specificity"] <- as.numeric(metrics[["Specificity"]])
    data_roc[i, "tpr"] <- as.numeric(metrics[["Sensitivity"]])
    data_roc[i, "fpr"] <- as.numeric(1 - metrics[["Specificity"]])
    data_roc[i, "ppv"] <- as.numeric(metrics[["PPV"]])
    data_roc[i, "npv"] <- as.numeric(metrics[["NPV"]])
  }

  data_steps <- data_roc %>%
    dplyr::arrange(fpr, tpr) %>%
    dplyr::mutate(
      fpr_dist = fpr - dplyr::lag(fpr, order_by = fpr),
      tpr_prev = dplyr::lag(tpr, order_by = fpr),
      # support ties which would result in a non-horizontal line
      #
      #      |       ______
      #  tpr |     /|
      #      |  __/ |
      #      |_|__|_|______
      #           fpr
      #
      # the area is split into a rectangle + triangle defined by
      #   rectangle:  (tpr<n-1> - 0) * (fpr<n> - fpr<n-1>) = tpr_prev * fpr_dist
      #   triangle:   (tpr<n> - tpr<n-1>) * (fpr<n> - fpr<n-1>) / 2 =
      #               (tpr - tpr_prev) * fpr_dist / 2
      # for step<n>
      #
      area = (tpr_prev * fpr_dist) + ((tpr - tpr_prev) * fpr_dist / 2)) %>%
    dplyr::select(- fpr_dist, - tpr_prev)

  auc <- sum(data_steps$area, na.rm = TRUE)

  data_steps_summarised <- data_steps %>%
    dplyr::group_by(fpr) %>%
    dplyr::mutate(
      tpr_max = max(tpr),
      predictor_max = min(predictor)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(tpr_max) %>%
    dplyr::mutate(
      fpr_min = min(fpr)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(tpr == tpr_max & fpr == fpr_min) %>%
    dplyr::select(-tpr_max, -fpr_min)

  plot <- ggplot2::ggplot(data = data_steps, ggplot2::aes(x = fpr, ymin = 0, ymax = tpr)) +
    ggplot2::geom_ribbon(alpha = 0.4) +
    ggplot2::geom_line(ggplot2::aes(y = tpr)) +
    ggplot2::geom_abline(slope = 1, intercept = 0)

  if (print_auc) {
    plot <- plot +
      ggplot2::annotate("text", x = 0.5, y = 0.15,
                        label = paste0("AUC=", format_number(auc, decimals = 2)))
  }

  if (print_points) {
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(y = tpr))
  } else if (print_steps) {
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(x = fpr_min, y = tpr_max), data = data_steps_summarised)
  }

  return(list(
    data = data_roc %>% dplyr::select(-predictor_pos),
    steps = data_steps,
    steps_summarised = data_steps_summarised,
    AUC = auc,
    plot = plot
  ))
}

#'
#' Format the output of `test_get_relation`.
#'
#' @export
#' @param raw_relations The output of `test_get_relation`.
#' @param relations Vector with at least one in `c("OR", "RR", "ARR")`.
#' @return named vector
#'
test_format_relations <- function(
  raw_relations,
  relations = c("OR")) {
	return(exposure_format_relations(
		raw_relations = raw_relations,
		relations = relations))
}
