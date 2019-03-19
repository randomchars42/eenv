#'
#' Work with test results vs. actual conditions.
#'
#' @description
#' Functions to retrieve true positives, prevalence, npv...
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
#' @name test_get
#' @param data A tibble holding the data
#' @param pred_cond The column holding the predicted conditions / test results.
#' @param act_cond The column holding the true conditions.
#' @param pred_cond_targ The value that signifies the targeted predicted condition.
#' @param act_cond_targ The value that signifies the targeted true condition.
#' @param prevalence The prevalence in the reference population.
#' @param alpha The alpha level.
#' @return variable
#'
NULL

#' @export
#' @rdname test_get
test_get_true_positives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_true_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
}

test_get_true_positives_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`

  data <- data %>% dplyr::filter(
    ! is.na(!! pred_cond) & ! is.na(!! act_cond),
    !! pred_cond == pred_cond_targ & !! act_cond == act_cond_targ)
  return(data)
}

#' @export
#' @rdname test_get
test_get_false_positives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_false_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
}

test_get_false_positives_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`

  data <- data %>% dplyr::filter(
    ! is.na(!! pred_cond) & ! is.na(!! act_cond),
    !! pred_cond == pred_cond_targ & !! act_cond != act_cond_targ)
  return(data)
}

#' @export
#' @rdname test_get
test_get_actual_positives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_actual_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
}

test_get_actual_positives_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`

  data <- data %>% dplyr::filter(
    ! is.na(!! pred_cond) & ! is.na(!! act_cond),
    !! act_cond == act_cond_targ)
  return(data)
}

#' @export
#' @rdname test_get
test_get_predicted_positives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_predicted_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
}

test_get_predicted_positives_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`

  data <- data %>% dplyr::filter(
    ! is.na(!! pred_cond) & ! is.na(!! act_cond),
    !! pred_cond == pred_cond_targ)
  return(data)
}

#' @export
#' @rdname test_get
test_get_true_negatives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_true_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
}

test_get_true_negatives_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`

  data <- data %>% dplyr::filter(
    ! is.na(!! pred_cond) & ! is.na(!! act_cond),
    !! pred_cond != pred_cond_targ & !! act_cond != act_cond_targ)
  return(data)
}

#' @export
#' @rdname test_get
test_get_false_negatives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_false_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
}

test_get_false_negatives_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`

  data <- data %>% dplyr::filter(
    ! is.na(!! pred_cond) & ! is.na(!! act_cond),
    !! pred_cond != pred_cond_targ & !! act_cond == act_cond_targ)
  return(data)
}

#' @export
#' @rdname test_get
test_get_actual_negatives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_actual_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
}

test_get_actual_negatives_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`

  data <- data %>% dplyr::filter(
    ! is.na(!! pred_cond) & ! is.na(!! act_cond),
    !! act_cond != act_cond_targ)
  return(data)
}

#' @export
#' @rdname test_get
test_get_predicted_negatives <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_predicted_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
}

test_get_predicted_negatives_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ) {
  `%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`

  data <- data %>% dplyr::filter(
    ! is.na(!! pred_cond) & ! is.na(!! act_cond),
    !! pred_cond != pred_cond_targ)
  return(data)
}

#' @export
#' @rdname test_get
test_get_sensitivity <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_sensitivity_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
}

test_get_sensitivity_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ) {
  true_pos <- nrow(test_get_true_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  act_pos <- nrow(test_get_actual_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  return(true_pos / act_pos)
}

#' @export
#' @rdname test_get
test_get_specificity <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_specificity_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
}

test_get_specificity_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ) {
  true_neg <- nrow(test_get_true_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  act_neg <- nrow(test_get_actual_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  return(true_neg / act_neg)
}

#' @export
#' @rdname test_get
test_get_prevalence <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_prevalence_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
}

test_get_prevalence_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ) {
  act_pos <- nrow(test_get_actual_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  act_neg <- nrow(test_get_actual_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  return(act_pos / (act_neg + act_pos))
}

#' @export
#' @rdname test_get
test_get_positive_predictive_value <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE, prevalence = NULL) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_positive_predictive_value_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ, prevalence))
}

test_get_positive_predictive_value_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ, prevalence) {
  if (is.null(prevalence)) {
    prevalence <- test_get_prevalence_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)
  }
  spec <- test_get_specificity_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)
  sens <- test_get_sensitivity_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)

  return((sens * prevalence) / ((sens * prevalence) + ((1-spec) * (1-prevalence))))
}

#' @export
#' @rdname test_get
test_get_negative_predictive_value <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE, prevalence = NULL) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_negative_predictive_value_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ, prevalence))
}

test_get_negative_predictive_value_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ, prevalence) {
  if (is.null(prevalence)) {
    prevalence <- test_get_prevalence_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)
  }
  spec <- test_get_specificity_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)
  sens <- test_get_sensitivity_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)

  return((spec * (1-prevalence)) / ((spec * (1-prevalence)) + ((1-sens) * prevalence)))
}

#' @export
#' @rdname test_get
test_get_metrics <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE, prevalence = NULL) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_metrics_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ, prevalence))
}

test_get_metrics_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ, prevalence) {
  if (is.null(prevalence)) {
    prevalence <- test_get_prevalence_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)
  }
  result <-c(
    nrow(test_get_actual_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)) +
      nrow(test_get_actual_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)),
    nrow(test_get_actual_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)),
    nrow(test_get_predicted_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)),
    nrow(test_get_true_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)),
    nrow(test_get_false_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)),
    nrow(test_get_actual_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)),
    nrow(test_get_predicted_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)),
    nrow(test_get_true_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)),
    nrow(test_get_false_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)),
    test_get_sensitivity_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ),
    test_get_specificity_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ),
    test_get_prevalence_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ),
    prevalence,
    test_get_positive_predictive_value_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ, prevalence),
    test_get_negative_predictive_value_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ, prevalence)
  )

  names(result) <- c(
    "Total",
    "Reference Positives",
    "Test Positives",
    "True Positives",
    "False Positives",
    "Reference Negatives",
    "Test Negatives",
    "True Negatives",
    "False Negatives",
    "Sensitivity",
    "Specificity",
    "Prevalence in the Given Cohort",
    "Prevalence used for PPV / NPV",
    "PPV",
    "NPV"
  )

  return(result)
}

#' @export
#' @rdname test_get
test_get_relation <- function(data, pred_cond, act_cond, pred_cond_targ = TRUE, act_cond_targ = TRUE, alpha = eenv_alpha) {
  pred_cond <- rlang::enquo(pred_cond)
  act_cond <- rlang::enquo(act_cond)
  return(test_get_relation_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ, alpha))
}

test_get_relation_int <- function(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ, alpha) {
  z <- stats::qnorm(1 - (alpha / 2))
  total <-
    nrow(test_get_actual_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ)) +
    nrow(test_get_actual_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  affected_yes <-
    nrow(test_get_actual_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  affected_no <-
    nrow(test_get_actual_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  factor_yes <-
    nrow(test_get_predicted_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  factor_no <-
    nrow(test_get_predicted_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  affected_yes_factor_yes <-
    nrow(test_get_true_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  affected_yes_factor_no <-
    nrow(test_get_false_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  affected_no_factor_no <-
    nrow(test_get_true_negatives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))
  affected_no_factor_yes <-
    nrow(test_get_false_positives_int(data, pred_cond, act_cond, pred_cond_targ, act_cond_targ))

  risk_affected_yes_factor_yes <-
    affected_yes_factor_yes / factor_yes
  risk_affected_yes_factor_no <-
    affected_yes_factor_no / factor_no

  risk_ratio <-
    risk_affected_yes_factor_yes / risk_affected_yes_factor_no
  risk_ratio_log_se <- sqrt(
    (1 / affected_yes_factor_yes) +
      (1 / affected_yes_factor_no) -
      (1 / (factor_yes)) -
      (1 / (factor_no)))
  risk_ratio_log <-
    log(risk_ratio)
  risk_ratio_lci <-
    exp(risk_ratio_log - z * risk_ratio_log_se)
  risk_ratio_uci <-
    exp(risk_ratio_log + z * risk_ratio_log_se)
  risk_ratio_chi_squared <-
    ((abs(affected_yes_factor_yes - (affected_yes * factor_yes / total)) - 0.5)^2) / (affected_yes * factor_yes / total) +
    ((abs(affected_yes_factor_no - (affected_yes * factor_no / total)) - 0.5)^2) / (affected_yes * factor_no / total) +
    ((abs(affected_no_factor_yes - (affected_no * factor_yes / total)) - 0.5)^2) / (affected_no * factor_yes / total) +
    ((abs(affected_no_factor_no - (affected_no * factor_no / total)) - 0.5)^2) / (affected_no * factor_no / total)
  risk_ratio_p <-
    (1 - stats::pnorm(sqrt(risk_ratio_chi_squared))) * 2

  risk_difference <-
    risk_affected_yes_factor_yes - risk_affected_yes_factor_no

  odds_affected_yes_factor_yes <-
    affected_yes_factor_yes / affected_no_factor_yes
  odds_affected_yes_factor_no <-
    affected_yes_factor_no / affected_no_factor_no
  odds_ratio <-
    odds_affected_yes_factor_yes / odds_affected_yes_factor_no
  odds_ratio_log_se <- sqrt(
    (1 / affected_yes_factor_yes) +
      (1 / affected_yes_factor_no) +
      (1 / affected_no_factor_yes) +
      (1 / affected_no_factor_no))
  odds_ratio_log <-
    log(odds_ratio)
  odds_ratio_lci <-
    exp(odds_ratio_log - z * odds_ratio_log_se)
  odds_ratio_uci <-
    exp(odds_ratio_log + z * odds_ratio_log_se)
  result_fisher <-
    stats::fisher.test(matrix(c(affected_yes_factor_yes, affected_yes_factor_no, affected_no_factor_yes, affected_no_factor_no), nrow=2))
  #http://www.biochemia-medica.com/content/odds-ratio-calculation-usage-and-interpretation
  odds_ratio_p <-
    (factorial(affected_yes) * factorial(affected_no) * factorial(factor_yes) *  factorial(factor_no)) /
    (factorial(total) * factorial(affected_yes_factor_yes) * factorial(affected_yes_factor_no) * factorial(affected_no_factor_yes) * factorial(affected_no_factor_no))

  result <-c(
    total,
    affected_yes,
    affected_no,
    factor_yes,
    factor_no,
    affected_yes_factor_yes,
    affected_yes_factor_no,
    affected_no_factor_no,
    affected_no_factor_yes,
    risk_affected_yes_factor_yes,
    risk_affected_yes_factor_no,
    risk_ratio,
    risk_ratio_log_se,
    risk_ratio_lci,
    risk_ratio_uci,
    risk_ratio_p,
    risk_difference,
    odds_affected_yes_factor_yes,
    odds_affected_yes_factor_no,
    odds_ratio,
    odds_ratio_log_se,
    odds_ratio_lci,
    odds_ratio_uci,
    odds_ratio_p
  )

  names(result) <- c(
    "Total",
    paste0(rlang::quo_name(act_cond), ' (affected) +'),
    paste0(rlang::quo_name(act_cond), ' (affected) -'),
    paste0(rlang::quo_name(pred_cond), ' (factor) +'),
    paste0(rlang::quo_name(pred_cond), ' (factor) -'),
    "affected + & factor +",
    "affected + & factor -",
    "affected - & factor -",
    "affected - & factor +",
    "risk for affected + with factor +",
    "risk for affected + with factor -",
    "risk ratio",
    "risk ratio, ln(SE)",
    "risk ratio, LCI",
    "risk ratio, UCI",
    "risk ratio, p",
    "absolute difference in risk",
    "odds for affected + with factor +",
    "odds for affected + with factor -",
    "odds ratio",
    "odds ratio, ln(SE)",
    "odds ratio, LCI",
    "odds ratio, UCI",
    "odds ratio, p"
  )

  return(result)
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
  predictor <- rlang::enquo(predictor)
  response <- rlang::enquo(response)
  return(test_roc_empiric_int(
    data = data,
    predictor = predictor,
    response = response,
    print_auc = print_auc,
    print_points = print_points,
    print_steps = print_steps))
}

test_roc_empiric_int <- function(data, predictor, response, print_auc,
                                 print_points = FALSE, print_steps = FALSE) {
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

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
  relations = c("OR", "RR", "ARR")) {

  result <- c(format_number(raw_relations["Total"], type = "int"))
  names(result) <- c("Total")

  if ("OR" %in% relations) {
    names <- names(result)
    result <- c(
      result,
      format_number(raw_relations["odds ratio"], type = "int"),
      paste0(
        format_number(raw_relations["odds ratio, LCI"], type = "int"),
        ' - ',
        format_number(raw_relations["odds ratio, UCI"], type = "int")),
      format_number(raw_relations["odds ratio, p"], type = "p"))

    names(result) <- c(
      names,
      "Odds Ratio",
      "CI",
      "p")
  }

  if ("RR" %in% relations) {
    names <- names(result)
    result <- c(
      result,
      format_number(raw_relations["risk ratio"], type = "int"),
      paste0(
        format_number(raw_relations["risk ratio, LCI"], type = "int"),
        ' - ',
        format_number(raw_relations["risk ratio, UCI"], type = "int")),
      format_number(raw_relations["risk ratio, p"], type = "p"))

    names(result) <- c(
      names,
      "Odds Ratio",
      "CI",
      "p")
  }

  if ("ARR" %in% relations) {
    names <- names(result)
    result <- c(
      result,
      format_number(
        raw_relations["absolute difference in risk"],
        type = "int"))

    names(result) <- c(
      names,
      "Absolute Difference In Risk")
  }

  return(result)
}
