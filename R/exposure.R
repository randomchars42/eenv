#'
#' Work with exposure vs. conditions.
#'
#' @description
#' Functions to retrieve risk ratio, difference in absolute risk and odds ratio.
#'
#' Structure: Suppose you want to compare a group of people exposed to x vs. a
#' group without exposure:
#'
#' |   | exposure    | observed    | comment           |
#' |---|-------------|-------------|-------------------|
#' | 1 | pos         | yes         | <- true positive  |
#' | 2 | pos         | no          | <- false positive |
#' | 3 | neg         | no          | <- true negative  |
#' | 4 | pos         | yes         | ...               |
#' | 5 | neg         | no          | ...               |
#'
#' @name exposure
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
#' @param relations Vector with at least one in `c("OR", "RR", "ARR")`.
#' necessary.
#' @param confusion_matrix A confusion matrix as returned by
#' `test_get_confusion_matrix`, `data` will be ignored.
#' @return variable
#'
NULL

#' @export
#' @rdname exposure
exposure_get_relation <- function(data, exposure, condition, exposure_targ = TRUE,
	condition_targ = TRUE, alpha = eenv_alpha, haldane_anscombe_correction = TRUE,
	relations = c("OR", "RR", "ARR"), confusion_matrix = NULL) {
	if (!is.null(confusion_matrix)) {
		cm <- confusion_matrix
	} else {
		`%>%` <- magrittr::`%>%`; `!!` <- rlang::`!!`; `:=` <- rlang::`:=`
		exposure <- rlang::enquo(exposure)
		condition <- rlang::enquo(condition)

		cm <- test_get_confusion_matrix(
			data = data,
			pred_cond = !! exposure,
			act_cond = !! condition,
			pred_cond_targ = exposure_targ,
			act_cond_targ = condition_targ)
	}
	# apply Haldane Anscombe correction (all cells + 0.5) if one of the cells in
	# the matrix is zero
	if (0 %in% cm & haldane_anscombe_correction) {
		cm <- cm + 0.5
		warning("Haldane Anscombe correction applied")
	} else {
		cm <- cm
	}
  z <- stats::qnorm(1 - (alpha / 2))

	# The confusion matrix is intended for tests.
	# "CP" means condition positive
	# "CN" means condition negative
	# "PP" translates to exposure (predictor) positive
	# "PN" translates to exposure (predictor) negative
	total <- sum(cm)
  cond_pos <- sum(cm[,"CP"])
  cond_neg <- sum(cm[,"CN"])
  exp_pos <- sum(cm["PP",])
  exp_neg <- sum(cm["PN",])
  cond_pos_exp_pos <- cm[["PP", "CP"]]
  cond_pos_exp_neg <- cm[["PN", "CP"]]
  cond_neg_exp_neg <- cm[["PN", "CN"]]
  cond_neg_exp_pos <- cm[["PP", "CN"]]

  result <- list(
    "total" = total,
    "condition +" = cond_pos,
    "condition -" = cond_neg,
    "exposure +" = exp_pos,
    "exposure -" = exp_neg,
    "condition + & exposure +" = cond_pos_exp_pos,
    "condition + & exposure -" = cond_pos_exp_neg,
    "condition - & exposure -" = cond_neg_exp_neg,
    "condition - & exposure +" = cond_neg_exp_pos)

	if (length(relations) == 0) {
		return(result)
	}

	if ("RR" %in% relations | "ARR" %in% relations) {
		risk_exp_pos <- cond_pos_exp_pos / exp_pos
		risk_exp_neg <- cond_pos_exp_neg / exp_neg
		risk_ratio <- risk_exp_pos / risk_exp_neg
		# http://sphweb.bumc.bu.edu/otlt/MPH-Modules/QuantCore/PH717_ComparingFrequencies/PH717_ComparingFrequencies7.html
		risk_ratio_log_se <- sqrt(
			((1 - risk_exp_pos) / (exp_pos * risk_exp_pos)) + 
			((1 - risk_exp_neg) / (exp_neg * risk_exp_neg)))
		risk_ratio_log <-	log(risk_ratio)
		risk_ratio_lci <-	exp(risk_ratio_log - z * risk_ratio_log_se)
		risk_ratio_uci <-	exp(risk_ratio_log + z * risk_ratio_log_se)
		result <- c(result, list(
			"risk for condition + with exposure +" = risk_exp_pos,
			"risk for condition + with exposure -" = risk_exp_neg,
			"risk ratio" = risk_ratio,
			"risk ratio, ln(SE)" = risk_ratio_log_se,
			"risk ratio, LCI" = risk_ratio_lci,
			"risk ratio, UCI" = risk_ratio_uci))

		if ("ARR" %in% relations) {
			risk_difference <- risk_exp_pos - risk_exp_neg
			result <- c(result, list(
				"absolute difference in risk" = risk_difference))
		}
	}

	if ("OR" %in% relations) {
		odds_exp_pos <-	cond_pos_exp_pos / cond_neg_exp_pos
		odds_exp_neg <- cond_pos_exp_neg / cond_neg_exp_neg
		odds_ratio <- odds_exp_pos / odds_exp_neg
		odds_ratio_log_se <- sqrt(
			(1 / cond_pos_exp_pos) +
				(1 / cond_pos_exp_neg) +
				(1 / cond_neg_exp_pos) +
				(1 / cond_neg_exp_neg))
		odds_ratio_log <-	log(odds_ratio)
		odds_ratio_lci <-	exp(odds_ratio_log - z * odds_ratio_log_se)
		odds_ratio_uci <-	exp(odds_ratio_log + z * odds_ratio_log_se)
		result <- c(result, list(
			"odds for condition + with exposure +" = odds_exp_pos,
			"odds for condition + with exposure -" = odds_exp_neg,
			"odds ratio" = odds_ratio,
			"odds ratio, ln(SE)" = odds_ratio_log_se,
			"odds ratio, LCI" = odds_ratio_lci,
			"odds ratio, UCI" = odds_ratio_uci))
	}

	# Fisher's exact test
	# http://www.pmean.com/11/Fishers.html
	fisher_p <- test_fisher(cm)
	# Chi Sqared with Yate's continuity correction
	chi_squared <-
    ((abs(cond_pos_exp_pos - (cond_pos * exp_pos / total)) - 0.5)^2) / (cond_pos * exp_pos / total) +
    ((abs(cond_pos_exp_neg - (cond_pos * exp_neg / total)) - 0.5)^2) / (cond_pos * exp_neg / total) +
    ((abs(cond_neg_exp_pos - (cond_neg * exp_pos / total)) - 0.5)^2) / (cond_neg * exp_pos / total) +
    ((abs(cond_neg_exp_neg - (cond_neg * exp_neg / total)) - 0.5)^2) / (cond_neg * exp_neg / total)
  chi_squared_p <- (1 - stats::pnorm(sqrt(chi_squared))) * 2
	result <- c(result, list(
		"Fisher's exact test, p" = fisher_p,
		"Chi Squared" = chi_squared,
		"Chi Squared, p" = chi_squared_p))
  return(unlist(result))
}

test_fisher <- function(m) {
	calc_p <- function(m) {
			(factorial(sum(m[1,])) * factorial(sum(m[2,])) * factorial(sum(m[,1])) *  factorial(sum(m[,2]))) /
			(factorial(sum(m)) * factorial(m[[1,1]]) * factorial(m[[1,2]]) * factorial(m[[2,1]]) *
				 factorial(m[[2,2]]))
	}

	total <- sum(m)
	# calculate p of initial table
	p_init <- calc_p(m)
	#print(p_init)
	# find field with min value
	field <- which(m == min(m), arr.ind = TRUE)
	f_r <- field[[1,"row"]]
	f_c <- field[[1,"col"]]
	# find boundaries (numbers do not correspond to row / col numbers, `b_r1`
	# represents the boundary of the row which contains the minimum value
	# same for column `b_c1`
	b_r1 <- sum(m[f_r,])
	b_r2 <- total - b_r1 
	b_c1 <- sum(m[,f_c])
	b_c2 <- total - b_c1 
	# to build a two-tailed p-value we have to calculate all tables with these
  # boundaries -> 0 to the lower of the two boundaries for the field we picked
	p_values <- c(p_init)
	for (n in 0:min(c(b_r1,b_c1))) {
		if (n == m[[f_r,f_c]]) next
		m_temp <- matrix(c(n, b_r1 - n, b_c1 - n, b_r2 - (b_c1 - n)),
										 nrow = 2, byrow = TRUE)
		#print(m_temp)
		p_temp <- calc_p(m_temp)
		#print(p_temp)
		if (p_temp <= p_init) {
			#print("used")
			p_values <- c(p_values, p_temp)
		}
	}
	return(sum(p_values))
}

#'
#' Format the output of `exposure_get_relation`.
#'
#' @export
#' @param raw_relations The output of `test_get_relation`.
#' @param relations Vector with at least one in `c("OR", "RR", "ARR")`.
#' @return named vector
#'
exposure_format_relations <- function(
  raw_relations,
  relations = c("OR", "RR", "ARR")) {

  result <- c(format_number(raw_relations["total"], type = "int"))
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
      format_number(raw_relations["Fisher's exact test, p"], type = "p"))

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
      format_number(raw_relations["Fisher's exact test, p"], type = "p"))

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
