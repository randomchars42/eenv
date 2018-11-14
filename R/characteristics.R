#'
#' Get metrics for a given characteristic of your dataset.
#'
#' @description
#' This function gives similar results to `summary()`.
#'
#' @export
#' @param data A tibble containing the data.
#' @param characteristic The column containing the characteristic to analyse.
#' @param ... Subgroups for which to do a separate calculation.
#' @param name The first item in vector (only if a `template` string is given).
#' @param events The event(s) to count (only if characteristic is not numeric or `force_count`).
#' @param quantiles The quantiles to collect (default: `c("q5", "q25", "q50", "q75", "q95")`,
#'   only if characteristic is numeric).
#' @param force_count Count frequency of characteristic although its numeric?
#' @param template Apply a template string.
#' @param decimals The number of decimals to display.
#' @param decimals_perc Display decimals for percentages.
#' @return A named vector.
#'
characteristic_get <- function(
  data,
  characteristic,
  ...,
  name = NULL,
  events = c(TRUE),
  quantiles = c("q5", "q25", "q50", "q75", "q95"),
  force_count = FALSE,
  template = NULL,
  decimals = eenv_decimals,
  decimals_perc = FALSE) {

  stopifnot(
    tibble::is.tibble(data),
    is.logical(force_count),
    is.character(quantiles))

  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`
  characteristic <- rlang::enquo(characteristic)
  subgroups <- rlang::enquos(...)
  result <- list()

  data_tmp <- data %>% dplyr::pull(!! characteristic)

  # decide what to collect
  if (force_count | ! is.numeric(data_tmp)) {
    collect <- c("count", "perc")
  } else {
    collect <- c("mean", quantiles)
  }

  n_total <- length(data_tmp)

  # crunch numbers for total
  result[["TOTAL"]] <- characteristic_calc(data_tmp, collect, events, n_total)

  # crunch numbers for each distinct value of each subgroup
  if (length(subgroups) > 0) {
    for (i in 1 : length(subgroups)) {
      # get each distinct value of each subgroup
      facettes <- data %>%
        dplyr::distinct(!! subgroups[[i]], .keep_all = TRUE) %>%
        dplyr::pull(!! subgroups[[i]])

      for (n in 1 : length(facettes)) {
        data_tmp <- data %>%
          dplyr::filter(!! subgroups[[i]] == facettes[[n]]) %>%
          dplyr::pull(!! characteristic)
        vec_name <- paste0(rlang::quo_name(subgroups[[i]]), "___", facettes[[n]])
        result[[vec_name]] <- characteristic_calc(data_tmp, collect, events, n_total)
      }
    }
  }

  if (! is.null(template)) {
    result <- characteristic_format(result, template = template, decimals = decimals, decimals_perc = decimals_perc)
    if (! is.null(name)) {
      tmp <- c(name)
      names(tmp) <- c("NAME")
      result <- c(tmp, result)
    }
    return(result)
  } else {
    return(result)
  }
}

characteristic_calc <- function(data, collect, events, n) {
  result <- c()

  for (i in 1 : length(collect)) {
    result <- switch(
      substr(collect[[i]], 1, 1),
      "c" = characteristic_calc_count(data, result, events),
      "p" = characteristic_calc_perc(data, result, events, n),
      "m" = characteristic_calc_mean(data, result),
      "q" = characteristic_calc_quantile(data, result, collect[[i]])
    )
  }

  return(result)
}

characteristic_calc_count <- function(data, result, events) {
  tmp <- c(sum(data %in% c(events)))
  names(tmp) <- "count"
  return(c(result, tmp))
}

characteristic_calc_perc <- function(data, result, events, n) {
  count <- characteristic_calc_count(data, c(), events)
  tmp <- c(count / length(data))
  names(tmp) <- "perc"
  tmp2 <- c(count / n)
  names(tmp2) <- "perc total"
  return(c(result, tmp, tmp2))
}

characteristic_calc_mean <- function(data, result) {
  tmp <- c(mean(data))
  names(tmp) <- "mean"
  return(c(result, tmp))
}

characteristic_calc_quantile <- function(data, result, collect) {
  quantile <- as.numeric(substr(collect, 2, nchar(collect)))
  tmp <- c(stats::quantile(data, probs = c(quantile / 100), na.rm = TRUE))
  names(tmp) <- collect
  return(c(result, tmp))
}

#'
#' Format a characteristic for printing.
#'
#' @export
#' @param characteristic The characteristic to format.
#' @param template A template to use. May contain the following variables:
#'   * "%c" for **c**ount
#'   * "%p" for **p**ercent
#'   * "%pt" for **p**ercent of **total**
#'   * "%m" for **m**ean
#'   * "%qX" for **q**uantile where X is a number between 0 and 100
#' @param decimals The number of decimals to display.
#' @param decimals_perc Display decimals for percentages.
#' @return A vector
#'
characteristic_format <- function(characteristic, template, decimals = eenv_decimals, decimals_perc = FALSE) {
  stopifnot(
    is.list(characteristic),
    is.character(template),
    is.numeric(decimals),
    is.logical(decimals_perc))

  result <- c()

  for (i in 1 : length(characteristic)) {
    tmp <- c(characteristic_apply_template(characteristic[[i]], template, decimals, decimals_perc))
    names(tmp) <- names(characteristic)[[i]]
    result <- c(result, tmp)
  }

  return(result)
}

characteristic_apply_template <- function(data, template, decimals, decimals_perc) {
  data_points <- names(data)
  result <- template

  if ("count" %in% data_points) {
    result <- gsub(
      "%c",
      format_number(data[["count"]], type = "int"),
      result, fixed = TRUE)
  }

  if ("perc total" %in% data_points) {
    result <- gsub(
      "%pt",
      format_number(data[["perc total"]], type = "perc", force_perc_decimals = decimals_perc),
      result, fixed = TRUE)
  }

  if ("perc" %in% data_points) {
    result <- gsub(
      "%p",
      format_number(data[["perc"]], type = "perc", force_perc_decimals = decimals_perc),
      result, fixed = TRUE)
  }

  if ("mean" %in% data_points) {
    result <- gsub(
      "%m",
      format_number(data[["mean"]], type = "float", decimals = decimals),
      result, fixed = TRUE)
  }

  quantiles <- grep("^q", data_points)

  if (length(quantiles) > 0) {
    for (i in 1 : length(quantiles)) {
      result <- gsub(
        paste0("%", data_points[[quantiles[[i]]]], "([^0-9]|$)"),
        paste0(format_number(data[[quantiles[[i]]]], type = "float", decimals = decimals), "\\1"),
        result,
        fixed = FALSE)
    }
  }

  return(result)
}

#'
#' Format several characteristics as a table.
#'
#' @export
#' @param ... Vectors of the same length to be coerced to a table.
#' @param headers The column in which to insert subsection-headers
#' @return A tibble
#'
characteristics_table <- function(..., headers = "NAME") {
  `%>%` <- magrittr::`%>%`
  rows <- list(...)
  table <- tibble::tibble()

  if (length(rows) == 0) {
    warning("No data supplied")
    return()
  }

  for (i in 1 : length(rows)) {
    if (is.null(names(rows[[i]]))) {
      names(rows[[i]]) <- c(headers)
    }

    table <- table %>%
      dplyr::bind_rows(rows[[i]])
  }

  return(table)
}
