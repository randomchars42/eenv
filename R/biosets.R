#'
#' Read sets (plates) and calculate concentrations and variability.
#'
#' @description
#' Writes the processed data into two files: `data_samples.csv`,
#' `data_all.csv` and returns a list containing:
#'  * all: all rows including duplicate and calibrators.
#'  * samples: samples only, no calibrators, no duplicates.
#'  * plateNUMBER:
#'    * plot: plot of the calibrators
#'    * model: model used to fit a line to the calibrators
#'
#' @export
#' @param plates The number of plates (e.g. 3 attempts to read "plate_1.csv",
#'   "plate_2.csv", "plate_3.csv")
#' @param exclude_cals A list of calibrators to exclude, e.g.:
#'   `list(plate1 = c("CAL1"))`.
#' @param plot_func Function used to display the fitted line.
#' @inheritParams bioset::set_read
#' @inheritParams bioset::set_calc_concentrations
#' @inheritParams bioset::set_calc_variability
#' @return A list of params.
#'
sets_read <- function(
  plates,
  cal_names,
  cal_values,
  exclude_cals = c(),
  additional_vars = c("name"),
  sep = ",",
  path = ".",
  model_func = bioset::fit_lnln,
  plot_func = bioset::plot_lnln,
  interpolate_func = bioset::interpolate_lnln
) {
  `%>%` <- magrittr::`%>%`

  results <- list()

  for (i in 1 : plates) {

    data_plate <-
      bioset::set_read(
        file_name = "plate_#NUM#.csv",
        path = path,
        num = i,
        sep = sep,
        cols = 0,
        rows = 0,
        additional_vars = additional_vars,
        additional_sep = "_"
      )

    exclude <- exclude_cals[[paste0("plate", i)]]

    if (!is.null(exclude)) {
      data_plate <- data_plate %>%
        dplyr::mutate(
          sample_id =
            ifelse(sample_id %in% exclude, paste0("x", sample_id), sample_id)
        )
    }

    real <- NULL

    data_plate <- data_plate %>%
      bioset::set_calc_concentrations(
        cal_names = cal_names,
        cal_values = cal_values,
        col_names = sample_id,
        col_values = value,
        col_target = conc,
        col_real = real,
        col_recov = recovery,
        model_func = model_func,
        interpolate_func = interpolate_func
      ) %>%
      bioset::set_calc_variability(sample_id, value, conc)

    if (i == 1) {
      data <- data_plate
    } else {
      data <- rbind(data, data_plate)
    }

    results[[paste0("plate", i)]] <- list(
      model = model_func(data$real, data$value),
      plot = plot_func(data$real, data$value)
    )
  }

  data_samples <- data %>%
    dplyr::filter(is.na(real)) %>%
    dplyr::mutate(
      plate = set,
      n = value_n,
      raw = value_mean,
      raw_sd = value_sd,
      raw_cv = value_cv,
      SelenBP1 = conc_mean,
      SelenBP1_sd = conc_sd,
      SelenBP1_cv = conc_cv
    ) %>%
    dplyr::select(
      -set,
      -real,
      -value,
      -conc,
      -recovery,
      -value_n,
      -value_mean,
      -value_sd,
      -value_cv,
      -conc_n,
      -conc_mean,
      -conc_sd,
      -conc_cv) %>%
    dplyr::distinct(sample_id, .keep_all = TRUE)

  readr::write_csv(data_samples, path = file.path(path, "data_samples.csv"))

  data_all <- data %>%
    dplyr::mutate(
      plate = set,
      n = value_n,
      raw = value,
      raw_mean = value_mean,
      raw_sd = value_sd,
      raw_cv = value_cv,
      SelenBP1 = conc_mean,
      SelenBP1_sd = conc_sd,
      SelenBP1_cv = conc_cv
    ) %>%
    dplyr::select(
      -set,
      -value,
      -conc,
      -value_n,
      -value_mean,
      -value_sd,
      -value_cv,
      -conc_n,
      -conc_mean,
      -conc_sd,
      -conc_cv)

  readr::write_csv(data_all, path = file.path(path, "data_all.csv"))

  results["samples"] <- list(data_samples)
  results["all"] <- list(data_all)

  return(results)
}

# declare to make R CMD check happy ;)
conc <- NULL
conc_cv <- NULL
conc_mean <- NULL
conc_n <- NULL
conc_sd <- NULL
recovery <- NULL
sample_id <- NULL
set <- NULL
value <- NULL
value_cv <- NULL
value_mean <- NULL
value_n <- NULL
value_sd <- NULL
