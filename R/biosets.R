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
#' @param plates The number of plates (e.g. `3`` attempts to read `plate_1.csv`,
#'   `plate_2.csv`, `plate_3.csv`), see `file_name`.
#' @param exclude_cals A list of calibrators to exclude, e.g.:
#'   `list(plate1 = c("CAL1"))`.
#' @param file_name Naming scheme for the files. The default is
#'   `plate_#NUM#.csv`, where `#NUM#` gets replaced by the number of the plates,
#'   see `plates`. The filename must contain `#NUM#`.
#' @inheritParams bioset::sets_read
#' @return A list of params.
#'
plates_read <- function(
  plates,
  cal_names,
  cal_values,
  exclude_cals = list(),
  additional_vars = c("name"),
  additional_sep = "_",
  sep = ",",
  dec = "AUTO",
  path = ".",
  file_name = "plate_#NUM#.csv",
  model_func = bioset::fit_lnln,
  plot_func = bioset::plot_lnln,
  interpolate_func = bioset::interpolate_lnln,
  write_data = TRUE,
  use_written_data = FALSE
) {
  results <- list()
  exclude_cals_set <- list()

  for (i in 1 : plates) {
    if (! is.null(exclude_cals[[paste0("plate", i)]])){
      tmp <- c(exclude_cals[[paste0("plate", i)]])
      exclude_cals_set[[paste0("set", i)]] <- tmp
    }
  }

  result_sets <- bioset::sets_read(
    sets = plates,
    cal_names = cal_names,
    cal_values = cal_values,
    exclude_cals = exclude_cals_set,
    additional_vars = additional_vars,
    additional_sep = additional_sep,
    sep = sep,
    dec = dec,
    path = path,
    file_name = file_name,
    model_func = model_func,
    plot_func = plot_func,
    interpolate_func = interpolate_func,
    write_data = write_data,
    use_written_data = use_written_data
  )

  for (i in 1 : plates) {
    if (! is.null(result_sets[[paste0("set", i)]])){
      results[[paste0("plate", i)]] <- list(
        plot = result_sets[[paste0("set", i)]]$plot,
        model = result_sets[[paste0("set", i)]]$model
      )
    }
  }

  results$all <- tibble::as.tibble(result_sets$all)
  results$samples <- tibble::as.tibble(result_sets$samples)

  return(results)
}
