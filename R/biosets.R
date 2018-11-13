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

#'
#' Search for and remove multiples which sets.
#'
#' @description
#' Dealing with raw data one might have to exclude a sample where duplicates /
#' triplicates / ... are too discrepant to use. This function identifies those
#' samples and might delete them.
#'
#' @export
#' @param data Tibble holding the data.
#' @param ids The column that holds the ids of the samples.
#' @param ... Columns to use for filtering.
#' @param threshold The threshold used for filtering:
#'   e.g. `concentration_cv` (coefficient of variation) > `0.2`. If a column
#'   `recovery` is found the threshold for warning will be:
#'   `recovery < 1 - threshold | recovery > 1 + threshold`
#' @param throw_warnings Throw a warning for each detected / removed sample?
#' @param remove Remove detected samples?
#' @param set The column that holds the id of the sets / plates.
#' @return The tibble.
#'
plate_check_discrepant_multiples <- function(
  data,
  ids,
  ...,
  threshold = 0.2,
  throw_warnings = TRUE,
  remove = TRUE,
  set = set) {
  # make some handy operators available
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  stopifnot(
    tibble::is.tibble(data),
    is.numeric(threshold),
    is.logical(throw_warnings),
    is.logical(remove)
  )

  ids <- rlang::enquo(ids)
  set <- rlang::enquo(set)
  ids_name <- rlang::quo_name(ids)
  calc_for <- rlang::quos(...)

  for (i in seq_along(calc_for)) {
    target <- calc_for[[i]]
    target_base <- rlang::quo_name(target)

    if (target_base == ids_name) {
      next()
    }

    if (target_base == "recovery") {
      data_discrepant <- data %>%
        dplyr::distinct(!! ids, .keep_all = TRUE) %>%
        dplyr::filter(!! target > 1 + threshold | !! target < 1 - threshold)

      if (remove) {
        data <- data %>%
          dplyr::filter(!! target <= 1 + threshold & target >= 1 - threshold)
      }
    } else {
      data_discrepant <- data %>%
        dplyr::distinct(!! ids, .keep_all = TRUE) %>%
        dplyr::filter(!! target > threshold)

      if (remove) {
        data <- data %>%
          dplyr::filter(!! target <= threshold)
      }
    }

    if (throw_warnings) {
      generate_warnings(
        ids = dplyr::pull(data_discrepant, !! ids),
        values = dplyr::pull(data_discrepant, !! target),
        sets = dplyr::pull(data_discrepant, !! set),
        property = target_base)
    }
  }

  return(data)
}

generate_warnings <- function(ids, values, sets, property) {
  if (length(ids) > 0) {
    # loop over the dataframe and check the deviations
    for (i in 1:length(ids)) {
      message(paste(
        "Set ", sets[i], ", ID ", ids[i], ": ", property, " = ", format_float(values[i]), sep = ""))
    }
  }
}
