cal_names <- c("CAL1", "CAL2", "CAL3", "CAL4", "CAL5", "CAL6", "CAL7", "CAL8", "CAL9", "CAL10")
cal_values <- c(4000, 2000, 1000, 500, 250, 125, 62.5, 31.25, 15.625, 7.8125)

sets_read <- function(
  plates,
  cal_names = cal_names,
  cal_values = cal_values,
  additional_vars = c("name"),
  sep = ","
) {
  results <- list()
  
  for (i in 1 : plates) {
    data_plate <-
      set_read(
        file_name = "plate_#NUM#.csv",
        path = "",
        num = i,
        sep = sep,
        cols = 0,
        rows = 0,
        additional_vars = properties,
        additional_sep = "_"
      ) %>%
      set_calc_concentrations(
        cal_names = cal_names,
        cal_values = cal_values,
        col_names = name,
        col_values = value,
        col_target = conc,
        col_real = real,
        col_recov = recovery,
        model_func = fit_lnln,
        interpolate_func = interpolate_lnln
      ) %>%
      set_calc_variability(sample_id, value, conc)
    
    if (i == 1) {
      data <- data_plate
    } else {
      data <- rbind(data, data_plate)
    }
  }
  
  data_samples <- data %>%
    filter(is.na(real)) %>%
    mutate(
      plate = set,
      n = value_n,
      raw = value_mean,
      raw_sd = value_sd,
      raw_cv = value_cv,
      SelenBP1 = conc_mean,
      SelenBP1_sd = conc_sd,
      SelenBP1_cv = conv_cv
    ) %>%
    select(
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
    distinct(sample_id, .keep_all = TRUE)
  
  write_csv(data = data_samples, path = "data_samples.csv")
  
  data_all <- data %>%
    mutate(
      plate = set,
      n = value_n,
      raw = value,
      raw_mean = value_mean,
      raw_sd = value_sd,
      raw_cv = value_cv,
      SelenBP1 = conc,
      SelenBP1 = conc_mean,
      SelenBP1_sd = conc_sd,
      SelenBP1_cv = conv_cv
    ) %>%
    select(
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
  
  write_csv(data = data_all, path = "data_all.csv")
  
  results["samples"] <- data_samples
  results["all"] <- data_all
  
  return(results)
}