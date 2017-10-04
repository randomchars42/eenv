source("setup.R")

calibrator_names <- c("CAL1", "CAL2", "CAL3", "CAL4", "CAL5", "CAL6", "CAL7", "CAL8", "CAL9", "CAL10")
calibrator_values <- exp(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
sets <-
  sets_read(
    plates = 2,
    path = "test/",
    cal_names = calibrator_names,
    cal_values = calibrator_values)

log(c(4000, 2000, 1000), base = 2)
