## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----gh-installation, echo = TRUE, eval = FALSE--------------------------
#  # install.packages("devtools")
#  devtools::install_github("randomchars42/eenv")

## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------
library("eenv")

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
data <-
  utils::read.csv2(
    system.file("extdata", "values.csv", package = "eenv"),
    header = FALSE)
rownames(data) <- LETTERS[1:6]

knitr::kable(
  data,
  row.names = TRUE,
  col.names = as.character(1:6))

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
data <-
  utils::read.csv2(
    system.file("extdata", "names.csv", package = "eenv"),
    header = FALSE)
rownames(data) <- LETTERS[1:6]

knitr::kable(
  data,
  row.names = TRUE,
  col.names = as.character(1:6))

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
data <-
  utils::read.csv2(
    system.file("extdata", "values_names.csv", package = "eenv"),
    header = FALSE)
rownames(data) <- LETTERS[1:12]

knitr::kable(
  data,
  row.names = TRUE,
  col.names = as.character(1:6))

## ---- eval = FALSE-------------------------------------------------------
#  # do not run this code yet
#  plates_read(
#    additional_vars = c("name")
#  )

## ---- echo = FALSE, message=FALSE, warning=FALSE-------------------------
data <-
  utils::read.csv2(
    system.file("extdata", "plate_1.csv", package = "eenv"),
    header = FALSE)
rownames(data) <- LETTERS[1:12]

knitr::kable(
  data,
  row.names = TRUE,
  col.names = as.character(1:6))

## ---- eval = FALSE-------------------------------------------------------
#  # do not run this code yet
#  plates_read(
#    additional_vars = c("name", "day")
#  )

## ------------------------------------------------------------------------
# try it
like <- c("sunny day", "cake", "coffee", "more coffee", "chocolate", "pizza")
like

## ------------------------------------------------------------------------
# try it
fibonacci <- c(1, 1, 2, 3, 5, 8, 13)
fibonacci

## ---- eval = FALSE-------------------------------------------------------
#  # try it
#  calibrator_names = c(
#    "CAL1", "CAL2", "CAL3", "CAL4", "CAL5", "CAL6", "CAL7", "CAL8", "CAL9",
#    "CAL10"
#  )
#  
#  calibrator_values = c(
#    4000, 2000, 1000, 500, 250, 125, 62.5, 31.25, 15.625, 7.8125
#  )

## ---- eval = FALSE-------------------------------------------------------
#  # do not run this code yet
#  
#  calibrator_names = c(
#    "CAL1", "CAL2", "CAL3", "CAL4", "CAL5", "CAL6", "CAL7", "CAL8", "CAL9",
#    "CAL10"
#  )
#  
#  calibrator_values = c(
#    4000, 2000, 1000, 500, 250, 125, 62.5, 31.25, 15.625, 7.8125
#  )
#  
#  plates_read(
#    additional_vars = c("name", "day"),
#    cal_names = calibrator_names,
#    cal_values = calibrator_values
#  )

## ---- eval = FALSE-------------------------------------------------------
#  # do not run this code yet
#  plates_read(
#    additional_vars = c("name", "day"),
#    cal_names = c(
#      "CAL1", "CAL2", "CAL3", "CAL4", "CAL5", "CAL6", "CAL7", "CAL8", "CAL9",
#      "CAL10"
#    ),
#    cal_values = c(
#      4000, 2000, 1000, 500, 250, 125, 62.5, 31.25, 15.625, 7.8125
#    )
#  )

## ---- eval = FALSE-------------------------------------------------------
#  # do not run this code yet
#  plates_read(
#    plates = 1,
#    additional_vars = c("name", "day"),
#    cal_names = c(
#      "CAL1", "CAL2", "CAL3", "CAL4", "CAL5", "CAL6", "CAL7", "CAL8", "CAL9",
#      "CAL10"
#    ),
#    cal_values = c(
#      4000, 2000, 1000, 500, 250, 125, 62.5, 31.25, 15.625, 7.8125
#    )
#  )

## ---- eval = FALSE-------------------------------------------------------
#  # try it
#  # (don't worry if it looks weird ;) )
#  if (! file.exists("plate_1.csv")) {
#    write.csv2(
#      x = read.csv2(
#        file = system.file("extdata", "values_names.csv", package = "eenv"),
#        header = FALSE),
#      file = "plate_1.csv",
#      col.names = FALSE
#    )
#  }

## ---- eval = FALSE-------------------------------------------------------
#  # do not run this code yet
#  plates_read(
#    plates = 1,
#    sep = ";",
#    additional_vars = c("name", "day"),
#    cal_names = c(
#      "CAL1", "CAL2", "CAL3", "CAL4", "CAL5", "CAL6", "CAL7", "CAL8", "CAL9",
#      "CAL10"
#    ),
#    cal_values = c(
#      4000, 2000, 1000, 500, 250, 125, 62.5, 31.25, 15.625, 7.8125
#    )
#  )

## ------------------------------------------------------------------------
# try it
my_list <- list(a = 1, favourite_food = "pizza", age = 20, a_vector = c(1, 2, 3))
my_list

## ------------------------------------------------------------------------
# try it
my_list$age

## ------------------------------------------------------------------------
# try it
my_list[[3]]

## ------------------------------------------------------------------------
# try it
my_list[["a_vector"]]

## ------------------------------------------------------------------------
# try it
my_list$age <- 30
my_list$age

## ---- eval = FALSE-------------------------------------------------------
#  # now you may run it :)
#  result_list <- plates_read(
#    plates = 1,
#    sep = ";",
#    additional_vars = c("name", "day"),
#    cal_names = c(
#      "CAL1", "CAL2", "CAL3", "CAL4", "CAL5", "CAL6", "CAL7", "CAL8", "CAL9",
#      "CAL10"
#    ),
#    cal_values = c(
#      4000, 2000, 1000, 500, 250, 125, 62.5, 31.25, 15.625, 7.8125
#    )
#  )

## ---- echo = FALSE-------------------------------------------------------
result_list <- eenv::plates_read(
  plates = 1,
  sep = ";",
  path = system.file("extdata", package = "eenv"),
  additional_vars = c("name", "day"),
  cal_names = c(
    "CAL1", "CAL2", "CAL3", "CAL4", "CAL5", "CAL6", "CAL7", "CAL8", "CAL9",
    "CAL10"
  ),
  cal_values = c(
    4000, 2000, 1000, 500, 250, 125, 62.5, 31.25, 15.625, 7.8125
  ),
  write_data = FALSE,
  use_written_data = FALSE
)

## ---- eval = FALSE-------------------------------------------------------
#  result_list$all

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(result_list$all)

## ---- eval = FALSE-------------------------------------------------------
#  result_list$samples

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(result_list$samples)

## ---- warnings = FALSE---------------------------------------------------
result_list$plate1$plot

## ---- eval = FALSE-------------------------------------------------------
#  # now you may run it :)
#  result_list <- plates_read(
#    plates = 1,
#    sep = ";",
#    additional_vars = c("name", "day"),
#    cal_names = c(
#      "CAL1", "CAL2", "CAL3", "CAL4", "CAL5", "CAL6", "CAL7", "CAL8", "CAL9",
#      "CAL10"
#    ),
#    cal_values = c(
#      4000, 2000, 1000, 500, 250, 125, 62.5, 31.25, 15.625, 7.8125
#    ),
#    exclude_cals =  list(plate1 = c("CAL9"))
#  )

## ---- echo = FALSE-------------------------------------------------------
result_list <- eenv::plates_read(
  plates = 1,
  sep = ";",
  path = system.file("extdata", package = "eenv"),
  additional_vars = c("name", "day"),
  cal_names = c(
    "CAL1", "CAL2", "CAL3", "CAL4", "CAL5", "CAL6", "CAL7", "CAL8", "CAL9",
    "CAL10"
  ),
  cal_values = c(
    4000, 2000, 1000, 500, 250, 125, 62.5, 31.25, 15.625, 7.8125
  ),
  exclude_cals =  list(plate1 = c("CAL9")),
  write_data = FALSE,
  use_written_data = FALSE
)

## ------------------------------------------------------------------------
# try it
result_list$plate1$plot

## ------------------------------------------------------------------------
# try it
my_data <- result_list$samples

## ------------------------------------------------------------------------
# try it
my_data %>%
  filter(concentration_cv > 0.20)

## ---- eval = FALSE-------------------------------------------------------
#  # try it
#  my_data <- my_data %>%
#    filter(concentration_cv < 0.20)
#  my_data

## ---- echo = FALSE-------------------------------------------------------
# try it
my_data <- my_data %>%
  filter(concentration_cv < 0.20)

knitr::kable(my_data)

## ---- eval = FALSE-------------------------------------------------------
#  # try it
#  my_data <- my_data %>%
#    mutate(
#      concentration = convert_conc(x = concentration, from = "ng / ml", to = "nmol / l", molar_mass = 52391)
#    )
#  my_data

## ---- echo = FALSE-------------------------------------------------------
# try it
my_data <- my_data %>%
  mutate(
    concentration = convert_conc(x = concentration, from = "ng / ml", to = "nmol / l", molar_mass = 52391)
  )

knitr::kable(my_data)

## ------------------------------------------------------------------------
# try it
# use our data
# use column "name" as x-axis
# use column "concentration" as y-axis
ggplot(data = my_data, aes(x = name, y = concentration, colour = name)) +
  # plot each value as a point
  geom_point() +
  # draw a sub-plot (facet) for each day
  facet_wrap(~day)

## ------------------------------------------------------------------------
# try it
# use our data
# use column "name" as x-axis
# use column "concentration" as y-axis
ggplot(data = my_data, aes(x = day, y = concentration)) +
  # draw boxplots
  geom_boxplot() +
  # draw each value as a point over the boxplot
  geom_jitter(width = 0.2)
  # you could try
  # geom_point() as well ;)

## ---- eval = FALSE-------------------------------------------------------
#  # try it but the result wont be as good
#  result_list <- plates_read(
#    plates = 1,
#    sep = ";",
#    additional_vars = c("name", "day"),
#    cal_names = c(
#      "CAL1", "CAL2", "CAL3", "CAL4", "CAL5", "CAL6", "CAL7", "CAL8", "CAL9",
#      "CAL10"
#    ),
#    cal_values = c(
#      4000, 2000, 1000, 500, 250, 125, 62.5, 31.25, 15.625, 7.8125
#    ),
#    exclude_cals =  list(plate1 = c("CAL9")),
#    model_func = fit_linear,
#    interpolate_func = interpolate_linear
#  )
#  
#  result_list$plate1$plot

