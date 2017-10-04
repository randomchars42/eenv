# file.edit("../../Laboratory Notebooks/Laboratory Notebook 1/Resources/setup.R")
# file.edit("../../Laboratory Notebooks/Laboratory Notebook 1/Resources/Toolkit.R")
# file.edit("../../Laboratory Notebooks/Laboratory Notebook 1/Resources/SelenBP1Toolkit.R")

if (!exists("global_confidence_level")) {
  global_confidence_level <- 0.95
}

if (!exists("global_confidence_level")) {
  global_alpha <- 1 - global_confidence_level
}

if (!exists("global_confidence_level")) {
  global_signif_digits <- 2
}

if (!exists("global_confidence_level")) {
  global_signif_bottom <- 0.0001
}

if (!exists("global_confidence_level")) {
  global_decimals <- 1
}

if (!exists("global_confidence_level")) {
  global_plot_dim <- list(
    "dpi" = 1200,
    "format" = c("pdf", "svg"),
    "units" = "in",
    "width" = c(3.25, 6.8),
    "height" = 3.25,
    "columns_max" = 2,
    "axis" = "rtbl",
    "align" = "hv")
}


if (!exists("global_confidence_level")) {
  colour_bg <- "#FFFFFF"
}

if (!exists("global_confidence_level")) {
  colour_base <- "#333333"
}

if (!exists("global_confidence_level")) {
  colour_secondary <- "#666666"
}

if (!exists("global_confidence_level")) {
  colour_tert <- "#999999"
  
}

if (!exists("global_confidence_level")) {
  colour_palette <- c(
    colour_base,
    colour_secondary,
    "#999999",
    "#CCCCCC",
    "#4D4D4D",
    "#808080",
    "#B3B3B3")
}

options(scipen = 18, digits = 2)