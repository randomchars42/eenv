#'
#' The default confidence level used in functions of this package.
#'
#' @export
#'
eenv_confidence_level <- 0.95

#'
#' The default alpha used in functions of this package.
#'
#' @export
#'
eenv_alpha <- 1 - eenv_confidence_level

#'
#' The number of non-zero digits to display for p-values.
#'
#' @export
#' @family variables and functions for displaying numbers
#'
eenv_signif_digits <- 2

#'
#' The number below which "< NUMBER" is displayed for p-values.
#'
#' @export
#' @family variables and functions for displaying numbers
#'
eenv_signif_bottom <- 0.0001

#'
#' The number of digits to round to (not for p-values).
#'
#' @export
#' @family variables and functions for displaying numbers
#'
eenv_decimals <- 1

#'
#' Default settings for [save_plots()].
#'
#' @description
#' List:
#'  * dpi -> The resolution to use in DPI (dots per inch).
#'  * format -> Formats to save plots in (plots can be saved in more than format
#'              simultaneously).
#'  * units -> Units for plot-size.
#'  * width -> Different steps of widht the plot may have.
#'  * height -> Height of one single plot.
#'  * columns_max -> When plotting multiple plots onto one grid, the number of
#'                   columns to use
#'  * axes -> Which axes to align (see [cowplot::plot_grid()]]).
#'  * align -> Which directions to use for alignment.
#'
#' @export
#' @seealso [cowplot::plot_grid()]
#' @family variables and functions for plotting
#'
eenv_plot_dim <- list(
  "dpi" = 1200,
  "format" = c("pdf", "svg"),
  "units" = "in",
  "width" = c(3.25, 6.8),
  "height" = 3.25,
  "columns_max" = 2,
  "axes" = "rtbl",
  "align" = "hv")

#'
#' Default background colour.
#'
#' @export
#' @family theme-related functions and objects (white)
#' @family variables and functions for plotting
#'
colour_bg <- "#FFFFFF"

#'
#' Default colour for dots / axes / text ... (dark grey).
#'
#' @export
#' @family theme-related functions and objects
#' @family variables and functions for plotting
#'
colour_base <- "#333333"

#'
#' The colour to use if more than 1 colours are needed (brighter grey).
#'
#' @export
#' @family theme-related functions and objects
#' @family variables and functions for plotting
#'
colour_secondary <- "#666666"

#'
#' The colour to use if more than 2 colours are needed (even brighter grey).
#'
#' @export
#' @family theme-related functions and objects
#' @family variables and functions for plotting
#'
colour_tert <- "#999999"

#'
#' A colour palette with 7 colours (black and white).
#'
#' @export
#' @family theme-related functions and objects
#' @family variables and functions for plotting
#'
colour_palette <- c(
  colour_base,
  colour_secondary,
  colour_tert,
  "#CCCCCC",
  "#4D4D4D",
  "#808080",
  "#B3B3B3")
