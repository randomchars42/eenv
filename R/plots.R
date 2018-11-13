#'
#' Save one or more plots onto a panel.
#'
#' @export
#' @family variables and functions for plotting
#' @param plots Either single ggplot or list of plots.
#' @param name The name of the plot.
#' @param dimensions List of settings to use, see [eenv_plot_dim].
#' @param theme The theme to use (for letters, etc.)
#' @param width The width (see [eenv_plot_dim]) to use,
#'   `NULL` means auto.
#' @param height The height as multiple of `dimensions$height`
#'   (see [eenv_plot_dim]) to use, `NULL` means auto.
#' @param columns The number of columns to use, `NULL` means auto. Begins a
#'   new row of plots if the number of plots exceeds `dimensions$columns_max`.
#' @return The final plot.
#'
save_plots <- function(
  plots,
  name,
  dimensions = eenv_plot_dim,
  theme = eenv_theme[[1]],
  width = NULL,
  height = NULL,
  columns = NULL) {

  if (ggplot2::is.ggplot(plots)) {
    # single plot

    plot <- plots

    if (is.null(width)) {
      # auto
      width <- 1
    }

    if (is.null(height)) {
      # auto
      height <- 1
    }

    columns <- 1

    if (is.null(dimensions$height)) {
      dimensions$height <- dimensions$width[1]
    }

  } else {
    # multiple plots

    if (is.null(columns)) {
      # auto
      columns <- dimensions$columns_max
    }

    if (is.null(width)) {
      # auto
      width <- length(dimensions$width)
    }

    if (is.null(height)) {
      # auto
      height <- ceiling(length(plots) / columns)
    }

    if (is.null(dimensions$height)) {
      dimensions$height <- dimensions$width[width] / columns
    }

    plot <- cowplot::plot_grid(
      plotlist = plots,
      align = dimensions$align,
      axis = dimensions$axes,
      ncol =  columns,
      labels = "AUTO",
      label_size = theme$text$size,
      label_fontfamily = theme$text$family,
      label_colour = theme$text$colour)

  }

  sapply(
    dimensions$format,
    function(format, dimensions, plot, width, height) {
      cowplot::ggsave(
        filename = paste0(name, '.', format),
        device = format,
        plot = plot,
        width = dimensions$width[width],
        height = dimensions$height * height,
        units = dimensions$units,
        dpi = dimensions$dpi)
    },
    dimensions = dimensions,
    plot = plot,
    width = width,
    height = height)

  return(plot)
}
