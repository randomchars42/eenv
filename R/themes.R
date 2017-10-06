#'
#' Create a conservative theme for ggplot2.
#'
#' @export
#' @include vars.R
#' @family theme-related functions and objects
#' @param base_size The font size in points used for the text.
#' @param base_family The font-family.
#' @param base_colour_fg The colour used for lines / dots / axes.
#' @param base_colour_bg The background colour.
#' @param base_colour_sec The colour used for the strips in facet_wraps, ... .
#' @param legend_pos Where to put the legend ("top", "right", "left", "bottom").
#' @return The theme.
#'
create_theme <- function(
  base_size = 10,
  base_family = "Helvetica",
  base_colour_fg = colour_base,
  base_colour_bg = colour_bg,
  base_colour_sec = colour_tert,
  legend_pos = "bottom") {

  half_line <- base_size / 2
  theme <- ggplot2::theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line = ggplot2::element_line(
      colour = base_colour_fg, size = 0.5, linetype = 1, lineend = "butt"),
    rect = ggplot2::element_rect(
      fill = base_colour_bg, colour = base_colour_fg, size = 0.5, linetype = 1),
    text = ggplot2::element_text(
      family = base_family, face = "plain", colour = base_colour_fg,
      size = base_size, lineheight = 1, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = ggplot2::margin(), debug = FALSE),

    axis.line = ggplot2::element_line(),
    axis.line.x = ggplot2::element_line(),
    axis.line.y = ggplot2::element_line(),
    axis.text = ggplot2::element_text(),
    axis.text.x =
      ggplot2::element_text(margin = ggplot2::margin(t = half_line), vjust = 1),
    axis.text.y =
      ggplot2::element_text(margin = ggplot2::margin(r = half_line), hjust = 1),
    axis.ticks = ggplot2::element_line(),
    axis.ticks.length = ggplot2::unit(half_line, "pt"),
    axis.title.x =
      ggplot2::element_text(margin = ggplot2::margin(t = half_line)),
    axis.title.y = ggplot2::element_text(
      angle = 90, margin = ggplot2::margin(r = half_line)),

    legend.background = ggplot2::element_rect(colour = NA),
    legend.margin =
      ggplot2::margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(size = ggplot2::rel(1)),
    legend.text.align =  NULL,
    legend.title = ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = legend_pos,
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,

    panel.background =
      ggplot2::element_rect(fill = base_colour_bg, colour = NA),
    panel.border = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y =  NULL,
    panel.ontop = FALSE,

    strip.background =
      ggplot2::element_rect(fill = base_colour_sec, colour = NA),
    strip.text =
      ggplot2::element_text(colour = base_colour_fg, size = ggplot2::rel(1)),
    strip.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = half_line, b = half_line)),
    strip.text.y = ggplot2::element_text(
      angle = -90, margin = ggplot2::margin(l = half_line, r = half_line)),
    strip.switch.pad.grid = ggplot2::unit(0.1, "cm"),
    strip.switch.pad.wrap = ggplot2::unit(0.1, "cm"),

    plot.background = ggplot2::element_rect(colour = base_colour_bg),
    plot.title =
      ggplot2::element_text(margin = ggplot2::margin(b = half_line * 1.2)),
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )

  # update geom_defaults as well
  # taken from:
  # https://stackoverflow.com/questions/21174625/ggplot-how-to-set-default-color-for-all-geoms
  geoms <- ls(pattern = '^Geom.', envir = loadNamespace("ggplot2"))

  lapply(
    geoms,
    function(name, args) {
      ggplot2::update_geom_defaults(
        eval(parse(text=paste0("ggplot2::", name))), args)
    },
    list(fill = base_colour_fg, colour = base_colour_fg)
  )

  return(theme)
}

#'
#' A conservative theme with default colours in black and white.
#'
#' @export
#' @family theme-related functions and objects
#'
global_theme <- list(
  create_theme(),
  ggplot2::scale_colour_manual(values = colour_palette),
  ggplot2::scale_fill_manual(values = colour_palette)
)
