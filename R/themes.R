#'
#' Create a conservative theme for ggplot2.
#'
#' @export
#' @include vars.R
#' @family theme-related functions and objects
#' @param base_size The font size in points used for the text.
#' @param base_line_size Base size for line elements.
#' @param base_rect_size Base size for rect elements.
#' @param base_family The font-family.
#' @param base_colour_fg The colour used for lines / dots / axes.
#' @param base_colour_bg The background colour.
#' @param base_colour_sec The colour used for the strips in facet_wraps, ... .
#' @param legend_pos Where to put the legend ("top", "right", "left", "bottom").
#' @return The theme.
#'
create_eenv_theme <- function(
  base_size = 10,
  base_line_size = base_size / 20,
  base_rect_size = base_size / 20,
  base_family = "Arial",
  base_colour_fg = colour_base,
  base_colour_bg = colour_bg,
  base_colour_sec = colour_tert,
  legend_pos = "bottom") {

  half_line <- base_size / 2
  theme <- ggplot2::theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line = ggplot2::element_line(
      colour = base_colour_fg, size = base_line_size, linetype = 1, lineend = "butt"),
    rect = ggplot2::element_rect(
      fill = base_colour_bg, colour = base_colour_fg, size = base_rect_size, linetype = 1),
    text = ggplot2::element_text(
      family = base_family, face = "plain", colour = base_colour_fg,
      size = base_size, lineheight = 1, hjust = 0.5, vjust = 0.5, angle = 0, # line-height = 0.9
      margin = ggplot2::margin(), debug = FALSE),

    axis.line = ggplot2::element_line(),
    axis.line.x = ggplot2::element_line(),
    axis.line.y = ggplot2::element_line(),
    axis.text = ggplot2::element_text(),
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 0.8 * half_line / 2) , hjust = 1),
    axis.text.y.right = ggplot2::element_text(
      margin = ggplot2::margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks = ggplot2::element_line(),
    axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
    axis.title.x =
      ggplot2::element_text(margin = ggplot2::margin(t = half_line / 2), vjust = 1),
    axis.title.x.top =
      ggplot2::element_text(margin = ggplot2::margin(b = half_line / 2), vjust = 0),
    axis.title.y = ggplot2::element_text(
      angle = 90, margin = ggplot2::margin(r = half_line / 2), vjust = 1),
    axis.title.y.right = ggplot2::element_text(
      angle = -90, margin = ggplot2::margin(l = half_line / 2), vjust = 0),

    legend.background = ggplot2::element_rect(colour = NA),
    legend.spacing = ggplot2::unit(2 * half_line, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin =
      ggplot2::margin(t = half_line, r = half_line, b = half_line, l = half_line),
    legend.key = ggplot2::element_rect(
      fill = base_colour_bg, colour = base_colour_bg),
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
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),

    panel.background =
      ggplot2::element_rect(fill = base_colour_bg, colour = NA),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y =  NULL,
    panel.ontop = FALSE,

    strip.background =
      ggplot2::element_rect(fill = base_colour_sec, colour = NA),
    strip.text = ggplot2::element_text(
      colour = base_colour_fg,
      size = ggplot2::rel(1),
      margin = ggplot2::margin(
        0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)),
    strip.text.x = NULL,#ggplot2::element_text(ggplot2::margin(t = half_line, b = half_line)),
    strip.text.y = ggplot2::element_text(
      angle = -90),#, ggplot2::margin(l = half_line,r = half_line)),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),

    plot.background = ggplot2::element_rect(colour = base_colour_bg),
    plot.title = ggplot2::element_text(
      hjust = 0, vjust = 1,
      margin = ggplot2::margin(b = half_line)),
    plot.subtitle = ggplot2::element_text(
      hjust = 0, vjust = 1,
      margin = ggplot2::margin(b = half_line)),
    plot.caption = ggplot2::element_text(
      hjust = 1, vjust = 1,
      margin = ggplot2::margin(t = half_line)),
    plot.tag = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0.5, vjust = 0.5),
    plot.tag.position = 'topleft',
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )

  # update geom_defaults as well
  # taken from:
  # https://stackoverflow.com/questions/21174625/ggplot-how-to-set-default-color-for-all-geoms
  geoms <- ls(pattern = '^Geom.', envir = loadNamespace("ggplot2"))

  lapply(
    geoms,
    function(name) {
      if (name %in% c("GeomBoxplot", "GeomRect", "GeomPolygon")) {
        ggplot2::update_geom_defaults(
          eval(parse(text=paste0("ggplot2::", name))),
          list(fill = base_colour_bg, colour = base_colour_fg))
      } else {
        ggplot2::update_geom_defaults(
          eval(parse(text=paste0("ggplot2::", name))),
          list(fill = base_colour_fg, colour = base_colour_fg))
      }
    }
  )

  return(theme)
}

#'
#' A conservative theme with default colours in black and white.
#'
#' @export
#' @family theme-related functions and objects
#'
eenv_theme <- list(
  create_eenv_theme(),
  ggplot2::scale_colour_manual(values = colour_palette),
  ggplot2::scale_fill_manual(values = colour_palette)
)
