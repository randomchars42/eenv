create_theme <- function(
  base_size = 10,
  base_family = "Helvetica",
  base_colour_fg = colour_base,
  base_colour_bg = colour_bg,
  base_colour_sec = colour_tert,
  legend_pos = "bottom") {
  
  half_line <- base_size / 2
  theme <- theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line = element_line(colour = base_colour_fg, size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = base_colour_bg, colour = base_colour_fg, size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = base_colour_fg, size = base_size, lineheight = 1, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),
    
    axis.line = element_line(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.text = element_text(),
    axis.text.x = element_text(margin = margin(t = half_line), vjust = 1),
    axis.text.y = element_text(margin = margin(r = half_line), hjust = 1),
    axis.ticks = element_line(),
    axis.ticks.length = unit(half_line, "pt"),
    axis.title.x = element_text(margin = margin(t = half_line)),
    axis.title.y = element_text(angle = 90, margin = margin(r = half_line)),
    
    legend.background =  element_rect(colour = NA),
    legend.margin =      margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(1)),
    legend.text.align =  NULL,
    legend.title =       element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position =    legend_pos,
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    
    panel.background =   element_rect(fill = base_colour_bg, colour = NA),
    panel.border =       element_blank(),
    panel.grid.major =   element_blank(),
    panel.grid.minor =   element_blank(),
    panel.spacing =       unit(half_line, "pt"),
    panel.spacing.x =     NULL,
    panel.spacing.y =     NULL,
    panel.ontop    =     FALSE,
    
    strip.background =   element_rect(fill = base_colour_sec, colour = NA),
    strip.text =         element_text(colour = base_colour_fg, size = rel(1)),
    strip.text.x =       element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y =       element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),
    
    plot.background =    element_rect(colour = base_colour_bg),
    plot.title =         element_text(margin = margin(b = half_line * 1.2)),
    plot.margin =        margin(half_line, half_line, half_line, half_line),
    
    complete = TRUE
  )
  
  # update geom_defaults as well
  # taken from:
  # https://stackoverflow.com/questions/21174625/ggplot-how-to-set-default-color-for-all-geoms
  geoms <- ls(pattern = '^Geom.', env = as.environment('package:ggplot2'))
  #lapply(geoms, function(name, args) {print(name); update_geom_defaults(eval(parse(text=name)), args)}, list(fill = base_colour_fg, colour = base_colour_fg))
  lapply(geoms, function(name, args) {update_geom_defaults(eval(parse(text=name)), args)}, list(fill = base_colour_fg, colour = base_colour_fg))
  
  return(theme)
}

global_theme <- list(
  create_theme(),
  scale_colour_manual(values = colour_palette),
  scale_fill_manual(values = colour_palette))