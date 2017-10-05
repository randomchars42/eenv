save_plots <- function(
  plots,
  name,
  dimensions = global_plot_dim,
  theme = global_theme[[1]],
  width = NULL,
  height = NULL,
  columns = NULL) {
  
  if (is.ggplot(plots)) {
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
      height <- ceiling(length(plots) / dimensions$columns_max)
    }
    
    plot <- plot_grid(
      plotlist = plots,
      align = dimensions$align,
      axis = dimensions$axis,
      ncol =  columns,
      labels = "AUTO",
      label_size = theme$text$size,
      label_fontfamily = theme$text$family,
      label_colour = theme$text$colour)
    
  }
  
  #plot(plot)
  
  sapply(
    dimensions$format,
    function(format, dimensions, plot, width, height) {
      ggsave(
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

format_number <- function(
  x,
  type = "float",
  decimals = global_decimals,
  signif_digits = global_signif_digits,
  signif_bottom = global_signif_bottom,
  force_perc_decimals = FALSE) {
  
  if (type == "int") {
    x <- format_int(x)
  } 
  
  if (type == "perc") {
    x <- format_perc(
      x,
      decimals = decimals,
      force_perc_decimals = force_perc_decimals)
  }
  
  if (type == "float") {
    x <- format_float(
      x,
      decimals = decimals)
  }
  
  if (type == "p") {
    x <- format_p(
      x,
      signif_digits = signif_digits,
      signif_bottom = signif_bottom)
  }
  
  return(x)
}

format_int <- function(x) {
  return(as.character(round(x, 0)))
}

format_perc <- function(
  x,
  decimals = global_decimals,
  force_perc_decimals = FALSE) {
  
  x <- x * 100
  
  if (force_perc_decimals) {
    x <- format_float(x, decimals = decimals)
  } else {
    x <- format_int(x)
  }
  
  x <- sapply(x, function(x) paste0(x, ' %'), USE.NAMES = TRUE)
  return(x)
}

format_pad <- function(x, width, side = "left", character = " ") {
  x <- sapply(
    x,
    function(x, width, side, character) {
      while (nchar(x) < width) {
        if (side == "left") {
          x <- paste0(character, x)
        } else if (side == "right") {
          x <- paste0(x, character)
        }
      }
      return(x)
    },
    width = width,
    side = side,
    character = character,
    USE.NAMES = TRUE)
  return(x)
}

format_float <- function(x, decimals = global_decimals) {
  scipen <- getOption("scipen")
  options(scipen = 999)
  x <- as.character(round(x, decimals))
  
  x_split <- strsplit(x, split = ".", fixed = TRUE)
  
  x <- sapply(
    x_split,
    function(x_split, decimals) {
      # x_split[1] = integer
      # x_split[2] = decimals, after round() shorter than param decimals
      #message(paste0(x[1],".",x[2],".",decimals))
      if (length(x_split) == 1) {
        # if x was an integer
        x_split[2] <- ""
      }
      
      x_split[2] <- format_pad(
        x_split[2],
        width = decimals,
        side = "right",
        character = "0"
      )
      
      return(paste0(x_split[1], ".", x_split[2]))
    },
    decimals = decimals,
    USE.NAMES = TRUE)
  options(scipen = scipen)
  return(x)
}

format_p <- function(
  x,
  signif_digits = global_signif_digits,
  signif_bottom = global_signif_bottom) {
  
  scipen <- getOption("scipen")
  options(scipen = 999)
  
  x <- sapply(
    x,
    function(x, signif_digits, signif_bottom) {
      x_orig <- x
      
      if (x > 1) {
        x <- 1
      } else if (x < signif_bottom) {
        x <- signif_bottom
        signif_digits <- 1
      } 
      
      # split "0.003046" into c("0", "003046) or "1" into c("1")
      x_split <- strsplit(as.character(x), split = ".", fixed = TRUE)[[1]]
      
      if (length(x_split) == 1) {
        # if x was an "1"
        zeros_after_point <- 0
      } else {
        # splits "003046" into c("00", "0", "")
        splitted <- strsplit(split = "[^0]", x = x_split[2], fixed = FALSE)[[1]]
        # count the letters of the first item
        zeros_after_point <- nchar(splitted[1])
        # round to signif_digits more than zeros behind "."
        x <- round(x, digits = zeros_after_point + signif_digits)
      }
      
      if (signif_digits > 1) { 
        # add padding to get exactly zeros_after_point + signif_digits
        x <- format_float(x = x, decimals = zeros_after_point + signif_digits)
      } else {
        x <- as.character(x)
      }
      
      if (x_orig < signif_bottom) {
        x <- paste0("< ", x)
      }
      
      return(x)
    },
    signif_digits = signif_digits,
    signif_bottom = signif_bottom,
    USE.NAMES = TRUE)
  
  options(scipen = scipen)
  return(x)
}

package_load <- function(package) {
  # find.package returns a string of length 0 if the package is not installed
  if (length(find.package(package = package, quiet = TRUE)) == 0) {
    message(paste0("Install missing package: ", package))
    install.packages(pkgs = c(package))
  }
  
  if (! require(
    package = package,
    quietly = TRUE,
    warn.conflicts = TRUE,
    character.only = TRUE)) {
    stop(paste0("Could not load package: ", package))
    return(FALSE)
  }
  return(TRUE)
}


test_format_functions <- function() {
  test <- c(1.3, 0.05, 0.051, 0.058, 0.00002, 0.0589, 0.0583)
  format_number(test, type = "p", decimals = 2, signif_digits = 2, signif_bottom = 0.0001, force_perc_decimals = FALSE)
  test <- c(102, 523.5, 21.244, 21.246, 21.284, 21.286, 0.05, 0.051, 0.058, 0.00002, 0.0589, 0.0583)
  format_number(test, type = "int", decimals = 2, signif_digits = 2, signif_bottom = 0.0001, force_perc_decimals = FALSE)
  format_number(test, type = "float", decimals = 2, signif_digits = 2, signif_bottom = 0.0001, force_perc_decimals = FALSE)
  test <- c(1.02, 0.5235, 0.21244, 0.21246, 0.21284, 0.21286, 0.05, 0.051, 0.058, 0.00002, 0.0589, 0.0583)
  format_number(test, type = "perc", decimals = 2, signif_digits = 2, signif_bottom = 0.0001, force_perc_decimals = FALSE)
  format_number(test, type = "perc", decimals = 2, signif_digits = 2, signif_bottom = 0.0001, force_perc_decimals = TRUE)
}
