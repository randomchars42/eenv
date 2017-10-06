#'
#' Format a number for displaying.
#'
#' @description
#' This function respects [global_signif_digits], [global_signif_bottom],
#' [global_decimals] as default.
#'
#' @export
#' @family variables and functions for displaying numbers
#' @param x The number to format.
#' @param type The type of number ("int", "perc", "float", "p").
#' @param decimals The number of decimals to round to (not p-values).
#' @param signif_digits The number of non-zero digits to display for p-values.
#' @param signif_bottom The number below which "< NUMBER" is displayed for
#'   p-values.
#' @param force_perc_decimals TRUE if decimals should be displayed for
#'   percentages.
#' @return A string.
#'
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

#'
#' Format an integer for displaying.
#'
#' @export
#' @family variables and functions for displaying numbers
#' @inheritParams format_number
#' @return A string.
#'
format_int <- function(x) {
  return(as.character(round(x, 0)))
}

#'
#' Format a percentage for displaying.
#'
#' @export
#' @family variables and functions for displaying numbers
#' @inheritParams format_number
#' @return A string.
#'
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
  names(x) <- NULL
  return(x)
}

#'
#' Round and format a floating point number.
#'
#' @export
#' @family variables and functions for displaying numbers
#' @inheritParams format_number
#' @return A string.
#'
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

#'
#' Format a p-value.
#'
#' @export
#' @family variables and functions for displaying numbers
#' @inheritParams format_number
#' @return A string.
#'
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

#'
#' Pad a string to a given length.
#'
#' @export
#' @family variables and functions for displaying numbers
#' @param width How long should the string be?
#' @param side To which side should the padding be added ("left" / "right")?
#' @param character Which character to use.
#' @return A string.
#'
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
