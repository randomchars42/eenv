% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_numbers.R
\name{format_p}
\alias{format_p}
\title{Format a p-value.}
\usage{
format_p(x, signif_digits = eenv_signif_digits,
  signif_bottom = eenv_signif_bottom)
}
\arguments{
\item{x}{The number to format.}

\item{signif_digits}{The number of non-zero digits to display for p-values.}

\item{signif_bottom}{The number below which "< NUMBER" is displayed for
p-values.}
}
\value{
A string.
}
\description{
Format a p-value.
}
\seealso{
Other variables and functions for displaying numbers: \code{\link{eenv_decimals}},
  \code{\link{eenv_signif_bottom}},
  \code{\link{eenv_signif_digits}},
  \code{\link{eenv_signif_steps}},
  \code{\link{format_float}}, \code{\link{format_int}},
  \code{\link{format_number}}, \code{\link{format_pad}},
  \code{\link{format_perc}}
}
\concept{variables and functions for displaying numbers}
