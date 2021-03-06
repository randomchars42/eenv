% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/characteristics.R
\name{characteristic_format}
\alias{characteristic_format}
\title{Format a characteristic for printing.}
\usage{
characteristic_format(characteristic, template, decimals = eenv_decimals,
  decimals_perc = FALSE)
}
\arguments{
\item{characteristic}{The characteristic to format.}

\item{template}{A template to use. May contain the following variables:
\itemize{
\item "%c" for \strong{c}ount
\item "%p" for \strong{p}ercent
\item "%pt" for \strong{p}ercent of \strong{total}
\item "%m" for \strong{m}ean
\item "%qX" for \strong{q}uantile where X is a number between 0 and 100
}}

\item{decimals}{The number of decimals to display.}

\item{decimals_perc}{Display decimals for percentages.}
}
\value{
A vector
}
\description{
Format a characteristic for printing.
}
