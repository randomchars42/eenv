% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore.R
\name{scan_simple_predictions}
\alias{scan_simple_predictions}
\title{Calculate a crosstable, ROC and metrics for each combination of variables.}
\usage{
scan_simple_predictions(data, predictors, responses, response_pos,
  alpha = eenv_alpha)
}
\arguments{
\item{data}{A tibble containing the data.}

\item{predictors}{A list of predictors (strings).}

\item{responses}{A list of responses (strings).}

\item{response_pos}{The value of \code{responses} considered positive.}

\item{alpha}{The alpha used for testing.}
}
\value{
list
}
\description{
Calculate a crosstable, a ROC and test metrics (if a cutoff is given) for
each combination of the given variables.
}
