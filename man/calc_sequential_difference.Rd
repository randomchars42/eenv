% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{calc_sequential_difference}
\alias{calc_sequential_difference}
\title{Calculate the difference between sequential rows while grouping.}
\usage{
calc_sequential_difference(data, values, arrange, ...,
  rel_diff_signif = 0.2, rise_above = 0, fall_below = 0)
}
\arguments{
\item{data}{A tibble.}

\item{values}{The column with the values to calculate the difference with.}

\item{arrange}{The column to arrange the data by.}

\item{...}{The columns to group by.}

\item{rel_diff_signif}{A relative difference above this cut-off is considered significant.}

\item{rise_above}{A rise above this cut-off is required before it is considered significant.}

\item{fall_below}{A fall below this cut-off is required before it is considered significant.}
}
\value{
A tibble.
}
\description{
Returns the data set with following columns added:
}
\details{
\itemize{
\item COLUMN_diff: The difference to the row before.
\item COLUMN_diff_rel: The difference to the row before as relative difference of the row before.
Either positive or negative.
\item COLUMN_diff_signif: Is the relative difference >= rel_diff_signif.
\item COLUMN_diff_cat: "rise", "fall" or "insignificant"
\item COLUMN_diff_risen: TRUE if "rise" at least once per group.
\item COLUMN_diff_rise_max: The maximal increase per group.
\item COLUMN_diff_rise_rel_max: The maximal relative increase per group.
\item COLUMN_diff_rise_first: The first rise.
\item COLUMN_diff_rise_first_seq: The first rise ignoring rows with COLUMN_diff_in_seq == FALSE.
\item COLUMN_diff_diff_rise_signif_above: TRUE if a rise was at least >= rise_above or
if the previous value was >= rise_above an increase of >= rel_diff_signif.
\item COLUMN_diff_rise_signif_above_first: The first rise in COLUMN_diff_rise_signif_above_first per group.
\item COLUMN_diff_risen_signif_above: TRUE if there was at least one per group with COLUMN_diff_rise_signif_above == TRUE.
\item COLUMN_diff_fallen: TRUE if "fall" at least once per group.
\item COLUMN_diff_fall_max: The maximal decrease per group.
\item COLUMN_diff_fall_rel_max: The maximal relative decrease per group
\item COLUMN_diff_fall_first: The first decrease.
\item COLUMN_diff_fall_first_seq: The first decrease ignoring rows with COLUMN_diff_in_seq == FALSE.
\item COLUMN_diff_diff_fall_signif_below: TRUE if a fall was at least <= fall_below or
if the previous value was <= fall_below an increase of <= rel_diff_signif.
\item COLUMN_diff_fall_signif_below_first: The first fall in COLUMN_diff_fall_signif_below_first per group.
\item COLUMN_diff_falln_signif_below: TRUE if there was at least one per group with COLUMN_diff_fall_signif_below == TRUE.
\item COLUMN_diff_max: The maximal increase or decrease per group.
\item COLUMN_diff_rel_max: The maximal relative increase or decrease per group.
\item COLUMN_diff_in_seq: Is this row the next in sequqnece or was >= 1 skipped?
}
}
