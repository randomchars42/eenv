#'
#' Calculate and plot a qq-plot.
#'
#' @description
#' Calculate and plot a qq-plot with actual values as axes instead of z-values.
#'
#' @export
#' @param data A tibble containing the data.
#' @param column Which column to use.
#' @param method How to calculate ranks.
#' @return list
#'
plot_qq <- function(data, column, method = "middle") {
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`
  column = rlang::enquo(column)

  data_tmp <-
    tibble::tibble(
      x = dplyr::pull(data, !! column)) %>%
    dplyr::mutate(
      rank = rank(x))

  total <- nrow(data_tmp)

  if (method == "") {
    data_tmp <- data_tmp %>%
      dplyr::mutate(
        probability = 1 / (total + 1),
        cumulative_probability = rank * probability)
  } else if (method == "middle") {
    data_tmp <- data_tmp %>%
      dplyr::mutate(
        probability = 1 / (total),
        cumulative_probability = (rank - 0.5) * probability)
  }

  data_tmp <- data_tmp %>%
    dplyr::mutate(
      z = stats::qnorm(cumulative_probability))

  plot <- ggplot2::ggplot(ggplot2::aes(x = z, y = x), data = data_tmp) +
    ggplot2::geom_point()

  result = list(
    data = data_tmp,
    plot = plot
  )

  return(result)
}

