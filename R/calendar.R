#' @importFrom rlang .data
#' @method plot calendar
#' @export
plot.calendar <- function(x, ...) {
  plt.df <- reshape2::melt(x$reserves, varnames = c("sim.idx", "calendar"))
  plt.df$calendar <- sapply(plt.df$calendar, function(i) {
    x$col_mapping[[i]]
  })
  ggplot2::ggplot(plt.df) +
    ggplot2::geom_density(ggplot2::aes(.data$value, col = factor(.data$calendar))) +
    ggplot2::ggtitle("Simulated reserve when omitting calendar year") +
    ggplot2::xlab("Reserve") +
    ggplot2::ylab("Probability density") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Calendar year"))
}
