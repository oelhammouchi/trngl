#' @importFrom rlang .data
#' @method plot origin
#' @export
plot.origin <- function(x, ...) {
  plt.df <- reshape2::melt(x$reserves, varnames = c("sim.idx", "origin"))
  plt.df$origin <- sapply(plt.df$origin, function(i) {
    x$col_mapping[[i]]
  })
  ggplot2::ggplot(plt.df) +
    ggplot2::geom_density(ggplot2::aes(.data$value, col = factor(.data$origin))) +
    ggplot2::ggtitle("Simulated reserve when omitting origin year") +
    ggplot2::xlab("Reserve") +
    ggplot2::ylab("Probability density") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Origin year"))
}
