#' @importFrom rlang .data
#' @method plot single
#' @export
plot.single <- function(x, ...) {
  plt.df <- reshape2::melt(x$reserves, varnames = c("sim.idx", "point"))
  plt.df$point <- sapply(plt.df$point, function(idx) {
    point <- x$col_mapping[[idx]]
    sprintf("(%i, %i)", point[1], point[2])
  })
  plt.df <- plt.df[order(plt.df$point), ]
  label.df <- labelDF(x)

  ggplot2::ggplot(plt.df) +
    ggplot2::geom_density(ggplot2::aes(x = .data$value, y = ggplot2::after_stat(density), col = factor(.data$point))) +
    ggrepel::geom_text_repel(ggplot2::aes(.data$x, .data$y, label = .data$label),
      data = label.df,
      force = 10,
      min.segment.length = 0,
      max.overlaps = Inf
    ) +
    ggplot2::ggtitle("Simulated reserve when omitting single point") +
    ggplot2::xlab("Reserve") +
    ggplot2::ylab("Probability density") +
    ggplot2::guides(colour = "none")
}
