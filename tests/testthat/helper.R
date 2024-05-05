save_png <- function(plot, width = 10.6, height = 6) {
  plot <- plot +
    ggplot2::theme_test() +
    ggplot2::theme_update(
      axis.title.x = ggplot2::element_text(size = 6),
      axis.title.y = ggplot2::element_text(size = 6),
      plot.title = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 4),
      legend.text = ggplot2::element_text(size = 4),
      legend.key.size = ggplot2::unit(4, "points"),
      legend.title = ggplot2::element_text(size = 4)
    )

  path <- tempfile(fileext = ".png")
  ggplot2::ggsave(path, plot, width = width, height = height, units = "cm", dpi = 600)
  path
}
