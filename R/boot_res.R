#' @importFrom rlang .data
#' @method plot boot.res
#' @export
plot.boot.res <- function(x, ...) {
  ggplot2::ggplot(data.frame(reserve = x["reserve"])) +
    ggplot2::geom_density(ggplot2::aes(x = .data$reserve)) +
    ggplot2::xlab("Reserve") +
    ggplot2::ylab("Probability Density") +
    ggplot2::ggtitle(sprintf(
      "Density plot of the bootstrap reserve with %d %s",
      x[["nboot"]],
      ifelse(x[["nboot"]] == 1, "simulation", "simulations")
    ))
}
