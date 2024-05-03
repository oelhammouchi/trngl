#' @importFrom rlang .data
#' @exportS3Method plot boot.res
plot.boot.res <- function(res, ...) {
  ggplot2::ggplot(data.frame(reserve = res["reserve"])) +
    ggplot2::geom_density(ggplot2::aes(x = .data$reserve)) +
    ggplot2::xlab("Reserve") +
    ggplot2::ylab("Probability Density") +
    ggplot2::ggtitle(sprintf(
      "Density plot of the bootstrap reserve with %d %s",
      res[["n_boot"]] * res[["n_sim"]],
      ifelse(res[["n_boot"]] * res[["n_sim"]] == 1, "simulation", "simulations")
    ))
}
