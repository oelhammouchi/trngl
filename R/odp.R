#' @importFrom rlang .data
#' @exportS3Method plot odp.single
plot.odp.single <- function(x, ...) {
  plt.df <- reshape2::melt(x$reserves, varnames = c("sim.idx", "point"))

  label.df <- do.call(rbind, lapply(seq_len(ncol(x$reserves)), function(j) {
    point <- x$col_mapping[[j]]

    z <- density(x$reserves[, j])

    x.candidates <- z$x[z$x > quantile(z$x, 0.1) & z$x < quantile(z$x, 0.9)]
    y.candidates <- z$y[z$x > quantile(z$x, 0.1) & z$x < quantile(z$x, 0.9)]
    idx <- sample(seq_along(x.candidates), size = 1)
    x <- x.candidates[idx]
    y <- y.candidates[idx]

    data.frame(label = sprintf("(%i, %i)", point[1], point[2]), x = x, y = y)
  }))

  ggplot2::ggplot(plt.df) +
    ggrepel::geom_text_repel(ggplot2::aes(.data$x, .data$y, label = .data$label),
      data = label.df,
      force = 10,
      min.segment.length = 0
    ) +
    ggplot2::geom_density(ggplot2::aes(x = .data$value, y = ggplot2::after_stat(density), col = factor(.data$point))) +
    ggplot2::ggtitle("Simulated reserve densities for different omitted points") +
    ggplot2::xlab("Reserve") +
    ggplot2::ylab("Probability density") +
    ggplot2::guides(colour = "none")
}

#' @importFrom rlang .data
#' @exportS3Method plot odp.calendar
plot.odp.calendar <- function(x, ...) {
  plt.df <- reshape2::melt(x$reserves, varnames = c("sim.idx", "cal.year"))
  ggplot2::ggplot(plt.df) +
    ggplot2::geom_density(
      ggplot2::aes(.data$value, col = factor(.data$cal.year))
    ) +
    ggplot2::xlab("Reserve") +
    ggplot2::ylab("Probability density") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Ommitted calendar year"))
}

#' @importFrom rlang .data
#' @exportS3Method plot odp.origin
plot.odp.origin <- function(x, ...) {
  plt.df <- reshape2::melt(x$reserves, varnames = c("sim.idx", "origin.year"))
  ggplot2::ggplot(plt.df) +
    ggplot2::geom_density(
      ggplot2::aes(.data$value, col = factor(.data$origin.year))
    ) +
    ggplot2::xlab("Reserve") +
    ggplot2::ylab("Probability density") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Ommitted origin year"))
}

#' @export
format.odp.single <- function(x, ...) {}

#' @export
print.odp.single <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' @export
odpParamBoot <- function(trngl, dist, n_boot = 1e3, n_sim = 1e3, progress = TRUE) {
  if (!inherits(trngl, "trngl")) {
    tryCatch(
      {
        trngl <- as.trngl(trngl)
      },
      error = function(err) {
        cli::cli_abort(c(
          "Failed to coerce {.var trngl} to 'trngl'"
        ))
      }
    )
  }
  return(.odpParamBoot(cum2incr(trngl), dist, n_boot, n_sim, progress))
}

#' @export
odpResidBoot <- function(trngl, n_boot = 1e3, n_sim = 1e3, progress = TRUE) {
  if (!inherits(trngl, "trngl")) {
    tryCatch(
      {
        trngl <- as.trngl(trngl)
      },
      error = function(err) {
        cli::cli_abort(c(
          "Failed to coerce {.var trngl} to 'trngl'"
        ))
      }
    )
  }
  return(.odpResidBoot(cum2incr(trngl), n_boot, n_sim, progress))
}


#' @export
odpParamSim <- function(trngl, sim_type, dist, n_boot = 1e3, n_sim = 1e3, progress = TRUE) {
  trngl.name <- deparse(substitute(trngl))
  if (!inherits(trngl, "trngl")) {
    tryCatch(
      {
        trngl <- as.trngl(trngl)
      },
      error = function(err) {
        cli::cli_abort(c(
          "Failed to coerce {.var trngl} to 'trngl'"
        ))
      }
    )
  }

  res <- .odpParamSim(cum2incr(trngl), sim_type, dist, n_boot, n_sim, progress)

  if (progress) cli::cli_progress_step("Flagging outliers")
  ref <- odpParamBoot(trngl, dist, n_boot, n_sim, progress = FALSE)

  dist <- rep(0, ncol(res$reserves))
  n <- ncol(res$reserves) # nolint
  k <- 0
  if (progress) cli::cli_progress_step("Processed {k} out of {n}")
  for (k in seq_len(ncol(res$reserves))) {
    dist[k] <- klDivergence(ref$reserve, res$reserves[, k])
    if (progress) cli::cli_progress_update()
  }

  k_max <- which.max(dist)
  outlier <- res$col_mapping[[k_max]]
  attr(trngl, "outliers") <- c(attr(trngl, "outliers"), list(outlier))

  assign(trngl.name, trngl, parent.frame())
  return(res)
}

#' @export
odpResidSim <- function(trngl, sim_type, n_boot = 1e3, n_sim = 1e3, progress = TRUE) {
  trngl.name <- deparse(substitute(trngl))
  if (!inherits(trngl, "trngl")) {
    tryCatch(
      {
        trngl <- as.trngl(trngl)
      },
      error = function(err) {
        cli::cli_abort(c(
          "Failed to coerce {.var trngl} to 'trngl'"
        ))
      }
    )
  }

  res <- .odpResidSim(cum2incr(trngl), sim_type, n_boot, n_sim, progress)

  if (progress) cli::cli_progress_step("Flagging outliers")
  ref <- odpResidBoot(trngl, n_boot, n_sim, progress = FALSE)

  dist <- rep(0, ncol(res$reserves))
  n <- ncol(res$reserves) # nolint
  k <- 0
  if (progress) cli::cli_progress_step("Processed {k} out of {n}")
  for (k in seq_len(ncol(res$reserves))) {
    dist[k] <- klDivergence(ref$reserve, res$reserves[, k])
    if (progress) cli::cli_progress_update()
  }

  k_max <- which.max(dist)
  outlier <- res$col_mapping[[k_max]]
  attr(trngl, "outliers") <- c(attr(trngl, "outliers"), list(outlier))

  assign(trngl.name, trngl, parent.frame())
  return(res)
}
