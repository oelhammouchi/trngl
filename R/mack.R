#' @importFrom rlang .data
#' @exportS3Method plot mack.single
plot.mack.single <- function(res) {
  plt.df <- reshape2::melt(res$reserves, varnames = c("sim.idx", "point"))
  ggplot2::ggplot(plt.df) +
    ggplot2::geom_density(ggplot2::aes(.data$value, col = factor(.data$point))) +
    ggplot2::xlab("Reserve") +
    ggplot2::ylab("Probability density") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Ommitted point"))
}

#' @importFrom rlang .data
#' @exportS3Method plot mack.calendar
plot.mack.calendar <- function(res) {
  plt.df <- reshape2::melt(res$reserves, varnames = c("sim.idx", "cal.year"))
  ggplot2::ggplot(plt.df) +
    ggplot2::geom_density(ggplot2::aes(.data$value, col = factor(.data$cal.year))) +
    ggplot2::xlab("Reserve") +
    ggplot2::ylab("Probability density") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Ommitted calendar year"))
}

#' @importFrom rlang .data
#' @exportS3Method plot mack.origin
plot.mack.origin <- function(res) {
  plt.df <- reshape2::melt(res$reserves, varnames = c("sim.idx", "origin.year"))
  ggplot2::ggplot(plt.df) +
    ggplot2::geom_density(ggplot2::aes(.data$value, col = factor(.data$origin.year))) +
    ggplot2::xlab("Reserve") +
    ggplot2::ylab("Probability density") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Ommitted origin year"))
}

#' @export
format.mack.single <- function(res, ...) {}

#' @export
print.mack.single <- function(res, ...) {
  cat(format(res, ...), "\n")
}

#' Perform a parametric bootstrap of the Mack chain ladder model.
#'
#' @param triangle Cumulative claims triangle
#' @param dist Distribution
#' @param cond Should the bootstrap should be conditional? Default is `TRUE`.
#' @param n_boot Number of iterations in the parameter bootstrap step. Default is 1000.
#' @param n_sim Number of iterations in the simulation step. Default is 10.
#' @param progress Show progress? Default is `TRUE.
#' @export
mackParamBoot <- function(trngl, dist, cond, n_boot = 1e3, n_sim = 10, progress = TRUE) {
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

  return(.mackParamBoot(trngl, dist, cond, n_boot, n_sim, progress))
}

#' Perform a semiparametric bootstrap of the Mack chain ladder model.
#'
#' @param triangle Cumulative claims triangle
#' @param resid_type Type of residuals to use in bootstrap. Possible choices are
#' `"standardised"`, `"studentised"` and `"log-normal"`.
#' @param cond Should the bootstrap should be conditional? Default is `TRUE`.
#' @param n_boot Number of iterations in the parameter bootstrap step. Default is 1000.
#' @param n_sim Number of iterations in the simulation step. Default is 10.
#' @param progress Show progress? Default is `TRUE.
#' @export
mackResidBoot <- function(trngl, resid_type, cond, n_boot = 1e3, n_sim = 10, progress = TRUE) {
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

  return(.mackResidBoot(trngl, resid_type, cond, n_boot, n_sim, progress))
}

#' Perform a pairs bootstrap of the Mack chain ladder model.
#'
#' @param triangle Cumulative claims triangle
#' @param n_boot Number of iterations in the parameter bootstrap step. Default is 1000.
#' @param n_sim Number of iterations in the simulation step. Default is 10.
#' @param progress Show progress? Default is `TRUE.
#' @export
mackPairsBoot <- function(trngl, n_boot = 1e3, n_sim = 10, progress = TRUE) {
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

  return(.mackPairsBoot(trngl, n_boot, n_sim, progress))
}

#' Perform a pairs bootstrap of the Mack chain ladder model.
#'
#' @param triangle Cumulative claims triangle
#' @param n_boot Number of iterations in the parameter bootstrap step. Default is 1000.
#' @param n_sim Number of iterations in the simulation step. Default is 10.
#' @param progress Show progress? Default is `TRUE.
#' @export
mackPairsSim <- function(trngl, sim_type, n_boot = 1e3, n_sim = 10, progress = TRUE) {
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

  res <- .mackPairsSim(trngl, sim_type, n_boot, n_sim, progress)

  if (progress) cli::cli_progress_step("Flagging outliers")
  ref <- mackPairsBoot(trngl, n_boot, n_sim, progress = FALSE)

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
mackParamSim <- function(trngl, sim_type, cond, dist, n_boot = 1e3, n_sim = 10, progress = TRUE) {
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

  res <- .mackParamSim(trngl, sim_type, cond, dist, n_boot, n_sim, progress)

  if (progress) cli::cli_progress_step("Flagging outliers")
  ref <- mackParamBoot(trngl, dist, cond, n_boot, n_sim, progress = FALSE)

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
mackResidSim <- function(trngl, sim_type, cond, resid_type, n_boot = 1e3, n_sim = 10, progress = TRUE) {
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

  res <- .mackResidSim(trngl, sim_type, cond, resid_type, n_boot, n_sim, progress)

  if (progress) cli::cli_progress_step("Flagging outliers")
  ref <- mackResidBoot(trngl, resid_type, cond, n_boot, n_sim, progress = FALSE)

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
