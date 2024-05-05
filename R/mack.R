#' @export
format.mack <- function(x, ...) {
  cli::cli_fmt(collapse = TRUE, {
    cli::cli_rule(left = "Mack bootstrap simulation test")
    cli::cli_ul()
    cli::cli_li("bootstrap iterations: {x$n_boot}")
    cli::cli_li("simulation iterations: {x$n_sim}")
    cli::cli_li("status:")
  })
}

#' @export
print.mack <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' Bootstrapping and simulation functions for the Mack chain ladder model
#' @param trngl A *cumulative* claims triangle
#' @param dist Distribution from which to simulate: "normal", "gamma" or "poisson"
#' @param resid_type Which type of residuals to use: "standardised", "studentised" or "log-normal"
#' @param cond Whether the bootstrap should be conditional
#' @param sim_type Type of simulation: "single", "origin" or "calendar"
#' @param n_boot Number of bootstrap iterations
#' @param n_sim Number of simulation iterations
#' @param progress Whether to show progress
#' @name mack
NULL

#' @rdname mack
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

#' @rdname mack
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

#' @rdname mack
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

#' @rdname mack
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

  if (progress) {
    n <- ncol(res$reserves) # nolint
    k <- 0
    cli::cli_alert_info("Flagging outliers")
    cli::cli_progress_bar(
      format = "{cli::pb_spin} Processed {k} out of {n}",
      format_done = "{cli::col_green(cli::symbol$tick)} Done",
      total = n,
      clear = FALSE
    )
  }

  ref <- mackPairsBoot(trngl, n_boot, n_sim, progress = FALSE)
  dist <- rep(0, ncol(res$reserves))
  for (k in seq_len(ncol(res$reserves))) {
    dist[k] <- klDivergence(ref$reserve, res$reserves[, k])
    if (progress) cli::cli_progress_update()
  }
  cli::cli_process_done()

  k_max <- which.max(dist)
  outlier <- res$col_mapping[[k_max]]
  attr(trngl, "outliers") <- c(attr(trngl, "outliers"), list(outlier))

  assign(trngl.name, trngl, parent.frame())
  return(res)
}

#' @rdname mack
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

  if (progress) {
    n <- ncol(res$reserves) # nolint
    k <- 0
    cli::cli_alert_info("Flagging outliers")
    cli::cli_progress_bar(
      format = "{cli::pb_spin} Processed {k} out of {n}",
      format_done = "{cli::col_green(cli::symbol$tick)} Done",
      total = n,
      clear = FALSE
    )
  }

  ref <- mackParamBoot(trngl, dist, cond, n_boot, n_sim, progress = FALSE)
  dist <- rep(0, ncol(res$reserves))
  for (k in seq_len(ncol(res$reserves))) {
    dist[k] <- klDivergence(ref$reserve, res$reserves[, k])
    if (progress) cli::cli_progress_update()
  }
  cli::cli_progress_done()

  k_max <- which.max(dist)
  outlier <- res$col_mapping[[k_max]]
  attr(trngl, "outliers") <- c(attr(trngl, "outliers"), list(outlier))

  assign(trngl.name, trngl, parent.frame())
  return(res)
}

#' @rdname mack
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

  if (progress) {
    n <- ncol(res$reserves) # nolint
    k <- 0
    cli::cli_alert_info("Flagging outliers")
    cli::cli_progress_bar(
      format = "{cli::pb_spin} Processed {k} out of {n}",
      format_done = "{cli::col_green(cli::symbol$tick)} Done",
      total = n,
      clear = FALSE
    )
  }

  ref <- mackResidBoot(trngl, resid_type, cond, n_boot, n_sim, progress = FALSE)
  dist <- rep(0, ncol(res$reserves))
  for (k in seq_len(ncol(res$reserves))) {
    dist[k] <- klDivergence(ref$reserve, res$reserves[, k])
    if (progress) cli::cli_progress_update()
  }
  cli::cli_progress_done()

  k_max <- which.max(dist)
  outlier <- res$col_mapping[[k_max]]
  attr(trngl, "outliers") <- c(attr(trngl, "outliers"), list(outlier))

  assign(trngl.name, trngl, parent.frame())
  return(res)
}
