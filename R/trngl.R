cumCheck <- function(triangle) {
  conds <- sapply(seq_len(nrow(triangle)), function(i) {
    row <- triangle[i, ]
    row <- row[!is.na(row)]
    all(sort(row) == row, na.rm = TRUE)
  })

  msg <- c(
    "{.var triangle} must a cumulative"
  )
  for (i in seq_along(conds)) {
    if (!conds[i]) {
      msg <- c(msg, "i" = "Row {i} is not increasing.")
    }
  }

  if (!all(conds)) {
    cli::cli_abort(msg)
  }
}

# When a new value is assigned to a triangle element, it's not certain that the
# resulting matrix will again be a valid cumulative claims triangle, hence drop
# the 'trngl' class.
#' @export
`[<-.trngl` <- function(trngl, i, j, value) {
  candidate <- NextMethod()

  tryCatch(
    {
      cumCheck(candidate)
    },
    error = function(err) {
      cli::cli_abort(c(
        "Could not complete assigment",
        "i" = "Assignment would lead to defective triangle"
      ))
    }
  )

  candidate
}

#' Coerce to trngl
#'
#' `as.trngl` is an S3 generic with methods for matrix and trngl itself.
#' The latter clears the outliers which are currently flagged.
#' @param triangle A *cumulative* claims triangle
#'
#' @name as.trngl
#' @return A trngl
#' @export
as.trngl <- function(triangle) {
  UseMethod("as.trngl")
}

#' @rdname as.trngl
#' @examples
#' triangle <- UKMotor
#' triangle[1, 7] <- 10 * triangle[1, 7]
#' res <- mackParamSim(triangle, "single",
#'   cond = TRUE,
#'   dist = "normal",
#'   n_boot = 1e2,
#'   n_sim = 1e2,
#'   progress = FALSE
#' )
#' print(triangle)
#' triangle <- as.trngl(triangle)
#' print(triangle) # no outliers marked
#' @export
as.trngl.trngl <- function(triangle) {
  attr(triangle, "outliers") <- list()
  return(triangle)
}

#' @rdname as.trngl
#' @examples
#' raw.triangle <- unclass(UKMotor)
#' as.trngl(raw.triangle)
#' @export
as.trngl.matrix <- function(triangle) {
  attr(triangle, "name") <- deparse(substitute(triangle))
  if (!(nrow(triangle) == ncol(triangle))) {
    cli::cli_abort(c(
      "{.var triangle} must square.",
      "i" = "{.var triangle} has {nrow(triangle)} rows and {ncol(triangle)} columns",
      "i" = "Non-square triangles are not yet supported."
    ))
  }

  cumCheck(triangle)

  return(
    structure(
      triangle,
      outliers = list(),
      class = c("trngl", class(triangle))
    )
  )
}

#' @export
format.trngl <- function(x, ...) {
  name <- attr(x, "name")
  class(x) <- "character"

  # highlight outliers
  outlier_type <- attr(x, "outlier_type")
  for (outlier in attr(x, "outliers")) {
    if (outlier_type == "origin") {
      x[outlier, !is.na(x[outlier, ])] <- cli::col_red(x[outlier, !is.na(x[outlier, ])])
    } else if (outlier_type == "calendar") {
      calendarPeriod(x) <- cli::col_red(calendarPeriod(x))
    } else {
      x[outlier[1], outlier[2]] <- cli::col_red(x[outlier[1], outlier[2]])
    }
  }

  vec <- as.character(x)
  vec <- ifelse(is.na(vec), "", vec)
  out <- cli::ansi_columns(vec,
    fill = "cols",
    max_cols = ncol(x),
    sep = " ",
    align = "center",
    width = floor(cli::console_width() / 1.2)
  )

  return(cli::boxx(out, header = name, padding = c(0, 1, 0, 1)))
}

#' @export
print.trngl <- function(x, ...) cat(format(x, ...), "\n")

#' Convert between representations of claims triangles
#'
#' @param trngl For `cum2incr` a cumulative claims triangle, for `incr2cum` an incremental one.
#'
#' @examples
#' UKMotor.incr <- cum2incr(UKMotor)
#' all.equal(UKMotor, incr2cum(UKMotor.incr))
#' @name convert
#' @return `cum2incr` returns the cumulative representation of a cumulative claims triangle.
#' Vice versa for `incr2cum`.
NULL

#' @rdname convert
#' @export
cum2incr <- function(trngl) {
  class(trngl) <- setdiff(class(trngl), "trngl")

  ndev <- ncol(trngl)
  for (j in seq(2, ndev)) {
    for (i in seq(1, ndev + 1 - j)) {
      trngl[i, j] <- trngl[i, j] - sum(trngl[i, 1:(j - 1)])
    }
  }

  return(trngl)
}

#' @rdname convert
#' @export
incr2cum <- function(trngl) {
  ndev <- ncol(trngl)
  for (j in seq(ndev, 2)) {
    for (i in seq(1, ndev + 1 - j)) {
      trngl[i, j] <- trngl[i, j] + sum(trngl[i, 1:(j - 1)])
    }
  }

  return(as.trngl(trngl))
}
