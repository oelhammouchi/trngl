#' @export
cumCheck <- function(triangle) {
  conds <- sapply(seq_len(nrow(triangle)), function(i) {
    row <- triangle[i, ]
    row <- row[!is.na(row)]
    all(sort(row) == row, na.rm = TRUE)
  })

  msg <- c(
    "{.var triangle} must a cumulative when {.var type} is 'Mack'."
  )
  for (i in seq_along(conds)) {
    if (!conds[i]) {
      msg <- c(msg, "i" = "Row {i} is not increasing.")
    }
  }
  return(list(test = all(conds), msg = msg))
}

# When a new value is assigned to a triangle element, it's not certain that the
# resulting matrix will again be a valid cumulative claims triangle, hence drop
# the 'trngl' class.
#' @export
`[<-.trngl` <- function(trngl, i, j, value) {
  candidate <- NextMethod()
  res <- cumCheck(candidate)

  if (!res$test) {
    cli::cli_abort(c(
      "Could not complete assigment",
      "i" = "Assignment would lead to defective triangle"
    ))
  } else {
    candidate
  }
}

#' @export
as.trngl <- function(triangle, ...) {
  UseMethod("as.trngl")
}

#' @export
as.trngl.trngl <- function(triangle, ...) {
  attr(triangle, "outliers") <- list()
  return(triangle)
}

#' @export
as.trngl.matrix <- function(triangle, ...) {
  if (!(nrow(triangle) == ncol(triangle))) {
    cli::cli_abort(c(
      "{.var triangle} must square.",
      "i" = "{.var triangle} has {nrow(triangle)} rows and {ncol(triangle)} columns",
      "i" = "Non-square triangles are not yet supported."
    ))
  }

  res <- cumCheck(triangle)
  if (!res$test) {
    cli::cli_abort(res$msg)
  }

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
  out.str <- ""
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x) + 1 - i)) {
      if (any(sapply(attr(x, "outliers"), function(point) {
        all(c(i, j) == point)
      }))) {
        out.str <- paste(out.str, cli::col_red(as.character(x[i, j])), sep = "\t")
      } else {
        out.str <- paste(out.str, x[i, j], sep = "\t")
      }
    }
    out.str <- paste0(out.str, "\n")
  }

  return(out.str)
}

#' @export
print.trngl <- function(x, ...) cat(format(x, ...), "\n")


#' Convert cumulative triangle to incremental one.
#'
#' @param trngl Cumulative claims triangle.
#'
#' @return Incremental claims triangle.
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

#' Convert incremental triangle to cumulative one.
#'
#' @param trngl Incremental claims triangle.
#'
#' @return Cumulative claims triangle.
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
