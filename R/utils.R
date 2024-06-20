labelDF <- function(sim.res) {
  do.call(rbind, lapply(seq_len(ncol(sim.res$reserves)), function(j) {
    point <- sim.res$col_mapping[[j]]

    z <- density(sim.res$reserves[, j])

    x.candidates <- z$x[z$x > quantile(z$x, 0.1) & z$x < quantile(z$x, 0.9)]
    y.candidates <- z$y[z$x > quantile(z$x, 0.1) & z$x < quantile(z$x, 0.9)]
    idx <- sample(seq_along(x.candidates), size = 1)

    x <- x.candidates[idx]
    y <- y.candidates[idx]

    data.frame(label = sprintf("(%i, %i)", point[1], point[2]), x = x, y = y)
  }))
}

#' @export
calendarPeriod <- function(triangle, index = 0) {
  n_dev <- ncol(triangle)
  res <- c()
  for (i in seq_len(n_dev)) {
    if (n_dev - i + 1 - index < 0) break
    res <- c(res, triangle[i, n_dev - i + 1 - index])
  }

  return(res)
}

#' @export
`calendarPeriod<-` <- function(triangle, index = 0, value) {
  n_dev <- ncol(triangle)
  for (i in seq_len(n_dev)) {
    if (n_dev - i + 1 - index < 0) break
    triangle[i, n_dev - i + 1 - index] <- value[i]
  }
  return(triangle)
}
