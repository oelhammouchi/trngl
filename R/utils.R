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
