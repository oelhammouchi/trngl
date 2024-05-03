klDivergence <- function(x, y) {
  p <- approxfun(density(x), rule = 2)
  q <- approxfun(density(y), rule = 2)
  integrand <- function(x) p(x) * log(p(x) / q(x))

  domain <- c(min(min(x), min(y)), max(max(x), max(y)))
  res <- try(
    integrate(integrand, domain[1], domain[2],
      subdivisions = 1e5,
      stop.on.error = FALSE,
      abs.tol = 0L
    )$value,
    silent = TRUE
  )
  if (class(res) == "try-error") {
    if (attr(res, "condition")$message == "non-finite function value") {
      return(Inf)
    }
  }

  res
}
