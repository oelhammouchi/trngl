test_that("KL divergence works correctly", {
  mu1 <- 0
  sigma1 <- 1
  mu2 <- 3
  sigma2 <- 2

  analytic <- 0.5 * ((mu2 - mu1)**2 / sigma2**2 + sigma1**2 / sigma2**2 - log(sigma1**2 / sigma2**2) - 1)
  res <- klDivergence(rnorm(1e5, mean = mu1, sd = sigma1), rnorm(1e5, mean = mu2, sd = sigma2))

  expect_true(abs(analytic - res) < 0.1)
})
