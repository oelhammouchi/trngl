test_that("resids conditional standardised", {
  res <- mackResidBoot(UKMotor,
    cond = TRUE,
    resid_type = "standardised",
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("resids conditional studentised", {
  res <- mackResidBoot(UKMotor,
    cond = TRUE,
    resid_type = "studentised",
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("resids conditional log-normal", {
  res <- mackResidBoot(UKMotor,
    cond = TRUE,
    resid_type = "log-normal",
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("resids unconditional standardised", {
  res <- mackResidBoot(UKMotor,
    cond = FALSE,
    resid_type = "standardised",
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("resids unconditional studentised", {
  res <- mackResidBoot(UKMotor,
    cond = FALSE,
    resid_type = "studentised",
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("resids unconditional log-normal", {
  res <- mackResidBoot(UKMotor,
    cond = FALSE,
    resid_type = "log-normal",
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("param conditional normal", {
  res <- mackParamBoot(UKMotor,
    cond = TRUE,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("param conditional gamma", {
  res <- mackParamBoot(UKMotor,
    cond = TRUE,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("param unconditional normal", {
  res <- mackParamBoot(UKMotor,
    cond = FALSE,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("param unconditional gamma", {
  res <- mackParamBoot(UKMotor,
    cond = FALSE,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("pairs", {
  res <- mackPairsBoot(UKMotor,
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})
