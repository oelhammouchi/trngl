test_that("single resids", {
  res <- odpResidBoot(UKMotor,
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("single param normal", {
  res <- odpParamBoot(UKMotor,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("single param gamma", {
  res <- odpParamBoot(UKMotor,
    dist = "gamma",
    progress = FALSE,
    n_boot = 1e2
  )
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})
