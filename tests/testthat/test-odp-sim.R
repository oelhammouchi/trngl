# single ------------------------------------

test_that("single resids", {
  res <- odpResidSim(UKMotor, "single",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "odp.single")
})

test_that("single param normal", {
  res <- odpParamSim(UKMotor, "single",
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "odp.single")
})

test_that("single param gamma", {
  res <- odpParamSim(UKMotor, "single",
    dist = "gamma",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "odp.single")
})

# calendar -----------------------------------

test_that("calendar resid", {
  res <- odpResidSim(UKMotor, "calendar",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "odp.calendar")
})

test_that("calendar param normal", {
  res <- odpParamSim(UKMotor, "calendar",
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "odp.calendar")
})

test_that("calendar param gamma", {
  res <- odpParamSim(UKMotor, "calendar",
    dist = "gamma",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "odp.calendar")
})

# origin --------------------------------------

test_that("origin resid", {
  res <- odpResidSim(UKMotor, "origin",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "odp.origin")
})

test_that("origin param normal", {
  res <- odpParamSim(UKMotor, "origin",
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "odp.origin")
})

test_that("origin param gamma", {
  res <- odpParamSim(UKMotor, "origin",
    dist = "gamma",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "odp.origin")
})
