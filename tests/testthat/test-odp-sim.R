# single ------------------------------------

test_that("single resids", {
  expect_no_error({
    res <- odpResidSim(UKMotor, "single",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "single")
})

test_that("single param normal", {
  expect_no_error({
    res <- odpParamSim(UKMotor, "single",
      dist = "normal",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "single")
})

test_that("single param gamma", {
  expect_no_error({
    res <- odpParamSim(UKMotor, "single",
      dist = "gamma",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "single")
})

test_that("single param poisson", {
  expect_no_error({
    res <- odpParamSim(UKMotor, "single",
      dist = "poisson",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "single")
})

# calendar -----------------------------------

test_that("calendar resid", {
  expect_no_error({
    res <- odpResidSim(UKMotor, "calendar",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "calendar")
})

test_that("calendar param normal", {
  expect_no_error({
    res <- odpParamSim(UKMotor, "calendar",
      dist = "normal",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "calendar")
})

test_that("calendar param gamma", {
  expect_no_error({
    res <- odpParamSim(UKMotor, "calendar",
      dist = "gamma",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "calendar")
})

test_that("calendar param poisson", {
  expect_no_error({
    res <- odpParamSim(UKMotor, "calendar",
      dist = "poisson",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "calendar")
})

# origin --------------------------------------

test_that("origin resid", {
  expect_no_error({
    res <- odpResidSim(UKMotor, "origin",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "origin")
})

test_that("origin param normal", {
  expect_no_error({
    res <- odpParamSim(UKMotor, "origin",
      dist = "normal",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "origin")
})

test_that("origin param gamma", {
  expect_no_error({
    res <- odpParamSim(UKMotor, "origin",
      dist = "gamma",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "origin")
})

test_that("origin param poisson", {
  expect_no_error({
    res <- odpParamSim(UKMotor, "origin",
      dist = "poisson",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "origin")
})

# other -----------------------------------

cli::test_that_cli("coercing bad triangle results in abort", {
  triangle <- unclass(UKMotor)
  triangle[1, 5] <- 10 * triangle[1, 5]

  expect_snapshot(
    error = TRUE,
    {
      res <- odpParamSim(triangle, "single",
        dist = "gamma",
        progress = FALSE,
        n_boot = 1e2,
        n_sim = 1e2
      )
    }
  )

  expect_snapshot(
    error = TRUE,
    {
      res <- odpResidSim(triangle, "single",
        progress = FALSE,
        n_boot = 1e2,
        n_sim = 1e2
      )
    }
  )
})

test_that("pretty printing sim result works", {
  res <- odpResidSim(UKMotor, "single",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_snapshot(print(res))
})
