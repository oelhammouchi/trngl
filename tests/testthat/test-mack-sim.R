# single ---------------------------------------------

test_that("single resids conditional standardised", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "single",
      cond = TRUE,
      resid_type = "standardised",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "single")
})

test_that("single resids conditional studentised", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "single",
      cond = TRUE,
      resid_type = "studentised",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "single")
})

test_that("single resids conditional log-normal", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "single",
      cond = TRUE,
      resid_type = "log-normal",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "single")
})

test_that("single resids unconditional standardised", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "single",
      cond = FALSE,
      resid_type = "standardised",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "single")
})

test_that("single resids unconditional studentised", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "single",
      cond = FALSE,
      resid_type = "studentised",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "single")
})

test_that("single resids unconditional log-normal", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "single",
      cond = FALSE,
      resid_type = "log-normal",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "single")
})

test_that("single param conditional normal", {
  expect_no_error({
    res <- mackParamSim(UKMotor, "single",
      cond = TRUE,
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

test_that("single param conditional gamma", {
  expect_no_error({
    res <- mackParamSim(UKMotor, "single",
      cond = TRUE,
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

test_that("single param unconditional normal", {
  expect_no_error({
    res <- mackParamSim(UKMotor, "single",
      cond = FALSE,
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

test_that("single param unconditional gamma", {
  expect_no_error({
    res <- mackParamSim(UKMotor, "single",
      cond = FALSE,
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

test_that("single pairs", {
  expect_no_error({
    res <- mackPairsSim(UKMotor, "single",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "single")
})

# calendar ---------------------------------------------

test_that("calendar resid conditional standardised", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "calendar",
      cond = TRUE,
      resid_type = "standardised",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "calendar")
})

test_that("calendar resid conditional studentised", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "calendar",
      cond = TRUE,
      resid_type = "studentised",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "calendar")
})

test_that("calendar resid conditional log-normal", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "calendar",
      cond = TRUE,
      resid_type = "log-normal",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "calendar")
})

test_that("calendar resid unconditional standardised", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "calendar",
      cond = FALSE,
      resid_type = "standardised",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "calendar")
})

test_that("calendar resid unconditional studentised", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "calendar",
      cond = FALSE,
      resid_type = "studentised",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "calendar")
})

test_that("calendar resid unconditional log-normal", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "calendar",
      cond = FALSE,
      resid_type = "log-normal",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "calendar")
})

test_that("calendar param conditional normal", {
  expect_no_error({
    res <- mackParamSim(UKMotor, "calendar",
      cond = TRUE,
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

test_that("calendar param conditional gamma", {
  expect_no_error({
    res <- mackParamSim(UKMotor, "calendar",
      cond = TRUE,
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

test_that("calendar param unconditional normal", {
  expect_no_error({
    res <- mackParamSim(UKMotor, "calendar",
      cond = FALSE,
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

test_that("calendar param unconditional gamma", {
  expect_no_error({
    res <- mackParamSim(UKMotor, "calendar",
      cond = FALSE,
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

test_that("calendar pairs", {
  expect_no_error({
    res <- mackPairsSim(UKMotor, "calendar",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "calendar")
})


# origin ---------------------------------------------

test_that("origin resid conditional standardised", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "origin",
      cond = TRUE,
      resid_type = "standardised",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "origin")
})

test_that("origin resid conditional studentised", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "origin",
      cond = TRUE,
      resid_type = "studentised",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "origin")
})

test_that("origin resid conditional log-normal", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "origin",
      cond = TRUE,
      resid_type = "log-normal",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "origin")
})

test_that("origin resid unconditional standardised", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "origin",
      cond = FALSE,
      resid_type = "standardised",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "origin")
})

test_that("origin resid unconditional studentised", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "origin",
      cond = FALSE,
      resid_type = "studentised",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "origin")
})

test_that("origin resid unconditional log-normal", {
  expect_no_error({
    res <- mackResidSim(UKMotor, "origin",
      cond = FALSE,
      resid_type = "log-normal",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "origin")
})

test_that("origin param conditional normal", {
  expect_no_error({
    res <- mackParamSim(UKMotor, "origin",
      cond = TRUE,
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

test_that("origin param conditional gamma", {
  expect_no_error({
    res <- mackParamSim(UKMotor, "origin",
      cond = TRUE,
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

test_that("origin param unconditional normal", {
  expect_no_error({
    res <- mackParamSim(UKMotor, "origin",
      cond = FALSE,
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

test_that("origin param unconditional gamma", {
  expect_no_error({
    res <- mackParamSim(UKMotor, "origin",
      cond = FALSE,
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

test_that("origin pairs", {
  expect_no_error({
    res <- mackPairsSim(UKMotor, "origin",
      progress = FALSE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "origin")
})

# origin ---------------------------------------------

cli::test_that_cli("coercing bad triangle results in abort", {
  triangle <- unclass(UKMotor)
  triangle[1, 5] <- 10 * triangle[1, 5]

  expect_snapshot(
    error = TRUE,
    {
      res <- mackParamSim(triangle, "single",
        cond = TRUE,
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
      res <- mackResidSim(triangle, "single",
        cond = TRUE,
        resid_type = "standardised",
        progress = FALSE,
        n_boot = 1e2,
        n_sim = 1e2
      )
    }
  )

  expect_snapshot(
    error = TRUE,
    {
      res <- mackPairsSim(triangle, "single",
        progress = FALSE,
        n_boot = 1e2,
        n_sim = 1e2
      )
    }
  )
})

test_that("pretty printing sim result works", {
  res <- mackResidSim(UKMotor, "single",
    cond = TRUE,
    resid_type = "standardised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_snapshot(print(res))
})
