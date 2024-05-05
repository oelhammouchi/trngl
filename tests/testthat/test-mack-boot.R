test_that("resids conditional standardised", {
  expect_no_error({
    res <- mackResidBoot(UKMotor,
      cond = TRUE,
      resid_type = "standardised",
      progress = FALSE,
      n_boot = 1e2
    )
  })
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("resids conditional studentised", {
  expect_no_error({
    res <- mackResidBoot(UKMotor,
      cond = TRUE,
      resid_type = "studentised",
      progress = FALSE,
      n_boot = 1e2
    )
  })
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("resids conditional log-normal", {
  expect_no_error({
    res <- mackResidBoot(UKMotor,
      cond = TRUE,
      resid_type = "log-normal",
      progress = FALSE,
      n_boot = 1e2
    )
  })
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("resids unconditional standardised", {
  expect_no_error({
    res <- mackResidBoot(UKMotor,
      cond = FALSE,
      resid_type = "standardised",
      progress = FALSE,
      n_boot = 1e2
    )
  })
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("resids unconditional studentised", {
  expect_no_error({
    res <- mackResidBoot(UKMotor,
      cond = FALSE,
      resid_type = "studentised",
      progress = FALSE,
      n_boot = 1e2
    )
  })
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("resids unconditional log-normal", {
  expect_no_error({
    res <- mackResidBoot(UKMotor,
      cond = FALSE,
      resid_type = "log-normal",
      progress = FALSE,
      n_boot = 1e2
    )
  })
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("param conditional normal", {
  expect_no_error({
    res <- mackParamBoot(UKMotor,
      cond = TRUE,
      dist = "normal",
      progress = FALSE,
      n_boot = 1e2
    )
  })
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("param conditional gamma", {
  expect_no_error({
    res <- mackParamBoot(UKMotor,
      cond = TRUE,
      dist = "normal",
      progress = FALSE,
      n_boot = 1e2
    )
  })
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("param unconditional normal", {
  expect_no_error({
    res <- mackParamBoot(UKMotor,
      cond = FALSE,
      dist = "normal",
      progress = FALSE,
      n_boot = 1e2
    )
  })
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("param unconditional gamma", {
  expect_no_error({
    res <- mackParamBoot(UKMotor,
      cond = FALSE,
      dist = "normal",
      progress = FALSE,
      n_boot = 1e2
    )
  })
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("pairs", {
  expect_no_error({
    res <- mackPairsBoot(UKMotor,
      progress = FALSE,
      n_boot = 1e2
    )
  })
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})


cli::test_that_cli("coercing bad triangle results in abort", {
  triangle <- unclass(UKMotor)
  triangle[1, 5] <- 10 * triangle[1, 5]

  expect_snapshot(
    error = TRUE,
    {
      res <- mackParamBoot(triangle,
        cond = TRUE,
        dist = "gamma",
        progress = FALSE,
        n_boot = 1e2
      )
    }
  )

  expect_snapshot(
    error = TRUE,
    {
      res <- mackResidBoot(triangle,
        cond = TRUE,
        resid_type = "standardised",
        progress = FALSE,
        n_boot = 1e2
      )
    }
  )

  expect_snapshot(
    error = TRUE,
    {
      res <- mackPairsBoot(triangle,
        progress = FALSE,
        n_boot = 1e2
      )
    }
  )
})
