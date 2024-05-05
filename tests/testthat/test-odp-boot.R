test_that("single resids", {
  expect_no_error({
    res <- odpResidBoot(UKMotor,
      progress = FALSE,
      n_boot = 1e2
    )
  })
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("single param normal", {
  expect_no_error({
    res <- odpParamBoot(UKMotor,
      dist = "normal",
      progress = FALSE,
      n_boot = 1e2
    )
  })
  expect_type(res$reserve, "double")
  expect_true(!any(is.na(res$reserve)))
  expect_s3_class(res, "boot.res")
})

test_that("single param gamma", {
  expect_no_error({
    res <- odpParamBoot(UKMotor,
      dist = "gamma",
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
      res <- odpParamBoot(triangle,
        dist = "gamma",
        progress = FALSE,
        n_boot = 1e2
      )
    }
  )

  expect_snapshot(
    error = TRUE,
    {
      res <- odpResidBoot(triangle,
        progress = FALSE,
        n_boot = 1e2
      )
    }
  )
})
