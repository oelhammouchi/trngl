# single ---------------------------------------------

test_that("single resids conditional standardised", {
  res <- mackResidSim(UKMotor, "single",
    cond = TRUE,
    resid_type = "standardised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.single")
})

test_that("single resids conditional studentised", {
  res <- mackResidSim(UKMotor, "single",
    cond = TRUE,
    resid_type = "studentised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.single")
})

test_that("single resids conditional log-normal", {
  res <- mackResidSim(UKMotor, "single",
    cond = TRUE,
    resid_type = "log-normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.single")
})

test_that("single resids unconditional standardised", {
  res <- mackResidSim(UKMotor, "single",
    cond = FALSE,
    resid_type = "standardised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.single")
})

test_that("single resids unconditional studentised", {
  res <- mackResidSim(UKMotor, "single",
    cond = FALSE,
    resid_type = "studentised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.single")
})

test_that("single resids unconditional log-normal", {
  res <- mackResidSim(UKMotor, "single",
    cond = FALSE,
    resid_type = "log-normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.single")
})

test_that("single param conditional normal", {
  res <- mackParamSim(UKMotor, "single",
    cond = TRUE,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.single")
})

test_that("single param conditional gamma", {
  res <- mackParamSim(UKMotor, "single",
    cond = TRUE,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.single")
})

test_that("single param unconditional normal", {
  res <- mackParamSim(UKMotor, "single",
    cond = FALSE,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.single")
})

test_that("single param unconditional gamma", {
  res <- mackParamSim(UKMotor, "single",
    cond = FALSE,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.single")
})

test_that("single pairs", {
  res <- mackPairsSim(UKMotor, "single",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.single")
})

# calendar ---------------------------------------------

test_that("calendar resid conditional standardised", {
  res <- mackResidSim(UKMotor, "calendar",
    cond = TRUE,
    resid_type = "standardised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.calendar")
})

test_that("calendar resid conditional studentised", {
  res <- mackResidSim(UKMotor, "calendar",
    cond = TRUE,
    resid_type = "studentised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.calendar")
})

test_that("calendar resid conditional log-normal", {
  res <- mackResidSim(UKMotor, "calendar",
    cond = TRUE,
    resid_type = "log-normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.calendar")
})

test_that("calendar resid unconditional standardised", {
  res <- mackResidSim(UKMotor, "calendar",
    cond = FALSE,
    resid_type = "standardised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.calendar")
})

test_that("calendar resid unconditional studentised", {
  res <- mackResidSim(UKMotor, "calendar",
    cond = FALSE,
    resid_type = "studentised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.calendar")
})

test_that("calendar resid unconditional log-normal", {
  res <- mackResidSim(UKMotor, "calendar",
    cond = FALSE,
    resid_type = "log-normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.calendar")
})

test_that("calendar param conditional normal", {
  res <- mackParamSim(UKMotor, "calendar",
    cond = TRUE,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.calendar")
})

test_that("calendar param conditional gamma", {
  res <- mackParamSim(UKMotor, "calendar",
    cond = TRUE,
    dist = "gamma",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.calendar")
})

test_that("calendar param unconditional normal", {
  res <- mackParamSim(UKMotor, "calendar",
    cond = FALSE,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.calendar")
})

test_that("calendar param unconditional gamma", {
  res <- mackParamSim(UKMotor, "calendar",
    cond = FALSE,
    dist = "gamma",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.calendar")
})

test_that("calendar pairs", {
  res <- mackPairsSim(UKMotor, "calendar",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.calendar")
})


# origin ---------------------------------------------

test_that("origin resid conditional standardised", {
  res <- mackResidSim(UKMotor, "origin",
    cond = TRUE,
    resid_type = "standardised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.origin")
})

test_that("origin resid conditional studentised", {
  res <- mackResidSim(UKMotor, "origin",
    cond = TRUE,
    resid_type = "studentised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.origin")
})

test_that("origin resid conditional log-normal", {
  res <- mackResidSim(UKMotor, "origin",
    cond = TRUE,
    resid_type = "log-normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.origin")
})

test_that("origin resid unconditional standardised", {
  res <- mackResidSim(UKMotor, "origin",
    cond = FALSE,
    resid_type = "standardised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.origin")
})

test_that("origin resid unconditional studentised", {
  res <- mackResidSim(UKMotor, "origin",
    cond = FALSE,
    resid_type = "studentised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.origin")
})

test_that("origin resid unconditional log-normal", {
  res <- mackResidSim(UKMotor, "origin",
    cond = FALSE,
    resid_type = "log-normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.origin")
})

test_that("origin param conditional normal", {
  res <- mackParamSim(UKMotor, "origin",
    cond = TRUE,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.origin")
})

test_that("origin param conditional gamma", {
  res <- mackParamSim(UKMotor, "origin",
    cond = TRUE,
    dist = "gamma",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.origin")
})

test_that("origin param unconditional normal", {
  res <- mackParamSim(UKMotor, "origin",
    cond = FALSE,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.origin")
})

test_that("origin param unconditional gamma", {
  res <- mackParamSim(UKMotor, "origin",
    cond = FALSE,
    dist = "gamma",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.origin")
})

test_that("origin pairs", {
  res <- mackPairsSim(UKMotor, "origin",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_type(res$reserves, "double")
  expect_true(!any(is.na(res$reserves)))
  expect_s3_class(res, "mack.origin")
})
