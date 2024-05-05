test_that("calendar sim plot works with correct output", {
  withr::local_seed(42)
  restore <- getTrnglSeed()
  setTrnglSeed(42)
  res <- mackResidSim(UKMotor, "calendar",
    cond = TRUE,
    resid_type = "standardised",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )
  expect_snapshot_file(save_png(plot(res)), "sim_calendar.png")
  setTrnglSeed(restore)
})
