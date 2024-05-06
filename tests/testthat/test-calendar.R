test_that("calendar sim plot works with correct output", {
  announce_snapshot_file(name = "sim_calendar.png")
  skip_on_ci()
  skip_on_os("windows")

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
