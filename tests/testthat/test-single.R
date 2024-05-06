test_that("single sim plot works with correct output", {
  announce_snapshot_file(name = "sim_single.png")
  skip_on_ci()
  skip_on_os("windows")

  withr::local_seed(42)
  restore <- getTrnglSeed()
  setTrnglSeed(42)
  res <- mackParamSim(UKMotor, "single",
    cond = TRUE,
    dist = "normal",
    progress = FALSE,
    n_boot = 1e2,
    n_sim = 1e2
  )

  # geom_text_repel converges to different positions on different runs,
  # setting seed doesn't seem to fix it, so just remove the offending
  # layer for the time being
  p <- plot(res)
  p$layers[[2]] <- NULL
  expect_snapshot_file(save_png(p), "sim_single.png")
  setTrnglSeed(restore)
})
