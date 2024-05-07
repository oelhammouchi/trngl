test_that("bootstrap plot works with correct output", {
  announce_snapshot_file(name = "boot_single.png")
  announce_snapshot_file(name = "boot_multiple.png")
  skip_on_ci()
  skip_on_os("windows")
  skip_on_os("mac")

  res <- withr::with_seed(
    42,
    mackResidBoot(UKMotor,
      cond = TRUE,
      resid_type = "standardised",
      progress = FALSE,
      n_boot = 1
    )
  )
  expect_snapshot_file(save_png(plot(res)), "boot_single.png")

  res <- withr::with_seed(
    42,
    mackResidBoot(UKMotor,
      cond = TRUE,
      resid_type = "standardised",
      progress = FALSE,
      n_boot = 1e3
    )
  )
  expect_snapshot_file(save_png(plot(res)), "boot_multiple.png")
})
