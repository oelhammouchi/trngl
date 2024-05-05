test_that("bootstrap plot works with correct output", {
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

# # You'd then also provide a helper that skips tests where you can't
# # be sure of producing exactly the same output
# expect_snapshot_plot <- function(name, code) {
#   # Other packages might affect results
#   skip_if_not_installed("ggplot2", "2.0.0")
#   # Or maybe the output is different on some operation systems
#   skip_on_os("windows")
#   # You'll need to carefully think about and experiment with these skips

#   name <- paste0(name, ".png")

#   # Announce the file before touching `code`. This way, if `code`
#   # unexpectedly fails or skips, testthat will not auto-delete the
#   # corresponding snapshot file.
#   announce_snapshot_file(name = name)

#   path <- save_png(code)
#   expect_snapshot_file(path, name)
# }
