cli::test_that_cli("progress bar and messages work", {
  expect_snapshot({
    res <- mackParamBoot(UKMotor,
      cond = TRUE,
      dist = "normal",
      progress = TRUE,
      n_boot = 1e2
    )
  })

  expect_snapshot({
    res <- mackResidBoot(UKMotor,
      cond = TRUE,
      resid_type = "standardised",
      progress = TRUE,
      n_boot = 1e2
    )
  })

  expect_snapshot({
    res <- mackPairsBoot(UKMotor,
      progress = TRUE,
      n_boot = 1e2
    )
  })

  expect_snapshot({
    res <- odpParamBoot(UKMotor,
      dist = "normal",
      progress = TRUE,
      n_boot = 1e2
    )
  })

  expect_snapshot({
    res <- odpResidBoot(UKMotor,
      progress = TRUE,
      n_boot = 1e2
    )
  })

  restore <- getTrnglThreads()
  setTrnglThreads(1)

  expect_snapshot({
    res <- mackParamSim(UKMotor, "single",
      cond = TRUE,
      dist = "normal",
      progress = TRUE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })

  expect_snapshot({
    res <- mackResidSim(UKMotor, "single",
      cond = TRUE,
      resid_type = "standardised",
      progress = TRUE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })

  expect_snapshot({
    res <- mackPairsSim(UKMotor, "single",
      progress = TRUE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })

  expect_snapshot({
    res <- odpParamSim(UKMotor, "single",
      dist = "normal",
      progress = TRUE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })

  expect_snapshot({
    res <- odpResidSim(UKMotor, "single",
      progress = TRUE,
      n_boot = 1e2,
      n_sim = 1e2
    )
  })

  setTrnglThreads(restore)
})
