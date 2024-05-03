test_that("as.tngl fails with non-square matrix", {
  expect_error(as.tngl(UKMotor[-1, ]))
})
