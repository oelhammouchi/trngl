test_that("as.trngl fails with non-square matrix", {
  expect_snapshot(as.trngl(UKMotor[-1, ]), error = TRUE)
})

test_that("as.trngl fails with non-cumulative triangle", {
  triangle <- ChainLadder::UKMotor
  triangle[1, 5] <- 10 * triangle[1, 5]
  expect_snapshot(as.trngl(triangle), error = TRUE)
})


test_that("subsetting trngl only works if result is non-defective", {
  expect_snapshot(
    {
      UKMotor[1, 3] <- 10 * UKMotor[1, 3]
    },
    error = TRUE
  )

  expect_no_error({
    UKMotor[1, 7] <- 10 * UKMotor[1, 7]
  })
})

test_that("coercing matrix to trngl works", {
  expect_no_error({
    res <- as.trngl(unclass(UKMotor))
  })

  expect_s3_class(res, "trngl")
  expect_identical(attr(UKMotor, "outliers"), list())
})

test_that("trngl pretty-printing works", {
  expect_snapshot(print(UKMotor))
})

test_that("incremental to cumulative conversion works", {
  expect_no_error({
    triangle <- incr2cum(cum2incr(UKMotor))
  })

  expect_s3_class(triangle, "trngl")
})
