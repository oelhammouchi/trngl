test_that("setting number of threads works", {
  restore <- getTrnglThreads()
  setTrnglThreads(3)
  expect_equal(3, getTrnglThreads())
  setTrnglThreads(restore)
})

test_that("setting seed works", {
  restore <- getTrnglSeed()
  setTrnglSeed(420)
  expect_equal(420, getTrnglSeed())
  setTrnglSeed(restore)
})
