test_that("setting number of threads works", {
  restore <- getCBThreads()
  setCBThreads(3)
  expect_equal(3, getCBThreads())
  setCBThreads(restore)
})

test_that("setting seed works", {
  restore <- getCBSeed()
  setCBSeed(420)
  expect_equal(420, getCBSeed())
  setCBSeed(restore)
})
