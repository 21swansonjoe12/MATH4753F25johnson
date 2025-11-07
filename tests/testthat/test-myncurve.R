test_that("returns proper mu", {
  expect_equal(myncurve(0 ,1, 0.5)$mu, 0)
})
test_that("returns proper sigma", {
  expect_equal(myncurve(9, 15, 8)$sigma, 15)
})
test_that("returns proper a", {
  expect_equal(myncurve(4, 20, 30)$a, 30)
})
