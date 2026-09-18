test_that("normalization delegates to the NCA utility", {
  dat <- data.frame(X = c(0, 5, 10), Y = c(10, 20, 30))
  normalized <- sca_normalize(dat)
  expect_equal(normalized$X, c(0, 0.5, 1))
  expect_equal(normalized$Y, c(0, 0.5, 1))
})

