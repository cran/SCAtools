test_that("random data retain logical and physical direction metadata", {
  set.seed(42)
  dat <- sca_random(20, intercepts = 0, slopes = 1, direction = "HH")
  expect_s3_class(dat, "data.frame")
  expect_equal(attr(dat, "sca_direction"), "HH")
  expect_equal(attr(dat, "empty_corner"), 4L)
  expect_true(all(dat$Y > dat$X))
})

test_that("power plotting validates its input", {
  expect_error(sca_powerplot(data.frame(n = 20)), "Missing power-result")
})

test_that("sca_power() refuses a slope its direction cannot use", {
  # Corners 2 and 3 can only be bounded by a falling line, corners 1 and 4 by a
  # rising one. The engine warns and returns NULL for the wrong sign, which is
  # silent by the time the result reaches sca_powerplot().
  expect_error(sca_power(direction = "LH", slope = 1), "empty corner 3")
  expect_error(sca_power(direction = "HL", slope = 0.5), "empty corner 2")
  expect_error(sca_power(direction = "HH", slope = -1), "empty corner 4")
  expect_error(sca_power(direction = "LL", slope = -0.8), "empty corner 1")

  # One bad value in a vector is enough, and it is named in the message.
  expect_error(sca_power(direction = "HH", slope = c(1, -2)), "-2")

  # A horizontal frontier is accepted for every direction, as by the engine,
  # so validation must not reject it. Checked without running the simulation.
  expect_silent(SCAtools:::.sca_check_slope(0, "LH", 3L))
  expect_silent(SCAtools:::.sca_check_slope(0, "HH", 4L))
  expect_silent(SCAtools:::.sca_check_slope(c(-1, -0.5), "HL", 2L))
  expect_silent(SCAtools:::.sca_check_slope(c(1, 2), "LL", 1L))

  expect_error(SCAtools:::.sca_check_slope("a", "HH", 4L), "finite numbers")
})

