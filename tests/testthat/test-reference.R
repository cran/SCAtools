reference_data <- function() {
  data.frame(
    X = c(0.02, 0.12, 0.24, 0.39, 0.43, 0.58, 0.67, 0.79, 0.88, 0.97),
    Y = c(0.08, 0.31, 0.17, 0.46, 0.62, 0.54, 0.83, 0.70, 0.94, 0.87)
  )
}

test_that("the defaults name frontiers only", {
  expect_identical(eval(formals(sca_analysis)$ceilings), c("ce_fdh", "cr_fdh"))
  expect_null(formals(sca_analysis)$reference)
  expect_identical(eval(formals(sca)$ceilings), c("ce_fdh", "cr_fdh"))
})

test_that("ols passed as a ceiling is moved to the reference line", {
  dat <- reference_data()

  expect_warning(
    fit <- sca_analysis(dat, "X", "Y", direction = "HH",
                        ceilings = c("ols", "ce_fdh")),
    "central-tendency"
  )
  # It is not an empty-space estimate, so it is not a row of the tidy table.
  expect_identical(unique(sca_table(fit)$frontier), "ce_fdh")
  expect_true(sca_reference(fit)$drawn)

  # A central-tendency line on its own leaves nothing to estimate.
  expect_warning(
    expect_error(
      sca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ols"),
      "No empty-space frontier"
    ),
    "central-tendency"
  )
})

test_that("the reference line is the ordinary least-squares fit", {
  dat <- reference_data()
  fit <- sca_analysis(dat, "X", "Y", direction = "HH",
                      ceilings = "ce_fdh", reference = "ols")
  line <- sca_reference(fit)
  expected <- stats::coef(stats::lm(Y ~ X, data = dat))

  expect_equal(nrow(line), 1L)
  expect_identical(line$line, "ols")
  expect_identical(line$line_type, "central tendency")
  expect_equal(line$intercept, unname(expected[[1L]]))
  expect_equal(line$slope, unname(expected[[2L]]))
  expect_equal(line$r_squared, stats::cor(dat$X, dat$Y)^2)
  expect_equal(line$observations, nrow(dat))
  expect_true(line$drawn)

  # Still a frontier analysis: the reference line adds no row anywhere.
  expect_identical(unique(sca_table(fit)$frontier), "ce_fdh")
  expect_false("ols" %in% sca_thresholds(fit)$frontier)
})

test_that("the reference line is computable whether or not it was drawn", {
  dat <- reference_data()
  drawn <- sca_reference(
    sca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh",
                 reference = "ols")
  )
  undrawn <- sca_reference(
    sca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh")
  )
  expect_false(undrawn$drawn)
  expect_equal(undrawn$slope, drawn$slope)
  expect_equal(undrawn$intercept, drawn$intercept)
})

test_that("unknown reference lines are refused", {
  dat <- reference_data()
  expect_error(
    sca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh",
                 reference = "loess"),
    "Unknown reference line"
  )
  # "none" is accepted as an explicit way of saying NULL.
  fit <- sca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh",
                      reference = "none")
  expect_false(sca_reference(fit)$drawn)
})

test_that("the glossary names the central-tendency line", {
  terms <- sca_terms()
  expect_true("central tendency" %in% terms$term)
  expect_match(
    terms$package_name[terms$term == "central tendency"],
    "sca_reference"
  )
})
