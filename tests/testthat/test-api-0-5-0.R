# Everything added in 0.5.0 that is not about the scope or the Settings block.

api_data <- function(n = 40, seed = 5) {
  set.seed(seed)
  x <- sort(runif(n))
  data.frame(X = x, Y = pmin(pmax(x + rnorm(n, 0, 0.08), 0), 1))
}


# ---- sca_extract() speaks the table's vocabulary (C5) --------------------

test_that("sca_extract() accepts the names sca_table() prints", {
  # Until 0.5.0 the table said `effect_size` and the extractor wanted
  # "Effect size": one operation, two vocabularies, and the second was the
  # engine's.
  fit <- sca_analysis(api_data(), "X", "Y", ceilings = "ce_fdh")
  tb <- sca_table(fit)

  expect_equal(
    as.numeric(sca_extract(fit, "X", "ce_fdh", "effect_size")),
    tb$effect_size[[1L]]
  )
  expect_equal(
    as.numeric(sca_extract(fit, "X", "ce_fdh", "empty_space_area")),
    tb$empty_space_area[[1L]]
  )
  expect_equal(
    as.numeric(sca_extract(fit, "X", "ce_fdh", "frontier_accuracy")),
    tb$frontier_accuracy[[1L]]
  )
})

test_that("the engine's own names still work", {
  fit <- sca_analysis(api_data(), "X", "Y", ceilings = "ce_fdh")
  expect_equal(
    as.numeric(sca_extract(fit, "X", "ce_fdh", "Effect size")),
    as.numeric(sca_extract(fit, "X", "ce_fdh", "effect_size"))
  )
})

test_that("a threshold-table column is refused with its own message", {
  fit <- sca_analysis(api_data(), "X", "Y", ceilings = "ce_fdh")
  expect_error(
    sca_extract(fit, "X", "ce_fdh", "sufficiency_threshold"),
    "one row per"
  )
})


# ---- na.action -----------------------------------------------------------

test_that("both na.action rules are accepted and recorded", {
  dat <- api_data()
  dat$Z <- dat$X
  dat$X[c(3, 9)] <- NA
  dat$Z[c(4, 11)] <- NA

  pairwise <- sca_analysis(dat, c("X", "Z"), "Y", ceilings = "ce_fdh",
                           na.action = "pairwise")
  listwise <- sca_analysis(dat, c("X", "Z"), "Y", ceilings = "ce_fdh",
                           na.action = "listwise")

  expect_identical(attr(pairwise, "sca")$na.action, "pairwise")
  expect_identical(attr(listwise, "sca")$na.action, "listwise")

  # Listwise drops any row with a missing value anywhere, so every condition
  # is fitted on the same rows and the counts must agree.
  counts <- sca_table(listwise)$observations
  expect_equal(length(unique(counts)), 1L)
})

test_that("an invalid na.action is refused by name", {
  expect_error(
    sca_analysis(api_data(), "X", "Y", ceilings = "ce_fdh",
                 na.action = "complete"),
    "pairwise"
  )
})


# ---- sca_locationshift() -------------------------------------------------

test_that("sca_locationshift() compares the three slopes on one frontier", {
  fit <- sca_analysis(api_data(), "X", "Y", ceilings = "cr_fdh")
  shift <- sca_locationshift(fit, ceiling = "cr_fdh")

  expect_equal(nrow(shift), 1L)
  expect_true(all(c("ols_slope", "median_slope", "frontier_slope",
                    "relative_spread", "tolerance", "location_shift",
                    "reading") %in% names(shift)))
  expect_equal(shift$tolerance, 0.10)
})

test_that("a step frontier has no slope, and that is said rather than NA", {
  # ce_fdh is a step function: reporting a bare NA would read as a failure.
  fit <- sca_analysis(api_data(), "X", "Y", ceilings = "ce_fdh")
  shift <- sca_locationshift(fit, ceiling = "ce_fdh")
  expect_true(is.na(shift$location_shift))
  expect_match(shift$reading, "no slope")
})


# ---- sca_check_direction() ----------------------------------------------

test_that("sca_check_direction() validates and recycles like the fitter does", {
  expect_identical(sca_check_direction("HH", 1L), "HH")
  expect_identical(sca_check_direction("HH", 2L), c("HH", "HH"))
  expect_identical(sca_check_direction(c("HH", "LH"), 2L), c("HH", "LH"))
  expect_error(sca_check_direction("XX", 1L))
})


# ---- the four support columns carry three separate questions -------------

test_that("relevance and significance stay separate questions", {
  dat <- api_data()

  untested <- sca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
  # Nothing was tested, so "not significant" would claim evidence of absence.
  expect_true(is.na(sca_table(untested)$significant[[1L]]))

  strict <- sca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                         test.rep = 30, test.seed = 1, relevance = 0.99)
  row <- sca_table(strict)
  expect_false(row$relevant[[1L]])
  expect_false(row$supported[[1L]])
})


# ---- qr as a reference line (F3) ----------------------------------------

test_that("sca_reference() reports one row per condition and line", {
  fit <- sca_analysis(api_data(), "X", "Y", ceilings = "ce_fdh",
                      reference = c("ols", "qr"))
  ref <- sca_reference(fit)

  expect_equal(nrow(ref), 2L)
  expect_setequal(ref$line, c("ols", "qr"))
  expect_true(all(ref$drawn))
  # The median line by default.
  expect_equal(ref$tau[ref$line == "qr"], 0.5)
  expect_true(is.na(ref$tau[ref$line == "ols"]))
})

test_that("a qr frontier and a qr reference can coexist", {
  # The engine takes one qr.tau per call, so this is exactly what could not be
  # expressed while reference lines were requested from it.
  fit <- sca_analysis(api_data(), "X", "Y",
                      ceilings = c("ce_fdh", "qr"), qr.tau = 0.95,
                      reference = "qr", reference.tau = 0.5)

  expect_true("qr" %in% sca_table(fit)$frontier)
  ref <- sca_reference(fit)
  expect_equal(ref$tau[[1L]], 0.5)
  # Two different lines, not the same one reported twice.
  frontier_slope <- sca_table(fit)$slope[sca_table(fit)$frontier == "qr"]
  expect_false(isTRUE(all.equal(frontier_slope, ref$slope[[1L]])))
})

test_that("qr is still a valid frontier and is not moved to reference", {
  # It is a frontier at a high tau and a central-tendency line at 0.5, so it
  # belongs in both sets; only ols can never be a frontier.
  expect_silent(
    fit <- sca_analysis(api_data(), "X", "Y", ceilings = c("ce_fdh", "qr"))
  )
  expect_true("qr" %in% sca_table(fit)$frontier)
})

test_that("ols in ceilings is still moved out with a warning", {
  expect_warning(
    fit <- sca_analysis(api_data(), "X", "Y", ceilings = c("ce_fdh", "ols")),
    "central-tendency"
  )
  expect_false("ols" %in% sca_table(fit)$frontier)
})

test_that("qr has no R-squared, and that is deliberate", {
  # Quantile regression minimises a different loss, so there is no residual
  # variance to take a share of and a pseudo-R1 is not comparable with an OLS
  # R-squared. An empty column beats one that looks the same and is not.
  fit <- sca_analysis(api_data(), "X", "Y", ceilings = "ce_fdh",
                      reference = "qr")
  ref <- sca_reference(fit)
  expect_true(is.na(ref$r_squared))
  expect_true(is.na(ref$beta))
  # But it gets the measure that does apply, under a name nobody can mistake
  # for R-squared: a share of absolute loss at one quantile, not of variance.
  expect_true(is.finite(ref$pseudo_r1))
  expect_true(ref$pseudo_r1 >= 0 && ref$pseudo_r1 <= 1)
})

test_that("pseudo_r1 is NA for ols, and r_squared is NA for qr", {
  fit <- sca_analysis(api_data(), "X", "Y", ceilings = "ce_fdh",
                      reference = c("ols", "qr"))
  ref <- sca_reference(fit)
  expect_true(is.na(ref$pseudo_r1[ref$line == "ols"]))
  expect_true(is.na(ref$r_squared[ref$line == "qr"]))
  expect_true(is.finite(ref$r_squared[ref$line == "ols"]))
})

test_that("the reference table is available for a model that asked for none", {
  fit <- sca_analysis(api_data(), "X", "Y", ceilings = "ce_fdh")
  ref <- sca_reference(fit)
  expect_equal(nrow(ref), 1L)
  expect_identical(ref$line, "ols")
  expect_false(ref$drawn)
})

test_that("an unknown reference line is refused by name", {
  expect_error(
    sca_analysis(api_data(), "X", "Y", ceilings = "ce_fdh",
                 reference = "loess"),
    "Unknown reference line"
  )
})

test_that("the plot names its unkeyed lines in the caption", {
  fit <- sca_analysis(api_data(), "X", "Y", ceilings = "ce_fdh",
                      reference = c("ols", "qr"), reference.tau = 0.5)
  caption <- sca_plot(fit)$labels$caption
  expect_match(caption, "OLS central tendency")
  expect_match(caption, "tau = 0.5", fixed = TRUE)
})
