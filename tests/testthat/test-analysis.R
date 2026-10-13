test_that("HH analysis is numerically identical to NCA physical corner 4", {
  dat <- data.frame(
    X = seq(0, 1, length.out = 12),
    Y = seq(0, 1, length.out = 12)
  )

  sca_fit <- sca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh")
  nca_fit <- NCA::nca_analysis(dat, "X", "Y", corner = 4, ceilings = "ce_fdh")

  expect_s3_class(sca_fit, "sca_result")
  expect_equal(sca_extract(sca_fit, "X", "ce_fdh", "empty_corner"), 4L)
  expect_equal(
    sca_extract(sca_fit, "X", "ce_fdh", "Effect size"),
    NCA::nca_extract(nca_fit, "X", "ce_fdh", "Effect size")
  )
})

test_that("all SCA directions reproduce their mapped NCA corner estimates", {
  dat <- data.frame(
    X = c(0.02, 0.12, 0.24, 0.39, 0.43, 0.58, 0.67, 0.79, 0.88, 0.97),
    Y = c(0.08, 0.31, 0.17, 0.46, 0.62, 0.54, 0.83, 0.70, 0.94, 0.87)
  )
  directions <- c("HH", "LH", "HL", "LL")

  for (direction in directions) {
    corner <- sca_corner(direction)
    sca_fit <- sca_analysis(dat, "X", "Y", direction = direction,
                            ceilings = "ce_fdh")
    nca_fit <- NCA::nca_analysis(dat, "X", "Y", corner = corner,
                                 ceilings = "ce_fdh")
    expect_equal(
      sca_extract(sca_fit, "X", "ce_fdh", "Effect size"),
      NCA::nca_extract(nca_fit, "X", "ce_fdh", "Effect size"),
      info = direction
    )
  }
})

test_that("mixed X directions are allowed for a common Y direction", {
  dat <- data.frame(
    X1 = seq(0, 1, length.out = 12),
    X2 = seq(1, 0, length.out = 12),
    Y = seq(0, 1, length.out = 12)
  )
  fit <- sca_analysis(
    dat, c("X1", "X2"), "Y", direction = c("HH", "LH"),
    ceilings = "ce_fdh"
  )
  expect_equal(sca_table(fit)$empty_corner, c(4L, 3L))
})

test_that("high-Y and low-Y statements must be separate models", {
  dat <- data.frame(X1 = 1:5, X2 = 5:1, Y = 1:5)
  expect_error(
    sca_analysis(dat, c("X1", "X2"), "Y", direction = c("HH", "HL")),
    "same outcome direction"
  )
})

test_that("tidy table and threshold rules expose SCA semantics", {
  dat <- data.frame(
    X = seq(0, 1, length.out = 12),
    Y = seq(0, 1, length.out = 12)
  )
  fit <- sca_analysis(
    dat, "X", "Y", direction = "HH", ceilings = "ce_fdh",
    threshold.x = "actual", threshold.y = "actual", steps = 4
  )
  tab <- sca_table(fit)
  thresholds <- sca_thresholds(fit)

  expect_equal(tab$direction, "HH")
  expect_equal(tab$empty_corner, 4L)
  expect_true(all(thresholds$condition_operator == ">"))
  expect_true(all(thresholds$outcome_operator == ">"))
  expect_equal(thresholds$outcome_level_actual, seq(0, 1, length.out = 5))
  expect_true(all(thresholds$status %in%
    c("estimable", "no_threshold", "always_satisfied")))
})

test_that("the deprecated bottleneck arguments still work and warn", {
  dat <- data.frame(
    X = seq(0, 1, length.out = 12),
    Y = seq(0, 1, length.out = 12)
  )
  # Removed in 0.5.0. They were deprecated aliases for threshold.x and
  # threshold.y, and the retirement policy is now that an old name stops
  # working rather than warning and carrying on. Nothing intercepts an argument
  # that is no longer in the signature, so R's own message is what a caller
  # sees; the mapping is documented in sca_legacy_names().
  expect_error(
    sca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh",
                 bottleneck.x = "actual", bottleneck.y = "actual", steps = 4),
    "unused argument"
  )
  fit <- sca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh",
                      threshold.x = "actual", threshold.y = "actual", steps = 4)
  expect_equal(sca_thresholds(fit)$condition_scale[[1L]], "actual")
})

test_that("the outcome column runs with the outcome, not against it", {
  # SCAtools 0.1.0 inherited a reversed display column for H-outcome
  # directions: the engine reverses the actual levels for a flipped corner but
  # leaves the displayed percentages ascending, so 0% carried the highest
  # outcome value. Both must now increase together.
  dat <- data.frame(
    X = seq(0, 1, length.out = 12),
    Y = seq(0, 1, length.out = 12)
  )
  for (direction in c("HH", "LH")) {
    fit <- sca_analysis(
      dat, "X", "Y", direction = direction, ceilings = "ce_fdh",
      threshold.y = "percentage.range", steps = 4
    )
    rules <- sca_thresholds(fit)
    expect_false(is.unsorted(rules$outcome_level), info = direction)
    expect_false(is.unsorted(rules$outcome_level_actual), info = direction)
    expect_gt(
      stats::cor(rules$outcome_level, rules$outcome_level_actual), 0.99
    )
  }
})

test_that("a fitted model can be re-expressed without refitting", {
  dat <- data.frame(
    X = c(0.02, 0.12, 0.24, 0.39, 0.43, 0.58, 0.67, 0.79, 0.88, 0.97),
    Y = c(0.08, 0.31, 0.17, 0.46, 0.62, 0.54, 0.83, 0.70, 0.94, 0.87)
  )
  fit <- sca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh",
                      steps = 4)
  actual <- sca_thresholds(fit, scale = "actual")
  percentile <- sca_thresholds(fit, scale = "percentile")

  # Re-expressing changes the reported numbers but never the underlying fit.
  expect_equal(actual$sufficiency_threshold_actual,
               percentile$sufficiency_threshold_actual)
  expect_equal(actual$condition_scale[[1L]], "actual")
  expect_equal(percentile$condition_scale[[1L]], "percentile")

  # The directional convention flips the condition inequality for a low-level
  # direction; the absolute convention does not.
  low <- sca_analysis(dat, "X", "Y", direction = "LL", ceilings = "ce_fdh",
                      steps = 4)
  expect_true(all(
    sca_thresholds(low, convention = "absolute")$condition_operator == "<"
  ))
  expect_true(all(
    sca_thresholds(low, convention = "directional")$condition_operator == ">"
  ))
})

test_that("low-X high-Y rules use the correct strict directions", {
  dat <- data.frame(
    X = seq(1, 0, length.out = 12),
    Y = seq(0, 1, length.out = 12)
  )
  fit <- sca_analysis(
    dat, "X", "Y", direction = "LH", ceilings = "ce_fdh",
    threshold.x = "actual", threshold.y = "actual", steps = 4
  )
  thresholds <- sca_thresholds(fit)
  expect_true(all(thresholds$condition_operator == "<"))
  expect_true(all(thresholds$outcome_operator == ">"))
  expect_equal(unique(thresholds$empty_corner), 3L)
})

test_that("SCA plot labels the physical corner", {
  dat <- data.frame(
    X = seq(0, 1, length.out = 12),
    Y = seq(0, 1, length.out = 12)
  )
  fit <- sca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh")
  chart <- sca_plot(fit)
  expect_s3_class(chart, "ggplot")
  expect_match(chart$labels$subtitle, "physical empty corner 4")
})

# ---------------------------------------------------------------- 0.4.0 terms

# Collect the conditions an expression signals without letting a warning
# unwind the call, so that a warning followed by an error can both be checked.
conditions_of <- function(expr) {
  warnings <- character(0)
  error <- NA_character_
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      error <<- conditionMessage(e)
      NULL
    }
  )
  list(value = value, warnings = warnings, error = error)
}


test_that("result tables carry the framework vocabulary", {
  dat <- data.frame(
    X = c(0.02, 0.12, 0.24, 0.39, 0.43, 0.58, 0.67, 0.79, 0.88, 0.97),
    Y = c(0.08, 0.31, 0.17, 0.46, 0.62, 0.54, 0.83, 0.70, 0.94, 0.87)
  )
  fit <- sca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh",
                      steps = 4)

  results <- sca_table(fit)
  expect_true(all(c("relationship", "empty_space", "boundary_type") %in%
                    names(results)))
  # HH: the empty corner is lower-right, so the boundary is a floor.
  expect_equal(results$boundary_type[[1L]], "floor")
  expect_equal(results$relationship[[1L]], "Higher X for higher Y")
  expect_equal(results$empty_space[[1L]], "High X with low Y")

  rules <- sca_thresholds(fit)
  expect_true(all(c("relationship", "boundary_type", "sufficiency_threshold",
                    "sufficiency_threshold_actual", "inequality") %in%
                    names(rules)))
  expect_equal(unique(rules$boundary_type), "floor")

  # HL puts the empty corner in the upper half, so the boundary is a ceiling.
  high_low <- sca_analysis(dat, "X", "Y", direction = "HL",
                           ceilings = "ce_fdh", steps = 4)
  expect_equal(sca_table(high_low)$boundary_type[[1L]], "ceiling")
  expect_equal(sca_table(high_low)$empty_space[[1L]], "High X with high Y")
})

test_that("the vocabulary columns name the user's own variables", {
  dat <- data.frame(
    training = seq(0, 1, length.out = 12),
    score = seq(0, 1, length.out = 12)
  )
  fit <- sca_analysis(dat, "training", "score", direction = "HH",
                      ceilings = "ce_fdh")
  results <- sca_table(fit)
  expect_equal(results$relationship[[1L]],
               "Higher training for higher score")
  expect_equal(results$empty_space[[1L]], "High training with low score")
  expect_equal(sca_extract(fit, param = "boundary_type"), "floor")
  expect_equal(sca_extract(fit, param = "empty_space"),
               "High training with low score")
})

test_that("retired names still work and say so", {
  dat <- data.frame(
    X = c(0.02, 0.12, 0.24, 0.39, 0.43, 0.58, 0.67, 0.79, 0.88, 0.97),
    Y = c(0.08, 0.31, 0.17, 0.46, 0.62, 0.54, 0.83, 0.70, 0.94, 0.87)
  )
  fit <- sca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh",
                      steps = 4)

  # legacy = TRUE appends the old names as duplicates of the new ones.
  legacy <- sca_thresholds(fit, legacy = TRUE)
  expect_true(all(c("condition_threshold", "condition_threshold_actual") %in%
                    names(legacy)))
  expect_equal(legacy$condition_threshold, legacy$sufficiency_threshold)
  expect_equal(legacy$condition_threshold_actual,
               legacy$sufficiency_threshold_actual)
  expect_false("condition_threshold" %in% names(sca_thresholds(fit)))

  # The retired 'boundary' argument was removed in 0.5.0; 'inequality' is the
  # only spelling. The two forms still differ, which is what the argument was
  # for.
  strict <- sca_thresholds(fit, inequality = "strict")
  inclusive <- sca_thresholds(fit, inequality = "inclusive")
  expect_false(identical(strict$condition_operator[[1L]],
                         inclusive$condition_operator[[1L]]))
  expect_error(sca_thresholds(fit, boundary = "inclusive"), "unused argument")

  # sca_extract() now refuses a retired name outright and says what replaced
  # it. Until 0.5.0 it promised to accept retired names "with a warning", but
  # all three of them are threshold-table columns, defined per outcome level,
  # so there was no single value to return: the call warned that the name had
  # been renamed and then failed with a different message about threshold
  # columns. One clear error beats a warning followed by an unrelated error.
  extracted <- conditions_of(sca_extract(fit, param = "condition_threshold"))
  expect_length(extracted$warnings, 0L)
  expect_match(extracted$error, "renamed to 'sufficiency_threshold'")
  expect_match(extracted$error, "legacy = TRUE")
})
