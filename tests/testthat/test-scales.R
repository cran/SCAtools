set.seed(11)
x_ref <- round(runif(200, 2, 98), 2)
lo <- min(x_ref)
hi <- max(x_ref)
scales <- c("actual", "percentage.range", "percentage.max", "percentile", "sd")
conventions <- c("absolute", "directional")

test_that("scale and convention names are validated", {
  expect_error(SCAtools:::.sca_validate_scale("percent"), "Invalid")
  expect_error(SCAtools:::.sca_validate_convention("relative"), "Invalid")
  expect_identical(SCAtools:::.sca_validate_scale(" Percentile "), "percentile")
  expect_identical(SCAtools:::.sca_validate_convention("ABSOLUTE"), "absolute")
})

test_that("only low directions are mirrored, and never on the actual scale", {
  expect_false(SCAtools:::.sca_is_mirrored("percentage.range", "H", "directional"))
  expect_true(SCAtools:::.sca_is_mirrored("percentage.range", "L", "directional"))
  expect_false(SCAtools:::.sca_is_mirrored("percentage.range", "L", "absolute"))
  expect_false(SCAtools:::.sca_is_mirrored("actual", "L", "directional"))
})

test_that("the absolute convention gives direction-independent values", {
  for (scale in scales) {
    high <- SCAtools:::.sca_to_scale(60, x_ref, lo, hi, scale, "H", "absolute")
    low <- SCAtools:::.sca_to_scale(60, x_ref, lo, hi, scale, "L", "absolute")
    expect_equal(high, low, info = scale)
  }
})

test_that("the absolute convention reproduces the four classical readings", {
  operators <- function(direction, convention) {
    letters <- c(substr(direction, 1L, 1L), substr(direction, 2L, 2L))
    vapply(
      letters,
      function(letter) {
        SCAtools:::.sca_operator(
          letter,
          SCAtools:::.sca_is_mirrored("percentage.range", letter, convention)
        )
      },
      character(1L)
    )
  }
  expect_equal(unname(operators("HH", "absolute")), c(">", ">"))
  expect_equal(unname(operators("LH", "absolute")), c("<", ">"))
  expect_equal(unname(operators("HL", "absolute")), c(">", "<"))
  expect_equal(unname(operators("LL", "absolute")), c("<", "<"))

  # Under the directional convention every direction reads "more of the
  # sufficient thing", so both operators point the same way.
  for (direction in c("HH", "LH", "HL", "LL")) {
    expect_equal(
      unname(operators(direction, "directional")), c(">", ">"),
      info = direction
    )
  }
})

test_that("mirrored transforms are decreasing and unmirrored ones increasing", {
  grid <- seq(lo, hi, length.out = 40L)
  for (scale in scales) {
    for (convention in conventions) {
      for (letter in c("H", "L")) {
        values <- SCAtools:::.sca_to_scale(
          grid, x_ref, lo, hi, scale, letter, convention
        )
        steps <- diff(values)
        label <- paste(scale, convention, letter)
        if (SCAtools:::.sca_is_mirrored(scale, letter, convention)) {
          expect_true(all(steps <= 1e-12), info = label)
        } else {
          expect_true(all(steps >= -1e-12), info = label)
        }
      }
    }
  }
})

test_that("converting to a scale and back recovers the actual value", {
  values <- c(lo, 25, 50, 73.5, hi)
  for (scale in setdiff(scales, "percentile")) {
    for (convention in conventions) {
      for (letter in c("H", "L")) {
        display <- SCAtools:::.sca_to_scale(
          values, x_ref, lo, hi, scale, letter, convention
        )
        back <- SCAtools:::.sca_from_scale(
          display, x_ref, lo, hi, scale, letter, convention
        )
        expect_equal(back, values, tolerance = 1e-9,
                     info = paste(scale, convention, letter))
      }
    }
  }
})

test_that("rescaling a rule preserves the cases it selects", {
  # This is the property that a wrong operator or a mismatched percentile
  # strictness would break: applying the reported rule to data expressed on
  # the same scale must select exactly the observations that satisfy the rule
  # in actual units. Thresholds include exact observations, to force ties.
  thresholds <- c(lo, 12.34, 40, 60, 77.7, hi, x_ref[7L], x_ref[123L])
  for (inequality in c("strict", "inclusive")) {
    for (scale in scales) {
      for (convention in conventions) {
        for (letter in c("H", "L")) {
          mirrored <- SCAtools:::.sca_is_mirrored(scale, letter, convention)
          operator <- SCAtools:::.sca_operator(letter, mirrored, inequality)
          scaled_data <- SCAtools:::.sca_to_scale(
            x_ref, x_ref, lo, hi, scale, letter, convention, inequality
          )
          for (threshold in thresholds) {
            expected <- if (letter == "H") {
              if (inequality == "inclusive") x_ref >= threshold else x_ref > threshold
            } else {
              if (inequality == "inclusive") x_ref <= threshold else x_ref < threshold
            }
            scaled_threshold <- SCAtools:::.sca_to_scale(
              threshold, x_ref, lo, hi, scale, letter, convention, inequality
            )
            observed <- switch(
              operator,
              ">" = scaled_data > scaled_threshold,
              "<" = scaled_data < scaled_threshold,
              ">=" = scaled_data >= scaled_threshold,
              "<=" = scaled_data <= scaled_threshold
            )
            expect_identical(
              observed, expected,
              info = paste(inequality, scale, convention, letter, threshold)
            )
          }
        }
      }
    }
  }
})

test_that("absolute and directional percentiles are exact complements", {
  for (threshold in c(12.34, 40, 60, x_ref[7L])) {
    absolute <- SCAtools:::.sca_to_scale(
      threshold, x_ref, lo, hi, "percentile", "L", "absolute"
    )
    directional <- SCAtools:::.sca_to_scale(
      threshold, x_ref, lo, hi, "percentile", "L", "directional"
    )
    expect_equal(absolute + directional, 100, tolerance = 1e-9)
  }
})

test_that("outcome levels respect the requested scale", {
  levels_percent <- SCAtools:::.sca_outcome_levels(
    x_ref, lo, hi, "percentage.range", "H", "absolute", steps = 4
  )
  expect_length(levels_percent, 5L)
  expect_equal(levels_percent, seq(lo, hi, length.out = 5L), tolerance = 1e-9)

  # Percentile levels sit at data quantiles, not at evenly spaced values.
  levels_percentile <- SCAtools:::.sca_outcome_levels(
    x_ref, lo, hi, "percentile", "H", "absolute", steps = 4
  )
  expect_equal(
    levels_percentile,
    unname(stats::quantile(x_ref, seq(0, 1, length.out = 5L), names = FALSE)),
    tolerance = 1e-9
  )

  # The grid is always ascending in actual units, whatever the direction, and
  # mirroring must not change the set of levels for a symmetric grid.
  levels_low <- SCAtools:::.sca_outcome_levels(
    x_ref, lo, hi, "percentage.range", "L", "directional", steps = 4
  )
  expect_equal(levels_low, levels_percent, tolerance = 1e-9)
  expect_false(is.unsorted(levels_low))
})

test_that("outcome levels are clamped, deduplicated and guarded", {
  levels <- SCAtools:::.sca_outcome_levels(
    x_ref, lo, hi, "percentage.max", "H", "absolute", steps = 10
  )
  expect_true(all(levels >= lo - 1e-9))
  expect_true(all(levels <= hi + 1e-9))
  expect_false(any(duplicated(levels)))
  expect_error(
    SCAtools:::.sca_outcome_levels(x_ref, 5, 5, "actual", "H", "absolute"),
    "no usable range"
  )
})

test_that("non-finite thresholds convert to NA rather than a number", {
  values <- SCAtools:::.sca_to_scale(
    c(50, Inf, -Inf, NA), x_ref, lo, hi, "percentage.range", "H", "absolute"
  )
  expect_false(is.na(values[[1L]]))
  expect_true(all(is.na(values[2:4])))
})

test_that("sca_scales documents every supported scale", {
  described <- sca_scales()
  expect_setequal(described$scale, scales)
  expect_false(described$mirrored_for_low_directions[described$scale == "actual"])
})
