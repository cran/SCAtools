# Direction-aware scale machinery for sufficiency threshold tables.
#
# SCAtools always asks the NCA engine for threshold values in actual units and
# performs every scale conversion here. This keeps the reporting convention
# under SCAtools' control and avoids the engine's own mixed conventions, in
# which percentage scales are measured from the low end of the range while the
# percentile scale is mirrored according to the physical empty corner.
#
# Two conventions are supported.
#
#   absolute     0 always sits at the low end of the axis and 100 at the high
#                end, whatever the sufficiency direction. Direction is carried
#                entirely by the inequality operator. Numbers are comparable
#                across directions.
#
#   directional  0 sits at the least sufficient end and 100 at the most
#                sufficient end. For a low-level direction the axis is
#                mirrored, so the transformation is decreasing and the
#                inequality operator flips. Every direction then reads
#                "more of the sufficient thing".
#
# The `actual` scale is never mirrored: actual units have their own meaning and
# reversing them would produce values that do not exist in the data.

.sca_scale_options <- c(
  "actual", "percentage.range", "percentage.max", "percentile", "sd"
)

.sca_convention_options <- c("absolute", "directional")

.sca_validate_scale <- function(scale, arg = "scale") {
  if (is.null(scale) || !is.character(scale) || length(scale) != 1L ||
      is.na(scale)) {
    stop(sprintf("'%s' must be a single scale name.", arg), call. = FALSE)
  }
  scale <- tolower(trimws(scale))
  if (!(scale %in% .sca_scale_options)) {
    stop(
      sprintf(
        "Invalid '%s': '%s'. Use one of: %s.",
        arg, scale, paste(.sca_scale_options, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  scale
}

.sca_validate_convention <- function(convention) {
  if (is.null(convention) || !is.character(convention) ||
      length(convention) == 0L || is.na(convention[[1L]])) {
    stop("'convention' must be \"absolute\" or \"directional\".", call. = FALSE)
  }
  convention <- tolower(trimws(convention[[1L]]))
  if (!(convention %in% .sca_convention_options)) {
    stop(
      sprintf(
        "Invalid 'convention': '%s'. Use \"absolute\" or \"directional\".",
        convention
      ),
      call. = FALSE
    )
  }
  convention
}

# Is the displayed axis mirrored relative to actual units?
#
# Mirroring happens only under the directional convention, only for a low-level
# direction, and never for the actual scale.
.sca_is_mirrored <- function(scale, letter, convention) {
  identical(convention, "directional") &&
    identical(letter, "L") &&
    !identical(scale, "actual")
}

# A mirrored transformation is decreasing in the actual value, which flips the
# inequality that expresses the sufficiency rule.
.sca_operator <- function(letter, mirrored, inequality = "strict") {
  operator <- if (identical(letter, "H")) ">" else "<"
  if (isTRUE(mirrored)) {
    operator <- if (identical(operator, ">")) "<" else ">"
  }
  if (identical(inequality, "inclusive")) {
    operator <- paste0(operator, "=")
  }
  operator
}

.sca_clean_reference <- function(reference) {
  reference <- as.numeric(reference)
  reference[is.finite(reference)]
}

# Accept the retired 'boundary' argument for one more release cycle. The name
# was retired because Goertz et al. (2013), and section 3.2 of the condition
# analysis in degree framework, use "boundary" for the theoretical line that
# separates an expected empty space from the compatible region. That line is
# what a frontier estimates; it has nothing to do with whether the reported
# rule uses ">" or ">=".
.sca_resolve_inequality <- function(inequality, boundary, supplied) {
  if (!is.null(boundary)) {
    .Deprecated(
      new = "inequality",
      package = "SCAtools",
      msg = paste0(
        "'boundary' is deprecated in SCAtools 0.4.0 and will be removed in a ",
        "future release; use 'inequality' instead. In the condition analysis ",
        "vocabulary a boundary is the theoretical line an empty space is ",
        "separated by, not the strictness of a reported inequality."
      )
    )
    if (isTRUE(supplied)) {
      warning(
        "Both 'boundary' and 'inequality' were supplied; using 'inequality'.",
        call. = FALSE
      )
    } else {
      inequality <- boundary
    }
  }
  match.arg(inequality, c("strict", "inclusive"))
}

#' Convert actual values to a reporting scale
#'
#' @param values Numeric vector in actual units.
#' @param reference Observed values of the same variable, used by the
#'   `percentile` and `sd` scales.
#' @param lo,hi Lower and upper bounds of the axis, including any theoretical
#'   scope supplied to [sca_analysis()].
#' @param scale One of `"actual"`, `"percentage.range"`, `"percentage.max"`,
#'   `"percentile"`, or `"sd"`.
#' @param letter `"H"` or `"L"`, the direction letter for this axis.
#' @param convention `"absolute"` or `"directional"`.
#' @param inequality `"strict"` or `"inclusive"`; only the `percentile` scale
#'   depends on it.
#' @return A numeric vector on the requested scale. Non-finite inputs become
#'   `NA`.
#' @noRd
.sca_to_scale <- function(values, reference, lo, hi, scale, letter,
                          convention, inequality = "strict") {
  values <- as.numeric(values)
  finite <- is.finite(values)
  mirrored <- .sca_is_mirrored(scale, letter, convention)
  out <- rep(NA_real_, length(values))
  if (!any(finite)) {
    return(out)
  }
  v <- values[finite]

  out[finite] <- switch(
    scale,
    actual = v,
    percentage.range = {
      span <- hi - lo
      if (!is.finite(span) || span <= 0) {
        rep(NA_real_, length(v))
      } else if (mirrored) {
        100 * (hi - v) / span
      } else {
        100 * (v - lo) / span
      }
    },
    percentage.max = {
      if (!is.finite(hi) || hi <= 0) {
        rep(NA_real_, length(v))
      } else if (mirrored) {
        100 * (hi - v) / hi
      } else {
        100 * v / hi
      }
    },
    percentile = {
      ref <- .sca_clean_reference(reference)
      if (length(ref) == 0L) {
        rep(NA_real_, length(v))
      } else {
        # A percentile is a step function, so its strictness has to match the
        # inequality the rule uses. Otherwise the observation sitting closest
        # to the threshold shares the threshold's percentile and the rule
        # selects a different set of cases once the data are expressed on the
        # same scale. Each branch below is the one that keeps the rule
        # truth-preserving; see tests/testthat/test-scales.R.
        inclusive <- identical(inequality, "inclusive")
        compare <- if (mirrored) {
          # Low-level direction read from the sufficient end: the operator has
          # already flipped to ">" (or ">="), so count upwards.
          if (inclusive) function(z) mean(ref > z) else function(z) mean(ref >= z)
        } else if (identical(letter, "H")) {
          if (inclusive) function(z) mean(ref < z) else function(z) mean(ref <= z)
        } else {
          if (inclusive) function(z) mean(ref <= z) else function(z) mean(ref < z)
        }
        vapply(v, function(z) 100 * compare(z), numeric(1L))
      }
    },
    sd = {
      ref <- .sca_clean_reference(reference)
      spread <- if (length(ref) > 1L) stats::sd(ref) else NA_real_
      centre <- if (length(ref) > 0L) mean(ref) else NA_real_
      if (!is.finite(spread) || spread <= 0) {
        rep(NA_real_, length(v))
      } else if (mirrored) {
        (centre - v) / spread
      } else {
        (v - centre) / spread
      }
    },
    stop(sprintf("Unsupported scale '%s'.", scale), call. = FALSE)
  )

  out
}

#' Convert displayed values back to actual units
#'
#' Inverse of [.sca_to_scale()]. Used to place outcome levels: a request for
#' levels every 10 percentile points has to become a set of actual outcome
#' values before the engine can be asked for thresholds at those levels.
#'
#' @inheritParams .sca_to_scale
#' @param display Numeric vector on the reporting scale.
#' @return A numeric vector in actual units.
#' @noRd
.sca_from_scale <- function(display, reference, lo, hi, scale, letter,
                            convention) {
  display <- as.numeric(display)
  finite <- is.finite(display)
  mirrored <- .sca_is_mirrored(scale, letter, convention)
  out <- rep(NA_real_, length(display))
  if (!any(finite)) {
    return(out)
  }
  d <- display[finite]

  out[finite] <- switch(
    scale,
    actual = d,
    percentage.range = {
      span <- hi - lo
      if (mirrored) hi - d * span / 100 else lo + d * span / 100
    },
    percentage.max = {
      if (mirrored) hi - d * hi / 100 else d * hi / 100
    },
    percentile = {
      ref <- .sca_clean_reference(reference)
      if (length(ref) == 0L) {
        rep(NA_real_, length(d))
      } else {
        probs <- if (mirrored) 1 - d / 100 else d / 100
        probs[probs < 0] <- 0
        probs[probs > 1] <- 1
        unname(stats::quantile(ref, probs = probs, na.rm = TRUE, names = FALSE))
      }
    },
    sd = {
      ref <- .sca_clean_reference(reference)
      spread <- if (length(ref) > 1L) stats::sd(ref) else NA_real_
      centre <- if (length(ref) > 0L) mean(ref) else NA_real_
      if (!is.finite(spread) || spread <= 0) {
        rep(NA_real_, length(d))
      } else if (mirrored) {
        centre - d * spread
      } else {
        centre + d * spread
      }
    },
    stop(sprintf("Unsupported scale '%s'.", scale), call. = FALSE)
  )

  out
}

# The displayed range of an axis, used to lay out an evenly spaced grid.
.sca_display_range <- function(reference, lo, hi, scale, letter, convention) {
  if (scale %in% c("percentage.range", "percentage.max", "percentile")) {
    return(c(0, 100))
  }
  ends <- .sca_to_scale(
    c(lo, hi), reference, lo, hi, scale, letter, convention
  )
  ends <- ends[is.finite(ends)]
  if (length(ends) < 2L) {
    return(c(NA_real_, NA_real_))
  }
  range(ends)
}

#' Outcome levels for a threshold table, in actual units
#'
#' The grid is laid out on the requested reporting scale and then converted to
#' actual units, so `percentile` places levels at data quantiles while the
#' percentage scales place them at evenly spaced values. Levels are clamped to
#' the axis bounds, deduplicated, and returned in ascending actual order, which
#' is what the engine expects when it receives explicit levels.
#'
#' @inheritParams .sca_to_scale
#' @param steps A single number of intervals, or an explicit vector of levels
#'   on the reporting scale.
#' @param step.size Optional spacing on the reporting scale.
#' @return An ascending numeric vector of actual outcome levels.
#' @noRd
.sca_outcome_levels <- function(reference, lo, hi, scale, letter, convention,
                                steps = 10, step.size = NULL) {
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
    stop("The outcome has no usable range; check for constant or missing data.",
         call. = FALSE)
  }

  if (length(steps) > 1L) {
    display <- as.numeric(steps)
    display <- display[is.finite(display)]
    if (length(display) == 0L) {
      stop("'steps' contains no usable levels.", call. = FALSE)
    }
  } else {
    bounds <- .sca_display_range(reference, lo, hi, scale, letter, convention)
    if (!all(is.finite(bounds))) {
      stop(
        sprintf("The '%s' scale cannot be applied to the outcome.", scale),
        call. = FALSE
      )
    }
    if (!is.null(step.size) && is.finite(step.size) && step.size > 0) {
      display <- seq(bounds[1L], bounds[2L], by = step.size)
      if (abs(display[length(display)] - bounds[2L]) > 1e-9) {
        display <- c(display, bounds[2L])
      }
    } else {
      n <- suppressWarnings(as.integer(steps[[1L]]))
      if (is.na(n) || n < 1L) {
        n <- 10L
      }
      display <- seq(bounds[1L], bounds[2L], length.out = n + 1L)
    }
  }

  values <- .sca_from_scale(
    display, reference, lo, hi, scale, letter, convention
  )
  values <- values[is.finite(values)]
  values[values < lo] <- lo
  values[values > hi] <- hi
  values <- sort(unique(values))

  if (length(values) < 2L) {
    stop(
      "Fewer than two distinct outcome levels remain; widen 'steps' or the scope.",
      call. = FALSE
    )
  }
  values
}

#' Human-readable label for a reporting scale
#' @noRd
.sca_scale_label <- function(scale, convention) {
  base <- switch(
    scale,
    actual = "actual units",
    percentage.range = "% of range",
    percentage.max = "% of maximum",
    percentile = "percentile",
    sd = "SD from mean",
    scale
  )
  if (identical(convention, "directional") && !identical(scale, "actual")) {
    paste0(base, ", from the sufficient end")
  } else {
    base
  }
}

#' Format a value for a threshold rule
#' @noRd
.sca_format_value <- function(value, scale, digits = 6L) {
  if (!is.finite(value)) {
    return(NA_character_)
  }
  formatted <- format(signif(value, digits), trim = TRUE, scientific = FALSE)
  if (scale %in% c("percentage.range", "percentage.max", "percentile")) {
    paste0(formatted, "%")
  } else if (identical(scale, "sd")) {
    paste0(formatted, " SD")
  } else {
    formatted
  }
}

#' Reporting scales available for SCA threshold tables
#'
#' Lists the scales and conventions understood by [sca_analysis()] and
#' [sca_thresholds()], with a note on how each behaves when the sufficiency
#' direction refers to a low level.
#'
#' @return A data frame describing each scale.
#' @export
#' @examples
#' sca_scales()
sca_scales <- function() {
  data.frame(
    scale = .sca_scale_options,
    description = c(
      "Original measurement units",
      "Percentage of the axis range, from minimum to maximum",
      "Percentage of the axis maximum",
      "Share of observations on the stated side of the threshold",
      "Standard deviations from the mean"
    ),
    mirrored_for_low_directions = c(
      FALSE, TRUE, TRUE, TRUE, TRUE
    ),
    note = c(
      "Never mirrored; actual units keep their own meaning",
      "Mirrored value is 100 minus the absolute value",
      "Mirrored value is measured downwards from the maximum",
      "Absolute reports P(X <= t); mirrored reports P(X >= t)",
      "Mirrored value changes sign"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Convert values to an SCA reporting scale
#'
#' The conversion used internally by [sca_thresholds()], exposed so that other
#' packages can express their own quantities on exactly the same scale. This
#' matters whenever two numbers are to be compared: a threshold produced
#' elsewhere is only comparable with an SCA threshold if both were converted by
#' the same rule, including the direction-aware mirroring and the percentile
#' strictness that follows the inequality.
#'
#' @param values Numeric vector in actual units.
#' @param reference Observed values of the same variable, used by the
#'   `percentile` and `sd` scales.
#' @param bounds Length-two axis bounds, including any theoretical scope.
#'   Defaults to the range of `reference`.
#' @param scale One of the names returned by [sca_scales()].
#' @param letter `"H"` or `"L"`, the direction letter for this axis.
#' @param convention `"absolute"` or `"directional"`.
#' @param inequality `"strict"` or `"inclusive"`; affects the `percentile` scale
#'   only. Renamed from `boundary` in 0.4.0.
#' @param boundary Deprecated. Former name of `inequality`.
#' @return A numeric vector on the requested scale. Non-finite inputs return
#'   `NA`.
#' @export
#' @examples
#' x <- c(1, 3, 4, 7, 9)
#' sca_rescale(4, x, scale = "percentile", letter = "H")
#' sca_rescale(4, x, scale = "percentile", letter = "L")
sca_rescale <- function(values, reference, bounds = range(reference, na.rm = TRUE),
                        scale = "percentage.range", letter = c("H", "L"),
                        convention = c("absolute", "directional"),
                        inequality = c("strict", "inclusive"),
                        boundary = NULL) {
  scale <- .sca_validate_scale(scale)
  letter <- match.arg(letter)
  convention <- .sca_validate_convention(convention)
  inequality <- .sca_resolve_inequality(inequality, boundary, !missing(inequality))
  bounds <- as.numeric(bounds)
  if (length(bounds) != 2L || anyNA(bounds)) {
    stop("'bounds' must be two finite values.", call. = FALSE)
  }
  .sca_to_scale(
    values, reference, bounds[[1L]], bounds[[2L]],
    scale, letter, convention, inequality
  )
}
