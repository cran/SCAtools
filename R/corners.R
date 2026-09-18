.sca_direction_to_corner <- c(HH = 4L, LH = 3L, HL = 2L, LL = 1L)
.sca_direction_to_number <- c(HH = 1L, LH = 2L, HL = 3L, LL = 4L)
.sca_corner_location <- c(
  `1` = "upper-left",
  `2` = "upper-right",
  `3` = "lower-left",
  `4` = "lower-right"
)

# Vocabulary of condition analysis in degree, section 3.2 and Table 1.
#
# Five terms are used in sequence: the SCOPE is the region of the X-Y space
# bounded by the theoretical or observed extremes; the EXPECTED EMPTY SPACE
# (or empty zone) is the part of that scope which should contain no
# observations if the hypothesised relation holds; the BOUNDARY is the
# theoretical line separating that space from the compatible region; the
# FRONTIER is the boundary estimated from data; and CEILING and FLOOR name the
# direction in which Y is bounded -- a ceiling limits how high Y can be for a
# given X, a floor how low.
#
# Boundary type therefore follows the empty corner and nothing else. An empty
# corner at the top means the frontier bounds the data from above, which is a
# ceiling; at the bottom it is a floor. For sufficiency this makes the boundary
# a floor whenever the outcome level is high (HH, LH) and a ceiling whenever it
# is low (HL, LL), matching Table 1.
.sca_corner_boundary <- c(
  `1` = "ceiling",
  `2` = "ceiling",
  `3` = "floor",
  `4` = "floor"
)

# What the expected empty space contains, in the wording of Table 1.
.sca_corner_empty_space <- c(
  `1` = "Low X with high Y",
  `2` = "High X with high Y",
  `3` = "Low X with low Y",
  `4` = "High X with low Y"
)

# The direction stated as a relationship between X and Y, as in Table 1. This
# is a claim about the theorised relationship, not about the shape of the
# estimated frontier, which may be a step function or a straight line under any
# of the four types.
.sca_relationship <- c(
  HH = "Higher X for higher Y",
  LH = "Lower X for higher Y",
  HL = "Higher X for lower Y",
  LL = "Lower X for lower Y"
)

# Fill the variable names into the Table 1 wording.
.sca_empty_space_text <- function(corner, x = "X", y = "Y") {
  text <- unname(.sca_corner_empty_space[as.character(corner)])
  text <- sub("X", x, text, fixed = TRUE)
  sub("Y", y, text, fixed = TRUE)
}

.sca_relationship_text <- function(direction, x = "X", y = "Y") {
  text <- unname(.sca_relationship[direction])
  text <- sub("X", x, text, fixed = TRUE)
  sub("Y", y, text, fixed = TRUE)
}

.sca_validate_direction <- function(direction, n = length(direction)) {
  if (is.factor(direction)) {
    direction <- as.character(direction)
  }
  if (!is.character(direction) || length(direction) == 0L || anyNA(direction)) {
    stop("'direction' must contain one or more of: HH, LH, HL, LL.",
         call. = FALSE)
  }

  direction <- toupper(trimws(direction))
  if (length(direction) == 1L && n > 1L) {
    direction <- rep(direction, n)
  }
  if (length(direction) != n) {
    stop("'direction' must have length 1 or the same length as 'x'.",
         call. = FALSE)
  }
  invalid <- setdiff(unique(direction), names(.sca_direction_to_corner))
  if (length(invalid) > 0L) {
    stop(
      sprintf(
        "Invalid direction(s): %s. Use HH, LH, HL, or LL; the first letter refers to X and the second to Y.",
        paste(invalid, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  direction
}

.sca_level <- function(letter) {
  ifelse(letter == "H", "High", "Low")
}

.sca_statement <- function(direction, x = "X", y = "Y") {
  direction <- .sca_validate_direction(direction)
  sprintf(
    "%s %s sufficient for %s %s",
    .sca_level(substr(direction, 1L, 1L)), x,
    .sca_level(substr(direction, 2L, 2L)), y
  )
}

.sca_equivalent_necessity <- function(direction, x = "X", y = "Y") {
  direction <- .sca_validate_direction(direction)
  opposite <- function(z) ifelse(z == "H", "Low", "High")
  sprintf(
    "%s %s necessary for %s %s",
    opposite(substr(direction, 1L, 1L)), x,
    opposite(substr(direction, 2L, 2L)), y
  )
}

#' Convert a sufficiency direction to its physical empty corner
#'
#' The first letter of a direction refers to X and the second to Y. For
#' example, `"HH"` means high X is sufficient for high Y. The returned corner
#' is a geometric location in the original, unflipped scatter plot.
#'
#' @param direction Character vector containing `"HH"`, `"LH"`, `"HL"`, or
#'   `"LL"`.
#' @return An integer vector of physical empty corners.
#' @export
sca_corner <- function(direction = c("HH", "LH", "HL", "LL")) {
  direction <- .sca_validate_direction(direction)
  unname(.sca_direction_to_corner[direction])
}

#' Convert a physical empty corner to its sufficiency direction
#'
#' @param corner Integer vector containing values from 1 through 4. Corner 1
#'   is upper-left, 2 upper-right, 3 lower-left, and 4 lower-right.
#' @return A character vector of sufficiency directions.
#' @export
sca_direction <- function(corner = 1:4) {
  if (!is.numeric(corner) || length(corner) == 0L || anyNA(corner) ||
      any(corner != as.integer(corner)) || any(!corner %in% 1:4)) {
    stop("'corner' must contain integers from 1 through 4.", call. = FALSE)
  }
  unname(c(`1` = "LL", `2` = "HL", `3` = "LH", `4` = "HH")[as.character(corner)])
}

#' Complete mapping between SCA directions and physical empty corners
#'
#' Reproduces the sufficiency half of Table 1 of the condition analysis in
#' degree framework, and adds the corner numbering the frontier engine uses.
#' `relationship` states the direction as a claim about the theorised X-Y
#' relationship; `empty_space` names what the expected empty space would
#' contain; `boundary_type` says whether the boundary separating it is a
#' ceiling or a floor.
#'
#' @return A data frame that distinguishes SCA's logical direction number from
#'   the physical empty corner used by the frontier engine.
#' @export
#' @examples
#' sca_corner_map()
sca_corner_map <- function() {
  direction <- names(.sca_direction_to_corner)
  corner <- unname(.sca_direction_to_corner[direction])
  data.frame(
    sca_number = unname(.sca_direction_to_number[direction]),
    direction = direction,
    relationship = unname(.sca_relationship[direction]),
    x_level = .sca_level(substr(direction, 1L, 1L)),
    y_level = .sca_level(substr(direction, 2L, 2L)),
    sufficiency_statement = .sca_statement(direction),
    empty_space = unname(.sca_corner_empty_space[as.character(corner)]),
    boundary_type = unname(.sca_corner_boundary[as.character(corner)]),
    empty_corner = corner,
    location = unname(.sca_corner_location[as.character(corner)]),
    equivalent_necessity = .sca_equivalent_necessity(direction),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Terminology of condition analysis in degree
#'
#' A machine-readable glossary linking the vocabulary of the condition analysis
#' in degree framework to the names this package uses. Terms retired in 0.4.0
#' are listed with their replacements in [sca_legacy_names()].
#'
#' @return A data frame with one row per term.
#' @seealso [sca_legacy_names()], [sca_corner_map()], [sca_scales()]
#' @export
#' @examples
#' sca_terms()
sca_terms <- function() {
  data.frame(
    term = c(
      "scope", "expected empty space", "boundary", "frontier",
      "ceiling", "floor", "ceiling technique", "effect size",
      "approximate permutation p value", "sufficiency threshold",
      "threshold table", "bottleneck table", "ceiling accuracy",
      "focal unit", "theoretical domain", "central tendency"
    ),
    meaning = c(
      "Region of the X-Y space bounded by the theoretical or observed extremes of X and Y",
      "Part of the scope that should hold no observations if the relation holds",
      "Theoretical line separating the expected empty space from the compatible region",
      "The boundary as estimated from data",
      "A boundary that limits how high Y can be for a given X",
      "A boundary that limits how low Y can be for a given X",
      "Estimator used to fit the frontier, such as CE-FDH or CR-FDH",
      "Empty space as a proportion of the declared scope",
      "Evidence against the null that X and Y are unrelated",
      "Lowest X at which the sufficiency frontier reaches a stated outcome target",
      "Sufficient X for each outcome target",
      "Necessity counterpart of the threshold table; reserved for NCA",
      "Proportion of observations on the compatible side of the frontier",
      "Entity to which the condition claim applies",
      "Set of cases the claim is intended to cover",
      "How the expected outcome moves with the condition; an average effect, not a boundary"
    ),
    package_name = c(
      "scope", "empty_zone_area", "(theoretical; not estimated)", "frontier",
      "boundary_type == \"ceiling\"", "boundary_type == \"floor\"",
      "ceilings", "effect_size", "p_value", "sufficiency_threshold",
      "sca_thresholds()", "(NCA only)", "frontier_accuracy",
      "(declare in reporting)", "(declare in reporting)",
      "reference = \"ols\", sca_reference()"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Columns and arguments retired in 0.4.0, mapped to their replacements.
.sca_legacy_columns <- c(
  condition_threshold = "sufficiency_threshold",
  condition_threshold_actual = "sufficiency_threshold_actual",
  boundary = "inequality"
)

# Columns that belong to sca_thresholds() rather than sca_table(). They are
# defined per outcome level, so sca_extract() cannot return one of them for a
# condition and frontier and says so instead of failing obscurely.
.sca_threshold_columns <- c(
  "convention", "condition_scale", "outcome_scale", "outcome_level",
  "outcome_level_actual", "condition_operator", "sufficiency_threshold",
  "sufficiency_threshold_actual", "outcome_operator", "inequality",
  "status", "estimable", "rule"
)

#' Names retired in SCAtools 0.4.0
#'
#' 0.4.0 adopts the vocabulary of the condition analysis in degree framework.
#' The threshold a sufficiency analysis reports is a *sufficiency threshold*,
#' and *boundary* is reserved for the theoretical line an empty space is
#' separated by, so the argument controlling the strictness of a reported
#' inequality was renamed to `inequality`.
#'
#' Old names remain usable. [sca_table()] and [sca_thresholds()] append them as
#' duplicate columns when called with `legacy = TRUE`, [sca_extract()] accepts
#' them with a warning, and the retired arguments warn but still work.
#'
#' @return A named character vector: names are the retired names, values the
#'   replacements.
#' @seealso [sca_terms()]
#' @export
#' @examples
#' sca_legacy_names()
sca_legacy_names <- function() {
  .sca_legacy_columns
}

