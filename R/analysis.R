.sca_x_count <- function(data, x) {
  if (length(x) == 0L) {
    stop("At least one condition must be selected in 'x'.", call. = FALSE)
  }
  selected <- tryCatch(
    as.data.frame(data)[x],
    error = function(e) {
      stop(conditionMessage(e), call. = FALSE)
    }
  )
  if (ncol(selected) == 0L) {
    stop("At least one condition must be selected in 'x'.", call. = FALSE)
  }
  ncol(selected)
}

.sca_metadata <- function(model) {
  metadata <- attr(model, "sca")
  if (is.null(metadata)) {
    stop("The supplied object is not a valid 'sca_result'.", call. = FALSE)
  }
  metadata
}

.sca_validate_result <- function(model) {
  required <- c("plots", "summaries", "bottlenecks", "peers", "tests")
  if (!is.list(model) || !all(required %in% names(model))) {
    stop(
      "The NCA engine returned an unsupported result structure; SCAtools requires NCA >= 5.0.2.",
      call. = FALSE
    )
  }
  invisible(model)
}

# Coerce a column as the engine does: anything non-numeric becomes NA.
.sca_numeric_column <- function(column) {
  suppressWarnings(as.numeric(as.character(column)))
}

# Reproduce the engine's axis bounds: the observed range, widened by any
# theoretical scope the user supplied. `scope` is c(xmin, xmax, ymin, ymax),
# either one vector for all conditions or a list with one vector per condition.
.sca_scope_bounds <- function(scope, index, offset) {
  if (is.null(scope)) {
    return(c(NA_real_, NA_real_))
  }
  segment <- if (is.list(scope)) {
    if (index <= length(scope)) scope[[index]] else scope[[1L]]
  } else {
    values <- as.numeric(scope)
    if (length(values) == 4L) {
      values
    } else if (length(values) >= index * 4L) {
      values[((index - 1L) * 4L + 1L):(index * 4L)]
    } else {
      return(c(NA_real_, NA_real_))
    }
  }
  segment <- as.numeric(segment)
  if (length(segment) < offset + 2L) {
    return(c(NA_real_, NA_real_))
  }
  segment[c(offset + 1L, offset + 2L)]
}

.sca_axis_bounds <- function(observed, scope_pair) {
  observed <- observed[is.finite(observed)]
  if (length(observed) == 0L) {
    stop("A variable has no usable observations.", call. = FALSE)
  }
  lo <- min(c(observed, scope_pair), na.rm = TRUE)
  hi <- max(c(observed, scope_pair), na.rm = TRUE)
  c(lo, hi)
}

# Per-condition data exactly as the engine sees it: complete pairs only.
.sca_condition_data <- function(data, x_names, y_name, scope) {
  frame <- as.data.frame(data)
  y_raw <- .sca_numeric_column(frame[[y_name]])
  y_bounds <- .sca_axis_bounds(y_raw, .sca_scope_bounds(scope, 1L, 2L))

  conditions <- vector("list", length(x_names))
  names(conditions) <- x_names
  for (i in seq_along(x_names)) {
    x_raw <- .sca_numeric_column(frame[[x_names[[i]]]])
    keep <- is.finite(x_raw) & is.finite(y_raw)
    conditions[[i]] <- list(
      x = x_raw[keep],
      y = y_raw[keep],
      bounds = .sca_axis_bounds(x_raw[keep], .sca_scope_bounds(scope, i, 0L))
    )
  }

  list(
    outcome = y_raw[is.finite(y_raw)],
    outcome_bounds = y_bounds,
    conditions = conditions
  )
}

# Lines that are not empty-space frontiers.
#
# "ols" asks the engine for an ordinary least-squares regression of the outcome
# on the condition, fitted to all the data. It is the line lm(y ~ x) returns: a
# central-tendency summary of how the expected outcome moves with the
# condition. It is not a frontier and it estimates no empty space, so it yields
# no ceiling zone, no effect size, no permutation test and no threshold table.
# Section 4.3 of the condition analysis in degree framework states the
# difference plainly: a frontier is fixed by the most extreme observations
# rather than by the central tendency. Section 6.1 is why the line is still
# worth drawing -- average-effect and condition analysis answer different
# questions about the same relationship, and are most informative shown
# together rather than merged into one number.
.sca_reference_lines <- c("ols")

.sca_validate_reference <- function(reference) {
  if (is.null(reference)) {
    return(character(0L))
  }
  reference <- as.character(reference)
  reference <- unique(tolower(trimws(reference[!is.na(reference)])))
  reference <- setdiff(reference, c("none", ""))
  if (length(reference) == 0L) {
    return(character(0L))
  }
  unknown <- setdiff(reference, .sca_reference_lines)
  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "Unknown reference line(s): %s. Available: %s, or NULL for none.",
        paste(unknown, collapse = ", "),
        paste(.sca_reference_lines, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  reference
}

# Keep the empty-space frontiers and the central-tendency lines apart, wherever
# the caller put them. Before 0.4.1 passing "ols" in 'ceilings' was the only
# way to draw it, so that still works: the line is moved to 'reference' with a
# warning rather than refused.
.sca_split_ceilings <- function(ceilings, reference) {
  if (!is.character(ceilings) || length(ceilings) == 0L) {
    stop("'ceilings' must name at least one frontier technique.", call. = FALSE)
  }
  ceilings <- unique(tolower(trimws(ceilings)))
  moved <- intersect(ceilings, .sca_reference_lines)
  if (length(moved) > 0L) {
    warning(
      sprintf(
        paste0(
          "Moving %s out of 'ceilings': it is a central-tendency line, not an ",
          "empty-space frontier, so it yields no effect size and no threshold ",
          "table. Pass reference = \"%s\" instead."
        ),
        paste(moved, collapse = ", "), moved[[1L]]
      ),
      call. = FALSE
    )
    ceilings <- setdiff(ceilings, moved)
  }
  if (length(ceilings) == 0L) {
    stop(
      paste0(
        "No empty-space frontier technique remains in 'ceilings'. A ",
        "central-tendency line alone estimates no empty space, so there is ",
        "nothing to report as sufficiency in degree."
      ),
      call. = FALSE
    )
  }
  list(ceilings = ceilings, reference = union(reference, moved))
}

.sca_deprecate_bottleneck <- function(value, old, new, supplied) {
  if (is.null(value)) {
    return(NULL)
  }
  .Deprecated(
    new = new,
    package = "SCAtools",
    msg = sprintf(
      paste0(
        "'%s' is deprecated in SCAtools and will be removed in a future ",
        "release; use '%s' instead. Note that SCAtools now applies the scale ",
        "itself rather than passing it to the NCA engine, so values may ",
        "differ from SCAtools 0.1.0 for low-level directions."
      ),
      old, new
    )
  )
  if (supplied) {
    warning(
      sprintf("Both '%s' and '%s' were supplied; using '%s'.", old, new, new),
      call. = FALSE
    )
    return(NULL)
  }
  value
}

#' Sufficiency condition analysis
#'
#' Fits empty-space frontiers for directional sufficiency statements. The
#' logical statement is converted to the physically empty scatter-plot corner
#' by contraposition and passed to [NCA::nca_analysis()].
#'
#' Threshold values are always requested from the engine in actual units and
#' converted to the reporting scale by SCAtools, so that a single, stated
#' convention governs every axis and every direction. See [sca_scales()] for
#' the available scales and [sca_thresholds()] for the resulting table.
#'
#' @param data A data frame or object coercible to a data frame.
#' @param x Columns containing one or more conditions, selected by name or
#'   position.
#' @param y A single outcome column, selected by name or position.
#' @param direction Sufficiency direction(s): `"HH"`, `"LH"`, `"HL"`, or
#'   `"LL"`. The first letter is the X level and the second is the Y level.
#' @param ceilings Empty-space frontier techniques understood by
#'   [NCA::nca_analysis()], such as `"ce_fdh"` and `"cr_fdh"`. `"ols"` is not
#'   one of them; it is moved to `reference` with a warning.
#' @param reference Central-tendency lines drawn beside the frontier for
#'   comparison, or `NULL` for none. `"ols"` is the ordinary least-squares
#'   regression of the outcome on the condition, fitted to all the data. It
#'   estimates no empty space, so it carries no effect size, no permutation
#'   test and no threshold table. See [sca_reference()].
#' @param custom Optional intercept-slope pairs for custom frontier lines.
#' @param scope Optional theoretical scope in the form
#'   `c(xmin, xmax, ymin, ymax)`.
#' @param threshold.x,threshold.y Reporting scales for the condition and
#'   outcome axes of the threshold table. One of `"actual"`,
#'   `"percentage.range"`, `"percentage.max"`, `"percentile"`, or `"sd"`.
#' @param convention `"absolute"` places 0 at the low end of each axis and lets
#'   the inequality carry the direction. `"directional"` places 0 at the least
#'   sufficient end, so low-level directions are mirrored and every rule reads
#'   as "more of the sufficient thing".
#' @param steps Number of threshold steps, or an explicit vector of outcome
#'   levels expressed on the `threshold.y` scale.
#' @param step.size Optional threshold step size on the `threshold.y` scale.
#' @param cutoff How out-of-range threshold values are represented; passed to
#'   [NCA::nca_analysis()].
#' @param qr.tau Quantile used by the quantile-regression frontier.
#' @param test.rep Number of permutation resamples. Zero skips the test.
#' @param test.p_confidence Confidence level for permutation p-value accuracy.
#' @param test.p_threshold Significance threshold shown in test output.
#' @param bottleneck.x,bottleneck.y Deprecated. Former names of `threshold.x`
#'   and `threshold.y`. "Bottleneck" describes a necessity reading and does not
#'   apply to sufficiency statements.
#' @return An object of class `sca_result` containing the NCA engine results and
#'   SCA-specific direction metadata.
#' @export
sca_analysis <- function(
    data,
    x,
    y,
    direction = "HH",
    ceilings = c("ce_fdh", "cr_fdh"),
    reference = NULL,
    custom = NULL,
    scope = NULL,
    threshold.x = "percentage.range",
    threshold.y = "percentage.range",
    convention = c("absolute", "directional"),
    steps = 10,
    step.size = NULL,
    cutoff = 0,
    qr.tau = 0.95,
    test.rep = 0,
    test.p_confidence = 0.95,
    test.p_threshold = 0.05,
    bottleneck.x = NULL,
    bottleneck.y = NULL) {

  legacy_x <- .sca_deprecate_bottleneck(
    bottleneck.x, "bottleneck.x", "threshold.x", !missing(threshold.x)
  )
  legacy_y <- .sca_deprecate_bottleneck(
    bottleneck.y, "bottleneck.y", "threshold.y", !missing(threshold.y)
  )
  if (!is.null(legacy_x)) {
    threshold.x <- legacy_x
  }
  if (!is.null(legacy_y)) {
    threshold.y <- legacy_y
  }

  threshold.x <- .sca_validate_scale(threshold.x, "threshold.x")
  threshold.y <- .sca_validate_scale(threshold.y, "threshold.y")
  convention <- .sca_validate_convention(convention)

  split <- .sca_split_ceilings(ceilings, .sca_validate_reference(reference))
  ceilings <- split$ceilings
  reference <- split$reference

  direction <- .sca_validate_direction(direction, .sca_x_count(data, x))
  y_directions <- unique(substr(direction, 2L, 2L))
  if (length(y_directions) > 1L) {
    stop(
      paste0(
        "All conditions in one analysis must use the same outcome direction. ",
        "Run separate models for high-Y and low-Y sufficiency statements."
      ),
      call. = FALSE
    )
  }
  y_letter <- y_directions[[1L]]

  frame <- as.data.frame(data)
  x_names <- colnames(frame[x])
  y_name <- colnames(frame[y])
  if (length(y_name) != 1L) {
    stop("'y' must select exactly one outcome column.", call. = FALSE)
  }

  raw <- .sca_condition_data(frame, x_names, y_name, scope)

  # Lay the outcome levels out on the requested reporting scale, then hand the
  # engine the resulting actual values. The engine is never asked to apply a
  # scale of its own.
  outcome_levels <- .sca_outcome_levels(
    reference = raw$outcome,
    lo = raw$outcome_bounds[[1L]],
    hi = raw$outcome_bounds[[2L]],
    scale = threshold.y,
    letter = y_letter,
    convention = convention,
    steps = steps,
    step.size = step.size
  )

  empty_corner <- sca_corner(direction)
  engine <- NCA::nca_analysis(
    data = frame,
    x = x,
    y = y,
    # The reference line is requested from the engine so that it appears on the
    # plot beside the frontier. It is kept out of `ceilings` in the metadata,
    # so every table in this package still reports frontiers only.
    ceilings = c(ceilings, reference),
    custom = custom,
    corner = empty_corner,
    scope = scope,
    bottleneck.x = "actual",
    bottleneck.y = "actual",
    steps = outcome_levels,
    step.size = NULL,
    cutoff = cutoff,
    qr.tau = qr.tau,
    effect_aggregation = 1,
    test.rep = test.rep,
    test.p_confidence = test.p_confidence,
    test.p_threshold = test.p_threshold
  )
  .sca_validate_result(engine)

  condition_names <- names(engine$summaries)
  outcome_name <- engine$summaries[[1L]]$names[[2L]]
  names(direction) <- condition_names
  names(empty_corner) <- condition_names
  statements <- vapply(
    seq_along(condition_names),
    function(i) .sca_statement(direction[[i]], condition_names[[i]], outcome_name),
    character(1L)
  )
  names(statements) <- condition_names

  # The engine may reorder or rename columns; align the stored raw data with
  # the names it actually returned.
  raw$conditions <- raw$conditions[
    match(condition_names, names(raw$conditions))
  ]
  names(raw$conditions) <- condition_names

  metadata <- list(
    call = match.call(),
    direction = direction,
    sca_number = stats::setNames(
      unname(.sca_direction_to_number[direction]), condition_names
    ),
    empty_corner = empty_corner,
    statement = statements,
    ceilings = ceilings,
    reference = reference,
    outcome = outcome_name,
    outcome_letter = y_letter,
    threshold.x = threshold.x,
    threshold.y = threshold.y,
    convention = convention,
    cutoff = cutoff,
    outcome_levels = outcome_levels,
    raw = raw,
    engine = "NCA",
    engine_version = as.character(utils::packageVersion("NCA"))
  )

  for (i in seq_along(engine$plots)) {
    condition <- names(engine$plots)[[i]]
    engine$plots[[i]]$sca_direction <- direction[[condition]]
    engine$plots[[i]]$sca_statement <- statements[[condition]]
    engine$plots[[i]]$empty_corner <- empty_corner[[condition]]
  }

  attr(engine, "sca") <- metadata
  attr(engine, "show.plots") <- FALSE
  class(engine) <- unique(c("sca_result", class(engine)))
  engine
}

#' Quick sufficiency condition analysis
#'
#' This convenience wrapper is identical to [sca_analysis()] but marks the
#' result for automatic plotting when it is printed interactively.
#'
#' @inheritParams sca_analysis
#' @return An object of class `sca_result`.
#' @export
sca <- function(
    data,
    x,
    y,
    direction = "HH",
    ceilings = c("ce_fdh", "cr_fdh"),
    reference = NULL) {
  model <- sca_analysis(
    data = data,
    x = x,
    y = y,
    direction = direction,
    ceilings = ceilings,
    reference = reference
  )
  attr(model, "show.plots") <- TRUE
  model
}
