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
  # The structure is only half the interface. Every number SCAtools reports is
  # fetched from the engine's result by string name, so a renamed parameter
  # would otherwise surface as a silently empty column rather than as an error.
  .sca_require_engine_contract(model)
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

# A pre-specified practical-relevance threshold for the effect size.
# Significance and relevance answer different questions, and a support decision
# that silently collapses them is the classic way to turn a large sample into a
# finding. NSCA has required this of both its components since 0.4.0; until
# 0.5.0 SCAtools had no way to state it, so the sufficiency side of a joint
# model was governed by the package while a standalone SCA was not.
.sca_validate_relevance <- function(relevance) {
  if (is.null(relevance)) {
    return(NA_real_)
  }
  value <- suppressWarnings(as.numeric(relevance))
  if (length(value) != 1L || anyNA(value) || value < 0) {
    stop(
      "'relevance' must be NULL or a single non-negative number.",
      call. = FALSE
    )
  }
  value
}

# Run an expression with a fixed seed and give the caller's random stream back.
#
# The engine draws one offset from the caller's stream and derives its whole
# permutation sequence from it, so seeding here fixes every reported p-value.
# It also calls set.seed(NULL) when it is finished, which reseeds from the
# clock rather than restoring anything -- so without this wrapper a single
# analysis silently replaces the stream the caller was using.
.sca_with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(force(expr))
  }
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    previous <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", previous, envir = globalenv()), add = TRUE)
  } else {
    on.exit(
      suppressWarnings(rm(".Random.seed", envir = globalenv())),
      add = TRUE
    )
  }
  set.seed(seed)
  force(expr)
}

.sca_validate_na_action <- function(na.action) {
  if (is.null(na.action) || !is.character(na.action) || length(na.action) == 0L) {
    stop("'na.action' must be \"pairwise\" or \"listwise\".", call. = FALSE)
  }
  na.action <- tolower(trimws(na.action[[1L]]))
  if (!(na.action %in% c("pairwise", "listwise"))) {
    stop(
      sprintf(
        "Invalid 'na.action': '%s'. Use \"pairwise\" or \"listwise\".",
        na.action
      ),
      call. = FALSE
    )
  }
  na.action
}

# Per-condition data exactly as the engine sees it.
#
# Both axes are bounded by the same rows. Until 0.5.0 the condition axis used
# that condition's complete pairs while the outcome axis used every row with a
# finite outcome, whether or not the condition was observed there -- neither
# pairwise nor listwise deletion but a third thing nobody chose. With missing
# data the two disagreed, and since every effect size is an area over the scope
# box the denominator then came from more rows than the numerator. Complete
# data was never affected, which is why it went unnoticed.
#
# `outcome_bounds` stays shared across conditions on purpose: it lays out the
# outcome levels, and a threshold table is only readable beside another one if
# both were tabulated at the same levels. `bounds` and `y_bounds` are
# per-condition, because they are denominators.
.sca_condition_data <- function(data, x_names, y_name, scope,
                                na.action = "pairwise") {
  frame <- as.data.frame(data)
  y_raw <- .sca_numeric_column(frame[[y_name]])
  x_raw <- lapply(x_names, function(nm) .sca_numeric_column(frame[[nm]]))
  names(x_raw) <- x_names

  usable <- lapply(x_raw, function(column) is.finite(column) & is.finite(y_raw))
  if (identical(na.action, "listwise")) {
    common <- Reduce(`&`, usable)
    usable <- lapply(usable, function(ignored) common)
  }

  # The rows any condition was fitted on. Under listwise this is the common
  # set; under pairwise it is their union, so the level grid still spans every
  # outcome value that contributed to some condition.
  covered <- Reduce(`|`, usable)
  y_scope <- .sca_scope_bounds(scope, 1L, 2L)

  conditions <- vector("list", length(x_names))
  names(conditions) <- x_names
  for (i in seq_along(x_names)) {
    keep <- usable[[i]]
    conditions[[i]] <- list(
      x = x_raw[[i]][keep],
      y = y_raw[keep],
      bounds = .sca_axis_bounds(x_raw[[i]][keep], .sca_scope_bounds(scope, i, 0L)),
      y_bounds = .sca_axis_bounds(y_raw[keep], y_scope)
    )
  }

  list(
    outcome = y_raw[covered],
    outcome_name = y_name,
    outcome_bounds = .sca_axis_bounds(y_raw[covered], y_scope),
    na.action = na.action,
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
.sca_reference_lines <- c("ols", "qr")

# Lines that can only ever be a reference, never a frontier.
#
# `ols` is fitted to all the data and estimates no empty space, so it is not a
# frontier under any setting and a request for it in `ceilings` is a mistake
# worth correcting. `qr` is both: at a high tau it sits at the edge of the
# cloud and bounds an empty space, which is why it stays a valid frontier
# (a theoretical decision, not a tooling one), and at tau = 0.5 it is a median
# line through the middle of it. Only the first belongs in this set -- moving
# `qr` out of `ceilings` would silently delete a frontier the caller asked for.
.sca_reference_only_lines <- c("ols")


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
  unknown <- setdiff(ceilings, .sca_known_ceilings)
  if (length(unknown) > 0L) {
    stop(
      sprintf(
        paste0(
          "Unknown frontier technique(s) in 'ceilings': %s. Available: %s. ",
          "An unrecognised name used to reach the engine, which quietly fell ",
          "back to its own defaults while the recorded settings still named ",
          "what you asked for."
        ),
        paste(sprintf("'%s'", unknown), collapse = ", "),
        paste(.sca_known_ceilings, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  moved <- intersect(ceilings, .sca_reference_only_lines)
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

#' Sufficient condition analysis
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
#' @param reference.tau Quantile for a `"qr"` reference line. Defaults to
#'   `0.5`, the median line: the central-tendency summary that does not move
#'   when one observation does, which is the reason to draw it beside OLS
#'   rather than instead of it.
#'
#'   Separate from `qr.tau`, and deliberately so. A `qr` *frontier* sits at the
#'   edge of the cloud and uses `qr.tau`; a `qr` *reference* sits in the middle
#'   of it and uses this. Both can be asked for in one analysis, which the
#'   engine cannot express -- it takes one `qr.tau` per call -- so reference
#'   lines are computed by this package rather than requested from it.
#' @param qr.tau Quantile used by the quantile-regression frontier, stated as
#'   the share of observations the line is meant to leave on the non-empty
#'   side. It is read that way in both directions: the engine fits `tau` when
#'   the empty space is an upper corner and `1 - tau` when it is a lower one
#'   (corners 3 and 4), so the default 0.95 is the 95th-percentile line where
#'   the empty space lies above and the 5th-percentile line where it lies
#'   below. Sufficiency in the `"HH"` direction has its empty space at high X
#'   with low Y, which is corner 4, and therefore fits the 5th percentile: a
#'   floor, not a ceiling. Do not invert `qr.tau` by hand to compensate.
#'
#'   Two reported quantities behave differently for `qr` than for the other
#'   frontiers, and both follow from the line being placed by a quantile
#'   rather than by the observations at the edge of the cloud.
#'   `frontier_accuracy` counts the share of observations on the non-empty
#'   side, which quantile regression sets to `qr.tau` by construction, so
#'   reading 0.95 back as evidence that the frontier fits well is circular:
#'   it is the input. `fit` compares this frontier's empty area with CE-FDH's
#'   and the engine returns `NA` whenever this area is the larger of the two,
#'   which a quantile line often produces because it is entitled to cut
#'   through observations where an envelope is not. An `NA` fit for `qr` is an
#'   ordinary outcome rather than a failure.

#' @param test.rep Number of permutation resamples. Zero skips the test.
#' @param test.p_confidence Confidence level for permutation p-value accuracy.
#' @param test.p_threshold Significance threshold shown in test output.
#' @param relevance Optional pre-specified practical-relevance threshold for
#'   the effect size. Support requires an effect at least this large as well as
#'   a significant test. `NULL` means no relevance criterion was pre-specified,
#'   and support then rests on significance alone, as
#'   [NSCA::nsca_analysis()] has always required for its two components.
#' @param test.seed Optional seed for the permutation test. The caller's random
#'   stream is restored afterwards, so setting it does not reseed the session.
#'   The engine derives its whole sequence from one draw, so this fixes every
#'   reported p-value.
#' @param na.action How missing values are dropped. `"pairwise"` fits each
#'   condition on its own complete pairs; `"listwise"` drops any row with a
#'   missing value anywhere, so every condition is fitted on the same rows.
#'   Under either, both axes of a condition are bounded by the same rows.
#' @return An object of class `sca_result` containing the NCA engine results and
#'   SCA-specific direction metadata.
#' @export
#' @examples
#' iv <- subset(sca_four_configurations, configuration == "IV")
#' fit <- sca_analysis(iv, x = "X", y = "Y", direction = "HH",
#'                     scope = c(0, 100, 0, 100))
#' fit
#'
#' # Report the condition axis as percentiles instead
#' sca_thresholds(fit, scale = "percentile")
#'
#' # sca_table() shows the same model with a permutation test added.
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
    reference.tau = 0.5,
    test.rep = 0,
    test.p_confidence = 0.95,
    test.p_threshold = 0.05,
    test.seed = NULL,
    relevance = NULL,
    na.action = c("pairwise", "listwise")) {

  test.rep <- .sca_validate_test_rep(test.rep)
  test.p_threshold <- .sca_validate_alpha(test.p_threshold,
                                          "test.p_threshold")
  test.p_confidence <- .sca_validate_alpha(test.p_confidence,
                                           "test.p_confidence")
  threshold.x <- .sca_validate_scale(threshold.x, "threshold.x")
  threshold.y <- .sca_validate_scale(threshold.y, "threshold.y")
  convention <- .sca_validate_convention(convention)
  na.action <- .sca_validate_na_action(na.action)
  relevance <- .sca_validate_relevance(relevance)

  split <- .sca_split_ceilings(ceilings, .sca_validate_reference(reference))
  ceilings <- split$ceilings
  reference <- split$reference

  frame <- .sca_validate_frame(data)
  x_names <- .sca_select_columns(frame, x, "x")
  y_name <- .sca_select_columns(frame, y, "y")
  if (length(y_name) != 1L) {
    stop("'y' must select exactly one outcome column.", call. = FALSE)
  }
  if (length(x_names) == 0L) {
    stop("At least one condition must be selected in 'x'.",
         call. = FALSE)
  }

  direction <- .sca_validate_direction(direction, length(x_names))
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


  # A condition that is also the outcome is a perfect diagonal: the empty
  # corner is half the box whatever the data says, so the fit returns a
  # plausible d of about .5 that means nothing at all.
  if (y_name %in% x_names) {
    stop(
      sprintf(
        paste0(
          "'%s' is named as both a condition and the outcome. A variable ",
          "compared with itself has an empty corner of half the scope ",
          "whatever the data contains, so the effect size would be an ",
          "artefact of the geometry."
        ),
        y_name
      ),
      call. = FALSE
    )
  }

  # Before anything reads the scope: one outcome column may not be given two
  # theoretical ranges. See .sca_validate_scope_y().
  .sca_validate_scope_length(scope, length(x_names))
  .sca_validate_scope_bounds(scope, length(x_names))
  .sca_validate_scope_y(scope, x_names, y_name)

  raw <- .sca_condition_data(frame, x_names, y_name, scope, na.action)
  .sca_require_varying_condition(raw)
  coverage <- .sca_warn_coverage(raw)

  # The engine does its own per-condition complete-case filtering, so
  # `listwise` has to be applied to the frame before it gets there or it
  # changes this package's bookkeeping and nothing the engine reports.
  frame <- .sca_apply_na_action(frame, x_names, y_name, na.action)
  scope_adjustment <- .sca_warn_scope_adjustment(scope, raw)

  # The engine leaves its scope-warning flag behind in the global
  # environment. Remove it afterwards unless it was already there.
  flag_existed <- exists("scope_warnings_nca", envir = globalenv(),
                         inherits = FALSE)
  on.exit(.sca_clear_engine_scope_flag(flag_existed), add = TRUE)

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
  engine <- .sca_with_seed(test.seed, NCA::nca_analysis(
    data = frame,
    x = x,
    y = y,
    # Frontiers only. Reference lines are computed and drawn by this package
    # rather than requested from the engine, for two reasons. The engine takes
    # one `qr.tau` per call, so a `qr` frontier at .95 beside a `qr` reference
    # at .5 is not expressible in one call, and a second engine run for a line
    # that is only ever drawn is waste. `sca_reference()` already computed the
    # OLS line itself with stats::lm(); this makes the two consistent.
    ceilings = ceilings,
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
  ))
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
    reference.tau = reference.tau,
    outcome = outcome_name,
    outcome_letter = y_letter,
    threshold.x = threshold.x,
    threshold.y = threshold.y,
    convention = convention,
    cutoff = cutoff,
    # Needed by anything that refits this model -- sca_scope_sensitivity()
    # would otherwise rebuild a `qr` frontier at the default tau rather than
    # the one that produced the numbers being tested.
    qr.tau = qr.tau,
    outcome_levels = outcome_levels,
    # The decision context, so that sca_table() can report support rather than
    # leaving every reader to re-apply the criteria by hand. NSCA has carried
    # all three since 0.4.0.
    alpha = test.p_threshold,
    test.rep = test.rep,
    test.seed = test.seed,
    relevance = relevance,
    na.action = na.action,
    scope_coverage = coverage,
    # Empty when the declared scope held every observation; one row per
    # bound the engine had to widen otherwise. Reported rather than only
    # warned about, because the widened scope is the denominator of every
    # effect size in the tables below.
    scope_adjustment = scope_adjustment,
    # Both the scope as the caller stated it and the scope the analysis used.
    # Every effect size is an area over the second one, and the two differ
    # whenever the observations fall outside a declared scope, so reporting
    # only the request would describe a denominator that was not used.
    scope_declared = scope,
    scope_used = lapply(raw$conditions, function(item) {
      c(x = item$bounds, y = item$y_bounds)
    }),
    scope_source = if (is.null(scope)) "observed" else "declared",
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

#' Quick sufficient condition analysis
#'
#' This convenience wrapper is identical to [sca_analysis()] but marks the
#' result for automatic plotting when it is printed interactively, and runs a
#' permutation test by default.
#'
#' `test.rep` defaults to zero in [sca_analysis()], where every inferential
#' choice is made deliberately, and to 1000 here, where a quick result should
#' still carry statistical evidence. [NSCA::nsca()] takes the same default for
#' the same reason.
#'
#' It runs that test whether or not you asked for one, so pass `test.seed` to
#' pin the p-value. An unseeded permutation test draws from the clock and
#' reports a different number on every run, which is not a number to put in a
#' paper; the `Settings` block says which of the two you got, and says so again
#' beneath the results when the p-values are unpinned.
#'
#' @inheritParams sca_analysis
#' @return An object of class `sca_result`.
#' @export
#' @examples
#' \donttest{
#' # sca() runs the default 1000 permutations, which takes longer than
#' # an example should; sca_analysis() runs none unless asked.
#' iv <- subset(sca_four_configurations, configuration == "IV")
#' fit <- sca(iv[seq(1, nrow(iv), by = 5), ], x = "X", y = "Y",
#'            direction = "HH", test.seed = 1)
#' fit
#' }
sca <- function(
    data,
    x,
    y,
    direction = "HH",
    ceilings = c("ce_fdh", "cr_fdh"),
    reference = NULL,
    test.rep = 1000,
    test.seed = NULL) {
  model <- sca_analysis(
    data = data,
    x = x,
    y = y,
    direction = direction,
    ceilings = ceilings,
    reference = reference,
    test.rep = test.rep,
    test.seed = test.seed
  )
  # The printed "Settings" block tells the researcher's decisions from the
  # package's by reading the recorded call. sca() forwards every one of its
  # own arguments to sca_analysis() by name, so sca_analysis()'s match.call()
  # reports all of them as supplied even when the caller wrote three. Record
  # sca()'s own call instead, which is also the call the researcher made.
  attr(model, "sca")$call <- match.call()
  attr(model, "show.plots") <- TRUE
  model
}

# How much of the declared scope the observations actually span, per axis.
#
# Ported from NSCA, where it has warned since 0.4.0. SCAtools had no equivalent
# even though it has the same exposure: an effect size is an empty area divided
# by the scope, so as the coverage falls towards zero the number stops
# describing the data and starts describing the scope -- and it does so
# flatteringly, because an axis that does not move leaves most of the box
# empty. Fitting on the observed range cannot trigger this; a declared `scope`
# much wider than the data can.
.sca_degenerate_tolerance <- 1e-8
.sca_thin_coverage <- 0.05

.sca_axis_coverage <- function(values, bounds) {
  span <- suppressWarnings(diff(as.numeric(bounds)))
  values <- values[is.finite(values)]
  if (length(span) != 1L || !is.finite(span) || span <= 0 ||
      length(values) == 0L) {
    return(NA_real_)
  }
  diff(range(values)) / span
}

# Warn once per fit about conditions whose axes barely move inside the scope.
.sca_warn_coverage <- function(raw) {
  conditions <- names(raw$conditions)
  coverage <- vapply(conditions, function(condition) {
    item <- raw$conditions[[condition]]
    min(c(.sca_axis_coverage(item$x, item$bounds),
          .sca_axis_coverage(item$y, item$y_bounds)))
  }, numeric(1L))

  flat <- conditions[!is.finite(coverage) |
                       coverage <= .sca_degenerate_tolerance]
  if (length(flat) > 0L) {
    warning(
      sprintf(
        paste0(
          "%s: the condition or the outcome does not vary inside the declared ",
          "scope, so the reported empty area is a property of the scope rather ",
          "than of the data. The effect size is not interpretable for these ",
          "rows; read the permutation p-value instead."
        ),
        paste(flat, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  thin <- conditions[is.finite(coverage) &
                       coverage > .sca_degenerate_tolerance &
                       coverage < .sca_thin_coverage]
  if (length(thin) > 0L) {
    warning(
      sprintf(
        paste0(
          "%s: the observations span less than %g%% of the declared scope on ",
          "at least one axis, so most of every reported area is unobserved ",
          "scope rather than evidence. Check that this is the intended scope."
        ),
        paste(thin, collapse = ", "), 100 * .sca_thin_coverage
      ),
      call. = FALSE
    )
  }
  invisible(coverage)
}

# Whether the engine widened the declared scope, and where.
#
# Every reported area is divided by the scope area, and the engine decides
# that area for itself: p_loop_data.R builds `scope.theo` as the declared
# scope widened to hold every observation, so a bound that excludes a data
# point is silently replaced by the data point. The denominator of every
# effect size in the analysis therefore changes while the declared scope, the
# thing the researcher wrote down and will report, does not.
#
# The engine does say so, once: "Theorectical scope has been adjusted to
# include all observations". That message names neither the condition nor the
# axis nor either bound, and a flag in `.GlobalEnv` suppresses it after the
# first condition, so an analysis of six conditions in which four were widened
# produces one anonymous sentence. `.sca_scope_adjustment()` reports it per
# condition and per axis, with the declared bound and the one that replaced
# it, so that the widening can be described in a methods section or the scope
# corrected.
.sca_scope_adjustment <- function(scope, raw) {
  conditions <- names(raw$conditions)
  empty <- data.frame(
    condition = character(0L), axis = character(0L),
    bound = character(0L), declared = numeric(0L), used = numeric(0L),
    stringsAsFactors = FALSE
  )
  if (is.null(scope)) {
    return(empty)
  }
  rows <- list()
  for (i in seq_along(conditions)) {
    condition <- conditions[[i]]
    item <- raw$conditions[[condition]]
    axes <- list(
      list(axis = condition,
           declared = .sca_scope_bounds(scope, i, 0L),
           used = item$bounds),
      list(axis = raw$outcome_name,
           declared = .sca_scope_bounds(scope, 1L, 2L),
           used = item$y_bounds)
    )
    for (entry in axes) {
      for (side in 1:2) {
        declared <- entry$declared[[side]]
        used <- entry$used[[side]]
        if (!is.finite(declared) || !is.finite(used) ||
            isTRUE(all.equal(declared, used))) {
          next
        }
        rows[[length(rows) + 1L]] <- data.frame(
          condition = condition,
          axis = entry$axis,
          bound = if (side == 1L) "lower" else "upper",
          declared = declared,
          used = used,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows) == 0L) {
    return(empty)
  }
  do.call(rbind, rows)
}

.sca_warn_scope_adjustment <- function(scope, raw) {
  adjustment <- .sca_scope_adjustment(scope, raw)
  if (nrow(adjustment) == 0L) {
    return(invisible(adjustment))
  }
  described <- sprintf(
    "%s: %s %s bound %s -> %s",
    adjustment$condition, adjustment$axis, adjustment$bound,
    format(adjustment$declared), format(adjustment$used)
  )
  warning(
    sprintf(
      paste0(
        "The declared scope does not contain every observation, so the engine ",
        "widened it. Every effect size is an area over the widened scope, not ",
        "over the scope as declared. Adjusted bounds -- %s. Either report the ",
        "widened scope or exclude the out-of-scope observations deliberately."
      ),
      paste(described, collapse = "; ")
    ),
    call. = FALSE
  )
  invisible(adjustment)
}

# The engine writes a suppression flag into the caller's global environment
# and does not remove it. `p_validate_data()` clears it at the start of the
# next analysis, so it does not silence anything later, but it is still a
# variable the analysis leaves in the user's workspace -- it turns up in ls(),
# in save.image(), and in any code that iterates over globals. Removed here,
# but only when this call is what created it.
.sca_clear_engine_scope_flag <- function(existed) {
  if (existed) {
    return(invisible(NULL))
  }
  if (exists("scope_warnings_nca", envir = globalenv(), inherits = FALSE)) {
    rm("scope_warnings_nca", envir = globalenv())
  }
  invisible(NULL)
}

# Refuse a per-condition scope whose outcome half is not the same in every
# segment.
#
# `scope` may be one c(xmin, xmax, ymin, ymax), a vector of 4 * k values, or a
# list of k such vectors. The first form is copied to every condition and can
# never disagree with itself. The other two exist for one reason: conditions
# measured on different scales need different X ranges. The Y half is not a
# capability, it is a repetition the format forces -- there is only ever one
# outcome column (`p_validate_clean()` in NCA: "Dependent variable can only be
# a single column!"), so writing it k times only creates k chances to write it
# differently.
#
# When they do differ, nothing downstream can represent it:
#
#   the threshold table   is one table, one row per outcome level, one column
#                         per condition. A row cannot mean y = 2 in one column
#                         and y = 20 in the next. NCA resolves this by reading
#                         the Y grid from the first segment only
#                         (`p_low_high()`: `scope[[1]][3]`), computed once
#                         before the per-condition loop.
#
#   the effect sizes      are areas over each condition's own box, because
#                         `p_create_loop_data()` reads `scope[[id.x]]`.
#
# So the table and the effect sizes would be measured against different boxes,
# and in NSCA the area identity that `reconstruction_error` checks would stop
# closing. NCA warns and carries on, which is the right call there: its
# per-condition analyses are independent and the table is the only shared
# thing. Here it is an error instead, because the fix costs nothing. The
# estimation was never joint -- one call with k conditions is a for-loop over
# k independent single-X analyses -- so a condition that genuinely needs a
# different outcome scope loses nothing by being run as its own call. The only
# thing a separate call gives up is the shared threshold table, which is
# exactly the thing that cannot hold two outcome ranges.
.sca_scope_segments <- function(scope, n) {
  if (is.null(scope)) {
    return(NULL)
  }
  if (is.list(scope)) {
    return(lapply(scope, function(segment) suppressWarnings(as.numeric(segment))))
  }
  values <- suppressWarnings(as.numeric(scope))
  # One vector for every condition: replicated, so it cannot disagree.
  if (length(values) == 4L || length(values) < 4L * n) {
    return(NULL)
  }
  lapply(seq_len(n), function(i) values[((i - 1L) * 4L + 1L):(i * 4L)])
}

# Two declared bounds agree when they are equal, or when both are absent. A
# bound given for one condition and left NA for another is the quiet version of
# this mistake: NA means "no theoretical bound, use the observed range", so the
# two conditions end up on different boxes without anyone typing two different
# numbers.
.sca_same_bound <- function(a, b) {
  if (is.na(a) && is.na(b)) {
    return(TRUE)
  }
  if (is.na(a) || is.na(b)) {
    return(FALSE)
  }
  isTRUE(all.equal(a, b))
}

.sca_describe_bound <- function(value) {
  if (is.na(value)) "not declared" else format(value)
}

.sca_validate_scope_y <- function(scope, x_names, y_name) {
  segments <- .sca_scope_segments(scope, length(x_names))
  if (is.null(segments) || length(segments) < 2L) {
    return(invisible(scope))
  }
  first <- segments[[1L]]
  if (length(first) < 4L) {
    return(invisible(scope))
  }

  mismatches <- character(0L)
  for (i in seq_along(segments)[-1L]) {
    segment <- segments[[i]]
    if (length(segment) < 4L) {
      next
    }
    for (side in 3:4) {
      if (.sca_same_bound(first[[side]], segment[[side]])) {
        next
      }
      mismatches <- c(mismatches, sprintf(
        "%s %s: %s (%s says %s)",
        y_name, if (side == 3L) "lower" else "upper",
        .sca_describe_bound(segment[[side]]),
        x_names[[1L]], .sca_describe_bound(first[[side]])
      ))
    }
  }
  if (length(mismatches) == 0L) {
    return(invisible(scope))
  }

  stop(
    sprintf(
      paste0(
        "'scope' gives '%s' more than one theoretical range. %s.\n",
        "\n",
        "There is one outcome column, and the threshold table has one row per ",
        "outcome level and one column per condition, so a row cannot mean one ",
        "value in one column and another in the next. The engine would resolve ",
        "this by taking the level grid from '%s' alone while each effect size ",
        "stayed an area over its own box, leaving the table and the effect ",
        "sizes measured against different rectangles.\n",
        "\n",
        "Two ways to fix it:\n",
        "  * Write the same ymin and ymax in every segment. A per-condition ",
        "scope exists so that conditions on different measurement scales can ",
        "have different X ranges; the outcome half is a repetition the format ",
        "requires, not a setting.\n",
        "  * If a condition genuinely needs a different outcome scope, fit it ",
        "in its own call. Nothing is lost: conditions are estimated ",
        "independently, so one call with several is a loop over several ",
        "single-condition analyses. The only shared result is the threshold ",
        "table, which is what cannot hold two outcome ranges."
      ),
      y_name, paste(mismatches, collapse = "; "), x_names[[1L]]
    ),
    call. = FALSE
  )
}

# Make `na.action` reach the engine, not only this package's bookkeeping.
#
# `.sca_condition_data()` honours both rules, but it only computes the bounds,
# the coverage and the data the plots draw. The effect sizes come from
# `NCA::nca_analysis()`, which drops incomplete pairs per condition inside its
# own loop (`p_create_loop_data()`: `tmp[complete.cases(tmp), ]`). Handing it
# the full frame therefore fitted every condition on its own rows however
# `na.action` was set -- so `"listwise"` changed the denominators without
# changing the numerators, which is the disagreement it exists to prevent.
#
# Filtering is on the selected columns only. A missing value in a column that
# is not in this analysis is not this analysis's business, and dropping its row
# would silently shrink the sample.
.sca_apply_na_action <- function(frame, x_names, y_name, na.action) {
  if (!identical(na.action, "listwise")) {
    return(frame)
  }
  usable <- lapply(
    c(x_names, y_name),
    function(nm) is.finite(.sca_numeric_column(frame[[nm]]))
  )
  keep <- Reduce(`&`, usable)
  if (!any(keep)) {
    stop(
      paste0(
        "No row has every selected variable observed, so listwise deletion ",
        "leaves nothing to fit. Use na.action = \"pairwise\" to fit each ",
        "condition on its own complete pairs."
      ),
      call. = FALSE
    )
  }
  frame[keep, , drop = FALSE]
}

# Refuse an analysis in which no condition varies.
#
# A constant outcome has been refused since 0.4.x -- it leaves the scope with
# no height, so every empty area is measured against zero. A constant
# *condition* does the same thing on the other axis and was not checked, so it
# returned a fit in which `scope_area` was 0, every effect size was NaN and
# every p-value NA. The same degeneracy, refused on one axis and reported as a
# number on the other.
#
# The refusal is only for the case where *every* condition is flat, because
# that is the case where the analysis has nothing to say at all. One flat
# condition among several still leaves the others estimable, and stopping
# would throw those away; those rows keep the coverage warning and their NaN,
# which is the honest answer for that row and not for the call.
.sca_require_varying_condition <- function(raw) {
  spans <- vapply(raw$conditions, function(item) {
    suppressWarnings(diff(as.numeric(item$bounds)))
  }, numeric(1L))
  flat <- !is.finite(spans) | spans <= 0
  if (!all(flat)) {
    return(invisible(raw))
  }
  stop(
    sprintf(
      paste0(
        "No condition varies inside its scope (%s), so every effect size ",
        "would be an area over a scope of zero width and every p-value would ",
        "be undefined. Supply a theoretical 'scope' with distinct xmin and ",
        "xmax if the condition is bounded by design -- the result then depends ",
        "on where the constant sits inside that scope -- or check the data for ",
        "a column that did not vary."
      ),
      paste(names(spans), collapse = ", ")
    ),
    call. = FALSE
  )
}

# The frontier techniques the engine understands.
#
# Hard-coded rather than read from `NCA:::ceilings`, because reaching into
# another package's namespace from package code is exactly what R CMD check
# objects to. The list is pinned against the engine's own by a test instead,
# so a future NCA release that adds or removes a technique breaks the suite
# rather than being silently accepted here.
#
# Until 0.5.0 nothing checked this at all. `ceilings = "banana"` was passed
# straight to the engine, which fell back to its own defaults and fitted
# ce_fdh and cr_fdh -- while the metadata still recorded "banana", so the
# Settings block printed `frontiers  banana` above a table of two techniques
# nobody had asked for. A fit that misreports itself is worse than one that
# refuses.
.sca_known_ceilings <- c(
  "ce_fdh", "ce_vrs", "cr_fdh", "cr_vrs", "cols", "qr", "c_lp", "ols"
)

# A probability, refused outside (0, 1).
#
# test.p_threshold = 2 used to be accepted and stored, so every row came back
# `significant = TRUE` and the Settings block printed `alpha 2` as though it
# were a choice. The threshold decides a reported verdict; it is worth the
# same protection as `relevance`.
.sca_validate_alpha <- function(value, label) {
  v <- suppressWarnings(as.numeric(value))
  if (length(v) != 1L || !is.finite(v) || v <= 0 || v >= 1) {
    stop(sprintf("'%s' must be a single number strictly between 0 and 1.",
                 label), call. = FALSE)
  }
  v
}

# Replications: a whole number, not negative. A negative count was accepted,
# stored, and printed in the Settings block beside a p-value of NA.
.sca_validate_test_rep <- function(value) {
  v <- suppressWarnings(as.numeric(value))
  if (length(v) != 1L || !is.finite(v) || v < 0 || v != round(v)) {
    stop("'test.rep' must be a single non-negative whole number.",
         call. = FALSE)
  }
  as.integer(v)
}

# Column selection that says which argument was wrong.
#
# `frame[x]` raises "undefined columns selected" -- R's own message, with no
# mention of x or y or of the name that was not found. A caller with eight
# conditions then has to bisect to find out which one they misspelled.
.sca_select_columns <- function(frame, selector, label) {
  if (is.numeric(selector)) {
    bad <- selector[selector < 1 | selector > ncol(frame)]
    if (length(bad) > 0L) {
      stop(sprintf("'%s' selects column %s, but the data has %d columns.",
                   label, paste(bad, collapse = ", "), ncol(frame)),
           call. = FALSE)
    }
    return(colnames(frame)[selector])
  }
  selector <- as.character(selector)
  missing <- setdiff(selector, colnames(frame))
  if (length(missing) > 0L) {
    stop(
      sprintf("'%s' names %s, which is not a column of the data. Available: %s.",
              label, paste(sprintf("'%s'", missing), collapse = ", "),
              paste(colnames(frame), collapse = ", ")),
      call. = FALSE
    )
  }
  duplicated_names <- unique(selector[duplicated(selector)])
  if (length(duplicated_names) > 0L) {
    stop(
      sprintf("'%s' names %s more than once.", label,
              paste(sprintf("'%s'", duplicated_names), collapse = ", ")),
      call. = FALSE
    )
  }
  selector
}

.sca_validate_frame <- function(data) {
  if (is.null(data) || (!is.data.frame(data) && !is.matrix(data))) {
    stop("'data' must be a data frame or a matrix.", call. = FALSE)
  }
  frame <- as.data.frame(data)
  if (nrow(frame) == 0L || ncol(frame) == 0L) {
    stop("'data' has no rows or no columns.", call. = FALSE)
  }
  frame
}

# A declared box with no width, or with its bounds the wrong way round.
#
# Both were accepted. The engine repairs them silently -- it widens whatever
# it is given until the data fits -- so the analysis was correct while
# sca_scope() reported `declared_xmin 1, declared_xmax 0`, which is not a
# scope anybody could put in a methods section. Refused rather than repaired,
# for the reason the per-condition outcome scope is refused: every way of
# reaching it is a mistake.
.sca_validate_scope_bounds <- function(scope, n) {
  if (is.null(scope)) {
    return(invisible(NULL))
  }
  segments <- .sca_scope_segments(scope, n)
  if (is.null(segments)) {
    values <- suppressWarnings(as.numeric(scope))
    if (length(values) != 4L) {
      return(invisible(NULL))
    }
    segments <- list(values)
  }
  for (i in seq_along(segments)) {
    s <- suppressWarnings(as.numeric(segments[[i]]))
    if (length(s) != 4L) next
    for (pair in list(c(1L, 2L, "x"), c(3L, 4L, "y"))) {
      lo <- s[[as.integer(pair[[1L]])]]
      hi <- s[[as.integer(pair[[2L]])]]
      if (is.na(lo) || is.na(hi)) next
      if (lo > hi) {
        stop(
          sprintf(
            paste0("'scope' gives the %s axis a minimum of %s above its ",
                   "maximum of %s%s. The engine would silently swap them."),
            pair[[3L]], format(lo), format(hi),
            if (length(segments) > 1L) sprintf(" (condition %d)", i) else ""
          ),
          call. = FALSE
        )
      }
      if (isTRUE(all.equal(lo, hi))) {
        stop(
          sprintf(
            paste0("'scope' gives the %s axis no width (%s to %s)%s, so ",
                   "every area would be measured against zero. The engine ",
                   "would silently widen it back to the observed range."),
            pair[[3L]], format(lo), format(hi),
            if (length(segments) > 1L) sprintf(" for condition %d", i) else ""
          ),
          call. = FALSE
        )
      }
    }
  }
  invisible(NULL)
}

# The shapes a declared scope may take, checked before the engine sees it.
#
# Until 0.5.0 a wrong length reached the engine, whose refusal reads "The
# length of scope needs to be equal to the length of x \n or" -- its own line
# break arriving mid-sentence, and no statement of what the legal lengths
# are. A caller with three conditions could not tell whether four numbers
# were wanted or twelve.
#
# Legal: four numbers for one box shared by every condition; four per
# condition; or a list with one entry per condition (or a single entry
# applied to all).
.sca_validate_scope_length <- function(scope, n) {
  if (is.null(scope)) {
    return(invisible(NULL))
  }
  if (is.list(scope)) {
    widths <- vapply(scope, function(s) length(suppressWarnings(as.numeric(s))),
                     integer(1L))
    if (any(widths != 4L)) {
      stop(
        sprintf(
          paste0("Every entry of a 'scope' list must be four numbers ",
                 "c(xmin, xmax, ymin, ymax); entry %d has %d."),
          which(widths != 4L)[[1L]], widths[widths != 4L][[1L]]
        ),
        call. = FALSE
      )
    }
    if (!(length(scope) %in% c(1L, n))) {
      stop(
        sprintf(
          paste0("'scope' is a list of %d boxes but there %s %d condition%s. ",
                 "Give one box, or one per condition."),
          length(scope), if (n == 1L) "is" else "are", n,
          if (n == 1L) "" else "s"
        ),
        call. = FALSE
      )
    }
    return(invisible(NULL))
  }
  values <- suppressWarnings(as.numeric(scope))
  if (length(values) %in% c(4L, 4L * n)) {
    return(invisible(NULL))
  }
  stop(
    sprintf(
      paste0("'scope' has %d value%s. With %d condition%s it must have 4 ",
             "-- c(xmin, xmax, ymin, ymax), one box for all of them -- or ",
             "%d, four per condition in the order the conditions were ",
             "given."),
      length(values), if (length(values) == 1L) "" else "s",
      n, if (n == 1L) "" else "s", 4L * n
    ),
    call. = FALSE
  )
}
