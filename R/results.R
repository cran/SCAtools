# Reading a value out of the engine's result.
#
# Three things can go wrong here and they are not the same thing:
#
#   the parameter name is absent   the engine does not report this quantity at
#                                  all, so SCAtools and NCA disagree about the
#                                  interface. That is a version mismatch, not a
#                                  property of the data, and it is raised by
#                                  .sca_require_engine_contract() once per fit
#                                  rather than turned into NA here.
#
#   the frontier column is absent  this technique was not estimated -- `ols`
#                                  never reaches the parameter matrix, for one.
#                                  NA is the right answer.
#
#   the value itself is NA         the engine computed it and it has no value.
#                                  A step frontier has no slope and no
#                                  intercept, and p_nca_ce_fdh.R returns NA for
#                                  both by construction. NA is the right answer
#                                  and must stay quiet, or every default
#                                  analysis would warn twice per row.
.sca_parameter <- function(params, parameter, ceiling) {
  if (!(ceiling %in% colnames(params))) {
    return(NA_real_)
  }
  if (!(parameter %in% rownames(params))) {
    .sca_engine_contract_error(parameter)
  }
  value <- params[[match(parameter, rownames(params)), match(ceiling, colnames(params))]]
  suppressWarnings(as.numeric(value))
}

.sca_global <- function(summary, parameter) {
  if (!(parameter %in% rownames(summary$global))) {
    .sca_engine_contract_error(parameter)
  }
  suppressWarnings(as.numeric(summary$global[match(parameter, rownames(summary$global)), 1L]))
}

# Carries its own condition class so that a caller which tolerates ordinary
# extraction failures -- NSCA wraps every lookup in tryCatch -- can still let
# this one through. An interface mismatch swallowed into NA is the failure this
# whole mechanism exists to prevent.
.sca_engine_contract_error <- function(parameter) {
  stop(structure(
    class = c("sca_engine_contract_error", "error", "condition"),
    list(
      message = sprintf(
        paste0(
          "The NCA engine (version %s) does not report '%s'. This is an engine ",
          "interface mismatch rather than a property of the data: SCAtools reads ",
          "every quantity out of the engine's result by name. Install NCA >= ",
          "5.0.2, or report this if the name has changed in a later release."
        ),
        as.character(utils::packageVersion("NCA")), parameter
      ),
      call = NULL
    )
  ))
}

# Every name SCAtools asks the engine for. Checked once, against the first
# summary the engine returns, so a mismatch is reported at the point of the fit
# with one clear message instead of surfacing as a table full of NA.
.sca_engine_params <- c(
  "Ceiling zone", "Effect size", "Slope", "Intercept",
  "p-value", "p-accuracy", "Fit", "Ceiling accuracy"
)
.sca_engine_globals <- c("Number of observations")

.sca_require_engine_contract <- function(engine) {
  item <- engine$summaries[[1L]]
  # An empty parameter matrix has no row names, and setdiff() against NULL
  # would then report every required name as missing. There is nothing to
  # check until the engine has estimated something.
  if (is.null(item) || is.null(item$params) || ncol(item$params) == 0L) {
    return(invisible(engine))
  }
  missing_params <- setdiff(.sca_engine_params, rownames(item$params))
  missing_globals <- setdiff(.sca_engine_globals, rownames(item$global))
  missing <- c(missing_params, missing_globals)
  if (length(missing) > 0L) {
    .sca_engine_contract_error(paste(missing, collapse = "', '"))
  }
  invisible(engine)
}

.sca_mapping_table <- function(model) {
  metadata <- .sca_metadata(model)
  conditions <- names(metadata$direction)
  data.frame(
    condition = conditions,
    outcome = metadata$outcome,
    direction = unname(metadata$direction[conditions]),
    sca_number = unname(metadata$sca_number[conditions]),
    empty_corner = unname(metadata$empty_corner[conditions]),
    location = unname(.sca_corner_location[
      as.character(metadata$empty_corner[conditions])
    ]),
    boundary_type = unname(.sca_corner_boundary[
      as.character(metadata$empty_corner[conditions])
    ]),
    empty_space = vapply(
      conditions,
      function(condition) .sca_empty_space_text(
        unname(metadata$empty_corner[[condition]]), condition, metadata$outcome
      ),
      character(1L), USE.NAMES = FALSE
    ),
    statement = unname(metadata$statement[conditions]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Tidy SCA results
#'
#' One row per condition and frontier technique.
#'
#' Three columns state the geometry in the vocabulary of the condition analysis
#' in degree framework. `relationship` gives the direction as a claim about the
#' theorised X-Y relationship, as in Table 1 of that framework. `empty_space`
#' names what the expected empty space would contain. `boundary_type` says
#' whether the boundary separating it is a ceiling, which limits how high Y can
#' be for a given X, or a floor, which limits how low. For sufficiency the
#' boundary is a floor whenever the outcome level is high and a ceiling
#' whenever it is low, so the technique names CE-FDH and CR-FDH keep the word
#' "ceiling" even where the fitted boundary is a floor.
#'
#' `scope_area` is the denominator: `effect_size` is `empty_space_area /
#' scope_area` exactly. `scope_adjusted` is `TRUE` where the engine had to
#' widen the declared scope to hold every observation, so that the
#' denominator is not the scope as declared. [sca_scope()] reports both scopes
#' bound by bound.
#'
#' `frontier` names the frontier technique the row was fitted with, such as
#' `"ce_fdh"`; it is the value given in `ceilings`.
#'
#' `empty_zone_area` is an exact copy of `empty_space_area` under the name it
#' had before 0.5.0. It is kept because NSCA 0.4.5 reads it, and will be
#' removed once a release of NSCA that reads `empty_space_area` has replaced
#' that version. New code should use `empty_space_area`.
#'
#' @section Multiple testing:
#' Each condition carries its own permutation test, so a model with `k`
#' conditions produces `k` p-values and a model with several frontier
#' techniques produces `k` of them per technique. None of these is adjusted
#' for multiplicity, and the `Settings` block printed by [print()] says so
#' with the count and the arithmetic: were `k` tests independent, the
#' chance of at least one false positive at `alpha` is
#' \eqn{1 - (1 - \alpha)^k}.
#'
#' Disclosure rather than adjustment, deliberately. Which correction fits
#' depends on what the family is -- whether the conditions were
#' pre-specified, whether one significant condition would be reported as
#' the finding or all of them together, whether the analysis is
#' confirmatory at all. Those are properties of the design, not of the
#' data, and a package that guessed at them would be making the
#' methodological claim on the researcher's behalf. Adjust with
#' [stats::p.adjust()] on the `p_value` column when the family is known.
#'
#' The frontier techniques are a different problem and not a family-wise
#' rate: two techniques are two estimates of one claim. Choosing between
#' them after seeing both p-values is a garden of forking paths, which the
#' `Settings` block also names.
#'
#' @param model An object returned by [sca_analysis()].
#' @param legacy Append the column names retired in 0.4.0 and 0.5.0 as
#'   duplicates. See [sca_legacy_names()].
#' @param p.adjust Multiple-testing correction applied across the conditions
#'   within each frontier technique, as a method name understood by
#'   [stats::p.adjust()]. `"none"`, the default, leaves the table exactly as
#'   it was. Any other method adds `p_value_adjusted` and `p_adjust_method`
#'   and re-derives `significant`, `supported` and `support_status` from the
#'   adjusted values.
#' @return A data frame with one row for every condition-frontier combination.
#' @seealso [sca_thresholds()], [sca_scope()], [sca_corner_map()],
#'   [sca_legacy_names()]
#' @export
#' @examples
#' iv <- subset(sca_four_configurations, configuration == "IV")
#' fit <- sca_analysis(iv, x = "X", y = "Y", direction = "HH",
#'                     scope = c(0, 100, 0, 100))
#' sca_table(fit)
#'
#' # Column names retired in earlier releases, appended as duplicates
#' names(sca_table(fit, legacy = TRUE))
#'
#' # With a permutation test, on every fifth case to keep the example quick.
#' # A report would use 1000 or more permutations.
#' small <- iv[seq(1, nrow(iv), by = 5), ]
#' fit_test <- sca_analysis(small, x = "X", y = "Y", direction = "HH",
#'                          ceilings = "ce_fdh", scope = c(0, 100, 0, 100),
#'                          test.rep = 99, test.seed = 1)
#' sca_table(fit_test)
sca_table <- function(model, legacy = FALSE, p.adjust = "none") {
  metadata <- .sca_metadata(model)
  rows <- list()
  row_id <- 0L

  for (condition in names(model$summaries)) {
    item <- model$summaries[[condition]]
    params <- item$params
    if (ncol(params) == 0L) {
      next
    }
    # Every column here is a frontier, so there is nothing to filter out.
    #
    # Until 0.5.0 this removed anything named in `reference`, because the
    # engine was asked for the reference line and would return a column for
    # it. Reference lines are now computed by this package and never reach the
    # engine, which makes the filter dead -- and worse than dead once `qr`
    # could be both: asking for a `qr` frontier at .95 beside a `qr` reference
    # at .5 deleted the frontier from the table, because its name was in the
    # reference list.
    frontiers <- colnames(params)
    if (length(frontiers) == 0L) {
      next
    }
    for (ceiling in frontiers) {
      row_id <- row_id + 1L
      direction <- unname(metadata$direction[[condition]])
      corner <- unname(metadata$empty_corner[[condition]])
      rows[[row_id]] <- data.frame(
        condition = condition,
        outcome = metadata$outcome,
        direction = direction,
        relationship = .sca_relationship_text(
          direction, condition, metadata$outcome
        ),
        sca_number = unname(metadata$sca_number[[condition]]),
        empty_corner = corner,
        location = unname(.sca_corner_location[as.character(corner)]),
        empty_space = .sca_empty_space_text(
          corner, condition, metadata$outcome
        ),
        boundary_type = unname(.sca_corner_boundary[as.character(corner)]),
        statement = unname(metadata$statement[[condition]]),
        frontier = ceiling,
        observations = .sca_global(item, "Number of observations"),
        # The denominator, beside the numerator it divides. effect_size is
        # empty_space_area / scope_area and nothing else, and printing the two
        # areas makes that checkable rather than a claim the reader has to
        # take from the documentation. scope_adjusted says whether the
        # denominator is the scope as declared or the scope the engine had to
        # widen to hold the data; see [sca_scope()].
        scope_area = .sca_scope_area(metadata, condition),
        scope_adjusted = .sca_scope_is_adjusted(metadata, condition),
        empty_space_area = .sca_parameter(params, "Ceiling zone", ceiling),
        effect_size = .sca_parameter(params, "Effect size", ceiling),
        # What random pairing of the same data produces at this n, scope
        # and technique, and how much of the effect size it does not
        # account for. NA when no permutation test was run: with nothing
        # to compare against, a zero here would be a claim.
        effect_size_null = .sca_null_mean(model, condition, ceiling),
        effect_size_excess = .sca_excess(
          .sca_parameter(params, "Effect size", ceiling),
          .sca_null_mean(model, condition, ceiling)
        ),
        p_value = .sca_parameter(params, "p-value", ceiling),
        p_accuracy = .sca_parameter(params, "p-accuracy", ceiling),
        frontier_accuracy = .sca_as_proportion(
          .sca_parameter(params, "Ceiling accuracy", ceiling)
        ),
        fit = .sca_as_proportion(.sca_parameter(params, "Fit", ceiling)),
        slope = .sca_parameter(params, "Slope", ceiling),
        intercept = .sca_parameter(params, "Intercept", ceiling),
        # The support decision, reported rather than left for the reader to
        # re-derive. `significant` and `relevant` are kept apart on purpose:
        # they answer different questions, and collapsing them is how a large
        # sample turns into a finding. These four mirror the sufficiency side of
        # NSCA's joint table exactly.
        significant = .sca_significant(
          .sca_parameter(params, "p-value", ceiling), metadata
        ),
        relevant = .sca_relevant(
          .sca_parameter(params, "Effect size", ceiling), metadata
        ),
        supported = .sca_supported(
          .sca_parameter(params, "p-value", ceiling),
          .sca_parameter(params, "Effect size", ceiling), metadata
        ),
        support_status = .sca_support_status(
          .sca_parameter(params, "p-value", ceiling),
          .sca_parameter(params, "Effect size", ceiling), metadata
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(
      condition = character(), outcome = character(), direction = character(),
      relationship = character(), sca_number = integer(),
      empty_corner = integer(), location = character(),
      empty_space = character(), boundary_type = character(),
      statement = character(), frontier = character(), observations = numeric(),
      empty_space_area = numeric(), effect_size = numeric(),
      effect_size_null = numeric(), effect_size_excess = numeric(),
      p_value = numeric(),
      p_accuracy = numeric(), frontier_accuracy = numeric(), fit = numeric(),
      slope = numeric(), intercept = numeric(), empty_zone_area = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out <- .sca_apply_p_adjust(out, metadata, p.adjust)
  # NSCA 0.4.5 selects this column from sca_table() under its pre-0.5.0 name.
  # It is kept, always and as an exact copy, until an NSCA release that reads
  # `empty_space_area` has replaced that version; then it goes.
  out$empty_zone_area <- out$empty_space_area
  .sca_append_legacy(out, legacy)
}

# Append retired names as duplicate columns, for scripts written against an
# earlier release. Only the retired names that this table actually carries a
# replacement for are added, so the helper is safe on either table.
.sca_append_legacy <- function(out, legacy) {
  if (!isTRUE(legacy)) {
    return(out)
  }
  for (old in names(.sca_legacy_columns)) {
    new <- .sca_legacy_columns[[old]]
    if (new %in% names(out)) {
      out[[old]] <- out[[new]]
    }
  }
  out
}

#' Extract one value from an SCA result
#'
#' @param model An object returned by [sca_analysis()].
#' @param x Condition name. Defaults to the first condition.
#' @param ceiling Frontier method. Defaults to the first available method.
#' @param param An NCA parameter name such as `"Effect size"`, one of the SCA
#'   metadata fields `"direction"`, `"sca_number"`, `"empty_corner"` and
#'   `"statement"`, one of the geometry fields `"relationship"`,
#'   `"empty_space"` and `"boundary_type"`, or a name retired in 0.4.0.
#' @return The selected value.
#' @seealso [sca_legacy_names()]
#' @export
#' @examples
#' iv <- subset(sca_four_configurations, configuration == "IV")
#' fit <- sca_analysis(iv, x = "X", y = "Y", direction = "HH",
#'                     scope = c(0, 100, 0, 100))
#' sca_extract(fit, param = "Effect size")
#' sca_extract(fit, ceiling = "cr_fdh", param = "Effect size")
#' sca_extract(fit, param = "relationship")
sca_extract <- function(model, x = NULL, ceiling = NULL, param = "Effect size") {
  metadata <- .sca_metadata(model)
  if (is.null(x)) {
    x <- names(model$summaries)[[1L]]
  }
  if (!(x %in% names(model$summaries))) {
    stop(sprintf("Unknown condition '%s'.", x), call. = FALSE)
  }

  if (param %in% names(.sca_legacy_columns)) {
    .sca_retired_stop(param)
  }

  # Threshold-table columns have never been extractable one value at a time:
  # they are defined per outcome level, not per condition and frontier. Say so
  # rather than letting the request fall through to the engine, which would
  # fail with a message about an unknown NCA parameter.
  if (param %in% .sca_threshold_columns) {
    stop(
      sprintf(
        paste0(
          "'%s' is a column of sca_thresholds(), which has one row per ",
          "outcome level rather than one value per condition and frontier. ",
          "Call sca_thresholds() and select the column."
        ),
        param
      ),
      call. = FALSE
    )
  }

  special <- c("direction", "sca_number", "empty_corner", "statement")
  if (param %in% special) {
    return(unname(metadata[[param]][[x]]))
  }

  # Geometry named in the vocabulary of the framework rather than by corner
  # number. Derived rather than stored, so that the corner stays the single
  # source of truth.
  corner <- unname(metadata$empty_corner[[x]])
  if (identical(param, "relationship")) {
    return(.sca_relationship_text(
      unname(metadata$direction[[x]]), x, metadata$outcome
    ))
  }
  if (identical(param, "empty_space")) {
    return(.sca_empty_space_text(corner, x, metadata$outcome))
  }
  if (identical(param, "boundary_type")) {
    return(unname(.sca_corner_boundary[as.character(corner)]))
  }

  # A column of sca_table(), asked for by the name sca_table() prints.
  #
  # Until 0.5.0 this fell straight through to the engine, so the table said
  # `effect_size`, `frontier_accuracy`, `empty_space_area` and the extractor
  # wanted `"Effect size"`, `"Ceiling accuracy"`, `"Ceiling zone"` -- one
  # operation, two vocabularies, and the second one is the engine's, which uses
  # *Ceiling* for a boundary this package spends a section explaining is a
  # floor under half the directions. NSCA's extractor has always looked in its
  # own table first.
  #
  # The engine's own names still work and are still read from the engine, so
  # nothing that used them changes. Where a name exists in both, the package
  # column wins, because that is the one the caller saw.
  results <- sca_table(model)
  if (param %in% names(results)) {
    ceiling <- .sca_resolve_ceiling(model, ceiling, unique(results$frontier))
    row <- results[results$condition == x & results$frontier == ceiling, ,
                   drop = FALSE]
    if (nrow(row) == 0L) {
      stop(
        sprintf("No result for condition '%s' with frontier '%s'.", x, ceiling),
        call. = FALSE
      )
    }
    return(row[[param]][[1L]])
  }

  NCA::nca_extract(model, x = x, ceiling = ceiling, param = param)
}

# Recover the condition thresholds in actual units, together with a status that
# is stated in sufficiency terms.
#
# The engine reports two out-of-range markers whose meaning inverts under
# contraposition. "NN" (not necessary, an infinite requirement in the flipped
# coordinates) means no minimum is required for the necessity reading, which is
# exactly the case in which no attainable condition value guarantees the
# outcome level. "NA" (a requirement outside the scope) means every observation
# already lies on the required side, so the sufficiency claim holds throughout
# the scope. Copying the engine's labels would therefore invert the reader's
# conclusion.
.sca_threshold_column <- function(table, condition, rows) {
  column <- table[[condition]]
  actual <- attr(column, "mpx.actual")
  if (is.null(actual)) {
    actual <- attr(table[condition], "mpx.actual")
  }

  if (!is.null(actual)) {
    actual <- as.numeric(actual)
    if (length(actual) == rows) {
      status <- ifelse(
        is.na(actual), "always_satisfied",
        ifelse(is.infinite(actual), "no_threshold", "estimable")
      )
      actual[!is.finite(actual)] <- NA_real_
      return(list(values = actual, status = status, source = "engine"))
    }
  }

  # The engine dropped the exact values; fall back to its printed output, which
  # is rounded to three decimals.
  text <- trimws(as.character(column))
  text[is.na(text)] <- "NA"
  parsed <- suppressWarnings(as.numeric(text))
  status <- ifelse(
    is.finite(parsed), "estimable",
    ifelse(text == "NN", "no_threshold",
      ifelse(text %in% c("NA", ""), "always_satisfied", "unavailable")
    )
  )
  parsed[!is.finite(parsed)] <- NA_real_
  list(values = parsed, status = status, source = "text")
}

#' Sufficiency threshold table
#'
#' Turns the fitted frontier into directional sufficient-condition rules. A
#' strict `HH` row reads `X > threshold => Y > outcome level`.
#'
#' A *sufficiency threshold* is defined for one outcome target: in a higher X
#' for higher Y relationship it is the lowest X value at which the estimated
#' sufficiency frontier reaches that target. The table is the sufficiency
#' counterpart of an NCA bottleneck table; the format is parallel but the
#' interpretation is not, and the term "bottleneck" is reserved for necessity.
#'
#' Values are stored in actual units and converted here, so a table can be
#' re-expressed on any scale without refitting the model. Under the
#' `"absolute"` convention 0 sits at the low end of each axis and the
#' inequality carries the direction; under `"directional"` the axis is mirrored
#' for a low-level direction, which flips the inequality so that every rule
#' reads as "more of the sufficient thing". See [sca_scales()].
#'
#' The `status` column reports each row in sufficiency terms:
#' `"estimable"` for a threshold inside the scope, `"no_threshold"` when no
#' attainable condition value reaches the outcome level, and
#' `"always_satisfied"` when every observation in scope already lies on the
#' required side.
#'
#' The inequalities describe an empirical empty-space pattern and should not be
#' read as causal guarantees without an appropriate research design.
#'
#' @param model An object returned by [sca_analysis()].
#' @param ceiling A single frontier method that produces a threshold table.
#' @param x Optional condition names or positions.
#' @param scale,outcome_scale Reporting scales for the condition and outcome
#'   axes. Both default to the scales requested in [sca_analysis()].
#' @param convention `"absolute"` or `"directional"`. Defaults to the
#'   convention requested in [sca_analysis()].
#' @param inequality Whether numeric rules use logically exact strict
#'   inequalities (`"strict"`, the default) or an inclusive reporting
#'   convention (`"inclusive"`).
#' @param digits Significant digits used in the formatted rule text.
#' @param legacy Append the column names retired in 0.4.0 and 0.5.0 as
#'   duplicates. See [sca_legacy_names()].
#' @return A long-form data frame of threshold rules.
#' @seealso [sca_table()], [sca_legacy_names()], [sca_scales()]
#' @export
#' @examples
#' iv <- subset(sca_four_configurations, configuration == "IV")
#' fit <- sca_analysis(iv, x = "X", y = "Y", direction = "HH",
#'                     ceilings = "ce_fdh", scope = c(0, 100, 0, 100))
#' sca_thresholds(fit)
#' sca_thresholds(fit, scale = "actual", inequality = "inclusive")
sca_thresholds <- function(
    model,
    ceiling = NULL,
    x = NULL,
    scale = NULL,
    outcome_scale = NULL,
    convention = NULL,
    inequality = c("strict", "inclusive"),
    digits = 6L,
    legacy = FALSE) {
  metadata <- .sca_metadata(model)
  inequality <- match.arg(inequality)

  scale <- .sca_validate_scale(
    if (is.null(scale)) metadata$threshold.x else scale, "scale"
  )
  outcome_scale <- .sca_validate_scale(
    if (is.null(outcome_scale)) metadata$threshold.y else outcome_scale,
    "outcome_scale"
  )
  convention <- .sca_validate_convention(
    if (is.null(convention)) metadata$convention else convention
  )

  ceiling <- .sca_resolve_ceiling(model, ceiling, names(model$bottlenecks))
  if (!(ceiling %in% names(model$bottlenecks))) {
    stop(
      sprintf(
        "No threshold table is available for '%s'. Available methods: %s.",
        ceiling, paste(names(model$bottlenecks), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  table <- model$bottlenecks[[ceiling]]
  condition_names <- colnames(table)[-1L]
  if (!is.null(x)) {
    if (is.numeric(x)) {
      x <- condition_names[x]
    }
    unknown <- setdiff(x, condition_names)
    if (length(unknown) > 0L) {
      stop(sprintf("Unknown condition(s): %s.", paste(unknown, collapse = ", ")),
           call. = FALSE)
    }
    if (length(x) == 0L) {
      stop(
        sprintf(
          "'x' selected no conditions. Leave it NULL for all of them, or choose from: %s.",
          paste(condition_names, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    condition_names <- x
  }

  # The engine was asked for actual units, so the first column holds the
  # outcome levels themselves.
  outcome_actual <- as.numeric(table[[1L]])
  rows_n <- length(outcome_actual)
  y_letter <- metadata$outcome_letter
  y_reference <- metadata$raw$outcome
  y_bounds <- metadata$raw$outcome_bounds

  outcome_level <- .sca_to_scale(
    outcome_actual, y_reference, y_bounds[[1L]], y_bounds[[2L]],
    outcome_scale, y_letter, convention, inequality
  )
  y_mirrored <- .sca_is_mirrored(outcome_scale, y_letter, convention)
  y_operator <- .sca_operator(y_letter, y_mirrored, inequality)

  degraded <- character(0L)
  rows <- vector("list", length(condition_names))

  for (i in seq_along(condition_names)) {
    condition <- condition_names[[i]]
    direction <- unname(metadata$direction[[condition]])
    x_letter <- substr(direction, 1L, 1L)
    x_mirrored <- .sca_is_mirrored(scale, x_letter, convention)
    x_operator <- .sca_operator(x_letter, x_mirrored, inequality)

    column <- .sca_threshold_column(table, condition, rows_n)
    if (identical(column$source, "text")) {
      degraded <- c(degraded, condition)
    }

    x_reference <- metadata$raw$conditions[[condition]]$x
    x_bounds <- metadata$raw$conditions[[condition]]$bounds
    threshold <- .sca_to_scale(
      column$values, x_reference, x_bounds[[1L]], x_bounds[[2L]],
      scale, x_letter, convention, inequality
    )

    status <- column$status
    estimable <- status == "estimable" & is.finite(threshold) &
      is.finite(outcome_level)
    status[status == "estimable" & !estimable] <- "unavailable"

    outcome_text <- vapply(
      outcome_level,
      function(v) .sca_format_value(v, outcome_scale, digits),
      character(1L)
    )
    threshold_text <- vapply(
      threshold,
      function(v) .sca_format_value(v, scale, digits),
      character(1L)
    )

    rule <- rep(NA_character_, rows_n)
    rule[estimable] <- sprintf(
      "%s %s %s => %s %s %s",
      condition, x_operator, threshold_text[estimable],
      metadata$outcome, y_operator, outcome_text[estimable]
    )
    # These describe the condition side only. Saying that the outcome itself
    # holds for every case would be a stronger claim than the row supports:
    # an observation sitting exactly on the frontier can still fail it.
    no_threshold <- status == "no_threshold"
    rule[no_threshold] <- sprintf(
      "unattainable within scope: no %s threshold gives %s %s %s",
      condition, metadata$outcome, y_operator, outcome_text[no_threshold]
    )
    always <- status == "always_satisfied"
    rule[always] <- sprintf(
      "unrestrictive within scope: %s %s %s needs no %s threshold",
      metadata$outcome, y_operator, outcome_text[always], condition
    )

    rows[[i]] <- data.frame(
      condition = condition,
      outcome = metadata$outcome,
      direction = direction,
      relationship = .sca_relationship_text(
        direction, condition, metadata$outcome
      ),
      sca_number = unname(metadata$sca_number[[condition]]),
      empty_corner = unname(metadata$empty_corner[[condition]]),
      boundary_type = unname(.sca_corner_boundary[
        as.character(metadata$empty_corner[[condition]])
      ]),
      frontier = ceiling,
      convention = convention,
      condition_scale = scale,
      outcome_scale = outcome_scale,
      outcome_level = outcome_level,
      outcome_level_actual = outcome_actual,
      condition_operator = x_operator,
      sufficiency_threshold = threshold,
      sufficiency_threshold_actual = column$values,
      outcome_operator = y_operator,
      inequality = inequality,
      status = status,
      estimable = estimable,
      rule = rule,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  if (length(degraded) > 0L) {
    warning(
      sprintf(
        paste0(
          "The frontier engine did not preserve exact threshold values for %s; ",
          "SCAtools fell back to its printed output, which is rounded to three ",
          "decimals. Rescale the affected variable if that precision is not enough."
        ),
        paste(unique(degraded), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  out <- do.call(rbind, rows)
  out$.condition_order <- match(out$condition, condition_names)
  out <- out[
    order(out$.condition_order, out$outcome_level, na.last = TRUE), ,
    drop = FALSE
  ]
  out$.condition_order <- NULL
  rownames(out) <- NULL
  .sca_append_legacy(out, legacy)
}

# One central-tendency line for one condition, on one technique.
#
# Both are computed here rather than asked of the engine. `ols` always was,
# with stats::lm(); `qr` has to be, because the engine takes one `qr.tau` per
# call and a `qr` frontier at .95 beside a `qr` reference at .5 cannot be
# expressed in one. Computing both the same way also means the numbers in this
# table and the lines on the plot come from the same fit, which was not
# guaranteed while one came from the engine and the other from here.
.sca_reference_fit <- function(line, xv, yv, tau) {
  empty <- list(intercept = NA_real_, slope = NA_real_, slope_se = NA_real_,
                slope_p = NA_real_, beta = NA_real_, r_squared = NA_real_,
                pseudo_r1 = NA_real_)
  if (length(xv) < 2L || !(stats::var(xv) > 0)) {
    return(empty)
  }

  if (identical(line, "ols")) {
    fitted <- stats::lm(yv ~ xv)
    coefficients <- stats::coef(fitted)
    out <- empty
    out$intercept <- unname(coefficients[[1L]])
    out$slope <- unname(coefficients[[2L]])
    # The fitted object already carries the standard error and the t test.
    estimates <- suppressWarnings(stats::coef(summary(fitted)))
    if (nrow(estimates) >= 2L) {
      out$slope_se <- unname(estimates[2L, 2L])
      out$slope_p <- unname(estimates[2L, 4L])
    }
    if (stats::var(yv) > 0) {
      correlation <- suppressWarnings(stats::cor(xv, yv))
      # For a simple regression R-squared is the squared correlation and the
      # standardised coefficient is the correlation, which is the scale
      # Cohen's conventions are stated on.
      out$r_squared <- correlation^2
      out$beta <- correlation
    }
    return(out)
  }

  # Quantile regression. At tau = 0.5 this is the median line: the
  # central-tendency summary that does not move when one observation does,
  # which is the reason to want it beside OLS rather than instead of it.
  #
  # No r_squared and no beta, and a `pseudo_r1` of its own instead.
  #
  # Quantile regression minimises a different loss, so there is no residual
  # variance to take a share of. Putting Koenker and Machado's R1 in the
  # `r_squared` column would be one column carrying two meanings, which is the
  # failure this package spends effort avoiding elsewhere; leaving it out
  # entirely would report a limitation as a missing value. A separate column
  # under a name nobody can mistake for R-squared says what it is: the right
  # measure for this line, not a version of the other one.
  #
  # R1 = 1 - V1/V0, where V is the sum of the check-function losses under the
  # fitted model and under the intercept-only quantile. It runs from 0 to 1 and
  # is not an OLS R-squared.
  fitted <- tryCatch(
    quantreg::rq(yv ~ xv, tau = tau),
    error = function(e) NULL
  )
  if (is.null(fitted)) {
    return(empty)
  }
  coefficients <- stats::coef(fitted)
  out <- empty
  out$intercept <- unname(coefficients[[1L]])
  out$slope <- unname(coefficients[[2L]])
  estimates <- tryCatch(
    suppressWarnings(stats::coef(summary(fitted))),
    error = function(e) NULL
  )
  if (!is.null(estimates) && nrow(estimates) >= 2L && ncol(estimates) >= 4L) {
    out$slope_se <- unname(estimates[2L, 2L])
    out$slope_p <- unname(estimates[2L, 4L])
  }
  out$pseudo_r1 <- .sca_pseudo_r1(fitted, yv, tau)
  out
}

# Koenker and Machado's R1 for a quantile regression.
#
# 1 - V1/V0, where V is the sum of the check-function losses: V1 under the
# fitted line and V0 under the intercept-only quantile, which is the sample
# quantile itself. It is the quantile-regression analogue of R-squared and it
# is not R-squared: it is a share of *absolute* loss at one quantile, not of
# variance, so a value here and a value in the `r_squared` column are not on a
# common scale and must not be compared.
.sca_pseudo_r1 <- function(fitted, yv, tau) {
  check <- function(u) sum(u * (tau - (u < 0)))
  v1 <- tryCatch(check(stats::residuals(fitted)), error = function(e) NA_real_)
  v0 <- tryCatch(
    check(yv - unname(stats::quantile(yv, probs = tau, names = FALSE))),
    error = function(e) NA_real_
  )
  if (!is.finite(v1) || !is.finite(v0) || v0 <= 0) {
    return(NA_real_)
  }
  1 - v1 / v0
}

#' Central-tendency reference lines
#'
#' Fits the central-tendency lines for each condition, on the same
#' observations the frontier was fitted to. `"ols"` is the ordinary
#' least-squares regression, the line `lm(y ~ x)` returns. `"qr"` is a quantile
#' regression at `reference.tau`, which defaults to `0.5`: the median line,
#' which is the central-tendency summary that does not move when one
#' observation does.
#'
#' Reported separately from [sca_table()] on purpose. Regression describes how
#' the expected outcome moves with the condition; a sufficiency frontier
#' describes which condition-outcome combinations are absent. Section 6.1 of
#' the condition analysis in degree framework treats these as different targets
#' that are most informative shown side by side rather than merged into one
#' number, and section 4.3 notes that a frontier is fixed by the most extreme
#' observations rather than by the central tendency. A reference line therefore
#' has a slope and an intercept but no empty space, no effect size, no
#' permutation p value and no sufficiency threshold.
#'
#' `"qr"` as a reference is not the same thing as `"qr"` as a frontier, and
#' they are set by different arguments. A frontier `qr` uses `qr.tau`, defaults
#' to `0.95` and is meant to sit at the edge of the cloud; a reference `qr`
#' uses `reference.tau`, defaults to `0.5` and is meant to sit in the middle of
#' it. Both can be asked for at once, which the engine could not do in a single
#' call: it takes one `qr.tau`, so the reference is computed here instead.
#'
#' `r_squared` and `beta` are `NA` for `"qr"`, and `pseudo_r1` is `NA` for
#' `"ols"`. Quantile regression minimises a different loss, so there is no
#' residual variance to take a share of; Koenker and Machado's R1 is the right
#' measure for it, and it is a share of absolute loss at one quantile rather
#' than of variance. Reporting it under the `r_squared` heading would be one
#' column carrying two meanings, and leaving it out would report a difference
#' in kind as a missing value, so it has a column of its own. A `pseudo_r1` and
#' an `r_squared` are not on a common scale and must not be compared.
#'
#' Every requested line is computed on request, and the lines are also computed
#' for a model that asked for none, so the table is available either way;
#' `drawn` records whether the line appears on [sca_plot()].
#'
#' @param model An object returned by [sca_analysis()].
#' @param x Optional condition names or positions. All conditions by default.
#' @param line Which reference lines to report. Defaults to those the model
#'   asked for, or to `"ols"` when it asked for none.
#' @return A data frame with one row per condition and reference line.
#' @seealso [sca_analysis()], [sca_table()], [sca_terms()]
#' @export
#' @examples
#' iv <- subset(sca_four_configurations, configuration == "IV")
#' fit <- sca_analysis(iv, x = "X", y = "Y", direction = "HH",
#'                     ceilings = "ce_fdh", scope = c(0, 100, 0, 100),
#'                     reference = c("ols", "qr"))
#' sca_reference(fit)
sca_reference <- function(model, x = NULL, line = NULL) {
  metadata <- .sca_metadata(model)
  conditions <- .sca_select_conditions(model, x)

  requested <- metadata$reference
  if (is.null(line)) {
    line <- if (length(requested) > 0L) requested else "ols"
  }
  line <- unique(tolower(trimws(as.character(line))))
  unknown <- setdiff(line, .sca_reference_lines)
  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "Unknown reference line(s): %s. Available: %s.",
        paste(unknown, collapse = ", "),
        paste(.sca_reference_lines, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  tau <- metadata$reference.tau
  if (is.null(tau) || !is.finite(tau)) {
    tau <- 0.5
  }

  rows <- list()
  id <- 0L
  for (condition in conditions) {
    item <- metadata$raw$conditions[[condition]]
    xv <- as.numeric(item$x)
    yv <- as.numeric(item$y)
    keep <- is.finite(xv) & is.finite(yv)
    xv <- xv[keep]
    yv <- yv[keep]

    for (this in line) {
      estimate <- .sca_reference_fit(this, xv, yv, tau)
      id <- id + 1L
      rows[[id]] <- data.frame(
        condition = condition,
        outcome = metadata$outcome,
        line = this,
        line_type = "central tendency",
        tau = if (identical(this, "qr")) tau else NA_real_,
        observations = length(xv),
        intercept = estimate$intercept,
        slope = estimate$slope,
        slope_se = estimate$slope_se,
        slope_p = estimate$slope_p,
        beta = estimate$beta,
        r_squared = as.numeric(estimate$r_squared),
        pseudo_r1 = as.numeric(estimate$pseudo_r1),
        drawn = this %in% requested,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @export
print.sca_result <- function(x, ...) {
  metadata <- .sca_metadata(x)
  cat("\nSufficient Condition Analysis\n")
  cat("Engine: ", metadata$engine, " ", metadata$engine_version, "\n", sep = "")
  mapping <- .sca_mapping_table(x)[
    c("condition", "outcome", "direction", "empty_space", "boundary_type",
      "empty_corner", "statement")
  ]
  print(mapping, row.names = FALSE)

  results <- sca_table(x)
  if (nrow(results) > 0L) {
    cat("\nFrontier results\n")
    print(
      results[c("condition", "frontier", "effect_size", "p_value",
                "frontier_accuracy", "fit")],
      row.names = FALSE
    )
  } else {
    cat("\nNo frontier effect size is available.\n")
  }

  if (length(metadata$reference) > 0L) {
    cat("\nReference lines (central tendency, no effect size):\n")
    print(
      sca_reference(x)[c("condition", "line", "tau", "intercept", "slope",
                         "beta", "r_squared")],
      row.names = FALSE
    )
  }

  .sca_settings_block(metadata)

  # No interactive() gate. sca() exists to draw the plot, and gating it on an
  # interactive session meant that under Rscript or source() -- which is how a
  # reproducible supplement is run -- sca() behaved exactly like
  # sca_analysis() and said nothing about the difference.
  if (isTRUE(attr(x, "show.plots"))) {
    plot(x)
  }
  invisible(x)
}

#' @export
summary.sca_result <- function(object, ...) {
  metadata <- .sca_metadata(object)
  out <- list(
    call = metadata$call,
    mapping = .sca_mapping_table(object),
    results = sca_table(object)
  )
  class(out) <- "summary_sca_result"
  out
}

#' @export
print.summary_sca_result <- function(x, ...) {
  cat("\nSufficient Condition Analysis summary\n\n")
  if (!is.null(x$call)) {
    cat("Call:\n")
    print(x$call)
    cat("\n")
  }
  cat("Logical-to-geometric mapping:\n")
  print(x$mapping, row.names = FALSE)
  cat("\nFrontier estimates:\n")
  print(x$results, row.names = FALSE)
  invisible(x)
}

#' Display selected SCA output
#'
#' @param model An object returned by [sca_analysis()].
#' @param summaries Print the tidy summary table.
#' @param thresholds Print sufficiency threshold rules.
#' @param plots Draw frontier plots.
#' @param tests Draw permutation-test plots when tests were requested.
#' @param selection Optional condition names or positions.
#' @param ceiling Frontier method used for threshold and permutation-test plots.
#' @param scale,outcome_scale,convention Optional reporting overrides passed to
#'   [sca_thresholds()]. Each defaults to the choice made in [sca_analysis()].
#' @param inequality `"strict"` or `"inclusive"`, passed to [sca_thresholds()].
#' @return Invisibly, a list containing generated plots and tables.
#' @export
#' @examples
#' iv <- subset(sca_four_configurations, configuration == "IV")
#' fit <- sca_analysis(iv, x = "X", y = "Y", direction = "HH",
#'                     ceilings = "ce_fdh", scope = c(0, 100, 0, 100))
#' sca_output(fit, thresholds = TRUE)
sca_output <- function(
    model,
    summaries = TRUE,
    thresholds = FALSE,
    plots = TRUE,
    tests = FALSE,
    selection = NULL,
    ceiling = NULL,
    scale = NULL,
    outcome_scale = NULL,
    convention = NULL,
    inequality = c("strict", "inclusive")) {
  metadata <- .sca_metadata(model)
  inequality <- match.arg(inequality)
  condition_names <- names(model$summaries)
  if (!is.null(selection)) {
    if (is.numeric(selection)) {
      selection <- condition_names[selection]
    }
    condition_names <- intersect(condition_names, selection)
  }
  if (length(condition_names) == 0L) {
    stop("No valid conditions were selected.", call. = FALSE)
  }

  result <- list()
  if (summaries) {
    result$summary <- sca_table(model)
    result$summary <- result$summary[result$summary$condition %in% condition_names, , drop = FALSE]
    print(result$summary, row.names = FALSE)
  }
  if (thresholds) {
    result$thresholds <- sca_thresholds(
      model,
      ceiling = ceiling,
      x = condition_names,
      scale = scale,
      outcome_scale = outcome_scale,
      convention = convention,
      inequality = inequality
    )
    print(
      result$thresholds[c(
        "condition", "direction", "outcome_level", "sufficiency_threshold",
        "status", "rule"
      )],
      row.names = FALSE
    )
  }
  if (plots) {
    result$plots <- sca_plot(model, x = condition_names)
    if (inherits(result$plots, "ggplot")) {
      print(result$plots)
    } else {
      invisible(lapply(result$plots, print))
    }
  }
  if (tests) {
    result$tests <- lapply(
      condition_names,
      function(condition) sca_test_plot(model, x = condition, ceiling = ceiling)
    )
    names(result$tests) <- condition_names
    invisible(lapply(result$tests, print))
  }
  # This function assembles a report, and a report has to say what it was run
  # with. print() ends with the same block; a caller who reaches for
  # sca_output() instead should not silently lose it.
  .sca_settings_block(metadata)
  invisible(result)
}

# Resolve a frontier technique the way every other selector in the two packages
# does: the first one actually estimated. Until 0.5.0 sca_thresholds(),
# sca_test_plot() and sca_output() hard-coded "ce_fdh" while sca_extract(),
# nsca_thresholds() and nsca_extract() took the first estimated one, so
# sca_analysis(ceilings = "cr_fdh") followed by sca_thresholds(fit) failed on a
# model that had been fitted perfectly well.
.sca_resolve_ceiling <- function(model, ceiling, available) {
  if (is.null(ceiling)) {
    metadata <- .sca_metadata(model)
    estimated <- intersect(metadata$ceilings, available)
    if (length(estimated) == 0L) {
      estimated <- available
    }
    if (length(estimated) == 0L) {
      stop("The model holds no estimated frontier technique.", call. = FALSE)
    }
    return(estimated[[1L]])
  }
  if (length(ceiling) != 1L) {
    stop("Select exactly one frontier technique.", call. = FALSE)
  }
  ceiling
}

# The support criteria, kept as three separate questions.
#
# `tested` is not the same as "significant": with test.rep = 0 nothing was
# tested, and reporting FALSE would claim evidence of absence. NA says the
# question was not asked.
.sca_tested <- function(metadata) {
  isTRUE(metadata$test.rep > 0)
}

.sca_significant <- function(p_value, metadata) {
  if (!.sca_tested(metadata) || !is.finite(p_value)) {
    return(NA)
  }
  p_value <= metadata$alpha
}

# With no pre-specified threshold there is no relevance criterion to fail, so
# the answer is TRUE rather than NA: support then rests on significance alone,
# which is what NULL was asked to mean.
.sca_relevant <- function(effect_size, metadata) {
  threshold <- metadata$relevance
  if (is.null(threshold) || !is.finite(threshold)) {
    return(TRUE)
  }
  isTRUE(is.finite(effect_size) && effect_size >= threshold)
}

.sca_supported <- function(p_value, effect_size, metadata) {
  significant <- .sca_significant(p_value, metadata)
  if (is.na(significant)) {
    return(NA)
  }
  significant && .sca_relevant(effect_size, metadata)
}

.sca_support_status <- function(p_value, effect_size, metadata) {
  if (!.sca_tested(metadata)) {
    return("not tested")
  }
  significant <- .sca_significant(p_value, metadata)
  if (is.na(significant)) {
    return("not tested")
  }
  relevant <- .sca_relevant(effect_size, metadata)
  if (significant && relevant) {
    return("supported")
  }
  if (!significant && !relevant) {
    return("neither significant nor relevant")
  }
  if (!significant) "not significant" else "below relevance threshold"
}

# Retired names are no longer usable. They stop with the replacement named:
# a name that quietly returns NULL turns a working script into a wrong one
# rather than a broken one. The column duplicates behind `legacy = TRUE`
# remain, because that mechanism copies the current column rather than
# translating a name, and so cannot give a different answer from it.
#
# This also settles a promise that could never be kept. All three retired names
# are threshold-table columns, defined per outcome level, so sca_extract() had
# no single value to return for them: it warned that the name had been renamed
# and then failed with a different message about threshold columns.
.sca_retired_stop <- function(name) {
  replacement <- .sca_legacy_columns[[name]]
  stop(
    sprintf(
      paste0(
        "'%s' was renamed to '%s' and is no longer accepted here. Retired ",
        "column names are still available as duplicates from ",
        "sca_table(legacy = TRUE) and sca_thresholds(legacy = TRUE); see ",
        "sca_legacy_names()."
      ),
      name, replacement
    ),
    call. = FALSE
  )
}

# The engine reports two of its metrics as percentages and everything else as
# proportions. `Ceiling accuracy` is 100 * (n - above) / n and `Fit` is
# 100 - 100 * |ceiling - fdh_ceiling| / fdh_ceiling, while `Effect size` is
# ceiling / scope.area and `p-value` and `p-accuracy` are probabilities. So a
# 0.4.3 row put effect_size = 0.52 beside frontier_accuracy = 99: two shares of
# something, on two scales, in adjacent columns.
#
# SCAtools reports every share on [0, 1]. The engine's own names still return
# the engine's own units through sca_extract(param = "Ceiling accuracy"),
# because that is the engine's number, asked for by the engine's name.
.sca_as_proportion <- function(value) {
  if (!is.finite(value)) {
    return(NA_real_)
  }
  value / 100
}

# What the analysis was actually run with, and which of it the caller chose.
#
# `sca()` exists so that three arguments produce a reported analysis, which
# means the package picks the frontier techniques, the number of replications,
# the significance level, the reporting scale and the scope on the caller's
# behalf. A reader of the output cannot otherwise tell which of those numbers
# was a decision by the researcher and which was a decision by the package,
# and that distinction is exactly what a methods section has to state. The
# recorded call says what was passed by name, so everything absent from it is
# a default and can be labelled as one.
#
# `data`, `x` and `y` are dropped because they are the analysis, not a
# setting, and they can never be defaults.
.sca_supplied <- function(metadata) {
  call <- metadata$call
  if (is.null(call)) {
    # No call recorded: nothing can be attributed either way, so claim
    # nothing. Every line then prints unmarked rather than falsely marked.
    return(NULL)
  }
  setdiff(names(as.list(call)), c("", "data", "x", "y"))
}

.sca_settings_block <- function(metadata) {
  supplied <- .sca_supplied(metadata)
  # NULL means "unknown", which is not the same as "supplied nothing": the
  # first must print no marks at all, the second must mark every line.
  mark <- if (is.null(supplied)) {
    function(...) ""
  } else {
    function(...) if (any(c(...) %in% supplied)) "" else "  (default)"
  }

  scope_text <- if (identical(metadata$scope_source, "observed")) {
    "observed range of each condition and the outcome"
  } else {
    paste0(
      "declared: ",
      paste(format(unlist(metadata$scope_declared)), collapse = ", ")
    )
  }

  relevance_text <- if (is.null(metadata$relevance) ||
                        !isTRUE(is.finite(metadata$relevance))) {
    "none pre-specified"
  } else {
    format(metadata$relevance)
  }

  cat("\nSettings\n")
  cat(sprintf("  frontiers     %s%s\n",
              paste(metadata$ceilings, collapse = ", "), mark("ceilings")))
  if (length(metadata$reference) > 0L) {
    cat(sprintf("  reference     %s%s\n",
                paste(metadata$reference, collapse = ", "), mark("reference")))
    if ("qr" %in% metadata$reference) {
      cat(sprintf("  reference.tau %s%s\n",
                  format(if (is.null(metadata$reference.tau)) 0.5
                         else metadata$reference.tau),
                  mark("reference.tau")))
    }
  }
  cat(sprintf("  test.rep      %s%s\n",
              format(metadata$test.rep), mark("test.rep")))
  cat(sprintf("  alpha         %s%s\n",
              format(metadata$alpha), mark("test.p_threshold")))
  cat(sprintf("  test.seed     %s%s\n",
              if (is.null(metadata$test.seed)) "not fixed" else
                format(metadata$test.seed),
              mark("test.seed")))
  if (isTRUE(metadata$test.rep > 0) && is.null(metadata$test.seed)) {
    # Said next to the number it is about, not only in the row above it. A
    # permutation p-value with no seed is a different number on every run.
    cat("                the p-values above will differ on the next run
")
  }
  .sca_multiplicity_block(metadata)

  cat(sprintf("  relevance     %s%s\n", relevance_text, mark("relevance")))
  cat(sprintf("  scope         %s%s\n", scope_text, mark("scope")))
  # Not a default and not a choice: something the engine did to the choice.
  # The declared scope is what a methods section reports, and the widened one
  # is what every effect size above was divided by, so a reader who is not
  # told cannot reproduce a single number.
  adjustment <- metadata$scope_adjustment
  if (!is.null(adjustment) && nrow(adjustment) > 0L) {
    cat(sprintf(
      "                widened by the engine to hold every observation (%s); see sca_scope()\n",
      paste(unique(adjustment$condition), collapse = ", ")
    ))
  }
  # Not a setting either, and for the same reason as the line above: it says
  # whether the effect sizes describe the data or describe the scope. Printed
  # only when an axis is thin, so an ordinary analysis shows nothing here.
  coverage <- unlist(metadata$scope_coverage)
  thin <- is.finite(coverage) & coverage < 0.5
  if (any(thin)) {
    cat(sprintf(
      "                observations span %s of it\n",
      paste(sprintf("%s %.0f%%", names(coverage)[thin],
                    100 * coverage[thin]), collapse = ", ")
    ))
  }
  cat(sprintf("  na.action     %s%s\n",
              metadata$na.action, mark("na.action")))
  # The readable scale names, not the argument values: this line used to sit
  # at the top of the output under its own heading, where it said what the
  # thresholds were reported on but not whether anyone had chosen it.
  cat(sprintf("  thresholds    condition on %s, outcome on %s%s\n",
              .sca_scale_label(metadata$threshold.x, metadata$convention),
              .sca_scale_label(metadata$threshold.y, metadata$convention),
              mark("threshold.x", "threshold.y")))
  cat(sprintf("  convention    %s%s\n",
              metadata$convention, mark("convention")))
  invisible(NULL)
}

# The scope area the engine actually divided by.
#
# `scope_used` holds the bounds after the engine's widening rule, computed the
# same way the engine computes them, so this is the denominator of every
# effect size for that condition rather than an approximation of it.
.sca_scope_area <- function(metadata, condition) {
  bounds <- metadata$scope_used[[condition]]
  if (is.null(bounds) || length(bounds) < 4L) {
    return(NA_real_)
  }
  bounds <- suppressWarnings(as.numeric(bounds))
  area <- (bounds[[2L]] - bounds[[1L]]) * (bounds[[4L]] - bounds[[3L]])
  if (!is.finite(area)) NA_real_ else area
}

.sca_scope_is_adjusted <- function(metadata, condition) {
  adjustment <- metadata$scope_adjustment
  if (is.null(adjustment) || nrow(adjustment) == 0L) {
    return(FALSE)
  }
  condition %in% adjustment$condition
}

#' The scope each effect size was divided by
#'
#' Every effect size in this package is an empty area divided by the area of
#' the X-Y scope, so the scope is half of every number reported. It is not
#' always the scope that was asked for: the NCA engine widens a declared scope
#' until it contains every observation, which changes the denominator without
#' changing what the researcher wrote down.
#'
#' This reports both. `declared_*` is `NA` when no scope was declared and the
#' observed range was used. `used_*` is what the engine divided by, and
#' `adjusted` is `TRUE` for a condition on which the two differ.
#'
#' @param model An object returned by [sca_analysis()].
#' @return A data frame with one row per condition.
#' @seealso [sca_table()], [sca_analysis()]
#' @export
#' @examples
#' fit <- sca_analysis(
#'   subset(sca_four_configurations, configuration == "IV"),
#'   x = "X", y = "Y", direction = "HH", scope = c(0, 100, 0, 100)
#' )
#' sca_scope(fit)
sca_scope <- function(model) {
  metadata <- .sca_metadata(model)
  conditions <- names(metadata$scope_used)
  declared <- metadata$scope_declared
  rows <- lapply(seq_along(conditions), function(i) {
    condition <- conditions[[i]]
    used <- suppressWarnings(as.numeric(metadata$scope_used[[condition]]))
    x_declared <- .sca_scope_bounds(declared, i, 0L)
    y_declared <- .sca_scope_bounds(declared, 1L, 2L)
    data.frame(
      condition = condition,
      outcome = metadata$outcome,
      source = metadata$scope_source,
      declared_xmin = x_declared[[1L]],
      declared_xmax = x_declared[[2L]],
      declared_ymin = y_declared[[1L]],
      declared_ymax = y_declared[[2L]],
      used_xmin = used[[1L]],
      used_xmax = used[[2L]],
      used_ymin = used[[3L]],
      used_ymax = used[[4L]],
      scope_area = .sca_scope_area(metadata, condition),
      adjusted = .sca_scope_is_adjusted(metadata, condition),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

# How many hypothesis tests the output above contains, and what is not being
# done about it.
#
# Disclosure rather than adjustment. Which correction is appropriate depends
# on what the family is, and that is a claim about the design: whether the
# conditions were pre-specified or chosen after looking, whether one
# significant condition would be reported as the finding or all of them
# together, whether the analysis is confirmatory at all. The package does not
# know any of that and would have to assume it in order to pick a method, so
# it reports the count and the arithmetic and leaves the choice where it
# belongs. The same reason sca_table() reports an interval rather than a
# verdict.
#
# The frontier techniques are counted separately from the conditions because
# they are not the same kind of multiplicity: two techniques are two
# estimates of one claim, so they inflate the number of p-values on the page
# without adding a hypothesis. Choosing between them after seeing both is a
# real problem and gets its own line; it is not a family-wise rate.
#
# Silent for the ordinary single-condition, single-technique analysis, where
# there is no multiplicity to disclose.
.sca_multiplicity_block <- function(metadata) {
  if (!isTRUE(metadata$test.rep > 0)) {
    return(invisible(NULL))
  }
  k <- length(metadata$direction)
  m <- length(metadata$ceilings)
  alpha <- suppressWarnings(as.numeric(metadata$alpha))

  if (k > 1L) {
    cat(sprintf(
      "  multiplicity  %d conditions%s = %d p-values, none adjusted\n",
      k,
      if (m > 1L) sprintf(" x %d techniques", m) else "",
      k * m
    ))
    if (m > 1L) {
      cat("                the techniques re-estimate one claim, so the family is the\n")
      cat(sprintf("                %d conditions\n", k))
    }
    if (isTRUE(is.finite(alpha)) && alpha > 0 && alpha < 1) {
      cat(sprintf(
        paste0("                were the %d tests independent, P(at least one false ",
               "positive)\n                at alpha %s would be %.2f\n"),
        k, format(alpha), 1 - (1 - alpha)^k
      ))
    }
  } else if (m > 1L) {
    cat(sprintf(
      "  multiplicity  1 condition x %d techniques = %d p-values for one claim\n",
      m, m
    ))
    cat("                report the technique chosen in advance, not the smallest p\n")
  }
  invisible(NULL)
}

# Multiple-testing adjustment, applied only when asked for.
#
# 0.5.0 already discloses the multiplicity in the Settings block: how many
# tests, that nothing is adjusted, and what the family-wise rate would be under
# independence. This is the other half -- the means to act on it -- and it is
# deliberately an argument rather than a default.
#
# The package will not pick a method. Which correction fits depends on what the
# family is, and that is a property of the design: whether the conditions were
# pre-specified, whether one significant condition would be reported as the
# finding or all of them together, whether the analysis is confirmatory at all.
# A default would answer all three on the researcher's behalf. What the package
# can do is make the adjustment one argument rather than a hand-rolled pass over
# the p-value column, and make sure that when it is applied every decision in
# the table is re-derived from the adjusted values rather than left standing on
# the raw ones.
#
# The family is the conditions, within one frontier technique. Two techniques
# are two estimates of one claim, not two hypotheses, so adjusting across them
# would inflate k with re-measurements and cost power for nothing. Choosing a
# technique after seeing both p-values is a real problem, but it is a garden of
# forking paths rather than a family-wise rate, and the Settings block names it
# separately.
.sca_validate_p_adjust <- function(method) {
  method <- as.character(method)[[1L]]
  if (!method %in% stats::p.adjust.methods) {
    stop(
      sprintf(
        "Unknown p.adjust method '%s'. Available: %s.",
        method, paste(stats::p.adjust.methods, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  method
}

# Insert one column immediately after another, so the adjusted value sits
# beside the value it adjusts rather than at the end of a forty-column table.
.sca_insert_after <- function(out, after, name, value) {
  out[[name]] <- value
  at <- match(after, names(out))
  keep <- setdiff(names(out), name)
  before <- keep[seq_len(match(after, keep))]
  out[c(before, name, setdiff(keep, before))]
}

# Adjust within each frontier technique, over the conditions.
.sca_adjust_within <- function(p, group, method) {
  out <- rep(NA_real_, length(p))
  for (g in unique(group)) {
    i <- group == g
    out[i] <- stats::p.adjust(p[i], method = method)
  }
  out
}

.sca_apply_p_adjust <- function(out, metadata, method) {
  method <- .sca_validate_p_adjust(method)
  if (identical(method, "none") || nrow(out) == 0L) {
    return(out)
  }

  .sca_warn_adjust_floor(
    length(unique(out$condition)), metadata$test.rep, metadata$alpha, method
  )
  adjusted <- .sca_adjust_within(out$p_value, out$frontier, method)
  out <- .sca_insert_after(out, "p_value", "p_value_adjusted", adjusted)
  out <- .sca_insert_after(out, "p_value_adjusted", "p_adjust_method", method)

  # Every decision the table reports is re-derived. Leaving them on the raw
  # p-value would put an adjusted number beside a verdict that ignored it,
  # which is the one thing worse than not adjusting at all.
  out$significant <- vapply(
    adjusted, function(p) .sca_significant(p, metadata), logical(1L)
  )
  out$supported <- vapply(
    seq_len(nrow(out)),
    function(i) .sca_supported(adjusted[[i]], out$effect_size[[i]], metadata),
    logical(1L)
  )
  out$support_status <- vapply(
    seq_len(nrow(out)),
    function(i) .sca_support_status(adjusted[[i]], out$effect_size[[i]],
                                    metadata),
    character(1L)
  )
  out
}

# What a random pairing of the same data produces.
#
# The FDH envelope is built from the sample's own extremes, so it overstates
# the empty area even when the condition and the outcome are independent: with
# n points there is always *some* corner nothing happens to occupy. The
# overstatement shrinks with n, which is why an effect size from 40 cases and
# one from 400 are not on the same scale.
#
# This is not modelled, it is read. The permutation test already builds the
# distribution of the effect size under random pairing and then throws all of
# it away except the tail probability. Under independence the true empty area
# is zero, so the mean of that distribution *is* the overstatement at this
# sample size, this scope and this technique -- measured on the data in hand
# rather than taken from an asymptotic constant.
#
# Reported rather than subtracted. `effect_size_excess` is a comparison, not a
# corrected estimator: subtracting the null mean does not give an unbiased
# effect size, because the bias under independence is not the bias under the
# alternative. What it gives is the part of the number that random pairing
# alone does not account for, which is the question a reader of a small-sample
# d actually has. The same restraint as sca_scope_sensitivity(), which reports
# the effect size on every box and leaves the subtraction to the reader.
.sca_null_mean <- function(model, condition, ceiling) {
  item <- model$tests[[condition]]
  if (is.null(item) || is.null(item[[ceiling]])) {
    return(NA_real_)
  }
  draws <- suppressWarnings(as.numeric(item[[ceiling]]$data))
  draws <- draws[is.finite(draws)]
  if (length(draws) == 0L) {
    return(NA_real_)
  }
  mean(draws)
}

.sca_excess <- function(observed, null_mean) {
  if (!is.finite(observed) || !is.finite(null_mean)) {
    return(NA_real_)
  }
  observed - null_mean
}

# Warn when the adjustment cannot reject whatever the data says.
#
# A permutation p-value cannot fall below 1/(test.rep + 1), and the most
# significant of k conditions is multiplied by k under both bonferroni and
# BH. So unless k / (test.rep + 1) <= alpha, no condition can be declared
# significant -- an arithmetic fact about the replication count that looks
# exactly like a null result about the data.
#
# A warning rather than a refusal: the adjusted p-values are still correct,
# and a caller may want them for their own reasons. What they must not do is
# read the absence of a rejection as evidence.
.sca_warn_adjust_floor <- function(k, test_rep, alpha, method) {
  if (!isTRUE(is.finite(test_rep) && test_rep > 0)) {
    return(invisible(NULL))
  }
  floor_p <- k / (test_rep + 1)
  if (floor_p <= alpha) {
    return(invisible(NULL))
  }
  warning(
    sprintf(
      paste0(
        "With %d conditions and test.rep = %d, the smallest p-value the ",
        "permutation test can produce is %.4f, and '%s' multiplies the most ",
        "significant of %d by %d. No condition can reach alpha = %s however ",
        "strong the evidence, so an absence of support here is arithmetic ",
        "rather than a finding. Use test.rep >= %d."
      ),
      k, test_rep, 1 / (test_rep + 1), method, k, k, format(alpha),
      as.integer(ceiling(k / alpha - 1))
    ),
    call. = FALSE
  )
  invisible(NULL)
}
