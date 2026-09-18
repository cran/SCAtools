.sca_parameter <- function(params, parameter, ceiling) {
  if (!(parameter %in% rownames(params)) || !(ceiling %in% colnames(params))) {
    return(NA_real_)
  }
  value <- params[[match(parameter, rownames(params)), match(ceiling, colnames(params))]]
  suppressWarnings(as.numeric(value))
}

.sca_global <- function(summary, parameter) {
  if (!(parameter %in% rownames(summary$global))) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(summary$global[match(parameter, rownames(summary$global)), 1L]))
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
#' @param model An object returned by [sca_analysis()].
#' @param legacy Append the column names retired in 0.4.0 as duplicates. See
#'   [sca_legacy_names()].
#' @return A data frame with one row for every condition-frontier combination.
#' @seealso [sca_thresholds()], [sca_corner_map()], [sca_legacy_names()]
#' @export
sca_table <- function(model, legacy = FALSE) {
  metadata <- .sca_metadata(model)
  rows <- list()
  row_id <- 0L

  for (condition in names(model$summaries)) {
    item <- model$summaries[[condition]]
    params <- item$params
    if (ncol(params) == 0L) {
      next
    }
    # Frontiers only. A central-tendency line requested through `reference` is
    # not an empty-space estimate, so it never becomes a row here even if the
    # engine reports a column for it.
    frontiers <- setdiff(colnames(params), metadata$reference)
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
        empty_zone_area = .sca_parameter(params, "Ceiling zone", ceiling),
        effect_size = .sca_parameter(params, "Effect size", ceiling),
        p_value = .sca_parameter(params, "p-value", ceiling),
        p_accuracy = .sca_parameter(params, "p-accuracy", ceiling),
        frontier_accuracy = .sca_parameter(params, "Ceiling accuracy", ceiling),
        fit = .sca_parameter(params, "Fit", ceiling),
        slope = .sca_parameter(params, "Slope", ceiling),
        intercept = .sca_parameter(params, "Intercept", ceiling),
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
      empty_zone_area = numeric(), effect_size = numeric(), p_value = numeric(),
      p_accuracy = numeric(), frontier_accuracy = numeric(), fit = numeric(),
      slope = numeric(), intercept = numeric(), stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
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
sca_extract <- function(model, x = NULL, ceiling = NULL, param = "Effect size") {
  metadata <- .sca_metadata(model)
  if (is.null(x)) {
    x <- names(model$summaries)[[1L]]
  }
  if (!(x %in% names(model$summaries))) {
    stop(sprintf("Unknown condition '%s'.", x), call. = FALSE)
  }

  if (param %in% names(.sca_legacy_columns)) {
    replacement <- .sca_legacy_columns[[param]]
    warning(
      sprintf(
        "'%s' was renamed to '%s' in SCAtools 0.4.0. See sca_legacy_names().",
        param, replacement
      ),
      call. = FALSE
    )
    param <- replacement
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
#'   inequalities or an inclusive reporting convention. Renamed from `boundary`
#'   in 0.4.0: in the terminology of Goertz et al. (2013) a *boundary* is the
#'   theoretical line separating an expected empty space from the compatible
#'   region, which is a different object from the strictness of the reported
#'   inequality. The old argument still works and warns.
#' @param boundary Deprecated. Former name of `inequality`.
#' @param digits Significant digits used in the formatted rule text.
#' @param legacy Append the column names retired in 0.4.0 as duplicates. See
#'   [sca_legacy_names()].
#' @return A long-form data frame of threshold rules.
#' @seealso [sca_table()], [sca_legacy_names()], [sca_scales()]
#' @export
sca_thresholds <- function(
    model,
    ceiling = "ce_fdh",
    x = NULL,
    scale = NULL,
    outcome_scale = NULL,
    convention = NULL,
    inequality = c("strict", "inclusive"),
    digits = 6L,
    legacy = FALSE,
    boundary = NULL) {
  metadata <- .sca_metadata(model)
  inequality <- .sca_resolve_inequality(inequality, boundary, !missing(inequality))

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

#' Central-tendency reference line
#'
#' Fits the ordinary least-squares regression of the outcome on each condition,
#' using the same observations the frontier was fitted to. This is the line
#' `lm(y ~ x)` returns, and it is what `reference = "ols"` draws.
#'
#' The line is reported separately from [sca_table()] on purpose. Regression
#' describes how the expected outcome moves with the condition; a sufficiency
#' frontier describes which condition-outcome combinations are absent. Section
#' 6.1 of the condition analysis in degree framework treats these as different
#' targets that are most informative shown side by side rather than merged into
#' one number, and section 4.3 notes that a frontier is fixed by the most
#' extreme observations rather than by the central tendency. A reference line
#' therefore has a slope and an intercept but no empty zone, no effect size, no
#' permutation p value and no sufficiency threshold.
#'
#' The line is computed on request, so it is available whether or not
#' `reference = "ols"` was set; `drawn` records whether it was also passed to
#' the engine and so appears on [sca_plot()].
#'
#' @param model An object returned by [sca_analysis()].
#' @param x Optional condition names or positions. All conditions by default.
#' @return A data frame with one row per condition.
#' @seealso [sca_analysis()], [sca_table()], [sca_terms()]
#' @export
sca_reference <- function(model, x = NULL) {
  metadata <- .sca_metadata(model)
  conditions <- .sca_select_conditions(model, x)
  drawn <- isTRUE("ols" %in% metadata$reference)

  rows <- lapply(conditions, function(condition) {
    item <- metadata$raw$conditions[[condition]]
    xv <- as.numeric(item$x)
    yv <- as.numeric(item$y)
    keep <- is.finite(xv) & is.finite(yv)
    xv <- xv[keep]
    yv <- yv[keep]

    intercept <- NA_real_
    slope <- NA_real_
    r_squared <- NA_real_
    if (length(xv) >= 2L && stats::var(xv) > 0) {
      fitted <- stats::lm(yv ~ xv)
      coefficients <- stats::coef(fitted)
      intercept <- unname(coefficients[[1L]])
      slope <- unname(coefficients[[2L]])
      # For a simple regression this is exactly the squared correlation, so it
      # needs no second pass over the fitted object.
      r_squared <- if (stats::var(yv) > 0) {
        suppressWarnings(stats::cor(xv, yv))^2
      } else {
        NA_real_
      }
    }

    data.frame(
      condition = condition,
      outcome = metadata$outcome,
      line = "ols",
      line_type = "central tendency",
      observations = length(xv),
      intercept = intercept,
      slope = slope,
      r_squared = as.numeric(r_squared),
      drawn = drawn,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @export
print.sca_result <- function(x, ...) {
  metadata <- .sca_metadata(x)
  cat("\nSufficiency Condition Analysis\n")
  cat("Engine: ", metadata$engine, " ", metadata$engine_version, "\n", sep = "")
  cat(
    "Threshold reporting: condition on ",
    .sca_scale_label(metadata$threshold.x, metadata$convention),
    ", outcome on ", .sca_scale_label(metadata$threshold.y, metadata$convention),
    " (", metadata$convention, ")\n",
    sep = ""
  )
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
    cat("\nReference line (central tendency, no effect size):\n")
    print(
      sca_reference(x)[c("condition", "line", "intercept", "slope",
                         "r_squared")],
      row.names = FALSE
    )
  }

  if (isTRUE(attr(x, "show.plots")) && interactive()) {
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
  cat("\nSufficiency Condition Analysis summary\n\n")
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
sca_output <- function(
    model,
    summaries = TRUE,
    thresholds = FALSE,
    plots = TRUE,
    tests = FALSE,
    selection = NULL,
    ceiling = "ce_fdh",
    scale = NULL,
    outcome_scale = NULL,
    convention = NULL,
    inequality = c("strict", "inclusive")) {
  .sca_metadata(model)
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
  invisible(result)
}
