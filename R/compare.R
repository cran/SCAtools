# Average effect beside sufficiency in degree.
#
# Section 6.1 of the condition analysis in degree framework keeps the two
# logics side by side rather than merging them into one number, and section 4.3
# says why: a frontier is fixed by the most extreme observations, a regression
# line by the central tendency. They answer different questions about the same
# relationship and neither implies the other.
#
# The consequence this file exists to make visible: a design that reports only
# a regression coefficient cannot tell configuration II from configuration IV.
# Both have a flat slope. One of them has a clean empty corner -- a real and
# actionable finding -- and gets written up as "no significant effect".

# The two cuts are each the lower edge of "medium" in their own tradition.
# They are different numbers because the traditions are different, not because
# one of them is a typo, and the printed output says so.
.sca_beta_cut_default <- 0.30   # Cohen (1988): .10 small, .30 medium, .50 large
.sca_d_cut_default <- 0.10      # Dul (2016): <.10 small, .10-.30 medium, .30-.50 large

# The numbering is the paper's, not this package's invention:
#
#   I    neither focal effect
#   II   sufficiency-dominant
#   III  mean-dominant
#   IV   mean and sufficiency
#
# It runs from weakest to strongest, which is the opposite of the order a
# classifier is usually written in, and an earlier draft of this file had I and
# IV the wrong way round. A table that disagrees with the paper it implements
# is worse than no table, so the demonstration data ships with the package and
# a test pins each of the four files to its own label.
.sca_configuration_label <- c(
  I = "neither",
  II = "sufficiency-dominant",
  III = "mean-dominant",
  IV = "mean and sufficiency"
)

.sca_configuration_reading <- c(
  I = paste0(
    "Neither the average effect nor the empty corner reaches its stated cut. ",
    "No evidence for either reading of the %s-%s relationship."
  ),
  II = paste0(
    "The average slope is flat, but the forbidden corner is largely empty: ",
    "beyond some level of %s the low-%s cases stop occurring. A ",
    "regression-only design would report this relationship as absent. Use ",
    "sca_locationshift() to ask whether the frontier shifts the whole ",
    "distribution or marks a genuine threshold."
  ),
  III = paste0(
    "Higher %s goes with higher expected %s on average, but counterexamples ",
    "remain. The average effect is supported; sufficiency is not."
  ),
  IV = paste0(
    "Higher %s goes with higher expected %s on average, and the corner the ",
    "sufficiency claim forbids is largely empty. Two findings, not one."
  )
)

#' Average effect beside sufficiency in degree
#'
#' Reports the ordinary least-squares coefficient and the sufficiency effect
#' size for the same relationship, and classifies the pair into the four
#' configurations of the condition analysis in degree framework.
#'
#' @section What is being compared:
#' `beta` is the standardised regression coefficient: how far the *expected*
#' outcome moves with the condition. `effect_size` is the share of the declared
#' scope left empty in the corner the sufficiency claim forbids. Neither
#' implies the other. A relationship can have a strong average effect and no
#' empty corner, or a clean empty corner and almost no slope.
#'
#' @section The four configurations:
#' \describe{
#'   \item{I, both}{The average effect and the empty corner both reach their
#'   cut. Two separate findings about one relationship.}
#'   \item{II, sufficiency only}{A flat slope with a largely empty corner.
#'   Typically a level shift or a threshold: beyond some level of the condition
#'   the low-outcome cases stop occurring, without further gain above it.}
#'   \item{III, average effect only}{A slope with counterexamples still
#'   present. The condition raises the expected outcome without being enough
#'   for it -- the reading most often mistaken for sufficiency.}
#'   \item{IV, neither}{No evidence for either reading.}
#' }
#'
#' Configurations II and IV are indistinguishable in a design that reports only
#' a regression coefficient, because both have a flat slope. Separating them is
#' what this table is for.
#'
#' @section The cuts:
#' `beta_cut` defaults to 0.30, the lower edge of the medium band in Cohen
#' (1988) for a standardised coefficient. `d_cut` defaults to the `relevance`
#' threshold given to [sca_analysis()] when one was pre-specified -- the
#' researcher has already stated their criterion and this function should not
#' override it -- and otherwise to 0.10, the lower edge of the medium band in
#' Dul (2016). The two numbers differ because the two conventions do; each is
#' the bottom of "medium" in its own tradition. Whichever cuts were used are
#' returned as columns, so a table always carries the lines that produced its
#' own classification.
#'
#' These are conventions, not benchmarks calibrated for this design.
#'
#' `effect_size_null` and `effect_size_excess` sit beside the
#' classification for that reason. A condition can clear `d_cut` on an
#' effect size that is mostly the envelope fitting sample extremes -- a
#' noise condition at n = 70 reached 0.104 against the conventional 0.10
#' with a floor of 0.057 -- and the label would read "dominant" either
#' way. The cut is left where the convention puts it; what the table adds
#' is the number that qualifies it.
#'
#' @param model An object returned by [sca_analysis()].
#' @param x Optional condition names or positions. All conditions by default.
#' @param ceiling Frontier technique. Defaults to the first estimated.
#' @param beta_cut Absolute standardised coefficient at or above which the
#'   average effect counts as present.
#' @param d_cut Sufficiency effect size at or above which the empty corner
#'   counts as present. Defaults to the model's `relevance`, then to 0.10.
#' @return A data frame with one row per condition.
#' @references
#' Chan, Y. (in press). *Mean Effects Are Not Sufficiency Effects: How
#' Sufficient Condition Analysis Complements Regression Analysis*.
#' University of Leeds.
#'
#' Chan, Y. (in press). *From Necessity to Sufficiency and
#' Necessity-and-Sufficiency: Extending Condition Analysis in Degree*.
#' University of Leeds.
#' @seealso [sca_locationshift()], [sca_reference()], [sca_table()]
#' @export
#' @examples
#' # Configuration II: a flat slope with a largely empty corner
#' ii <- subset(sca_four_configurations, configuration == "II")
#' fit <- sca_analysis(ii, x = "X", y = "Y", direction = "HH",
#'                     ceilings = "ce_fdh", scope = c(0, 100, 0, 100))
#' sca_compare(fit)
sca_compare <- function(model, x = NULL, ceiling = NULL,
                        beta_cut = .sca_beta_cut_default, d_cut = NULL) {
  metadata <- .sca_metadata(model)
  conditions <- .sca_select_conditions(model, x)
  results <- sca_table(model)
  ceiling <- .sca_resolve_ceiling(model, ceiling, unique(results$frontier))

  if (is.null(d_cut)) {
    stated <- metadata$relevance
    d_cut <- if (!is.null(stated) && is.finite(stated)) {
      stated
    } else {
      .sca_d_cut_default
    }
  }
  beta_cut <- abs(as.numeric(beta_cut)[[1L]])
  d_cut <- as.numeric(d_cut)[[1L]]

  # Pinned rather than left to the model's own `reference`: `beta` is
  # documented as the least-squares coefficient and the configuration
  # readings say "on average", so a model fitted with reference = "qr"
  # would otherwise put the median slope in that column under an OLS
  # label. Asking for a line the model did not request costs nothing --
  # reference lines are computed here, not by the engine.
  reference <- sca_reference(model, x = conditions, line = "ols")

  rows <- lapply(conditions, function(condition) {
    line <- reference[reference$condition == condition, , drop = FALSE][1L, ]
    row <- results[
      results$condition == condition & results$frontier == ceiling, ,
      drop = FALSE
    ]
    if (nrow(row) == 0L) {
      stop(
        sprintf("No result for condition %s with frontier %s.",
                condition, ceiling),
        call. = FALSE
      )
    }
    row <- row[1L, ]

    beta <- line$beta
    d <- row$effect_size
    average_present <- isTRUE(is.finite(beta) && abs(beta) >= beta_cut)
    sufficiency_present <- isTRUE(is.finite(d) && d >= d_cut)
    configuration <- if (!is.finite(beta) || !is.finite(d)) {
      NA_character_
    } else if (!average_present && !sufficiency_present) {
      "I"
    } else if (!average_present && sufficiency_present) {
      "II"
    } else if (average_present && !sufficiency_present) {
      "III"
    } else {
      "IV"
    }

    data.frame(
      condition = condition,
      outcome = metadata$outcome,
      direction = unname(metadata$direction[[condition]]),
      frontier = ceiling,
      observations = row$observations,
      beta = beta,
      # Cohen's band for the coefficient, beside Dul's for the effect
      # size below. The two share their edges and shift their labels,
      # so a reader comparing the pair needs both named.
      beta_magnitude = sca_magnitude(beta, scale = "beta"),
      slope = line$slope,
      slope_se = line$slope_se,
      slope_p = line$slope_p,
      r_squared = line$r_squared,
      effect_size = d,
      # The conventional band, cited rather than calibrated. It was
      # already being applied through d_cut and was invisible.
      magnitude = sca_magnitude(d),
      # Beside the number the classification thresholds: how much of it
      # random pairing alone produces at this n, scope and technique. A
      # condition can clear the conventional cut on an effect size that is
      # mostly the envelope fitting sample extremes, and the label alone
      # would not say so. The cut is unchanged -- moving it to the excess
      # would be inventing a criterion.
      effect_size_null = row$effect_size_null,
      effect_size_excess = row$effect_size_excess,
      p_value = row$p_value,
      beta_cut = beta_cut,
      d_cut = d_cut,
      average_effect_present = average_present,
      sufficiency_present = sufficiency_present,
      configuration = configuration,
      configuration_label = if (is.na(configuration)) {
        NA_character_
      } else {
        unname(.sca_configuration_label[[configuration]])
      },
      reading = if (is.na(configuration)) {
        NA_character_
      } else {
        .sca_configuration_text(configuration, condition, metadata$outcome)
      },
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# The reading templates take the variable names a differing number of times, so
# they are filled by count rather than through one fixed argument list.
.sca_configuration_text <- function(configuration, x, y) {
  template <- unname(.sca_configuration_reading[[configuration]])
  fills <- switch(
    configuration,
    I = c(x, y),
    II = c(x, y),
    III = c(x, y),
    IV = c(x, y)
  )
  do.call(sprintf, c(list(template), as.list(fills)))
}

#' Is the frontier a level shift or a threshold?
#'
#' Places three slopes side by side for the same relationship: the ordinary
#' least-squares slope, the median (quantile regression at `tau = 0.5`) slope,
#' and the slope of the fitted frontier.
#'
#' When all three agree, the frontier is a *location shift*: it is the central
#' tendency moved bodily to the edge of the data, so sufficiency analysis adds
#' a level to what the regression already said and does not add a slope. When
#' the frontier slope departs from the other two, the boundary has a shape of
#' its own and the relationship is not a shift.
#'
#' This is the natural follow-up to configuration II of [sca_compare()], where
#' the average slope is flat but the forbidden corner is empty.
#'
#' Only frontier techniques that fit a line report a slope. A step frontier --
#' `ce_fdh`, `ce_vrs` -- has none by construction, so `frontier_slope` is `NA`
#' for those and the comparison needs a regression-type frontier such as
#' `cr_fdh`.
#'
#' @param model An object returned by [sca_analysis()].
#' @param x Optional condition names or positions.
#' @param ceiling Frontier technique. Defaults to the first estimated.
#' @param tolerance Relative spread among the three slopes at or below which
#'   they are reported as agreeing.
#' @return A data frame with one row per condition.
#' @seealso [sca_compare()]
#' @export
#' @examples
#' iv <- subset(sca_four_configurations, configuration == "IV")
#' fit <- sca_analysis(iv, x = "X", y = "Y", direction = "HH",
#'                     ceilings = "cr_fdh", scope = c(0, 100, 0, 100))
#' sca_locationshift(fit)
sca_locationshift <- function(model, x = NULL, ceiling = NULL,
                              tolerance = 0.10) {
  metadata <- .sca_metadata(model)
  conditions <- .sca_select_conditions(model, x)
  results <- sca_table(model)
  ceiling <- .sca_resolve_ceiling(model, ceiling, unique(results$frontier))

  rows <- lapply(conditions, function(condition) {
    item <- metadata$raw$conditions[[condition]]
    xv <- as.numeric(item$x)
    yv <- as.numeric(item$y)
    keep <- is.finite(xv) & is.finite(yv)
    xv <- xv[keep]
    yv <- yv[keep]

    ols_slope <- NA_real_
    median_slope <- NA_real_
    if (length(xv) >= 2L && stats::var(xv) > 0) {
      ols_slope <- unname(stats::coef(stats::lm(yv ~ xv))[[2L]])
      median_slope <- tryCatch(
        unname(stats::coef(quantreg::rq(yv ~ xv, tau = 0.5))[[2L]]),
        error = function(e) NA_real_
      )
    }

    row <- results[
      results$condition == condition & results$frontier == ceiling, ,
      drop = FALSE
    ]
    frontier_slope <- if (nrow(row) == 0L) NA_real_ else row$slope[[1L]]

    slopes <- c(ols_slope, median_slope, frontier_slope)
    spread <- if (all(is.finite(slopes))) {
      scale <- max(abs(slopes))
      if (scale > 0) (max(slopes) - min(slopes)) / scale else 0
    } else {
      NA_real_
    }

    data.frame(
      condition = condition,
      outcome = metadata$outcome,
      frontier = ceiling,
      observations = length(xv),
      ols_slope = ols_slope,
      median_slope = median_slope,
      frontier_slope = frontier_slope,
      relative_spread = spread,
      tolerance = tolerance,
      location_shift = if (is.finite(spread)) spread <= tolerance else NA,
      reading = if (!is.finite(spread)) {
        paste0(
          "Not assessable: a step frontier has no slope, so compare against a ",
          "regression-type frontier such as cr_fdh."
        )
      } else if (spread <= tolerance) {
        paste0(
          "The three slopes agree. The frontier is the central tendency moved ",
          "to the edge of the data, so the sufficiency analysis adds a level ",
          "rather than a slope."
        )
      } else {
        paste0(
          "The frontier slope departs from the central tendency, so the ",
          "boundary has a shape of its own. This is not a location shift."
        )
      },
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# How far the effect size moves when the scope does.
#
# The scope is the denominator, so it is half of every effect size, and unlike
# the frontier it is not estimated from the data -- somebody chose it. A
# reported d is therefore a claim about the data *and* a claim about the box,
# and nothing in the usual output separates the two. This refits the same
# condition on a series of boxes and shows how much of the number each one was
# carrying.
#
# The widening is expressed as a fraction of the observed span, added to each
# side, because that is the quantity a reader can judge: "a box 50% wider than
# the data on each side" means something, where "scope = c(0, 140)" does not
# unless you already know the data. A `scopes` list overrides it for the case
# where specific theoretical boxes are the point.
.sca_widened_scope <- function(x, y, fraction) {
  x_span <- diff(range(x))
  y_span <- diff(range(y))
  c(
    min(x) - fraction * x_span, max(x) + fraction * x_span,
    min(y) - fraction * y_span, max(y) + fraction * y_span
  )
}

.sca_sensitivity_rows <- function(metadata, condition, ceiling, scopes,
                                  labels) {
  item <- metadata$raw$conditions[[condition]]
  outcome <- metadata$outcome
  frame <- data.frame(item$x, item$y)
  names(frame) <- c(condition, outcome)
  direction <- unname(metadata$direction[[condition]])

  rows <- lapply(seq_along(scopes), function(i) {
    scope <- scopes[[i]]
    fit <- tryCatch(
      suppressWarnings(sca_analysis(
        frame, condition, outcome,
        direction = direction, ceilings = ceiling, scope = scope,
        # The engine settings that change the frontier have to come from the
        # model being tested, or the series compares boxes and techniques at
        # once. `qr.tau` matters only for a `qr` frontier and `cutoff` only
        # for out-of-range thresholds, but both are cheap to carry.
        cutoff = metadata$cutoff,
        qr.tau = if (is.null(metadata$qr.tau)) 0.95 else metadata$qr.tau,
        test.rep = 0
      )),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(NULL)
    }
    row <- sca_table(fit)
    row <- row[row$frontier == ceiling, , drop = FALSE]
    if (nrow(row) == 0L) {
      return(NULL)
    }
    data.frame(
      condition = condition,
      outcome = outcome,
      frontier = ceiling,
      scope = labels[[i]],
      xmin = scope[[1L]], xmax = scope[[2L]],
      ymin = scope[[3L]], ymax = scope[[4L]],
      scope_area = row$scope_area[[1L]],
      empty_space_area = row$empty_space_area[[1L]],
      effect_size = row$effect_size[[1L]],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

#' How much of the effect size is the scope
#'
#' Refits one condition on a series of scopes and reports the effect size each
#' one produces. The scope is the denominator of every effect size and, unlike
#' the frontier, it is chosen rather than estimated, so a reported `d` is a
#' claim about the data and a claim about the box at the same time. This
#' separates them.
#'
#' By default the boxes are the observed range widened by a fraction of the
#' observed span on each side, because that is a quantity a reader can judge:
#' "a box half as wide again as the data" means something where a pair of
#' numbers does not. The first row is always the observed range itself: the
#' box the engine uses when no scope is declared, and the smallest box that
#' contains the data, since any smaller one is widened back. When the model
#' was fitted on a declared scope, that box is added as a final row labelled
#' `"as fitted (declared)"`, so the caller can see where their own choice
#' sits among the alternatives instead of working it out by eye.
#'
#' No column reports a change from a baseline, because which row is the
#' baseline is the caller's question. The observed range answers "how much of
#' this number is box, counting from the smallest box that holds the data";
#' the scope the model was fitted on answers "how sensitive is my number to
#' the box I chose". Those are different questions and the table cannot
#' silently pick one, so it reports the effect sizes and leaves the
#' subtraction to the reader. The data does not change between rows: every
#' difference in `effect_size` down the column is the box.
#'
#' @param model An object returned by [sca_analysis()].
#' @param x A condition name or position. Defaults to the first condition.
#' @param ceiling Frontier technique. Defaults to the first estimated.
#' @param widen Fractions of the observed span to add to each side of each
#'   axis. `0` is the observed range.
#' @param scopes Optional list of explicit `c(xmin, xmax, ymin, ymax)` boxes,
#'   used instead of `widen` when specific theoretical scopes are the point.
#' @return A data frame with one row per scope.
#' @seealso [sca_scope()], [sca_table()]
#' @export
#' @examples
#' fit <- sca_analysis(
#'   subset(sca_four_configurations, configuration == "IV"),
#'   x = "X", y = "Y", direction = "HH", ceilings = "ce_fdh"
#' )
#' sca_scope_sensitivity(fit)
sca_scope_sensitivity <- function(model, x = NULL, ceiling = NULL,
                                  widen = c(0, 0.25, 0.5, 1),
                                  scopes = NULL) {
  metadata <- .sca_metadata(model)
  conditions <- .sca_select_conditions(model, x)
  if (is.null(x)) {
    conditions <- conditions[[1L]]
  }
  if (length(conditions) != 1L) {
    stop("Select exactly one condition for a scope sensitivity analysis.",
         call. = FALSE)
  }
  condition <- conditions[[1L]]
  ceiling <- .sca_resolve_ceiling(model, ceiling, metadata$ceilings)

  item <- metadata$raw$conditions[[condition]]
  if (is.null(item)) {
    stop(sprintf("Unknown condition '%s'.", condition), call. = FALSE)
  }

  if (is.null(scopes)) {
    widen <- suppressWarnings(as.numeric(widen))
    if (length(widen) == 0L || anyNA(widen) || any(widen < 0)) {
      stop("'widen' must be one or more non-negative numbers.", call. = FALSE)
    }
    widen <- sort(unique(widen))
    scopes <- lapply(widen, function(f) .sca_widened_scope(item$x, item$y, f))
    labels <- ifelse(
      widen == 0,
      "observed range",
      sprintf("observed + %g%% of span per side", 100 * widen)
    )
    # The box this model was actually fitted on, when one was declared.
    # Without it the series answers a question the caller did not ask.
    declared <- metadata$scope_used[[condition]]
    if (identical(metadata$scope_source, "declared") && !is.null(declared)) {
      scopes <- c(scopes, list(suppressWarnings(as.numeric(declared))))
      labels <- c(labels, "as fitted (declared)")
    }
  } else {
    if (!is.list(scopes) || length(scopes) == 0L) {
      stop("'scopes' must be a non-empty list of c(xmin, xmax, ymin, ymax).",
           call. = FALSE)
    }
    scopes <- lapply(scopes, function(s) suppressWarnings(as.numeric(s)))
    bad <- vapply(scopes, function(s) length(s) != 4L || anyNA(s), logical(1L))
    if (any(bad)) {
      stop("Every entry of 'scopes' must be four finite numbers.",
           call. = FALSE)
    }
    labels <- names(scopes)
    if (is.null(labels) || !all(nzchar(labels))) {
      labels <- vapply(
        scopes,
        function(s) paste(format(s), collapse = ", "),
        character(1L)
      )
    }
  }

  out <- .sca_sensitivity_rows(metadata, condition, ceiling, scopes, labels)
  if (is.null(out) || nrow(out) == 0L) {
    stop(
      sprintf("No scope in the series could be fitted for '%s'.", condition),
      call. = FALSE
    )
  }

  # No derived share, deliberately.
  #
  # An earlier version reported the change from the first row and that change
  # as a fraction of the effect size, which meant choosing a baseline on the
  # caller's behalf: the observed range answers "how much of this number is
  # box, counting from the smallest box that holds the data", and the scope
  # the model was fitted on answers "how sensitive is my number to the box I
  # chose". Those are different questions and the table cannot silently pick
  # one. The effect sizes are here; subtracting two of them is the reader's
  # decision, and it is the same reason this package reports an interval
  # rather than a verdict elsewhere.
  row.names(out) <- NULL
  out
}
