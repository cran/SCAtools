# How much the effect size moves when the sample does.
#
# The deferred item this answers was written down as "bootstrap confidence
# intervals", and it deliberately does not produce one. Two reasons, both
# specific to frontier estimators rather than general caution:
#
# 1. The ordinary n-out-of-n bootstrap is inconsistent for boundary estimators.
#    Resampling with replacement duplicates points, and a duplicated extreme
#    cannot push the envelope further out, so the resampled frontier sits
#    inside the original far more often than chance -- the resampling
#    distribution does not converge to the sampling distribution. This is the
#    standard result for DEA and FDH efficiency scores, and the standard
#    remedies are subsampling or a smoothed bootstrap.
#
# 2. Subsampling fixes consistency but not the bias, and here the bias has a
#    direction. A subsample of size m < n has a *higher* expected effect size
#    than the full sample, because the envelope is built from sample extremes
#    and fewer points leave more of the corner empty. So a percentile interval
#    taken straight from the subsample distribution sits systematically above
#    the quantity it is supposed to cover. Correcting for that needs the
#    estimator's convergence rate, which depends on the technique and the
#    dimension, and which this package will not assert.
#
# What is left after removing the part that cannot be justified is still the
# useful part: refit on subsamples of several sizes and show what happens. The
# spread at each size is the sampling variability. The trend of the median
# across sizes is the bias, visible rather than modelled -- it falls as m rises
# because there is less corner left to be empty by accident. A reader who sees
# 0.48 at m = 30 and 0.37 at m = 120 knows what their own n buys them without
# anyone having to supply a constant.
#
# Same restraint as sca_scope_sensitivity(), which reports the effect size on
# every box and refuses to subtract two of them for you, and for the same
# reason: the subtraction encodes a question the caller has not been asked.

.sca_resample_sizes <- function(sizes, n) {
  sizes <- suppressWarnings(as.numeric(sizes))
  if (length(sizes) == 0L || anyNA(sizes) || any(sizes <= 0)) {
    stop("'sizes' must be one or more positive numbers.", call. = FALSE)
  }
  # A value at or below 1 is a fraction of n; above 1 it is a count, so that
  # a caller who knows the sample sizes they want can state them directly.
  m <- ifelse(sizes <= 1, round(sizes * n), round(sizes))
  m <- sort(unique(pmin(pmax(m, 3L), n)))
  if (length(m) == 0L) {
    stop("No usable subsample size.", call. = FALSE)
  }
  as.integer(m)
}

.sca_resample_one <- function(frame, condition, outcome, direction, ceiling,
                              scope, cutoff, qr.tau, index) {
  fit <- tryCatch(
    suppressWarnings(suppressMessages(sca_analysis(
      frame[index, , drop = FALSE], condition, outcome,
      direction = direction, ceilings = ceiling,
      # The scope is pinned to the full sample's. Letting it be recomputed
      # would shrink the box along with the data, and the table would then be
      # showing the frontier and the denominator moving at once -- the very
      # confusion sca_scope_sensitivity() exists to separate.
      scope = scope,
      cutoff = cutoff, qr.tau = qr.tau, test.rep = 0
    ))),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(NA_real_)
  }
  row <- sca_table(fit)
  row <- row[row$frontier == ceiling, , drop = FALSE]
  if (nrow(row) == 0L) {
    return(NA_real_)
  }
  row$effect_size[[1L]]
}

#' How much the effect size moves when the sample does
#'
#' Refits one condition on random subsamples of several sizes and reports the
#' distribution of the effect size at each size. Two things become visible: how
#' far `d` moves under resampling at a given sample size, and how it trends as
#' the sample grows.
#'
#' @section Why there is no confidence interval:
#' Deliberately, and for reasons specific to frontier estimators rather than
#' general caution.
#'
#' The ordinary n-out-of-n bootstrap is *inconsistent* here. Resampling with
#' replacement duplicates observations, and a duplicated extreme point cannot
#' push the envelope further out than the original did, so the resampled
#' frontier lies inside the fitted one far more often than chance -- the
#' resampling distribution does not converge to the sampling distribution. This
#' is the familiar result for DEA and FDH efficiency scores.
#'
#' Subsampling without replacement, which is what this function does, restores
#' consistency but leaves a bias with a known direction: a subsample of size
#' `m < n` has a *higher* expected effect size than the full sample, because
#' the envelope is built from sample extremes and fewer points leave more of
#' the corner empty by accident. A percentile interval read straight off the
#' subsample distribution would therefore sit above the quantity it claims to
#' cover. Correcting for that requires the estimator's convergence rate, which
#' depends on the technique and the dimension and which this package does not
#' assert.
#'
#' So the table reports the distribution and stops there.
#'
#' @section Reading it:
#' `effect_size` is the full-sample value, repeated on every row as the anchor.
#' `subsample_median` and the two percentiles describe the subsample
#' distribution at that size.
#'
#' The *spread* at one size answers "how much does my `d` depend on which cases
#' I happened to collect". The *trend* down the `m` column answers "how much of
#' my `d` is the sample being small" -- it falls as `m` rises. Compare it with
#' `effect_size_null` from [sca_table()], which measures the same overstatement
#' a different way, by random pairing at the full sample size.
#'
#' @section Cost:
#' One refit per replication per size. The default is 200 by 3, so a call is
#' six hundred fits of one condition; nothing is cached between them.
#'
#' @param model An object returned by [sca_analysis()].
#' @param x A condition name or position. Defaults to the first condition.
#' @param ceiling Frontier technique. Defaults to the first estimated.
#' @param sizes Subsample sizes. A value at or below 1 is a fraction of `n`,
#'   above 1 a count.
#' @param reps Subsamples drawn per size.
#' @param seed Optional seed. The caller's random stream is restored
#'   afterwards, as everywhere else in this package.
#' @return A data frame with one row per subsample size.
#' @seealso [sca_scope_sensitivity()], [sca_outliers()], [sca_table()]
#' @export
#' @examples
#' iv <- subset(sca_four_configurations, configuration == "IV")
#' fit <- sca_analysis(iv[seq(1, nrow(iv), by = 5), ], x = "X", y = "Y",
#'                     direction = "HH", ceilings = "ce_fdh",
#'                     scope = c(0, 100, 0, 100))
#' # A report would use the default reps = 200
#' sca_resample(fit, reps = 20, seed = 1)
sca_resample <- function(model, x = NULL, ceiling = NULL,
                         sizes = c(0.4, 0.6, 0.8), reps = 200, seed = NULL) {
  metadata <- .sca_metadata(model)
  conditions <- .sca_select_conditions(model, x)
  if (is.null(x)) {
    conditions <- conditions[[1L]]
  }
  if (length(conditions) != 1L) {
    stop("Select exactly one condition for a resampling analysis.",
         call. = FALSE)
  }
  condition <- conditions[[1L]]
  ceiling <- .sca_resolve_ceiling(model, ceiling, metadata$ceilings)

  reps <- suppressWarnings(as.integer(reps)[[1L]])
  if (!isTRUE(is.finite(reps) && reps >= 1L)) {
    stop("'reps' must be a positive whole number.", call. = FALSE)
  }

  item <- metadata$raw$conditions[[condition]]
  xv <- as.numeric(item$x)
  yv <- as.numeric(item$y)
  keep <- is.finite(xv) & is.finite(yv)
  xv <- xv[keep]
  yv <- yv[keep]
  n <- length(xv)
  if (n < 4L) {
    stop("Too few complete observations to resample.", call. = FALSE)
  }

  frame <- data.frame(xv, yv, stringsAsFactors = FALSE)
  names(frame) <- c(condition, metadata$outcome)
  outcome <- metadata$outcome
  direction <- unname(metadata$direction[[condition]])
  # The box every subsample is measured against: the one the model itself
  # used, after any widening the engine applied.
  scope <- unname(metadata$scope_used[[condition]])
  qr.tau <- if (is.null(metadata$qr.tau)) 0.95 else metadata$qr.tau

  full <- sca_table(model)
  full <- full[full$condition == condition &
                 full$frontier == ceiling, , drop = FALSE]
  observed <- if (nrow(full) == 0L) NA_real_ else full$effect_size[[1L]]

  m_values <- .sca_resample_sizes(sizes, n)

  rows <- .sca_with_seed(seed, lapply(m_values, function(m) {
    # At m = n there is exactly one subsample without replacement, so drawing
    # `reps` of them would report a spread of zero as though it had been
    # estimated. One draw, and the row says reps = 1.
    draws <- if (m >= n) 1L else reps
    values <- vapply(seq_len(draws), function(i) {
      .sca_resample_one(
        frame, condition, outcome, direction, ceiling, scope,
        metadata$cutoff, qr.tau, sample.int(n, m)
      )
    }, numeric(1L))
    usable <- values[is.finite(values)]

    data.frame(
      condition = condition,
      outcome = outcome,
      frontier = ceiling,
      n = n,
      m = m,
      fraction = m / n,
      reps = draws,
      converged = length(usable),
      effect_size = observed,
      subsample_median = if (length(usable)) stats::median(usable) else NA_real_,
      subsample_mean = if (length(usable)) mean(usable) else NA_real_,
      subsample_sd = if (length(usable) > 1L) stats::sd(usable) else NA_real_,
      subsample_p05 = if (length(usable)) {
        unname(stats::quantile(usable, 0.05, names = FALSE))
      } else NA_real_,
      subsample_p95 = if (length(usable)) {
        unname(stats::quantile(usable, 0.95, names = FALSE))
      } else NA_real_,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
