#' Normalize numeric variables for SCA
#'
#' @param data Input data frame or matrix.
#' @param scale Output minimum-maximum pair, or one pair per column.
#' @param min_max Optional empirical or theoretical minimum-maximum pair, or one
#'   pair per column.
#' @return Data on the requested scales.
#' @export
#' @examples
#' dat <- data.frame(X = c(0, 2, 4, 10), Y = c(10, 20, 30, 50))
#' sca_normalize(dat)
sca_normalize <- function(data, scale = NULL, min_max = NULL) {
  NCA::nca_util_normalize(data = data, scale = scale, min_max = min_max)
}

# One condition's influential observations, with the condition named.
#
# `NCA::nca_outliers()` is given one column at a time and returns a table with
# no column saying which one it was, because at that level there is only one.
# A model holds several, so the name has to be carried alongside -- and once
# the model form carries it the data-frame form has to as well, or the same
# function returns two different shapes.
.sca_outliers_one <- function(condition, data, x, y, corner, ceiling, scope, k,
                              min.dif, max.results, condensed) {
  result <- NCA::nca_outliers(
    data = data,
    x = x,
    y = y,
    ceiling = ceiling,
    corner = corner,
    scope = scope,
    k = k,
    min.dif = min.dif,
    max.results = max.results,
    plotly = FALSE,
    condensed = condensed
  )
  if (is.null(result) || nrow(result) == 0L) {
    return(NULL)
  }
  cbind(
    data.frame(condition = condition, stringsAsFactors = FALSE),
    as.data.frame(result, stringsAsFactors = FALSE)
  )
}

# The declared scope belonging to one condition.
#
# `.sca_scope_segments()` returns NULL for a single four-number scope, because
# at fitting time that case needs no splitting: every condition gets the same
# box. Here it does need splitting, since the engine is called once per
# condition, so the single vector is handed back for each of them.
.sca_scope_for <- function(scope, index, n) {
  if (is.null(scope)) {
    return(NULL)
  }
  segments <- .sca_scope_segments(scope, n)
  if (!is.null(segments)) {
    return(segments[[index]])
  }
  suppressWarnings(as.numeric(scope))
}

# One block per condition, and they need not have the same columns.
#
# `NCA::nca_outliers()` drops its `scope` column when a theoretical scope was
# declared. Every condition here draws its scope from the same model, so in
# practice they agree -- but a declared scope can be given per condition, which
# means one condition can sit on a declared box and another on the observed
# range, and then they genuinely differ. That is a property of the fit rather
# than an interface change, so it is repaired rather than reported: the union
# of the columns, with the missing cells left NA.
.sca_stack_outliers <- function(blocks) {
  if (length(blocks) == 1L) {
    out <- blocks[[1L]]
    row.names(out) <- NULL
    return(out)
  }
  columns <- unique(unlist(lapply(blocks, names), use.names = FALSE))
  filled <- lapply(blocks, function(block) {
    for (column in setdiff(columns, names(block))) {
      block[[column]] <- NA
    }
    block[columns]
  })
  out <- do.call(rbind, filled)
  row.names(out) <- NULL
  out
}

#' Diagnose observations that influence an SCA empty-space effect
#'
#' Which observations hold up the frontier, and how far the effect size would
#' move without them. A frontier is fixed by the most extreme cases rather than
#' by the central tendency, so a handful of observations can carry a whole
#' claim; this says how few.
#'
#' @section Two ways to call it:
#' Pass a fitted model and the condition, outcome, direction and scope are
#' taken from it, so the diagnostic cannot quietly describe a different
#' analysis from the one being reported:
#'
#' ```
#' fit <- sca_analysis(data, x = c("A", "B"), y = "Y", direction = "HH")
#' sca_outliers(fit)
#' ```
#'
#' Every condition in the model is examined unless `x` selects some. Passing
#' `y`, `direction` or `scope` alongside a model is an error rather than an
#' override: the model fixes all three already, and a second answer for any of
#' them would produce a table that does not describe the fit it came from.
#'
#' Passing a data frame instead works as it always has, for the case where
#' there is no fitted model to hand.
#'
#' @section Reading it:
#' This is a fragility check, not a cleaning step. An observation that moves
#' the frontier is doing what the method says it should -- frontiers are fixed
#' by extreme cases -- so finding one is not grounds for removing it. What the
#' table says is how much of the claim rests on how few cases, which belongs in
#' the write-up either way.
#'
#' @param data A fitted [sca_analysis()] model, or a data frame.
#' @param x With a model, optional condition names or positions; all conditions
#'   by default. With a data frame, one condition column selected by name or
#'   position.
#' @param y One outcome column selected by name or position. Taken from the
#'   model when one is supplied, and an error if given as well.
#' @param direction One sufficiency direction. Taken from the model when one is
#'   supplied, and an error if given as well.
#' @param ceiling Frontier technique. CE-FDH is used when omitted; with a
#'   model, the first technique that model estimated.
#' @param scope Optional theoretical scope. Taken from the model when one is
#'   supplied, and an error if given as well.
#' @param k Number of observations considered jointly.
#' @param min.dif Minimum relative effect-size difference.
#' @param max.results Maximum number of rows returned.
#' @param condensed Suppress redundant combinations when `k > 1`.
#' @return An outlier-sensitivity table whose first column names the condition,
#'   or `NULL` when no observation reaches `min.dif`.
#' @seealso [sca_analysis()], [sca_compare()]
#' @export
#' @examples
#' iv <- subset(sca_four_configurations, configuration == "IV")
#' fit <- sca_analysis(iv[seq(1, nrow(iv), by = 5), ], x = "X", y = "Y",
#'                     direction = "HH", ceilings = "ce_fdh",
#'                     scope = c(0, 100, 0, 100))
#' sca_outliers(fit)
sca_outliers <- function(
    data,
    x = NULL,
    y = NULL,
    direction = "HH",
    ceiling = NULL,
    scope = NULL,
    k = 1,
    min.dif = 1e-2,
    max.results = 25,
    condensed = FALSE) {
  if (inherits(data, "sca_result")) {
    # A model answers y, direction and scope already. Letting the argument win
    # would head the table with a fit it does not describe, and accepting the
    # argument and ignoring it would be worse, so both are refused by name.
    given <- c(
      if (!is.null(y)) "y",
      if (!missing(direction)) "direction",
      if (!is.null(scope)) "scope"
    )
    if (length(given) > 0L) {
      stop(
        sprintf(
          paste0(
            "%s cannot be supplied alongside a fitted model: the model fixes ",
            "%s already, and a second answer would describe a different ",
            "analysis. Drop the argument, or call sca_outliers() on the data ",
            "frame instead."
          ),
          paste(sprintf("'%s'", given), collapse = " and "),
          if (length(given) == 1L) "it" else "them"
        ),
        call. = FALSE
      )
    }

    metadata <- .sca_metadata(data)
    conditions <- .sca_select_conditions(data, x)
    ceiling <- .sca_resolve_ceiling(data, ceiling, metadata$ceilings)
    outcome <- metadata$outcome
    available <- names(metadata$raw$conditions)

    blocks <- lapply(conditions, function(condition) {
      item <- metadata$raw$conditions[[condition]]
      frame <- data.frame(
        as.numeric(item$x), as.numeric(item$y),
        stringsAsFactors = FALSE
      )
      names(frame) <- c(condition, outcome)
      .sca_outliers_one(
        condition = condition,
        data = frame, x = condition, y = outcome,
        corner = unname(metadata$empty_corner[[condition]]),
        ceiling = ceiling,
        scope = .sca_scope_for(
          metadata$scope_declared,
          match(condition, available),
          length(available)
        ),
        k = k, min.dif = min.dif, max.results = max.results,
        condensed = condensed
      )
    })
    blocks <- Filter(Negate(is.null), blocks)
    if (length(blocks) == 0L) {
      return(NULL)
    }
    result <- .sca_stack_outliers(blocks)
    attr(result, "sca_direction") <- paste(
      unique(unname(metadata$direction[conditions])), collapse = ", "
    )
    attr(result, "empty_corner") <- paste(
      unique(unname(metadata$empty_corner[conditions])), collapse = ", "
    )
    class(result) <- unique(c("sca_outliers", class(result)))
    return(result)
  }

  if (is.null(x) || is.null(y)) {
    stop(
      "'x' and 'y' are required when the first argument is a data frame.",
      call. = FALSE
    )
  }
  direction <- .sca_validate_direction(direction, 1L)
  corner <- sca_corner(direction)
  frame <- as.data.frame(data, stringsAsFactors = FALSE)
  condition <- if (is.numeric(x)) names(frame)[[x[[1L]]]] else as.character(x)[[1L]]
  result <- .sca_outliers_one(
    condition = condition,
    data = frame, x = x, y = y, corner = corner, ceiling = ceiling,
    scope = scope, k = k, min.dif = min.dif, max.results = max.results,
    condensed = condensed
  )
  if (!is.null(result)) {
    attr(result, "sca_direction") <- direction
    attr(result, "empty_corner") <- corner
    class(result) <- unique(c("sca_outliers", class(result)))
  }
  result
}

#' @export
print.sca_outliers <- function(x, ...) {
  cat(
    "\nSCA outlier sensitivity: direction ", attr(x, "sca_direction"),
    "; physical empty corner ", attr(x, "empty_corner"), "\n",
    sep = ""
  )
  NextMethod()
  invisible(x)
}

