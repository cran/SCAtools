#' Normalize numeric variables for SCA
#'
#' @param data Input data frame or matrix.
#' @param scale Output minimum-maximum pair, or one pair per column.
#' @param min_max Optional empirical or theoretical minimum-maximum pair, or one
#'   pair per column.
#' @return Data on the requested scales.
#' @export
sca_normalize <- function(data, scale = NULL, min_max = NULL) {
  NCA::nca_util_normalize(data = data, scale = scale, min_max = min_max)
}

#' Diagnose observations that influence an SCA empty-space effect
#'
#' @param data A data frame or object coercible to a data frame.
#' @param x One condition column selected by name or position.
#' @param y One outcome column selected by name or position.
#' @param direction One sufficiency direction.
#' @param ceiling Frontier technique. CE-FDH is used when omitted.
#' @param scope Optional theoretical scope.
#' @param k Number of observations considered jointly.
#' @param min.dif Minimum relative effect-size difference.
#' @param max.results Maximum number of rows returned.
#' @param condensed Suppress redundant combinations when `k > 1`.
#' @return An outlier-sensitivity table, or `NULL` when none is identified.
#' @export
sca_outliers <- function(
    data,
    x,
    y,
    direction = "HH",
    ceiling = NULL,
    scope = NULL,
    k = 1,
    min.dif = 1e-2,
    max.results = 25,
    condensed = FALSE) {
  direction <- .sca_validate_direction(direction, 1L)
  corner <- sca_corner(direction)
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

