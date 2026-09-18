#' Generate data with a directional sufficiency pattern
#'
#' @param n Number of observations.
#' @param intercepts Intercept or vector of intercepts for frontier lines.
#' @param slopes Slope or vector of slopes for frontier lines.
#' @param direction One sufficiency direction: `"HH"`, `"LH"`, `"HL"`, or
#'   `"LL"`.
#' @param distribution.x,distribution.y Marginal distribution, `"uniform"` or
#'   `"normal"`.
#' @param mean.x,mean.y Means for truncated normal distributions.
#' @param sd.x,sd.y Standard deviations for truncated normal distributions.
#' @return A data frame with X condition columns and one Y outcome column.
#' @export
sca_random <- function(
    n,
    intercepts,
    slopes,
    direction = "HH",
    distribution.x = "uniform",
    distribution.y = "uniform",
    mean.x = 0.5,
    mean.y = 0.5,
    sd.x = 0.2,
    sd.y = 0.2) {
  direction <- .sca_validate_direction(direction, 1L)
  corner <- sca_corner(direction)
  data <- NCA::nca_random(
    n = n,
    intercepts = intercepts,
    slopes = slopes,
    corner = corner,
    distribution.x = distribution.x,
    distribution.y = distribution.y,
    mean.x = mean.x,
    mean.y = mean.y,
    sd.x = sd.x,
    sd.y = sd.y
  )
  attr(data, "sca_direction") <- direction
  attr(data, "empty_corner") <- corner
  data
}

# A frontier line can only bound two of the four corners. A rising line bounds
# the upper-left and lower-right corners (1 and 4, so HH and LL); a falling one
# bounds the upper-right and lower-left corners (2 and 3, so LH and HL). A
# horizontal line is accepted for any corner, as it is by the engine.
#
# sca_random() inherits the engine's own error for the impossible combination,
# but nca_power() only warns and returns NULL, which is silent by the time the
# result reaches sca_powerplot(). Refusing it here keeps the two functions
# behaving alike and names the direction that caused it.
.sca_check_slope <- function(slope, direction, corner) {
  values <- suppressWarnings(as.numeric(slope))
  if (length(values) == 0L || anyNA(values)) {
    stop("'slope' must be one or more finite numbers.", call. = FALSE)
  }
  falling <- corner %in% c(2L, 3L)
  wrong <- if (falling) values > 0 else values < 0
  if (!any(wrong)) {
    return(invisible(slope))
  }
  stop(
    sprintf(
      paste0(
        "Direction \"%s\" is empty corner %d, which only a %s frontier can ",
        "bound, but 'slope' contains %s. Use a %s slope for %s."
      ),
      direction, corner,
      if (falling) "falling" else "rising",
      paste(format(values[wrong], trim = TRUE), collapse = ", "),
      if (falling) "negative or zero" else "positive or zero",
      if (falling) "LH and HL" else "HH and LL"
    ),
    call. = FALSE
  )
}

#' Estimate power for a directional sufficiency analysis
#'
#' The frontier slope has to be able to bound the corner the direction
#' implies. A rising line bounds the upper-left and lower-right corners, so it
#' suits `HH` and `LL`; a falling line bounds the upper-right and lower-left
#' corners, so it suits `LH` and `HL`. A horizontal line is accepted for every
#' direction, as it is by the engine.
#'
#' @param n Sample size or vector of sample sizes.
#' @param effect Population empty-space effect size(s).
#' @param slope Frontier slope(s). The sign must match the direction: zero or
#'   positive for `HH` and `LL`, zero or negative for `LH` and `HL`.
#' @param ceiling Frontier method(s).
#' @param direction One sufficiency direction.
#' @param p Significance level.
#' @param distribution.x,distribution.y Marginal distribution(s).
#' @param rep Number of simulated datasets per design cell.
#' @param test.rep Number of permutations in each simulated analysis.
#' @return A data frame of estimated power values with SCA direction metadata.
#' @export
sca_power <- function(
    n = c(20, 50, 100),
    effect = 0.10,
    slope = 1,
    ceiling = "ce_fdh",
    direction = "HH",
    p = 0.05,
    distribution.x = "uniform",
    distribution.y = "uniform",
    rep = 100,
    test.rep = 200) {
  direction <- .sca_validate_direction(direction, 1L)
  corner <- sca_corner(direction)
  .sca_check_slope(slope, direction, corner)
  result <- NCA::nca_power(
    n = n,
    effect = effect,
    slope = slope,
    ceiling = ceiling,
    corner = corner,
    p = p,
    distribution.x = distribution.x,
    distribution.y = distribution.y,
    rep = rep,
    test.rep = test.rep
  )
  if (!is.null(result)) {
    result$direction <- direction
    result$empty_corner <- corner
  }
  result
}

#' Plot SCA power results
#'
#' @param df_power Data frame returned by [sca_power()].
#' @param x.variable Column mapped to the x axis.
#' @param x.name X-axis label.
#' @param x.min,x.max X-axis limits.
#' @param line.variable Column mapped to line colour.
#' @param line.name Legend title.
#' @return A `ggplot` object.
#' @export
sca_powerplot <- function(
    df_power,
    x.variable = "n",
    x.name = "Sample size",
    x.min = 0,
    x.max = NULL,
    line.variable = "ES",
    line.name = "Effect size") {
  required <- c("power", x.variable, line.variable)
  missing <- setdiff(required, colnames(df_power))
  if (length(missing) > 0L) {
    stop(sprintf("Missing power-result column(s): %s.", paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  if (is.null(x.min)) {
    x.min <- min(df_power[[x.variable]], na.rm = TRUE)
  }
  if (is.null(x.max)) {
    x.max <- max(df_power[[x.variable]], na.rm = TRUE)
  }
  plot_data <- df_power
  plot_data$.sca_x <- plot_data[[x.variable]]
  plot_data$.sca_line <- as.factor(plot_data[[line.variable]])

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .sca_x, y = power, colour = .sca_line, group = .sca_line)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::coord_cartesian(xlim = c(x.min, x.max), ylim = c(0, 1)) +
    ggplot2::labs(x = x.name, y = "Power", colour = line.name) +
    ggplot2::theme_minimal(base_size = 11)
}
