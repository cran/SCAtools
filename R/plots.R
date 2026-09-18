.sca_select_conditions <- function(model, x) {
  available <- names(model$plots)
  if (is.null(x)) {
    return(available)
  }
  if (is.numeric(x)) {
    x <- available[x]
  }
  unknown <- setdiff(x, available)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown condition(s): %s.", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  x
}

.sca_valid_line <- function(line) {
  is.matrix(line) && ncol(line) == 2L && nrow(line) > 1L && all(is.finite(line))
}

.sca_line_data <- function(plot_data, ceilings) {
  rows <- list()
  id <- 0L
  corner <- plot_data$empty_corner
  for (ceiling in ceilings) {
    line <- plot_data$line_matrices[[ceiling]]
    if (!.sca_valid_line(line)) {
      next
    }
    order_y <- if (corner %in% c(2L, 3L)) -line[, 2L] else line[, 2L]
    line <- line[order(line[, 1L], order_y), , drop = FALSE]
    id <- id + 1L
    rows[[id]] <- data.frame(
      x = line[, 1L],
      y = line[, 2L],
      method = ceiling,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(data.frame(x = numeric(), y = numeric(), method = character()))
  }
  do.call(rbind, rows)
}

.sca_empty_polygon <- function(plot_data, ceiling) {
  line <- plot_data$line_matrices[[ceiling]]
  if (!.sca_valid_line(line)) {
    return(NULL)
  }
  corner <- plot_data$empty_corner
  order_y <- if (corner %in% c(2L, 3L)) -line[, 2L] else line[, 2L]
  line <- line[order(line[, 1L], order_y), , drop = FALSE]
  scope <- plot_data$scope.theo
  boundary_y <- if (corner %in% c(1L, 2L)) scope[[4L]] else scope[[3L]]
  rbind(
    data.frame(x = line[, 1L], y = line[, 2L]),
    data.frame(x = c(scope[[2L]], scope[[1L]]), y = boundary_y)
  )
}

.sca_plot_one <- function(model, condition, ceilings, shade, annotate) {
  metadata <- .sca_metadata(model)
  plot_data <- model$plots[[condition]]
  available <- names(plot_data$line_matrices)
  if (is.null(ceilings)) {
    ceilings <- available
  }
  ceilings <- intersect(ceilings, available)
  if (length(ceilings) == 0L) {
    stop(sprintf("No requested frontier is available for '%s'.", condition), call. = FALSE)
  }

  points <- data.frame(
    x = as.numeric(plot_data$x),
    y = as.numeric(plot_data$y)
  )
  lines <- .sca_line_data(plot_data, ceilings)
  scope <- plot_data$scope.theo
  direction <- metadata$direction[[condition]]
  corner <- metadata$empty_corner[[condition]]

  chart <- ggplot2::ggplot(points, ggplot2::aes(x = x, y = y))
  if (shade && nrow(lines) > 0L) {
    exact_frontiers <- intersect(c("ce_fdh", "ce_vrs"), ceilings)
    shade_ceiling <- if (length(exact_frontiers) > 0L) exact_frontiers[[1L]] else NULL
    polygon <- if (!is.null(shade_ceiling)) {
      .sca_empty_polygon(plot_data, shade_ceiling)
    } else {
      NULL
    }
    if (!is.null(polygon)) {
      chart <- chart + ggplot2::geom_polygon(
        data = polygon,
        mapping = ggplot2::aes(x = x, y = y),
        inherit.aes = FALSE,
        fill = "#D9ECFF",
        colour = NA,
        alpha = 0.65
      )
    }
  }
  chart <- chart +
    ggplot2::geom_point(shape = 21, colour = "#174A7E", fill = "white", size = 2) +
    ggplot2::geom_vline(xintercept = scope[1:2], linetype = "dashed", colour = "grey65") +
    ggplot2::geom_hline(yintercept = scope[3:4], linetype = "dashed", colour = "grey65")

  if (nrow(lines) > 0L) {
    chart <- chart + ggplot2::geom_line(
      data = lines,
      mapping = ggplot2::aes(x = x, y = y, colour = method, linetype = method),
      inherit.aes = FALSE,
      linewidth = 0.8
    )
  }

  if (annotate) {
    dx <- diff(scope[1:2])
    dy <- diff(scope[3:4])
    label_x <- if (corner %in% c(1L, 3L)) scope[[1L]] + 0.03 * dx else scope[[2L]] - 0.03 * dx
    label_y <- if (corner %in% c(1L, 2L)) scope[[4L]] - 0.03 * dy else scope[[3L]] + 0.03 * dy
    chart <- chart + ggplot2::annotate(
      "label",
      x = label_x,
      y = label_y,
      label = sprintf("Empty corner %d\n%s", corner, direction),
      hjust = if (corner %in% c(1L, 3L)) 0 else 1,
      vjust = if (corner %in% c(1L, 2L)) 1 else 0,
      size = 3.2,
      fill = "white"
    )
  }

  # A central-tendency line drawn beside the frontiers is not a frontier, so
  # the legend cannot go on calling everything one.
  legend_title <- if (any(lines$method %in% .sca_reference_lines)) {
    "Line"
  } else {
    "Frontier"
  }

  chart +
    ggplot2::coord_cartesian(xlim = scope[1:2], ylim = scope[3:4]) +
    ggplot2::labs(
      x = condition,
      y = metadata$outcome,
      title = paste0("SCA: ", metadata$statement[[condition]]),
      subtitle = sprintf(
        "Logical direction %s (SCA %d); physical empty corner %d (%s)",
        direction,
        metadata$sca_number[[condition]],
        corner,
        .sca_corner_location[[as.character(corner)]]
      ),
      colour = legend_title,
      linetype = legend_title,
      caption = "The frontier is an empirical empty-space pattern; causal sufficiency requires separate justification."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "grey35", fill = NA),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
}

#' Plot SCA frontiers
#'
#' @param model An object returned by [sca_analysis()].
#' @param x Optional condition names or positions. All conditions are selected
#'   by default.
#' @param ceilings Optional frontier methods to draw.
#' @param shade Shade the empty zone under an exact CE-FDH or CE-VRS frontier.
#' @param annotate Label the physical empty corner and logical direction.
#' @return A `ggplot` for one condition or a named list of plots for multiple
#'   conditions.
#' @export
sca_plot <- function(model, x = NULL, ceilings = NULL, shade = TRUE, annotate = TRUE) {
  conditions <- .sca_select_conditions(model, x)
  plots <- lapply(
    conditions,
    function(condition) .sca_plot_one(model, condition, ceilings, shade, annotate)
  )
  names(plots) <- conditions
  if (length(plots) == 1L) plots[[1L]] else plots
}

#' @export
plot.sca_result <- function(x, ...) {
  plots <- sca_plot(x, ...)
  if (inherits(plots, "ggplot")) {
    print(plots)
  } else {
    invisible(lapply(plots, print))
  }
  invisible(plots)
}

#' Plot an SCA permutation test
#'
#' @param model An object returned by [sca_analysis()] with `test.rep > 0`.
#' @param x A condition name or position. Defaults to the first condition.
#' @param ceiling Frontier method.
#' @param bins Number of histogram bins.
#' @return A `ggplot` object.
#' @export
sca_test_plot <- function(model, x = NULL, ceiling = "ce_fdh", bins = 30) {
  metadata <- .sca_metadata(model)
  if (is.null(x)) {
    x <- names(model$plots)[[1L]]
  }
  condition <- .sca_select_conditions(model, x)
  if (length(condition) != 1L) {
    stop("Select exactly one condition for a permutation-test plot.", call. = FALSE)
  }
  condition <- condition[[1L]]
  if (is.null(model$tests[[condition]]) ||
      is.null(model$tests[[condition]][[ceiling]])) {
    stop(
      sprintf("No permutation test for '%s' with frontier '%s'.", condition, ceiling),
      call. = FALSE
    )
  }
  test <- model$tests[[condition]][[ceiling]]
  values <- data.frame(effect_size = as.numeric(test$data))
  p_value <- if (!is.null(test$p_value)) test$p_value else NA_real_

  ggplot2::ggplot(values, ggplot2::aes(x = effect_size)) +
    ggplot2::geom_histogram(bins = bins, fill = "white", colour = "grey25") +
    ggplot2::geom_vline(
      xintercept = test$observed,
      colour = "#B22222",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      x = "Effect size under permutation",
      y = "Frequency",
      title = paste0("SCA permutation test: ", metadata$statement[[condition]]),
      subtitle = sprintf(
        "%s; observed d = %.3f, p = %.3f",
        ceiling, test$observed, p_value
      )
    ) +
    ggplot2::theme_minimal(base_size = 11)
}
