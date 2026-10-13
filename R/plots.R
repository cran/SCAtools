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
  # An empty selection is a mistake, not a request for nothing. NULL already
  # means "all of them", so character(0) can only have come from a filter that
  # matched nothing -- and it used to be carried silently to the end, where
  # sca_reference() returned NULL and sca_thresholds() turned that NULL into a
  # list and then failed with "incorrect number of dimensions". Neither message
  # mentions the selection.
  if (length(x) == 0L) {
    stop(
      sprintf(
        paste0(
          "'x' selected no conditions. Leave it NULL for all of them, or ",
          "choose from: %s."
        ),
        paste(available, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  x
}

# ggplot2 wraps neither a title nor a caption, and a caption is right-aligned
# by default, so a long line loses its end on one and its beginning on the
# other, silently and only in the exported file. Wrapped here rather than left
# to the caller to guess an export width. Kept identical to NSCA's
# .nsca_wrap(): the two packages' plots appear in the same figure.
.sca_wrap <- function(text, width = 95L) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    return(text)
  }
  paste(strwrap(text, width = width), collapse = "\n")
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

  # The central-tendency lines, drawn outside the frontier legend on purpose:
  # they belong to neither frontier, and colouring them as one would put an
  # average effect and an empty-space boundary in the same key. Computed by
  # this package rather than by the engine, so a `qr` reference at the median
  # can sit beside a `qr` frontier at the 95th percentile.
  if (length(metadata$reference) > 0L) {
    reference <- sca_reference(model, x = condition,
                               line = metadata$reference)
    for (i in seq_len(nrow(reference))) {
      if (!is.finite(reference$slope[[i]]) ||
          !is.finite(reference$intercept[[i]])) {
        next
      }
      chart <- chart + ggplot2::geom_abline(
        intercept = reference$intercept[[i]],
        slope = reference$slope[[i]],
        colour = "grey40",
        linetype = if (identical(reference$line[[i]], "ols")) "dotted"
                   else "longdash",
        linewidth = 0.7
      )
      # Named on the line. It stays out of the frontier legend -- an
      # average effect and an empty-space frontier are not the same kind
      # of object -- but until 0.5.0 that meant it had no key at all, and
      # a reader saw a grey line through the cloud with nothing but a
      # caption sentence to identify it.
      # Bounds from the points and the scope the engine used, since this
      # function does not carry xb/yb the way the NSCA one does.
      chart <- chart + .sca_reference_label(
        reference[i, ], range(points$x, na.rm = TRUE),
        range(points$y, na.rm = TRUE), metadata$reference.tau, i
      )
    }
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
      fill = "white",
      # The label sits inside the corner the claim says is empty, which is
      # exactly where a counterexample would appear. An opaque box there hides
      # the one observation a reader most needs to see, so the label is
      # translucent and its border is thin. annotate = FALSE removes it.
      alpha = 0.85
    )
  }

  # Every line in the keyed legend is a frontier: reference lines are drawn
  # unkeyed above, because they are not frontiers and a shared key said they
  # were. Named in the caption instead. The title matches the `frontier`
  # column of sca_table().
  legend_title <- "Frontier"

  chart +
    ggplot2::coord_cartesian(xlim = scope[1:2], ylim = scope[3:4]) +
    ggplot2::labs(
      x = condition,
      y = metadata$outcome,
      title = .sca_wrap_title(paste0("SCA: ", metadata$statement[[condition]])),
      subtitle = .sca_wrap(sprintf(
        "Logical direction %s (SCA %d); physical empty corner %d (%s)",
        direction,
        metadata$sca_number[[condition]],
        corner,
        .sca_corner_location[[as.character(corner)]]
      )),
      colour = legend_title,
      linetype = legend_title,
      caption = .sca_wrap(paste0(
        .sca_reference_caption(metadata),
        "The frontier is an empirical empty-space pattern; causal sufficiency ",
        "requires separate justification."
      ))
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "grey35", fill = NA),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9),
      # Left, not right. A caption is read as the last sentence of the figure,
      # and a wrapped right-aligned block has a ragged left edge that no
      # reader tracks back to.
      plot.caption = ggplot2::element_text(hjust = 0, size = 8)
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
#'   `FALSE` removes both labels, which is what a small multiple or a figure
#'   with its own caption wants.
#' @return A `ggplot` for one condition or a named list of plots for multiple
#'   conditions.
#' @export
#' @examples
#' iv <- subset(sca_four_configurations, configuration == "IV")
#' fit <- sca_analysis(iv, x = "X", y = "Y", direction = "HH",
#'                     scope = c(0, 100, 0, 100), reference = "ols")
#' sca_plot(fit)
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
#' @examples
#' iv <- subset(sca_four_configurations, configuration == "IV")
#' # Every fifth case, to keep the permutation test quick
#' small <- iv[seq(1, nrow(iv), by = 5), ]
#' fit <- sca_analysis(small, x = "X", y = "Y", direction = "HH",
#'                     ceilings = "ce_fdh", scope = c(0, 100, 0, 100),
#'                     test.rep = 99, test.seed = 1)
#' sca_test_plot(fit)
sca_test_plot <- function(model, x = NULL, ceiling = NULL, bins = 30) {
  metadata <- .sca_metadata(model)
  if (is.null(x)) {
    x <- names(model$plots)[[1L]]
  }
  condition <- .sca_select_conditions(model, x)
  if (length(condition) != 1L) {
    stop("Select exactly one condition for a permutation-test plot.", call. = FALSE)
  }
  condition <- condition[[1L]]
  # Resolved against the frontiers the model estimated, not against the tests:
  # with test.rep = 0 there are no tests, and resolving against them would
  # report "no estimated frontier" for a model that has several.
  ceiling <- .sca_resolve_ceiling(model, ceiling, metadata$ceilings)
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
      title = .sca_wrap_title(paste0(
        "SCA permutation test: ", metadata$statement[[condition]]
      )),
      subtitle = .sca_wrap(sprintf(
        "%s; observed d = %.3f, p = %.3f",
        ceiling, test$observed, p_value
      )),
      caption = .sca_wrap(paste0(
        "The null is random X-Y pairing. A small p-value says an empty space ",
        "this large is unusual under that null, not that the condition is ",
        "sufficient."
      ))
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9),
      plot.caption = ggplot2::element_text(hjust = 0, size = 8)
    )
}

# Name the unkeyed lines, since they are not in the legend.
#
# A dotted and a long-dashed grey line on a plot with no key for them is a
# puzzle. They are kept out of the frontier legend deliberately -- they are not
# frontiers -- so the caption has to say what they are, including the quantile,
# because a median line and a 95th-percentile line look identical on paper.
.sca_reference_caption <- function(metadata) {
  reference <- metadata$reference
  if (length(reference) == 0L) {
    return("")
  }
  tau <- metadata$reference.tau
  if (is.null(tau) || !is.finite(tau)) {
    tau <- 0.5
  }
  described <- vapply(reference, function(line) {
    if (identical(line, "ols")) {
      "dotted grey: OLS central tendency"
    } else {
      sprintf("long-dashed grey: quantile regression at tau = %g", tau)
    }
  }, character(1L))
  paste0(paste(described, collapse = "; "), ", drawn for comparison only. ")
}

# Titles wrap narrower than captions, because they are set in a larger,
# bolder face and the panel is the same width for both. 95 characters of
# caption fit; 95 of title overflowed and were clipped mid-word. The number
# below was found by rendering, not by arithmetic on point sizes.
.sca_wrap_title <- function(text) {
  .sca_wrap(text, width = 62L)
}

# Name the reference line where it is drawn. The counterpart of NSCA's
# `.nsca_reference_label()`, and separate for the same reason every other
# paired helper is: that one is private to the other package.
#
# ggplot2 allows one scale per aesthetic and both are taken here, so a third
# legend would mean bending colour or linetype -- which would put an average
# effect and an empty-space frontier in one key. A label on the line names it
# without claiming it is either. Staggered by index, because an OLS line and a
# median line very nearly coincide on symmetric data.
.sca_reference_label <- function(row, xb, yb, tau, index = 1L) {
  at <- xb[[1L]] + max(0.74 - 0.19 * (index - 1L), 0.15) * diff(xb)
  y <- row$intercept[[1L]] + row$slope[[1L]] * at
  lift <- 0.035 * diff(yb)
  y <- min(max(y + lift, yb[[1L]] + lift), yb[[2L]] - lift)
  label <- if (identical(row$line[[1L]], "ols")) {
    "OLS"
  } else {
    sprintf("QR tau=%g", if (is.null(tau) || !is.finite(tau)) 0.5 else tau)
  }
  ggplot2::annotate(
    "label", x = at, y = y, label = label,
    size = 2.7, colour = "grey30", fill = "white",
    label.size = 0, label.padding = ggplot2::unit(0.12, "lines"),
    hjust = 0.5, vjust = 0.5
  )
}
