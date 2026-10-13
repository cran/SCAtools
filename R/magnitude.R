# The conventional magnitude bands, made visible -- both of them.
#
# Until 0.5.0 these lived in two code comments. The package was already
# applying Dul's lower edge as the default `d_cut` and Cohen's as the default
# `beta_cut` in sca_compare(), so a caller had two conventions applied to their
# analysis and no way to see either. That is the worst of both: the judgement
# is made and it is not disclosed.
#
# So they are exposed, with the citation attached and the status stated. They
# are conventions to cite, in the way a reader of a regression cites Cohen.
# They are not benchmarks calibrated for a particular design.
#
# The trap this also exists to make visible: the two conventions use the SAME
# band edges -- .10, .30, .50 -- with the labels SHIFTED BY ONE. Cohen's medium
# runs .30 to .50; Dul's large runs .30 to .50. So a value of 0.35 is "medium"
# as a standardised coefficient and "large" as an NCA effect size, and the
# package's two defaults differ (0.30 against 0.10) for exactly that reason.
# Anyone reading a beta and a d side by side in sca_compare() needs to know it,
# and a comment in the source was not telling them.
#
# What is not supplied is a band for the joint indices. Those are NSCA's own
# constructions; nobody has published a convention for them, and inventing one
# is a different act from citing somebody else's.

.sca_magnitude_bands <- list(
  effect_size = data.frame(
    lower = c(0, 0.1, 0.3, 0.5),
    upper = c(0.1, 0.3, 0.5, Inf),
    label = c("small", "medium", "large", "very large"),
    stringsAsFactors = FALSE
  ),
  beta = data.frame(
    lower = c(0, 0.1, 0.3, 0.5),
    upper = c(0.1, 0.3, 0.5, Inf),
    label = c("below small", "small", "medium", "large"),
    stringsAsFactors = FALSE
  )
)

.sca_magnitude_source <- c(
  effect_size = "Dul (2016), for an NCA effect size; conventional, not calibrated",
  beta = "Cohen (1988), for a standardised coefficient; conventional, not calibrated"
)

.sca_magnitude_quantity <- c(
  effect_size = "effect size: share of the scope left empty",
  beta = "standardised regression coefficient (absolute value)"
)

#' Conventional magnitude bands
#'
#' The bands Dul (2016) proposes for an NCA effect size and Cohen (1988) for a
#' standardised regression coefficient, and the label a given value falls in.
#' Called with no value it returns the band table.
#'
#' @section What these are, and are not:
#' They are a **convention to cite**, in the way a reader of a regression
#' coefficient cites Cohen. Reporting "d = 0.35, large by the conventional
#' bands (Dul, 2016)" is a citation, and a normal one.
#'
#' They are **not** calibrated for your design. An effect size is the share of
#' the declared scope left empty, so it depends on the scope you declared, the
#' frontier technique, and -- because the frontier is fitted to the sample's own
#' extremes -- on the sample size. Two studies of the same relationship at
#' n = 30 and n = 800 will not report the same number, and neither is wrong:
#' each describes its own data inside its own scope. What does not follow is
#' that the two can be read off one band table without comment.
#'
#' @section The two conventions share their edges and shift their labels:
#' This is worth stating plainly, because the coincidence is a trap.
#'
#' ```
#' value    as a coefficient (Cohen)   as an effect size (Dul)
#' 0.15     small                      medium
#' 0.35     medium                     large
#' 0.60     large                      very large
#' ```
#'
#' Both use .10, .30 and .50 as edges; the labels sit one band apart. It is
#' why [sca_compare()] defaults `beta_cut` to 0.30 and `d_cut` to 0.10 -- each
#' is the lower edge of "medium" in its own tradition, and those are different
#' numbers even though the tradition's edges are not.
#'
#' @section Read a band beside the floor:
#' `effect_size_null` from [sca_table()] is what random pairing alone produces
#' at that sample size, scope and technique. A band says where a number sits on
#' a conventional scale; the floor says how much of it the geometry would have
#' produced anyway. They answer different questions and belong in the same
#' sentence.
#'
#' @param value Optional numeric vector. Omitted, the band table is returned.
#'   For `scale = "beta"` the absolute value is used, since a band describes a
#'   magnitude and not a direction.
#' @param scale Which convention: `"effect_size"` for Dul's bands, `"beta"` for
#'   Cohen's.
#' @return With `value`, a character vector of labels, `NA` where the value is
#'   not finite. Without it, a data frame of bands.
#' @references
#' Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences*
#' (2nd ed.). Lawrence Erlbaum.
#'
#' Dul, J. (2016). Necessary Condition Analysis (NCA): Logic and methodology
#' of "necessary but not sufficient" causality. *Organizational Research
#' Methods*, 19(1), 10-52. \doi{10.1177/1094428115584005}
#' @seealso [sca_table()], [sca_compare()]
#' @export
#' @examples
#' sca_magnitude()
#' sca_magnitude(scale = "beta")
#' sca_magnitude(c(0.04, 0.18, 0.35, 0.62))
#' # The same values under the other convention, one band apart:
#' sca_magnitude(c(0.04, 0.18, 0.35, 0.62), scale = "beta")
sca_magnitude <- function(value = NULL, scale = c("effect_size", "beta")) {
  scale <- match.arg(scale)
  bands <- .sca_magnitude_bands[[scale]]
  bands$rule <- sprintf("%s <= x < %s", format(bands$lower),
                        format(bands$upper))
  last <- nrow(bands)
  bands$rule[[last]] <- sprintf("x >= %s", format(bands$lower[[last]]))
  bands$quantity <- unname(.sca_magnitude_quantity[[scale]])
  bands$source <- unname(.sca_magnitude_source[[scale]])

  if (is.null(value)) {
    return(bands)
  }
  values <- suppressWarnings(as.numeric(value))
  if (identical(scale, "beta")) {
    values <- abs(values)
  }
  vapply(values, function(v) {
    if (!is.finite(v) || v < 0) {
      return(NA_character_)
    }
    hit <- which(v >= bands$lower & v < bands$upper)
    if (length(hit) == 0L) NA_character_ else bands$label[[hit[[1L]]]]
  }, character(1L))
}
