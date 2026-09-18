\name{sca_analysis}
\alias{sca_analysis}
\alias{sca}
\title{Sufficiency Condition Analysis}
\usage{
sca_analysis(
  data, x, y, direction = "HH",
  ceilings = c("ce_fdh", "cr_fdh"), reference = NULL, custom = NULL,
  scope = NULL, threshold.x = "percentage.range",
  threshold.y = "percentage.range",
  convention = c("absolute", "directional"),
  steps = 10, step.size = NULL,
  cutoff = 0, qr.tau = 0.95, test.rep = 0,
  test.p_confidence = 0.95, test.p_threshold = 0.05,
  bottleneck.x = NULL, bottleneck.y = NULL
)

sca(
  data, x, y, direction = "HH",
  ceilings = c("ce_fdh", "cr_fdh"), reference = NULL
)
}
\arguments{
  \item{data}{A data frame or object coercible to a data frame.}
  \item{x}{One or more condition columns selected by name or position.}
  \item{y}{One outcome column selected by name or position.}
  \item{direction}{One direction or one per condition: \code{"HH"},
  \code{"LH"}, \code{"HL"}, or \code{"LL"}.}
  \item{ceilings}{Empty-space frontier techniques accepted by
  \code{\link[NCA]{nca_analysis}}, such as \code{"ce_fdh"} and
  \code{"cr_fdh"}. \code{"ols"} is not one of them: it is moved to
  \code{reference} with a warning.}
  \item{reference}{Central-tendency lines drawn beside the frontier for
  comparison, or \code{NULL} for none. \code{"ols"} is the ordinary
  least-squares regression of the outcome on the condition, fitted to all the
  data. It estimates no empty space, so it carries no effect size, no
  permutation test and no threshold table. See
  \code{\link{sca_reference}}.}
  \item{custom}{Optional intercept-slope pairs for custom frontiers.}
  \item{scope}{Optional theoretical scope
  \code{c(xmin, xmax, ymin, ymax)}.}
  \item{threshold.x, threshold.y}{Reporting scales for the condition and
  outcome axes: \code{"actual"}, \code{"percentage.range"},
  \code{"percentage.max"}, \code{"percentile"}, or \code{"sd"}. See
  \code{\link{sca_scales}}.}
  \item{convention}{\code{"absolute"} places 0 at the low end of every axis
  and lets the inequality carry the direction. \code{"directional"} places 0
  at the least sufficient end, mirroring low-level directions so that every
  rule reads as "more of the sufficient thing".}
  \item{steps}{Number of threshold steps, or an explicit vector of outcome
  levels expressed on the \code{threshold.y} scale.}
  \item{step.size}{Optional threshold step size on the \code{threshold.y}
  scale.}
  \item{cutoff}{Representation of out-of-range threshold values.}
  \item{qr.tau}{Quantile for the quantile-regression frontier.}
  \item{test.rep}{Number of permutation resamples; zero skips testing.}
  \item{test.p_confidence}{Confidence level for p-value accuracy.}
  \item{test.p_threshold}{Significance threshold shown in test output.}
  \item{bottleneck.x, bottleneck.y}{Deprecated former names of
  \code{threshold.x} and \code{threshold.y}. A bottleneck is a necessity
  concept and does not describe a sufficiency statement.}
}
\description{
Maps each logical sufficiency direction to the corresponding physical empty
corner and calls the NCA frontier engine. \code{sca()} is a compact convenience
wrapper.
}
\details{
Directions in one call must share the same Y letter. Run separate models when
both high-Y and low-Y statements are required. Effect sizes quantify fitted
empty-zone area relative to the declared scope; they are not by themselves
proof of causality.

Threshold values are always requested from the engine in actual units, and
every scale conversion is performed by this package. The engine applies its
own conventions inconsistently across scales and mirrors some of them
according to the physical empty corner, which does not correspond to the
logical sufficiency direction. Keeping the conversion here means one stated
convention governs both axes in all four directions, and it lets a fitted
model be re-expressed on any scale by \code{\link{sca_thresholds}} without
refitting.
}
\value{An object of class \code{sca_result}.}
\examples{
dat <- data.frame(X = seq(0, 1, length.out = 12),
                  Y = seq(0, 1, length.out = 12))
fit <- sca_analysis(dat, "X", "Y", direction = "HH",
                    ceilings = "ce_fdh")
fit

# Report the condition axis as percentiles instead
sca_thresholds(fit, scale = "percentile")
}
\seealso{\code{\link{sca_table}}, \code{\link{sca_scales}},
\code{\link{sca_plot}}}
\keyword{methods}

