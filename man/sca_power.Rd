\name{sca_power}
\alias{sca_power}
\alias{sca_powerplot}
\title{Power Analysis for Directional SCA}
\usage{
sca_power(
  n = c(20, 50, 100), effect = 0.1, slope = 1,
  ceiling = "ce_fdh", direction = "HH", p = 0.05,
  distribution.x = "uniform", distribution.y = "uniform",
  rep = 100, test.rep = 200
)

sca_powerplot(
  df_power, x.variable = "n", x.name = "Sample size",
  x.min = 0, x.max = NULL, line.variable = "ES",
  line.name = "Effect size"
)
}
\arguments{
  \item{n}{Sample size or vector of sample sizes.}
  \item{effect}{Population effect size(s).}
  \item{slope}{Frontier slope(s). The sign must match the direction: zero or
    positive for \code{"HH"} and \code{"LL"}, zero or negative for \code{"LH"}
    and \code{"HL"}. Any other combination is an error.}
  \item{ceiling}{Frontier method(s).}
  \item{direction}{One SCA direction.}
  \item{p}{Significance level.}
  \item{distribution.x, distribution.y}{Marginal distribution(s).}
  \item{rep}{Number of datasets per design cell.}
  \item{test.rep}{Number of permutations per simulated analysis.}
  \item{df_power}{A data frame returned by \code{sca_power()}.}
  \item{x.variable}{Column mapped to the x axis.}
  \item{x.name}{X-axis label.}
  \item{x.min, x.max}{X-axis limits.}
  \item{line.variable}{Column mapped to line colour.}
  \item{line.name}{Legend title.}
}
\description{
Estimates power through repeated samples and permutation tests, and plots the
result.

A frontier line bounds only two of the four corners. A rising line bounds the
upper-left and lower-right corners, so it suits \code{"HH"} and \code{"LL"}; a
falling line bounds the upper-right and lower-left corners, so it suits
\code{"LH"} and \code{"HL"}. A horizontal line is accepted for every direction.
\code{sca_power()} checks this before calling the engine, which would otherwise
warn and return \code{NULL}.
}
\value{\code{sca_power()} returns a data frame; \code{sca_powerplot()} returns a
\code{ggplot} object.}
\examples{
\donttest{
# Deliberately small: cost is length(n) x length(effect) x rep x test.rep
# engine fits, so a reporting-grade run uses rep = 100 and test.rep = 200
# and takes minutes rather than seconds.
power <- sca_power(n = c(30, 60), rep = 10, test.rep = 25)
sca_powerplot(power)
}
}
\keyword{methods}

