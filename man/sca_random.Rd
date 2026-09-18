\name{sca_random}
\alias{sca_random}
\title{Generate Data with a Directional Sufficiency Pattern}
\usage{
sca_random(
  n, intercepts, slopes, direction = "HH",
  distribution.x = "uniform", distribution.y = "uniform",
  mean.x = 0.5, mean.y = 0.5, sd.x = 0.2, sd.y = 0.2
)
}
\arguments{
  \item{n}{Number of observations.}
  \item{intercepts}{Frontier intercept or vector of intercepts.}
  \item{slopes}{Frontier slope or vector of slopes.}
  \item{direction}{One SCA direction.}
  \item{distribution.x, distribution.y}{\code{"uniform"} or \code{"normal"}.}
  \item{mean.x, mean.y}{Means for truncated normal distributions.}
  \item{sd.x, sd.y}{Standard deviations for truncated normal distributions.}
}
\description{
Generates observations in the unit square while excluding the physical corner
implied by the requested sufficiency direction.
}
\value{A data frame with X condition columns and one Y outcome column.}
\examples{
set.seed(42)
dat <- sca_random(25, intercepts = 0, slopes = 1, direction = "HH")
head(dat)
}
\keyword{distribution}

