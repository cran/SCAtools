\name{sca_normalize}
\alias{sca_normalize}
\alias{sca_outliers}
\alias{print.sca_outliers}
\title{Normalize Data and Diagnose Influential Observations}
\usage{
sca_normalize(data, scale = NULL, min_max = NULL)

sca_outliers(
  data, x, y, direction = "HH", ceiling = NULL, scope = NULL,
  k = 1, min.dif = 0.01, max.results = 25, condensed = FALSE
)

\method{print}{sca_outliers}(x, ...)
}
\arguments{
  \item{data}{Input data frame or matrix.}
  \item{scale}{Output minimum-maximum pair, or one pair per column.}
  \item{min_max}{Optional source minimum-maximum pair, or one per column.}
  \item{x}{One condition column or an SCA outlier object in the print method.}
  \item{y}{One outcome column.}
  \item{direction}{One SCA direction.}
  \item{ceiling}{Frontier technique; CE-FDH is used when omitted.}
  \item{scope}{Optional theoretical scope.}
  \item{k}{Number of observations considered jointly.}
  \item{min.dif}{Minimum relative effect-size difference.}
  \item{max.results}{Maximum number of rows returned.}
  \item{condensed}{Suppress redundant combinations when \code{k > 1}.}
  \item{...}{Additional print arguments.}
}
\description{
Provides min-max normalization and identifies observations or combinations of
observations that materially influence the fitted empty-space effect size.
}
\value{
\code{sca_normalize()} returns normalized data. \code{sca_outliers()} returns
an outlier-sensitivity table or \code{NULL}.
}
\examples{
dat <- data.frame(X = c(0, 2, 4), Y = c(10, 20, 30))
sca_normalize(dat)
}
\keyword{methods}

