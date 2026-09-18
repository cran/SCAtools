\name{sca_plot}
\alias{sca_plot}
\alias{plot.sca_result}
\alias{sca_test_plot}
\title{Plot SCA Frontiers and Permutation Tests}
\usage{
sca_plot(model, x = NULL, ceilings = NULL, shade = TRUE, annotate = TRUE)

\method{plot}{sca_result}(x, ...)

sca_test_plot(model, x = NULL, ceiling = "ce_fdh", bins = 30)
}
\arguments{
  \item{model}{An object returned by \code{sca_analysis()}.}
  \item{x}{Condition names or positions. In \code{plot.sca_result()}, the SCA
  result object.}
  \item{ceilings}{Optional frontier methods to draw.}
  \item{shade}{Shade the fitted empty zone when an exact CE-FDH or CE-VRS
  frontier is selected.}
  \item{annotate}{Label the logical direction and physical corner.}
  \item{ceiling}{Frontier method for the permutation test.}
  \item{bins}{Number of histogram bins.}
  \item{...}{Arguments passed to \code{sca_plot()}.}
}
\description{
Draws SCA-labelled scatter plots and fitted frontiers or the permutation
distribution of an empty-space effect size.
}
\value{
A \code{ggplot} object for one condition, or a named list of plots for several
conditions. The plot method returns the plot object invisibly.
}
\keyword{hplot}
