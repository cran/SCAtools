\name{sca_reference}
\alias{sca_reference}
\title{Central-Tendency Reference Line}
\usage{
sca_reference(model, x = NULL)
}
\arguments{
  \item{model}{An object returned by \code{\link{sca_analysis}}.}
  \item{x}{Optional condition names or positions. All conditions by default.}
}
\description{
Fits the ordinary least-squares regression of the outcome on each condition,
using the same observations the frontier was fitted to. This is the line
\code{lm(y ~ x)} returns, and it is what \code{reference = "ols"} draws on
\code{\link{sca_plot}}.
}
\details{
The line is reported separately from \code{\link{sca_table}} on purpose.
Regression describes how the expected outcome moves with the condition; a
sufficiency frontier describes which condition-outcome combinations are
absent. Section 6.1 of the condition analysis in degree framework treats these
as different targets, most informative when shown side by side rather than
merged into one number, and section 4.3 notes that a frontier is fixed by the
most extreme observations rather than by the central tendency. A reference
line therefore has a slope and an intercept but no empty zone, no effect size,
no permutation p value and no sufficiency threshold.

The line is computed on request, so it is available whether or not
\code{reference = "ols"} was set. The \code{drawn} column records whether it
was also passed to the engine and so appears on the plot.
}
\value{
A data frame with one row per condition: \code{condition}, \code{outcome},
\code{line}, \code{line_type}, \code{observations}, \code{intercept},
\code{slope}, \code{r_squared} and \code{drawn}.
}
\examples{
dat <- data.frame(X = seq(0, 1, length.out = 12),
                  Y = seq(0, 1, length.out = 12))
fit <- sca_analysis(dat, "X", "Y", direction = "HH",
                    ceilings = "ce_fdh", reference = "ols")
sca_reference(fit)
}
\seealso{\code{\link{sca_analysis}}, \code{\link{sca_table}},
\code{\link{sca_terms}}}
\keyword{methods}
