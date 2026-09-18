\name{SCAtools-package}
\alias{SCAtools}
\alias{SCAtools-package}
\docType{package}
\title{Direction-Aware Sufficiency Condition Analysis}
\description{
Tools for expressing bivariate sufficiency statements as empty-space frontier
analyses while keeping logical directions separate from physical scatter-plot
corners. Computation is delegated to \pkg{NCA} through contraposition.
}
\details{
The four directions are \code{HH}, \code{LH}, \code{HL}, and \code{LL}; the
first letter refers to X and the second to Y. Their physical empty corners are
4, 3, 2, and 1, respectively. Empty-space evidence alone does not establish
causal or deterministic sufficiency.
}
\seealso{\code{\link{sca_analysis}}, \code{\link{sca_corner_map}}}
\keyword{package}

