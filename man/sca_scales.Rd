\name{sca_scales}
\alias{sca_scales}
\alias{sca_rescale}
\title{Reporting Scales for SCA Threshold Tables}
\usage{
sca_scales()

sca_rescale(
  values, reference, bounds = range(reference, na.rm = TRUE),
  scale = "percentage.range", letter = c("H", "L"),
  convention = c("absolute", "directional"),
  inequality = c("strict", "inclusive"), boundary = NULL
)
}
\arguments{
  \item{values}{Numeric vector in actual units.}
  \item{reference}{Observed values of the same variable, used by the
  \code{percentile} and \code{sd} scales.}
  \item{bounds}{Length-two axis bounds, including any theoretical scope.
  Defaults to the range of \code{reference}.}
  \item{scale}{One of the names returned by \code{sca_scales()}.}
  \item{letter}{\code{"H"} or \code{"L"}, the direction letter for this axis.}
  \item{convention}{\code{"absolute"} or \code{"directional"}.}
  \item{inequality}{\code{"strict"} or \code{"inclusive"}; affects the
  \code{percentile} scale only. Renamed from \code{boundary} in 0.4.0.}
  \item{boundary}{Deprecated. Former name of \code{inequality}.}
}
\description{
\code{sca_scales()} lists the reporting scales understood by
\code{\link{sca_analysis}} and \code{\link{sca_thresholds}}, with a note on how
each behaves when the sufficiency direction refers to a low level.
\code{sca_rescale()} applies one of those scales to arbitrary values, using
exactly the conversion \code{\link{sca_thresholds}} uses internally.
}
\details{
Five scales are available. \code{"actual"} keeps the original measurement
units. \code{"percentage.range"} and \code{"percentage.max"} express a value as
a percentage of the axis range or of the axis maximum. \code{"percentile"}
reports the share of observations on the stated side of the value.
\code{"sd"} reports standard deviations from the mean.

Under the \code{"absolute"} convention every scale is measured from the low end
of the axis, whatever the direction, and the inequality carries the direction.
Under the \code{"directional"} convention a low-level direction mirrors the
axis, so the transformation decreases and the inequality flips. The
\code{"actual"} scale is never mirrored, because reversing measurement units
would produce values that do not occur in the data.

A percentile is a step function, so its strictness follows the inequality used
by the rule. Pairing a strict \code{<} rule with \eqn{P(X \le t)} would drop the
observation nearest the threshold once the data are expressed on the same
scale.

\code{sca_rescale()} is exported because a threshold produced elsewhere is
comparable with a sufficiency threshold only when both went through the same
direction-aware mirroring and the same percentile strictness rule. The NSCA
package uses it to place the necessity and sufficiency thresholds of one row on
identical scales.
}
\value{
\code{sca_scales()} returns a data frame with one row per scale, giving its
description, whether it is mirrored for low-level directions, and a note on the
mirrored form. \code{sca_rescale()} returns a numeric vector on the requested
scale; non-finite inputs return \code{NA}.
}
\examples{
sca_scales()

x <- c(1, 3, 4, 7, 9)
sca_rescale(4, x, scale = "percentile", letter = "H")
sca_rescale(4, x, scale = "percentile", letter = "L")
}
\seealso{\code{\link{sca_analysis}}, \code{\link{sca_thresholds}},
\code{\link{sca_terms}}}
\keyword{documentation}
