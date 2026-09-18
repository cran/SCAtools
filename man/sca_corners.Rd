\name{sca_corner}
\alias{sca_corner}
\alias{sca_direction}
\alias{sca_corner_map}
\alias{sca_terms}
\alias{sca_legacy_names}
\title{Map Sufficiency Directions and Physical Empty Corners}
\usage{
sca_corner(direction = c("HH", "LH", "HL", "LL"))

sca_direction(corner = 1:4)

sca_corner_map()

sca_terms()

sca_legacy_names()
}
\arguments{
  \item{direction}{Character vector containing \code{"HH"}, \code{"LH"},
  \code{"HL"}, or \code{"LL"}. The first letter refers to X and the second
  to Y.}
  \item{corner}{Integer vector of physical corner numbers from 1 through 4:
  upper-left, upper-right, lower-left, and lower-right.}
}
\description{
Converts between logical SCA directions and physical empty corners, returns the
complete mapping table, lists the vocabulary of condition analysis in degree
with the names this package uses for it, or lists the names retired in 0.4.0.
}
\details{
\code{sca_corner_map()} reproduces the sufficiency half of Table 1 of the
condition analysis in degree framework and adds the corner numbering the
frontier engine uses. Five terms are used in sequence: the \emph{scope} is the
region of the X-Y space bounded by the theoretical or observed extremes of X and
Y; the \emph{expected empty space} is the part of that scope which should hold
no observations if the hypothesised relation holds; the \emph{boundary} is the
theoretical line separating that space from the compatible region; the
\emph{frontier} is the boundary estimated from data; and \emph{ceiling} and
\emph{floor} name the direction in which Y is bounded, a ceiling limiting how
high Y can be for a given X and a floor how low.

Boundary type follows the empty corner and nothing else, so for sufficiency the
boundary is a floor whenever the outcome level is high and a ceiling whenever it
is low.

The direction is a claim about the theorised X-Y relationship, not about the
shape of the estimated frontier, which may be a step function or a straight line
under any of the four types.
}
\value{
\code{sca_corner()} returns an integer vector; \code{sca_direction()} returns a
character vector; \code{sca_corner_map()} and \code{sca_terms()} return data
frames; \code{sca_legacy_names()} returns a named character vector whose names
are the retired names and whose values are the replacements.
}
\seealso{
\code{\link{sca_scales}}, \code{\link{sca_table}}, \code{\link{sca_thresholds}}
}
\examples{
sca_corner("HH")
sca_direction(4)
sca_corner_map()
sca_terms()
sca_legacy_names()
}
\keyword{methods}
