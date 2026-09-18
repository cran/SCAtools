\name{sca_table}
\alias{sca_table}
\alias{sca_extract}
\alias{sca_thresholds}
\alias{sca_output}
\alias{print.sca_result}
\alias{summary.sca_result}
\alias{print.summary_sca_result}
\title{Summarize and Extract SCA Results}
\usage{
sca_table(model, legacy = FALSE)

sca_extract(model, x = NULL, ceiling = NULL, param = "Effect size")

sca_thresholds(
  model, ceiling = "ce_fdh", x = NULL, scale = NULL,
  outcome_scale = NULL, convention = NULL,
  inequality = c("strict", "inclusive"), digits = 6L,
  legacy = FALSE, boundary = NULL
)

sca_output(
  model, summaries = TRUE, thresholds = FALSE, plots = TRUE,
  tests = FALSE, selection = NULL, ceiling = "ce_fdh",
  scale = NULL, outcome_scale = NULL, convention = NULL,
  inequality = c("strict", "inclusive")
)

\method{print}{sca_result}(x, ...)

\method{summary}{sca_result}(object, ...)

\method{print}{summary_sca_result}(x, ...)
}
\arguments{
  \item{model, object}{An object returned by \code{sca_analysis()}.}
  \item{x}{For extraction and thresholds, a condition name or position. For
  print methods, the object to print.}
  \item{ceiling}{A frontier method.}
  \item{param}{An NCA parameter name, an SCA metadata field
  (\code{"direction"}, \code{"sca_number"}, \code{"empty_corner"},
  \code{"statement"}), a geometry field (\code{"relationship"},
  \code{"empty_space"}, \code{"boundary_type"}), or a name retired in 0.4.0.}
  \item{scale, outcome_scale}{Reporting scales for the condition and outcome
  axes. Both default to the scales requested in \code{sca_analysis()}. See
  \code{\link{sca_scales}}.}
  \item{convention}{\code{"absolute"} or \code{"directional"}; defaults to
  the convention requested in \code{sca_analysis()}.}
  \item{inequality}{Use logically exact strict inequalities or an inclusive
  reporting convention for numeric frontier rules. Renamed from
  \code{boundary} in 0.4.0, which is reserved for the theoretical line
  separating an expected empty space from the compatible region.}
  \item{boundary}{Deprecated. Former name of \code{inequality}.}
  \item{legacy}{Append the column names retired in 0.4.0 as duplicates. See
  \code{\link{sca_legacy_names}}.}
  \item{digits}{Significant digits used in the formatted rule text.}
  \item{summaries}{Print the tidy summary.}
  \item{thresholds}{Print directional threshold rules.}
  \item{plots}{Draw frontier plots.}
  \item{tests}{Draw permutation-test plots.}
  \item{selection}{Optional condition names or positions.}
  \item{...}{Additional arguments reserved for methods.}
}
\description{
Creates tidy estimates, extracts a single parameter, translates fitted
frontiers into directional sufficiency rules, or displays selected output.
}
\details{
For an HH model, the default strict threshold rule has the form
\code{X > threshold => Y > outcome level}. Strict inequalities follow exact
contraposition at the fitted inequality. Set \code{inequality = "inclusive"} only
when the measurement or calibration convention justifies including equality.

Thresholds are held in actual units in \code{sufficiency_threshold_actual} and
\code{outcome_level_actual}, and converted to the reporting scale in
\code{sufficiency_threshold} and \code{outcome_level}. A model can therefore be
re-expressed on any scale without refitting. Under the \code{"absolute"}
convention 0 sits at the low end of each axis and the inequality carries the
direction; under \code{"directional"} the axis is mirrored for a low-level
direction, which flips the inequality so that every rule reads as "more of the
sufficient thing".

The \code{status} column states each row in sufficiency terms. This matters
because the engine's own out-of-range markers invert under contraposition: a
cell meaning "no minimum is required" for a necessity reading is exactly the
case in which no attainable condition value reaches the outcome level.

\describe{
  \item{\code{estimable}}{A threshold lies inside the scope.}
  \item{\code{no_threshold}}{No attainable condition value guarantees this
  outcome level.}
  \item{\code{always_satisfied}}{The threshold falls outside the scope on the
  permissive side, so the condition restricts nothing. This describes the
  condition side only: it does not assert that the outcome holds for every
  observation.}
}

Three columns state the geometry in the vocabulary of condition analysis in
degree. \code{relationship} gives the direction as a claim about the theorised
X-Y relationship; \code{empty_space} names what the expected empty space would
contain; \code{boundary_type} is \code{"ceiling"} when the boundary limits how
high Y can be for a given X and \code{"floor"} when it limits how low. For
sufficiency the boundary is a floor whenever the outcome level is high (HH, LH)
and a ceiling whenever it is low (HL, LL), so the estimator names CE-FDH and
CR-FDH keep the word "ceiling" even where the fitted boundary is a floor.

These are empirical frontier rules and should not be described as causal
guarantees unless the design supports that interpretation.
}
\value{
\code{sca_table()} and \code{sca_thresholds()} return data frames;
\code{sca_extract()} returns one value; \code{sca_output()} invisibly returns
the generated output; the methods return or print summary objects invisibly.
}
\seealso{
\code{\link{sca_legacy_names}}, \code{\link{sca_terms}},
\code{\link{sca_corner_map}}, \code{\link{sca_scales}}
}
\keyword{methods}
