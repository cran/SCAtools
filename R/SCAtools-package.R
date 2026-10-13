#' @section The method:
#' A condition is sufficient for an outcome when the condition guarantees the
#' outcome. In a scatter plot of condition against outcome, such a claim rules
#' out one corner: high X sufficient for high Y, for instance, leaves no cases
#' with high X and low Y. Necessary condition analysis (NCA; Dul, 2016)
#' estimates the empty corner of a necessity claim. By contraposition a
#' sufficiency claim is a necessity claim about the opposite levels -- high X
#' sufficient for high Y is low X necessary for low Y -- so its empty corner
#' can be estimated with the same frontier techniques. SCAtools states the four
#' sufficiency directions as claims, maps each to the corner it leaves empty,
#' and reports the effect size, permutation test and sufficiency thresholds in
#' the vocabulary of condition analysis in degree, following Chan (in press),
#' *From Necessity to Sufficiency and Necessity-and-Sufficiency: Extending
#' Condition Analysis in Degree*.
#'
#' @section What it is for:
#' * Finding the condition level that guarantees each outcome level,
#'  reported in actual units, percentages, percentiles or standard
#'  deviations ([sca_thresholds()]).
#' * Measuring an effect that regression does not show: a condition can
#'  rule out low outcomes while barely moving the average. [sca_compare()]
#'  reports the two side by side.
#' * Comparing several conditions, each with its own direction, in one
#'  table ([sca_table()]).
#' * Planning a study: [sca_power()] estimates the sample size a
#'  sufficiency effect of a given size needs.
#'
#' @section How to use it:
#' 1. Fit the claim with [sca_analysis()], naming the condition, the
#'    outcome and the direction, and the scope if theory gives one. Use
#'    `test.rep` for the permutation test; [sca()] runs 1000 permutations and
#'    draws the plot in one step.
#' 2. Read the results with [sca_table()], [sca_thresholds()] and
#'    [sca_plot()]; [sca_output()] prints and draws them together.
#' 3. Set the result beside regression with [sca_compare()] and
#'    [sca_locationshift()].
#' 4. Check it with [sca_test_plot()], [sca_scope()],
#'    [sca_scope_sensitivity()], [sca_resample()] and [sca_outliers()].
#'
#' @section Reading the result:
#' A fitted model describes the empty-space pattern in the sample under the
#' stated scope and frontier technique, and its thresholds are empirical rules
#' for the population and design studied. Claims beyond the sample -- that the
#' condition causes the outcome, that future cases will follow the frontier, or
#' that the pattern holds once common causes are controlled -- rest on the
#' research design. Each direction predicts a single empty corner, which
#' assumes the relationship is monotone, so plot the data before choosing one.
#'
#' @section Regression as a reference:
#' An ordinary least-squares or median regression line can be drawn beside the
#' frontier as a central-tendency reference. It summarises the average effect
#' and is never a component of a sufficiency claim. Chan (in press), *Mean
#' Effects Are Not Sufficiency Effects: How Sufficient Condition Analysis
#' Complements Regression Analysis*, sets out why the two kinds of effect are
#' reported side by side; [sca_compare()] and [sca_four_configurations] follow
#' it.
#'
#' @section The engine:
#' Frontier estimation, effect sizes, threshold estimation, permutation tests,
#' random-data generation and power analysis are delegated to the NCA package
#' by Jan Dul and Govert Buijs. SCAtools contributes the mapping from a logical
#' sufficiency direction to the physically empty corner, SCA-specific tables
#' and labels, direction-aware reporting scales, and the shared vocabulary of
#' condition analysis in degree that the NSCA package also uses.
#'
#' @references
#' Chan, Y. (in press). *From Necessity to Sufficiency and
#' Necessity-and-Sufficiency: Extending Condition Analysis in Degree*.
#' University of Leeds.
#'
#' Chan, Y. (in press). *Mean Effects Are Not Sufficiency Effects: How
#' Sufficient Condition Analysis Complements Regression Analysis*.
#' University of Leeds.
#'
#' Dul, J. (2016). Necessary Condition Analysis (NCA): Logic and methodology
#' of "necessary but not sufficient" causality. *Organizational Research
#' Methods*, 19(1), 10-52. \doi{10.1177/1094428115584005}
"_PACKAGE"
