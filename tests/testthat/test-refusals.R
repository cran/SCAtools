# Inputs that could only be a mistake, and were accepted until 0.5.0.
#
# Three of these did not merely produce a wrong number -- they produced a fit
# that misreported itself, which is the failure mode a user cannot catch by
# reading the output. Each test names the consequence rather than only the
# refusal, so that a future reader can tell whether the rule is still earning
# its place.

ref_data <- function(n = 40, seed = 3) {
  set.seed(seed)
  x <- sort(stats::runif(n))
  data.frame(X = x, Y = pmin(pmax(x + stats::rnorm(n, 0, 0.08), 0), 1))
}


test_that("an unknown frontier technique is refused, not handed to the engine", {
  # The worst of them. `ceilings = "banana"` reached the engine, which fell
  # back to its own defaults and fitted ce_fdh *and* cr_fdh, while the metadata
  # still recorded "banana" -- so print() showed `frontiers  banana` above a
  # table of two techniques nobody asked for.
  expect_error(
    sca_analysis(ref_data(), "X", "Y", ceilings = "banana", test.rep = 0),
    "Unknown frontier technique"
  )
  # The message lists what is available, so the fix does not need the manual.
  expect_error(
    sca_analysis(ref_data(), "X", "Y", ceilings = "banana", test.rep = 0),
    "ce_fdh"
  )
  # One bad name among good ones is still refused.
  expect_error(
    sca_analysis(ref_data(), "X", "Y", ceilings = c("ce_fdh", "nope"),
                 test.rep = 0),
    "'nope'"
  )
})

test_that("the pinned technique list is the engine's own", {
  # Hard-coded in the package because `NCA:::ceilings` from package code is
  # what R CMD check objects to. Pinned here instead: an engine release that
  # adds or drops a technique breaks this test rather than being accepted.
  engine <- get("ceilings", envir = asNamespace("NCA"))
  ours <- get(".sca_known_ceilings", envir = asNamespace("SCAtools"))
  expect_setequal(ours, engine)
})

test_that("every valid technique is still accepted", {
  # The refusal must not be so eager that it blocks the engine's own list.
  for (technique in c("ce_fdh", "ce_vrs", "cr_fdh", "cr_vrs")) {
    expect_silent(suppressWarnings(suppressMessages(
      sca_analysis(ref_data(), "X", "Y", ceilings = technique, test.rep = 0)
    )))
  }
  # `ols` is on the engine's list and is moved to `reference` with a warning
  # rather than refused, which is the 0.4.1 behaviour and still correct.
  expect_warning(
    sca_analysis(ref_data(), "X", "Y", ceilings = c("ce_fdh", "ols"),
                 test.rep = 0),
    "central-tendency"
  )
})


test_that("a significance threshold outside (0, 1) is refused", {
  # `test.p_threshold = 2` was accepted and stored, so every row came back
  # significant = TRUE and the Settings block printed `alpha 2` as though it
  # were a considered choice. This argument decides a reported verdict.
  for (bad in list(2, 0, 1, -0.5, NA, c(0.01, 0.05))) {
    expect_error(
      sca_analysis(ref_data(), "X", "Y", ceilings = "ce_fdh", test.rep = 0,
                   test.p_threshold = bad),
      "test.p_threshold"
    )
  }
  expect_silent(suppressWarnings(suppressMessages(
    sca_analysis(ref_data(), "X", "Y", ceilings = "ce_fdh", test.rep = 0,
                 test.p_threshold = 0.01)
  )))
})

test_that("a negative or fractional replication count is refused", {
  # -5 was accepted, stored, and printed in the Settings block beside a p-value
  # of NA.
  # A string that parses to a number is coerced rather than refused, the
    # way every other validator in this package treats one.
    for (bad in list(-5, 1.5, NA, "not a number")) {
    expect_error(
      sca_analysis(ref_data(), "X", "Y", ceilings = "ce_fdh", test.rep = bad),
      "test.rep"
    )
  }
  expect_silent(suppressWarnings(suppressMessages(
    sca_analysis(ref_data(), "X", "Y", ceilings = "ce_fdh", test.rep = 0)
  )))
})


test_that("a declared scope with its bounds reversed is refused", {
  # Accepted, and then silently repaired: the engine widens whatever it is
  # given until the data fits, so the analysis was right while sca_scope()
  # reported `declared_xmin 1, declared_xmax 0` -- not a scope anybody could
  # put in a methods section.
  expect_error(
    sca_analysis(ref_data(), "X", "Y", ceilings = "ce_fdh", test.rep = 0,
                 scope = c(1, 0, 0, 1)),
    "minimum"
  )
  expect_error(
    sca_analysis(ref_data(), "X", "Y", ceilings = "ce_fdh", test.rep = 0,
                 scope = c(0, 1, 1, 0)),
    "y axis"
  )
})

test_that("a declared scope with no width is refused", {
  expect_error(
    sca_analysis(ref_data(), "X", "Y", ceilings = "ce_fdh", test.rep = 0,
                 scope = c(0.5, 0.5, 0, 1)),
    "no width"
  )
})

test_that("an undeclared bound is still NA, not an error", {
  # NA means "no theoretical bound on this side, use the observed range". That
  # is a documented way to declare half a box and must survive the new check.
  expect_silent(suppressWarnings(suppressMessages(
    sca_analysis(ref_data(), "X", "Y", ceilings = "ce_fdh", test.rep = 0,
                 scope = c(0, NA, 0, 1))
  )))
})


test_that("a variable may not be both a condition and the outcome", {
  # It returned a value: a perfect diagonal, whose empty corner is half the
  # scope whatever the data contains, so d came back near .5 and looked like a
  # finding.
  expect_error(
    sca_analysis(ref_data(), "X", "X", test.rep = 0),
    "both a condition and the outcome"
  )
  data <- ref_data()
  data$Z <- data$X
  expect_error(
    sca_analysis(data, c("X", "Y"), "Y", test.rep = 0),
    "both a condition and the outcome"
  )
})


test_that("a failed column selection names the argument and the name", {
  # R's own "undefined columns selected" mentions neither, so a caller with
  # eight conditions had to bisect to find the one they had misspelled.
  expect_error(sca_analysis(ref_data(), "X", "Z", test.rep = 0), "'y' names")
  expect_error(sca_analysis(ref_data(), "Z", "Y", test.rep = 0), "'x' names")
  expect_error(sca_analysis(ref_data(), "Z", "Y", test.rep = 0), "'Z'")
  # And it lists what is there, so the fix needs no second call.
  expect_error(sca_analysis(ref_data(), "Z", "Y", test.rep = 0), "Available")
})

test_that("a repeated condition is refused by name", {
  expect_error(sca_analysis(ref_data(), c("X", "X"), "Y", test.rep = 0),
               "more than once")
})

test_that("an out-of-range column position says how many columns there are", {
  expect_error(sca_analysis(ref_data(), 9L, "Y", test.rep = 0), "9")
  expect_error(sca_analysis(ref_data(), 9L, "Y", test.rep = 0), "2 columns")
})

test_that("something that is not data is refused as such", {
  expect_error(sca_analysis("hello", "X", "Y", test.rep = 0),
               "data frame or a matrix")
  expect_error(sca_analysis(ref_data()[0, ], "X", "Y", test.rep = 0),
               "no rows")
})
