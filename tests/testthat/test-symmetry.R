# The two ways of calling sca_outliers(), and the disclosure that goes with a
# table of several p-values.
#
# The model form exists because the data form makes the caller restate what the
# fit already knows, which means it also lets them restate it *differently*.
# Most of these tests are about that: a diagnostic that silently describes a
# different analysis from the one being reported is worse than no diagnostic.

symmetry_data <- function(n = 60, seed = 11) {
  set.seed(seed)
  a <- sort(stats::runif(n))
  data.frame(
    A = a,
    B = stats::runif(n),
    Y = pmin(pmax(0.15 + 0.7 * a + stats::rnorm(n, 0, 0.07), 0), 1)
  )
}

symmetry_fit <- function(..., data = symmetry_data()) {
  suppressWarnings(suppressMessages(
    sca_analysis(data, c("A", "B"), "Y", direction = "HH",
                 ceilings = "ce_fdh", test.rep = 0, ...)
  ))
}

printed <- function(fit) {
  paste(utils::capture.output(suppressWarnings(print(fit))), collapse = "\n")
}


test_that("sca_outliers() takes a fitted model and names the condition", {
  fit <- symmetry_fit()
  out <- suppressMessages(sca_outliers(fit))
  skip_if(is.null(out), "no observation reached min.dif on this sample")

  expect_identical(names(out)[[1L]], "condition")
  expect_true(all(out$condition %in% c("A", "B")))
})

test_that("the data-frame form returns the same shape", {
  # One function, one shape. The condition column is the name the caller gave,
  # which the data form has known all along and simply never reported.
  out <- suppressMessages(sca_outliers(symmetry_data(), "A", "Y"))
  skip_if(is.null(out), "no observation reached min.dif on this sample")

  expect_identical(names(out)[[1L]], "condition")
  expect_identical(unique(out$condition), "A")
})

test_that("a column position is resolved to its name", {
  out <- suppressMessages(sca_outliers(symmetry_data(), 1L, "Y"))
  skip_if(is.null(out), "no observation reached min.dif on this sample")
  expect_identical(unique(out$condition), "A")
})

test_that("arguments the model already fixes are refused, not overridden", {
  fit <- symmetry_fit()
  for (extra in list(list(y = "Y"), list(direction = "LH"),
                     list(scope = c(0, 1, 0, 1)))) {
    expect_error(
      do.call(sca_outliers, c(list(fit), extra)),
      "fitted model"
    )
  }
})

test_that("a direction equal to the default is still refused", {
  # The check is on whether the argument was named, not on whether it differs
  # from the fit: a caller who writes direction = "HH" is stating something,
  # and the honest answer is that the model has already stated it. Comparing
  # values instead would accept the one call that happens to agree and reject
  # the identical call that does not, which is harder to explain than a flat
  # rule.
  expect_error(sca_outliers(symmetry_fit(), direction = "HH"), "fitted model")
})

test_that("a data frame without x and y is refused by name", {
  expect_error(sca_outliers(symmetry_data()), "required")
})

test_that("x selects a subset of the model's conditions", {
  fit <- symmetry_fit()
  out <- suppressMessages(sca_outliers(fit, x = "B"))
  skip_if(is.null(out), "no observation reached min.dif on this sample")
  expect_identical(unique(out$condition), "B")
})


test_that("print() discloses multiplicity once there is more than one test", {
  fit <- suppressWarnings(sca_analysis(
    symmetry_data(), c("A", "B"), "Y", ceilings = "ce_fdh",
    test.rep = 20, test.seed = 1
  ))
  lines <- printed(fit)

  expect_match(lines, "multiplicity  2 conditions", fixed = TRUE)
  expect_match(lines, "none adjusted", fixed = TRUE)
  # The arithmetic, not just the word: 1 - 0.95^2.
  expect_match(lines, "at alpha 0.05 would be 0.10", fixed = TRUE)
})

test_that("a single condition and a single technique disclose nothing", {
  # The block is disclosure, and there is nothing to disclose here. An
  # unconditional line would train readers to skip it.
  fit <- suppressWarnings(sca_analysis(
    symmetry_data(), "A", "Y", ceilings = "ce_fdh",
    test.rep = 20, test.seed = 1
  ))
  expect_false(grepl("multiplicity", printed(fit), fixed = TRUE))
})

test_that("several techniques for one claim get their own warning", {
  # Not a family-wise rate: two techniques estimate one claim. The problem is
  # choosing between them after seeing both, which is a different mistake and
  # gets a different sentence.
  fit <- suppressWarnings(sca_analysis(
    symmetry_data(), "A", "Y", ceilings = c("ce_fdh", "cr_fdh"),
    test.rep = 20, test.seed = 1
  ))
  lines <- printed(fit)
  expect_match(lines, "1 condition x 2 techniques", fixed = TRUE)
  expect_match(lines, "chosen in advance", fixed = TRUE)
  expect_false(grepl("at least one false positive", lines, fixed = TRUE))
})

test_that("no permutation test means no disclosure", {
  fit <- symmetry_fit()
  expect_false(grepl("multiplicity", printed(fit), fixed = TRUE))
})


test_that("sca_compare() reports the least-squares beta whatever line was asked for", {
  # `beta` is documented as the OLS coefficient and the configuration readings
  # say "on average". A model fitted with reference = "qr" used to put the
  # median slope in that column under the OLS label.
  data <- symmetry_data()
  ols <- suppressWarnings(sca_analysis(
    data, "A", "Y", ceilings = "ce_fdh", test.rep = 0, reference = "ols"
  ))
  qr <- suppressWarnings(sca_analysis(
    data, "A", "Y", ceilings = "ce_fdh", test.rep = 0, reference = "qr"
  ))

  expect_equal(sca_compare(qr)$beta, sca_compare(ols)$beta)
  expect_equal(
    sca_compare(qr)$beta,
    sca_reference(qr, line = "ols")$beta
  )
})
