# Three things that were deferred past 0.5.0 and then pulled back into it:
# a multiple-testing correction the caller asks for, the independence floor
# the permutation test was already computing and discarding, and resampling.
#
# What most of these tests are really guarding is restraint. The adjustment
# must not become a default, the floor must not become a corrected effect
# size, and the resampling must not grow a confidence interval.

ar_data <- function(n = 60, seed = 11) {
  set.seed(seed)
  a <- sort(stats::runif(n))
  data.frame(
    A = a,
    B = stats::runif(n),
    C = stats::runif(n),
    Y = pmin(pmax(0.15 + 0.7 * a + stats::rnorm(n, 0, 0.07), 0), 1)
  )
}

ar_fit <- function(x = c("A", "B", "C"), ..., data = ar_data()) {
  suppressWarnings(suppressMessages(
    sca_analysis(data, x, "Y", ceilings = "ce_fdh",
                 test.rep = 200, test.seed = 1, ...)
  ))
}


## ------------------------------------------------- multiple-testing p.adjust

test_that("the default changes nothing at all", {
  # The package discloses multiplicity and does not correct for it unasked.
  # A default here would be the package deciding what the family is.
  fit <- ar_fit()
  expect_identical(names(sca_table(fit)), names(sca_table(fit, p.adjust = "none")))
  expect_false("p_value_adjusted" %in% names(sca_table(fit)))
})

test_that("an adjustment adds the adjusted value and the method beside it", {
  out <- sca_table(ar_fit(), p.adjust = "BH")
  expect_true(all(c("p_value_adjusted", "p_adjust_method") %in% names(out)))
  expect_identical(unique(out$p_adjust_method), "BH")
  # Beside the column it adjusts, not at the far end of a forty-column table.
  expect_identical(
    which(names(out) == "p_value_adjusted"),
    which(names(out) == "p_value") + 1L
  )
})

test_that("the adjusted p is stats::p.adjust() over the conditions", {
  fit <- ar_fit()
  raw <- sca_table(fit)
  out <- sca_table(fit, p.adjust = "holm")
  expect_equal(out$p_value_adjusted,
               stats::p.adjust(raw$p_value, method = "holm"))
  expect_true(all(out$p_value_adjusted >= raw$p_value - 1e-12))
})

test_that("the family is the conditions, not the techniques", {
  # Two techniques are two estimates of one claim. Adjusting across them would
  # inflate k with re-measurements and cost power for nothing.
  fit <- suppressWarnings(suppressMessages(
    sca_analysis(ar_data(), c("A", "B", "C"), "Y",
                 ceilings = c("ce_fdh", "cr_fdh"), test.rep = 200,
                 test.seed = 1)
  ))
  out <- sca_table(fit, p.adjust = "bonferroni")
  for (technique in unique(out$frontier)) {
    block <- out[out$frontier == technique, ]
    expect_equal(block$p_value_adjusted,
                 stats::p.adjust(block$p_value, method = "bonferroni"),
                 info = technique)
  }
})

test_that("every decision is re-derived, not left on the raw p-value", {
  # An adjusted number printed beside a verdict that ignored it is worse than
  # not adjusting at all.
  fit <- ar_fit()
  out <- sca_table(fit, p.adjust = "bonferroni")
  expect_identical(out$significant, out$p_value_adjusted <= 0.05)
  expect_false(any(out$support_status == "supported" & !out$significant))
})

test_that("relevance is untouched by an adjustment", {
  # An effect size does not shrink because more tests were run.
  fit <- ar_fit(relevance = 0.2)
  raw <- sca_table(fit)
  out <- sca_table(fit, p.adjust = "bonferroni")
  expect_identical(raw$relevant, out$relevant)
  expect_identical(raw$effect_size, out$effect_size)
})

test_that("an unknown method is refused and lists what is available", {
  expect_error(sca_table(ar_fit(), p.adjust = "banana"), "Unknown p.adjust")
  expect_error(sca_table(ar_fit(), p.adjust = "banana"), "bonferroni")
})


## ---------------------------------------------------- the independence floor

test_that("the table reports what random pairing alone produces", {
  out <- sca_table(ar_fit("A"))
  expect_true(all(c("effect_size_null", "effect_size_excess") %in% names(out)))
  expect_equal(out$effect_size_excess, out$effect_size - out$effect_size_null)
  expect_true(out$effect_size_null > 0)
})

test_that("the floor is the mean of the permutation null, not a formula", {
  fit <- ar_fit("A")
  draws <- fit$tests[["A"]][["ce_fdh"]]$data
  expect_equal(sca_table(fit)$effect_size_null, mean(draws))
})

test_that("the floor falls as the sample grows", {
  # This is the whole point of reporting it: an effect size from 40 cases and
  # one from 400 are not on the same scale.
  small <- sca_table(ar_fit("A", data = ar_data(n = 40)))$effect_size_null
  large <- sca_table(ar_fit("A", data = ar_data(n = 300)))$effect_size_null
  expect_true(small > large)
})

test_that("with no permutation test there is no floor to report", {
  # Zero would be a claim; NA says the question was not asked, which is the
  # same rule the significance columns already follow.
  fit <- suppressWarnings(suppressMessages(
    sca_analysis(ar_data(), "A", "Y", ceilings = "ce_fdh", test.rep = 0)
  ))
  out <- sca_table(fit)
  expect_true(is.na(out$effect_size_null))
  expect_true(is.na(out$effect_size_excess))
})

test_that("the excess can be negative, and that is the finding", {
  # A condition that is pure noise can still show a positive effect size,
  # because the envelope is fitted to sample extremes. When the observed value
  # sits below what random pairing produces, all of it was artefact.
  out <- sca_table(ar_fit(c("A", "B", "C")))
  noise <- out[out$condition %in% c("B", "C"), ]
  expect_true(any(noise$effect_size > 0))
  expect_true(any(noise$effect_size_excess < 0.05))
})


## --------------------------------------------------------- sca_resample()

test_that("the subsample distribution is reported at every size", {
  fit <- ar_fit("A", data = ar_data(n = 100))
  out <- sca_resample(fit, sizes = c(0.4, 0.8), reps = 20, seed = 1)

  expect_identical(nrow(out), 2L)
  expect_identical(out$m, c(40L, 80L))
  expect_true(all(out$converged == 20L))
  # The anchor, repeated: the full-sample value the subsamples are read against.
  expect_equal(unique(out$effect_size), sca_table(fit)$effect_size)
})

test_that("no confidence interval is produced", {
  # Deliberate. The n-out-of-n bootstrap is inconsistent for a boundary
  # estimator, and the subsampling distribution is biased upward, so a
  # percentile interval would sit above what it claims to cover. The columns
  # are named for what they are.
  out <- sca_resample(ar_fit("A"), sizes = 0.5, reps = 10, seed = 1)
  expect_false(any(grepl("conf|ci_|lower|upper", names(out))))
  expect_true(all(c("subsample_p05", "subsample_p95") %in% names(out)))
})

test_that("the subsample median falls as the subsample grows", {
  # The bias, made visible rather than modelled: fewer points leave more of
  # the corner empty by accident.
  fit <- ar_fit("A", data = ar_data(n = 150))
  out <- sca_resample(fit, sizes = c(0.3, 0.6, 0.9), reps = 30, seed = 1)
  expect_true(all(diff(out$subsample_median) < 0))
  expect_true(all(out$subsample_median > out$effect_size))
})

test_that("a full-size draw is taken once, not reps times", {
  # Without replacement there is exactly one subsample of size n, so drawing
  # it repeatedly would report a spread of zero as though it were estimated.
  out <- sca_resample(ar_fit("A"), sizes = 1, reps = 50, seed = 1)
  expect_identical(out$reps, 1L)
  expect_true(is.na(out$subsample_sd))
  expect_equal(out$subsample_median, out$effect_size)
})

test_that("sizes may be counts as well as fractions", {
  out <- sca_resample(ar_fit("A"), sizes = c(20, 40), reps = 10, seed = 1)
  expect_identical(out$m, c(20L, 40L))
})

test_that("the caller's random stream is given back", {
  fit <- ar_fit("A")
  set.seed(99)
  before <- stats::runif(1)
  set.seed(99)
  invisible(sca_resample(fit, sizes = 0.5, reps = 5, seed = 3))
  expect_identical(before, stats::runif(1))
})

test_that("the seed makes it reproducible", {
  fit <- ar_fit("A")
  a <- sca_resample(fit, sizes = 0.5, reps = 15, seed = 5)
  b <- sca_resample(fit, sizes = 0.5, reps = 15, seed = 5)
  expect_identical(a, b)
})

test_that("more than one condition is refused rather than silently reduced", {
  expect_error(sca_resample(ar_fit(), x = c("A", "B")), "exactly one")
})

test_that("bad sizes and reps are refused by name", {
  fit <- ar_fit("A")
  expect_error(sca_resample(fit, sizes = -1), "positive")
  expect_error(sca_resample(fit, sizes = 0.5, reps = 0), "positive whole")
})


## ------------------------------------------- the replication floor warning

test_that("an adjustment that cannot reject says so", {
  # A permutation p cannot fall below 1/(test.rep + 1), and the most
  # significant of k conditions is multiplied by k, so unless
  # k/(test.rep + 1) <= alpha nothing can reach alpha however strong the
  # evidence -- which looks exactly like a null result and is not one.
  set.seed(4)
  n <- 80
  data <- data.frame(Y = stats::runif(n))
  for (i in 1:8) data[[paste0("C", i)]] <- stats::runif(n)
  data$C1 <- pmin(pmax(data$Y + stats::rnorm(n, 0, 0.1), 0), 1)

  thin <- suppressWarnings(suppressMessages(sca_analysis(
    data, paste0("C", 1:8), "Y", ceilings = "ce_fdh",
    test.rep = 50, test.seed = 1
  )))
  expect_warning(sca_table(thin, p.adjust = "BH"), "arithmetic rather than")
  expect_warning(sca_table(thin, p.adjust = "bonferroni"), "test.rep >= 159")
})

test_that("the floor warning is silent when it should be", {
  fit <- ar_fit()
  expect_silent(sca_table(fit))
  expect_silent(sca_table(fit, p.adjust = "BH"))
})
