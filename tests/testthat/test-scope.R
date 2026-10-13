# The scope is half of every effect size, so everything that decides it, warns
# about it or reports it is checked here.

scope_data <- function(n = 40, seed = 11) {
  set.seed(seed)
  x <- sort(runif(n))
  data.frame(X = x, Y = pmin(pmax(x + rnorm(n, 0, 0.08), 0), 1))
}

two_conditions <- function(n = 40, seed = 11) {
  dat <- scope_data(n, seed)
  data.frame(A = dat$X, B = dat$X * 2, Y = dat$Y)
}


# ---- sca_scope() ---------------------------------------------------------

test_that("sca_scope() reports the observed scope when none was declared", {
  fit <- sca_analysis(scope_data(), "X", "Y", ceilings = "ce_fdh")
  s <- sca_scope(fit)

  expect_equal(nrow(s), 1L)
  expect_identical(s$source, "observed")
  expect_false(s$adjusted)
  # Nothing was declared, so the declared columns have nothing to report.
  expect_true(all(is.na(c(s$declared_xmin, s$declared_xmax,
                          s$declared_ymin, s$declared_ymax))))
  expect_true(is.finite(s$scope_area))
})

test_that("sca_scope() reports both scopes when one was declared", {
  fit <- sca_analysis(scope_data(), "X", "Y", ceilings = "ce_fdh",
                      scope = c(0, 1, 0, 1))
  s <- sca_scope(fit)

  expect_identical(s$source, "declared")
  expect_false(s$adjusted)
  expect_equal(s$declared_xmin, 0)
  expect_equal(s$declared_ymax, 1)
  expect_equal(s$scope_area, 1)
})

test_that("sca_scope() has one row per condition", {
  fit <- sca_analysis(two_conditions(), c("A", "B"), "Y", ceilings = "ce_fdh")
  s <- sca_scope(fit)
  expect_equal(nrow(s), 2L)
  expect_setequal(s$condition, c("A", "B"))
})


# ---- the denominator, in the table --------------------------------------

test_that("effect_size is empty_space_area over scope_area, exactly", {
  # The point of carrying both areas is that the ratio is checkable in the
  # table rather than taken from the documentation.
  fit <- sca_analysis(scope_data(), "X", "Y",
                      ceilings = c("ce_fdh", "cr_fdh"),
                      scope = c(0, 1, 0, 1))
  tb <- sca_table(fit)

  expect_true(all(c("scope_area", "scope_adjusted") %in% names(tb)))
  expect_equal(tb$effect_size, tb$empty_space_area / tb$scope_area)
  expect_false(any(tb$scope_adjusted))
})


# ---- the engine widens a scope that does not hold the data ---------------

test_that("a scope that excludes observations is widened, and said so", {
  # p_loop_data.R replaces a declared bound with the observation that falls
  # outside it, which changes the denominator of every effect size while the
  # declared scope stays as written.
  dat <- scope_data()
  narrow <- c(0.2, 0.8, 0.2, 0.8)

  expect_warning(
    fit <- sca_analysis(dat, "X", "Y", ceilings = "ce_fdh", scope = narrow),
    "widened"
  )

  s <- suppressWarnings(sca_scope(fit))
  expect_true(s$adjusted)
  expect_true(s$used_xmin < 0.2 || s$used_xmax > 0.8 ||
                s$used_ymin < 0.2 || s$used_ymax > 0.8)
  expect_true(suppressWarnings(sca_table(fit))$scope_adjusted[[1L]])
})

test_that("the widening warning names the condition and both bounds", {
  # The engine's own message names neither, and is suppressed after the first
  # condition, so an analysis of several reports one anonymous sentence.
  dat <- scope_data()
  seen <- character(0)
  withCallingHandlers(
    sca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                 scope = c(0.2, 0.8, 0.2, 0.8)),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  widening <- seen[grepl("widened", seen)]
  # One warning for the whole fit, listing every bound it had to move.
  expect_length(widening, 1L)
  expect_match(widening, "bound")
  expect_match(widening, "->", fixed = TRUE)
})

test_that("a scope that holds every observation does not warn about it", {
  # Not expect_silent(): the engine prints its own progress and timing, and
  # this test is about our warnings, not about the engine being quiet.
  seen <- character(0)
  withCallingHandlers(
    sca_analysis(scope_data(), "X", "Y", ceilings = "ce_fdh",
                 scope = c(0, 1, 0, 1)),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("widened", seen)))
  expect_false(any(grepl("span", seen)))
})


# ---- the engine's flag in the caller's global environment ----------------

test_that("the engine's scope flag does not survive the call", {
  # p_loop_data.R writes `scope_warnings_nca` into .GlobalEnv to collapse its
  # own repeated warnings, and never removes it.
  if (exists("scope_warnings_nca", envir = globalenv(), inherits = FALSE)) {
    rm("scope_warnings_nca", envir = globalenv())
  }
  suppressWarnings(
    sca_analysis(scope_data(), "X", "Y", ceilings = "ce_fdh",
                 scope = c(0.2, 0.8, 0.2, 0.8))
  )
  expect_false(exists("scope_warnings_nca", envir = globalenv(),
                      inherits = FALSE))
})

test_that("a flag that was already there is left alone", {
  assign("scope_warnings_nca", TRUE, envir = globalenv())
  on.exit(
    suppressWarnings(rm("scope_warnings_nca", envir = globalenv())),
    add = TRUE
  )
  suppressWarnings(
    sca_analysis(scope_data(), "X", "Y", ceilings = "ce_fdh")
  )
  expect_true(exists("scope_warnings_nca", envir = globalenv(),
                     inherits = FALSE))
})


# ---- coverage: when the areas describe the scope, not the data -----------

test_that("a scope far wider than the data warns about coverage", {
  # As coverage falls the effect size stops describing the data and starts
  # describing the scope, and it does so flatteringly.
  seen <- character(0)
  withCallingHandlers(
    sca_analysis(scope_data(), "X", "Y", ceilings = "ce_fdh",
                 scope = c(-1000, 1000, -1000, 1000)),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("span", seen)))
})

test_that("coverage is recorded per condition whether or not it warns", {
  fit <- sca_analysis(two_conditions(), c("A", "B"), "Y", ceilings = "ce_fdh")
  coverage <- attr(fit, "sca")$scope_coverage
  expect_setequal(names(coverage), c("A", "B"))
  expect_true(all(coverage > 0.5))
})


# ---- one outcome column, one theoretical range (D-19) --------------------

test_that("a per-condition scope with two outcome ranges is refused", {
  expect_error(
    sca_analysis(two_conditions(), c("A", "B"), "Y", ceilings = "ce_fdh",
                 scope = list(c(0, 1, 0, 1), c(0, 2, 0, 10))),
    "more than one theoretical range"
  )
})

test_that("the 4 * k vector form is refused too", {
  # NCA checks the list form and not this one: p_validate_scope_vector() only
  # splits, while p_validate_scope_list() compares the outcome halves.
  expect_error(
    sca_analysis(two_conditions(), c("A", "B"), "Y", ceilings = "ce_fdh",
                 scope = c(0, 1, 0, 1, 0, 2, 0, 10)),
    "more than one theoretical range"
  )
})

test_that("a bound declared for one condition and NA for another is refused", {
  # The quiet form: nobody writes two different numbers, the second segment
  # just leaves the outcome undeclared, which means "use the observed range".
  expect_error(
    sca_analysis(two_conditions(), c("A", "B"), "Y", ceilings = "ce_fdh",
                 scope = list(c(0, 1, 0, 1), c(0, 2, NA, NA))),
    "not declared"
  )
})

test_that("the refusal names the bound and says how to fix it", {
  err <- tryCatch(
    sca_analysis(two_conditions(), c("A", "B"), "Y", ceilings = "ce_fdh",
                 scope = list(c(0, 1, 0, 1), c(0, 2, 0, 10))),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "Y upper", fixed = TRUE)
  expect_match(err, "same ymin and ymax in every segment", fixed = TRUE)
  expect_match(err, "its own call", fixed = TRUE)
})

test_that("differing X ranges with one outcome range still work", {
  # This is what the per-condition form exists for and must not be refused.
  fit <- sca_analysis(two_conditions(), c("A", "B"), "Y", ceilings = "ce_fdh",
                      scope = list(c(0, 1, 0, 1), c(0, 2, 0, 1)))
  expect_s3_class(fit, "sca_result")
  expect_equal(nrow(sca_table(fit)), 2L)
})

test_that("a single four-vector is replicated and cannot trigger the refusal", {
  fit <- sca_analysis(two_conditions(), c("A", "B"), "Y", ceilings = "ce_fdh",
                      scope = c(0, 2, 0, 1))
  expect_s3_class(fit, "sca_result")
})

test_that("one condition is never affected", {
  fit <- sca_analysis(scope_data(), "X", "Y", ceilings = "ce_fdh",
                      scope = c(0, 1, 0, 1))
  expect_s3_class(fit, "sca_result")
})


# ---- the sensitivity series reports effect sizes only (4d) -------------

test_that("sca_scope_sensitivity() reports the series without a chosen baseline", {
  # An earlier version reported the change from the first row and that change
  # as a share of the effect size. Which row is the baseline is the caller's
  # question, not the table's, so the table gives the effect sizes and stops.
  fit <- sca_analysis(scope_data(), "X", "Y", ceilings = "ce_fdh")
  out <- sca_scope_sensitivity(fit, widen = c(0, 0.5))

  expect_equal(nrow(out), 2L)
  expect_true(all(c("scope", "scope_area", "empty_space_area",
                    "effect_size") %in% names(out)))
  expect_false("share_of_d_from_widening" %in% names(out))
  expect_false("d_change" %in% names(out))
  expect_identical(out$scope[[1L]], "observed range")
})

test_that("the series is monotone in the area of the box", {
  fit <- sca_analysis(scope_data(), "X", "Y", ceilings = "ce_fdh")
  out <- sca_scope_sensitivity(fit, widen = c(0, 0.25, 0.5))
  expect_true(all(diff(out$scope_area) > 0))
})

test_that("the box the model was fitted on appears when one was declared", {
  fit <- sca_analysis(scope_data(), "X", "Y", ceilings = "ce_fdh",
                      scope = c(0, 1, 0, 1))
  out <- sca_scope_sensitivity(fit, widen = c(0, 0.5))
  expect_true("as fitted (declared)" %in% out$scope)
})

test_that("the refit carries the engine settings that change the frontier", {
  # Otherwise a qr sensitivity compares boxes and taus at once.
  fit <- sca_analysis(scope_data(), "X", "Y", ceilings = "qr", qr.tau = 0.80)
  expect_equal(attr(fit, "sca")$qr.tau, 0.80)
  out <- sca_scope_sensitivity(fit, ceiling = "qr", widen = 0)
  # The zero-widening row is the observed range, which is the box the model
  # itself used, so the two effect sizes must agree.
  expect_equal(out$effect_size[[1L]], sca_table(fit)$effect_size[[1L]],
               tolerance = 1e-8)
})


# ---- a flat condition is the same degeneracy as a flat outcome ----------

test_that("an analysis in which no condition varies is refused", {
  # A constant outcome has been refused since 0.4.x: it leaves the scope with
  # no height, so every empty area is measured against zero. A constant
  # condition does the same on the other axis, and until 0.5.0 it returned a
  # fit with scope_area = 0, every effect size NaN and every p-value NA.
  flat <- data.frame(X = rep(0.5, 30), Y = runif(30))
  expect_error(sca_analysis(flat, "X", "Y", ceilings = "ce_fdh"),
               "No condition varies")
})

test_that("the refusal names the escape hatch, and it works", {
  flat <- data.frame(X = rep(0.5, 30), Y = runif(30))
  err <- tryCatch(sca_analysis(flat, "X", "Y", ceilings = "ce_fdh"),
                  error = function(e) conditionMessage(e))
  expect_match(err, "theoretical 'scope'", fixed = TRUE)

  # With a declared scope the box has width, so the areas are defined -- and
  # the message says the result then depends on where the constant sits.
  fit <- sca_analysis(flat, "X", "Y", ceilings = "ce_fdh",
                      scope = c(0, 1, 0, 1))
  expect_true(is.finite(sca_table(fit)$effect_size[[1L]]))
})

test_that("one flat condition among several does not stop the others", {
  # The refusal is for the case where the analysis has nothing to say at all.
  # Stopping here would throw away the condition that is estimable.
  dat <- data.frame(A = rep(0.5, 30), B = sort(runif(30)), Y = runif(30))
  fit <- suppressWarnings(
    sca_analysis(dat, c("A", "B"), "Y", ceilings = "ce_fdh")
  )
  tb <- suppressWarnings(sca_table(fit))
  expect_true(is.na(tb$effect_size[tb$condition == "A"]))
  expect_true(is.finite(tb$effect_size[tb$condition == "B"]))
})

test_that("a flat condition still warns about what it means", {
  dat <- data.frame(A = rep(0.5, 30), B = sort(runif(30)), Y = runif(30))
  seen <- character(0)
  withCallingHandlers(
    sca_analysis(dat, c("A", "B"), "Y", ceilings = "ce_fdh"),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("does not vary", seen)))
})
