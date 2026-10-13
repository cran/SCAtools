# The conventional bands, and the line between citing one and inventing one.
#
# Until 0.5.0 these lived in a code comment while the package quietly applied
# Dul's lower edge as the default `d_cut`. A convention applied and not
# disclosed is worse than either applying it openly or not applying it, which
# is what most of these tests are really guarding.

test_that("the band table names its source and its status", {
  bands <- sca_magnitude()
  expect_identical(nrow(bands), 4L)
  expect_identical(bands$label, c("small", "medium", "large", "very large"))
  # Cited, not calibrated -- and the table says so in a column, not only in
  # the help page, because a table gets pasted into a script and a help page
  # does not.
  expect_true(all(grepl("Dul", bands$source)))
  expect_true(all(grepl("not calibrated", bands$source)))
})

test_that("the cuts are the ones the package already applies", {
  # `.sca_d_cut_default` is 0.10, the lower edge of medium. If the band table
  # and the default cut ever disagree, one of them is lying about the other.
  bands <- sca_magnitude()
  expect_identical(
    bands$lower[bands$label == "medium"],
    get(".sca_d_cut_default", envir = asNamespace("SCAtools"))
  )
})

test_that("values land in the band the table describes", {
  expect_identical(
    sca_magnitude(c(0, 0.04, 0.10, 0.18, 0.30, 0.35, 0.50, 0.62)),
    c("small", "small", "medium", "medium", "large", "large",
      "very large", "very large")
  )
})

test_that("the boundaries belong to the band above them", {
  # 0.10 is medium, not small: the convention is stated as `.10-.30 medium`.
  expect_identical(sca_magnitude(0.1), "medium")
  expect_identical(sca_magnitude(0.3), "large")
  expect_identical(sca_magnitude(0.5), "very large")
  expect_identical(sca_magnitude(0.0999999), "small")
})

test_that("a value that is not an effect size gets no label", {
  expect_true(is.na(sca_magnitude(NA_real_)))
  expect_true(is.na(sca_magnitude(-0.2)))
  expect_true(is.na(sca_magnitude(Inf)))
})

test_that("sca_compare() carries the band beside the number", {
  set.seed(11)
  n <- 60
  a <- sort(stats::runif(n))
  data <- data.frame(
    A = a, B = stats::runif(n),
    Y = pmin(pmax(0.15 + 0.7 * a + stats::rnorm(n, 0, 0.07), 0), 1)
  )
  fit <- suppressWarnings(suppressMessages(
    sca_analysis(data, c("A", "B"), "Y", ceilings = "ce_fdh",
                 test.rep = 100, test.seed = 1)
  ))
  out <- sca_compare(fit)
  expect_true("magnitude" %in% names(out))
  expect_identical(out$magnitude, sca_magnitude(out$effect_size))
  # And beside the floor, so a band label and the geometry that produced part
  # of it are read together rather than one without the other.
  expect_true(all(c("effect_size_null", "effect_size_excess") %in% names(out)))
})


## ------------------------------------------------- Cohen's bands for beta

test_that("the beta bands name Cohen and the quantity they describe", {
  bands <- sca_magnitude(scale = "beta")
  expect_identical(bands$label,
                   c("below small", "small", "medium", "large"))
  expect_true(all(grepl("Cohen", bands$source)))
  expect_true(all(grepl("not calibrated", bands$source)))
  expect_true(all(grepl("standardised", bands$quantity)))
})

test_that("the beta cut the package applies is the edge of Cohen's medium", {
  bands <- sca_magnitude(scale = "beta")
  expect_identical(
    bands$lower[bands$label == "medium"],
    get(".sca_beta_cut_default", envir = asNamespace("SCAtools"))
  )
})

test_that("the two conventions share edges and shift labels by one band", {
  # The trap this function exists to expose. Both use .10, .30 and .50; a value
  # of 0.35 is medium as a coefficient and large as an effect size. It is why
  # beta_cut defaults to 0.30 and d_cut to 0.10.
  v <- c(0.05, 0.15, 0.35, 0.60)
  expect_identical(sca_magnitude(v, scale = "beta"),
                   c("below small", "small", "medium", "large"))
  expect_identical(sca_magnitude(v),
                   c("small", "medium", "large", "very large"))
  expect_identical(
    sca_magnitude(scale = "beta")$lower,
    sca_magnitude()$lower
  )
})

test_that("a beta band describes a magnitude, not a direction", {
  expect_identical(sca_magnitude(-0.45, scale = "beta"),
                   sca_magnitude(0.45, scale = "beta"))
  # The effect-size scale has no negative values to speak of, and refuses one.
  expect_true(is.na(sca_magnitude(-0.45)))
})

test_that("sca_compare() carries both bands beside their own numbers", {
  set.seed(11)
  n <- 60
  a <- sort(stats::runif(n))
  data <- data.frame(
    A = a, B = stats::runif(n),
    Y = pmin(pmax(0.15 + 0.7 * a + stats::rnorm(n, 0, 0.07), 0), 1)
  )
  fit <- suppressWarnings(suppressMessages(
    sca_analysis(data, c("A", "B"), "Y", ceilings = "ce_fdh",
                 test.rep = 100, test.seed = 1, reference = "ols")
  ))
  out <- sca_compare(fit)
  expect_true(all(c("beta_magnitude", "magnitude") %in% names(out)))
  expect_identical(out$beta_magnitude,
                   sca_magnitude(out$beta, scale = "beta"))
  expect_identical(out$magnitude, sca_magnitude(out$effect_size))
})

test_that("an unknown scale is refused", {
  expect_error(sca_magnitude(0.3, scale = "banana"))
})
