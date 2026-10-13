# The four demonstration datasets, and the classifier that has to agree with
# the paper they come from.
#
# An earlier draft of sca_compare() numbered the configurations from strongest
# to weakest while the paper numbers them from weakest to strongest, so I and
# IV came out reversed. These tests pin each shipped dataset to its own label,
# which is the only check that would have caught it.

scope_100 <- c(0, 100, 0, 100)

fit_configuration <- function(tag, ...) {
  data <- subset(sca_four_configurations, configuration == tag)
  sca_analysis(data, x = "X", y = "Y", direction = "HH",
               scope = scope_100, ceilings = "ce_fdh", ...)
}

test_that("the dataset ships with four equal groups on the stated scope", {
  expect_equal(nrow(sca_four_configurations), 2000L)
  expect_setequal(unique(sca_four_configurations$configuration),
                  c("I", "II", "III", "IV"))
  expect_equal(as.integer(table(sca_four_configurations$configuration)),
               rep(500L, 4L))
  expect_true(all(sca_four_configurations$X >= 0 &
                    sca_four_configurations$X <= 100))
  expect_true(all(sca_four_configurations$Y >= 0 &
                    sca_four_configurations$Y <= 100))
})

test_that("each configuration lands on its own label", {
  for (tag in c("I", "II", "III", "IV")) {
    fit <- fit_configuration(tag)
    expect_identical(sca_compare(fit)$configuration[[1L]], tag,
                     info = paste("configuration", tag))
  }
})

test_that("the labels read weakest to strongest, as the paper numbers them", {
  expect_identical(
    unname(SCAtools:::.sca_configuration_label[c("I", "II", "III", "IV")]),
    c("neither", "sufficiency-dominant", "mean-dominant",
      "mean and sufficiency")
  )
})

test_that("I and II are the pair a regression cannot separate", {
  # Both have a flat slope. Only one has an empty corner. This is the whole
  # argument the four demonstrations exist to make, so it is worth an
  # assertion rather than a comment.
  compare <- function(tag) sca_compare(fit_configuration(tag))
  one <- compare("I")
  two <- compare("II")
  expect_false(one$average_effect_present)
  expect_false(two$average_effect_present)
  expect_false(one$sufficiency_present)
  expect_true(two$sufficiency_present)
})

test_that("the declared scope changes the effect size, and is recorded", {
  # Every effect size is an area over the declared scope, so fitting the same
  # data on the observed range is a different denominator and a different d.
  data <- subset(sca_four_configurations, configuration == "IV")
  declared <- sca_analysis(data, "X", "Y", ceilings = "ce_fdh",
                           scope = scope_100)
  observed <- sca_analysis(data, "X", "Y", ceilings = "ce_fdh")

  expect_identical(attr(declared, "sca")$scope_source, "declared")
  expect_identical(attr(observed, "sca")$scope_source, "observed")
  expect_false(isTRUE(all.equal(sca_table(declared)$effect_size,
                                sca_table(observed)$effect_size)))
})
