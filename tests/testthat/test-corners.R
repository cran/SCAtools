test_that("all logical directions map to the correct physical corners", {
  expect_equal(sca_corner(c("HH", "LH", "HL", "LL")), c(4L, 3L, 2L, 1L))
  expect_equal(sca_direction(1:4), c("LL", "HL", "LH", "HH"))
})

test_that("corner table preserves logical and geometric numbering", {
  map <- sca_corner_map()
  expect_equal(map$sca_number, 1:4)
  expect_equal(map$direction, c("HH", "LH", "HL", "LL"))
  expect_equal(map$empty_corner, c(4L, 3L, 2L, 1L))
  expect_equal(map$location, c("lower-right", "lower-left", "upper-right", "upper-left"))
})

test_that("invalid directions and corners fail clearly", {
  expect_error(sca_corner("HX"), "Invalid direction")
  expect_error(sca_direction(5), "integers from 1 through 4")
})

test_that("corner table reproduces the sufficiency half of Table 1", {
  map <- sca_corner_map()
  expect_equal(
    map$relationship,
    c("Higher X for higher Y", "Lower X for higher Y",
      "Higher X for lower Y", "Lower X for lower Y")
  )
  expect_equal(
    map$empty_space,
    c("High X with low Y", "Low X with low Y",
      "High X with high Y", "Low X with high Y")
  )
  expect_equal(map$boundary_type, c("floor", "floor", "ceiling", "ceiling"))
})

test_that("boundary type follows the empty corner and nothing else", {
  map <- sca_corner_map()
  # A corner in the upper half is bounded from above, which is a ceiling.
  expect_equal(
    map$boundary_type,
    ifelse(map$empty_corner %in% c(1L, 2L), "ceiling", "floor")
  )
  # Equivalently, sufficiency has a floor exactly when the outcome level is
  # high, which is the reading of Table 1.
  expect_equal(
    map$boundary_type,
    ifelse(substr(map$direction, 2L, 2L) == "H", "floor", "ceiling")
  )
})

test_that("the glossary and the legacy map are self-consistent", {
  terms <- sca_terms()
  expect_true(all(c("scope", "boundary", "frontier", "ceiling", "floor") %in%
                    terms$term))
  expect_equal(ncol(terms), 3L)

  legacy <- sca_legacy_names()
  expect_true(is.character(legacy))
  expect_true(all(c("condition_threshold", "boundary") %in% names(legacy)))
  expect_equal(unname(legacy[["condition_threshold"]]), "sufficiency_threshold")
  expect_equal(unname(legacy[["boundary"]]), "inequality")
})

