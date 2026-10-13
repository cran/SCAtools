# The Settings block exists so that a reader can tell which numbers were the
# researcher's decisions and which were the package's. That distinction is the
# thing being tested here, not the formatting.

settings_data <- function(n = 40, seed = 7) {
  set.seed(seed)
  x <- sort(runif(n))
  data.frame(X = x, Y = pmin(pmax(x + rnorm(n, 0, 0.08), 0), 1))
}

printed_lines <- function(fit) {
  strsplit(
    paste(utils::capture.output(suppressWarnings(print(fit))), collapse = "\n"),
    "\n", fixed = TRUE
  )[[1L]]
}

line_for <- function(lines, label) {
  hit <- grep(paste0("^  ", label), lines, value = TRUE)
  if (length(hit) == 0L) NA_character_ else hit[[1L]]
}


test_that("print() ends with a Settings block naming every choice", {
  fit <- sca_analysis(settings_data(), "X", "Y", ceilings = "ce_fdh")
  lines <- printed_lines(fit)

  expect_true(any(grepl("^Settings$", lines)))
  for (label in c("frontiers", "test.rep", "alpha", "test.seed", "relevance",
                  "scope", "na.action", "thresholds", "convention")) {
    expect_false(is.na(line_for(lines, label)), info = label)
  }
})

test_that("an argument the call named is not marked as a default", {
  fit <- sca_analysis(settings_data(), "X", "Y",
                      ceilings = "ce_fdh", test.rep = 20, test.seed = 1)
  lines <- printed_lines(fit)

  expect_false(grepl("(default)", line_for(lines, "test.rep"), fixed = TRUE))
  expect_false(grepl("(default)", line_for(lines, "frontiers"), fixed = TRUE))
  # Never passed, so the package chose it.
  expect_true(grepl("(default)", line_for(lines, "na.action"), fixed = TRUE))
  expect_true(grepl("(default)", line_for(lines, "relevance"), fixed = TRUE))
})

test_that("the marks come from the call, not from a comparison with formals", {
  # Passing a value that happens to equal the default must still read as the
  # caller's decision: they stated it, and a methods section reports that.
  fit <- sca_analysis(settings_data(), "X", "Y", ceilings = "ce_fdh",
                      na.action = "pairwise")
  expect_false(
    grepl("(default)", line_for(printed_lines(fit), "na.action"), fixed = TRUE)
  )
})

test_that("sca() records its own call so its defaults read as defaults", {
  # sca() forwards every one of its arguments to sca_analysis() by name, so
  # that function's match.call() would report all of them as supplied.
  fit <- suppressWarnings(
    sca(settings_data(), "X", "Y", test.rep = 0)
  )
  expect_identical(as.character(attr(fit, "sca")$call[[1L]]), "sca")

  lines <- printed_lines(fit)
  expect_true(grepl("(default)", line_for(lines, "frontiers"), fixed = TRUE))
  expect_false(grepl("(default)", line_for(lines, "test.rep"), fixed = TRUE))
})

test_that("the block reports the scope source and the reporting scales", {
  observed <- sca_analysis(settings_data(), "X", "Y", ceilings = "ce_fdh")
  expect_match(line_for(printed_lines(observed), "scope"), "observed range")

  declared <- sca_analysis(settings_data(), "X", "Y", ceilings = "ce_fdh",
                           scope = c(0, 1, 0, 1))
  expect_match(line_for(printed_lines(declared), "scope"), "declared")

  # The readable scale names, which used to sit under their own header without
  # saying whether anyone had chosen them.
  expect_match(line_for(printed_lines(observed), "thresholds"), "condition on")
})

test_that("a widened scope is reported under the scope line", {
  fit <- suppressWarnings(
    sca_analysis(settings_data(), "X", "Y", ceilings = "ce_fdh",
                 scope = c(0.2, 0.8, 0.2, 0.8))
  )
  printed <- paste(printed_lines(fit), collapse = "\n")
  expect_match(printed, "widened by the engine")
  expect_match(printed, "sca_scope()", fixed = TRUE)
})

test_that("sca_output() ends with the same block", {
  # It assembles a report, so it must not silently drop what print() says.
  fit <- sca_analysis(settings_data(), "X", "Y", ceilings = "ce_fdh")
  printed <- paste(
    utils::capture.output(suppressWarnings(sca_output(fit, plots = FALSE))),
    collapse = "\n"
  )
  expect_match(printed, "Settings")
  expect_match(printed, "na.action")
})
