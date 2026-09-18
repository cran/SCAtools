# SCAtools 0.4.3

## Authorship and maintainer

`Authors@R` named a placeholder, `SCAtools Contributors
<maintainer@example.org>`, which CRAN rejects: the maintainer must be a real
person at a working address. Both are now Young Chan
<youngchanresearcher@gmail.com>.

`inst/CITATION` was missed by that pass and still returned the placeholder
author with `R package version 0.1.0`, so `citation("SCAtools")` contradicted
both `DESCRIPTION` and the package guide. It now names the same person and
reads the version from `DESCRIPTION`, so it cannot fall behind again.

`URL` pointed at the CRAN page of NCA, the computational engine, which reads as
if it were this package's home. It now points at this package's repository, and
`BugReports` is set alongside it:

    URL: https://github.com/youngchanresearcher/SCAtools
    BugReports: https://github.com/youngchanresearcher/SCAtools/issues

NCA's authorship and licence are stated in `Description`, `inst/NOTICE` and the
README, which is where that attribution belongs.

## sca_power() refuses a slope its direction cannot use

A frontier line bounds only two of the four corners: a rising line the
upper-left and lower-right (`HH`, `LL`), a falling line the upper-right and
lower-left (`LH`, `HL`). A horizontal line is accepted for any direction.

`sca_random()` inherited the engine's error for the impossible combination, but
`nca_power()` only warned and returned `NULL`. `sca_power(direction = "LH")`
with the default `slope = 1` therefore produced no result at all, and the
failure was silent by the time the `NULL` reached `sca_powerplot()`.
`sca_power()` now checks the sign itself and stops with the direction, the
corner and the offending value named. Every combination that worked before
still works: the check refuses exactly what the engine already refused.

## Documentation

- The 0.4.0 entry below overstated the reach of `legacy = TRUE`; corrected
  there.
- The README, its Traditional Chinese translation and the package guide all
  installed `SCAtools_0.1.0.tar.gz`, and both READMEs still asked the reader to
  replace a placeholder maintainer that no longer exists.

# SCAtools 0.4.2

A packaging release. No code, argument, column or default changed.

- `README_zh-TW.md` moves from the top level of the package to
  `inst/docs/README_zh-TW.md`. `R CMD check` counts any unrecognised
  top-level file as a NOTE; from `inst/` the translation is still shipped and
  is now reachable from R:
  `file.show(system.file("docs", "README_zh-TW.md", package = "SCAtools"))`.

`R CMD check --as-cran` on 0.4.1 was already clean for this package -- 0
errors, 0 warnings, 505 tests passing. This removes the one remaining
avoidable NOTE.

# SCAtools 0.4.1

## OLS is a reference line, not a ceiling technique

`ceilings = "ols"` asked the NCA engine for an ordinary least-squares
regression of the outcome on the condition, fitted to all the data: the line
`lm(y ~ x)` returns. It is a central-tendency summary, not a frontier. It
estimates no empty space, so it yields no ceiling zone, no effect size, no
permutation p value and no threshold table -- and it was the *first* entry of
the default `ceilings`, which made it the line `sca_extract()` reached for when
no ceiling was named.

Section 4.3 of the condition analysis in degree framework states the
difference: a frontier is fixed by the most extreme observations rather than by
the central tendency. Section 6.1 is why the line is still worth drawing.
Average-effect models and condition analysis answer different questions about
the same relationship and are most informative side by side, not merged into
one number.

- The default is now `ceilings = c("ce_fdh", "cr_fdh")`. Frontiers only.
- New `reference` argument for central-tendency lines drawn beside the
  frontier. `reference = "ols"` is the supported way to ask for the regression
  line.
- `"ols"` in `ceilings` still works: it is moved to `reference` with a warning.
  On its own it is now an error, because no empty space is left to estimate.
  This matches NSCA, which has always refused it as a component.
- New `sca_reference()` reports the line's intercept, slope, R-squared and the
  observations it was fitted to. It is computed here rather than read back from
  the engine, and is available whether or not the line was drawn.
- `sca_table()` reports frontiers only, even where an engine returns a column
  for a reference line.
- `sca_plot()` titles the legend "Line" rather than "Frontier" when a
  central-tendency line is among those drawn.
- `sca_terms()` gains a *central tendency* row.

Nothing else changed. No column, argument or status value was renamed.

# SCAtools 0.4.0

This release adopts the vocabulary of *condition analysis in degree*, the
framework that sets SCA and NSCA beside NCA. Terminology follows the article;
the package's own names are kept only where the article has nothing to say.
Nothing was removed: every retired name still works.

## Boundary and frontier are different objects

Section 3.2 of the framework uses five terms in sequence. The **scope** is the
region of the X-Y space bounded by the theoretical or observed extremes. An
**expected empty space** (or empty zone) is the part of that scope which should
hold no observations if the hypothesised relation holds. A **boundary** is the
theoretical line separating that space from the compatible region. A
**frontier** is the boundary estimated from data. **Ceiling** and **floor** name
the direction in which Y is bounded.

The package already used `frontier` for the estimated line, which is correct.
It also used `boundary` for something else entirely -- whether a reported rule
uses `>` or `>=` -- and that name is now needed for the theoretical line.

- `sca_thresholds()` and `sca_rescale()` rename the `boundary` argument to
  `inequality`. The old argument still works and warns; supplying both keeps
  `inequality` and warns again.
- The `boundary` output column of `sca_thresholds()` is renamed `inequality`.

## Ceiling and floor are now reported

For sufficiency the fitted boundary is a **floor** whenever the outcome level is
high (HH, LH) and a **ceiling** whenever it is low (HL, LL). The package
estimated the right thing but never said which it was, because the argument is
called `ceilings` in every direction. Three columns now state the geometry in
the framework's own words:

- `boundary_type` -- `"ceiling"` or `"floor"`.
- `empty_space` -- what the expected empty space contains, in the wording of
  Table 1 and with the user's own variable names filled in, for example
  `"High training with low score"`.
- `relationship` -- the direction as a claim about the theorised X-Y
  relationship, for example `"Higher training for higher score"`. This is a
  claim about the relationship, not about the shape of the estimated frontier,
  which may be a step function or a straight line under any of the four types.

All three appear in `sca_table()`, `sca_corner_map()` and the printed mapping;
`boundary_type` and `relationship` also appear in `sca_thresholds()`. They are
derived from the empty corner, which remains the single source of truth.

## The threshold is a sufficiency threshold

Section 4.3 names the quantity a sufficiency analysis reports: the lowest X at
which the estimated sufficiency frontier reaches a stated outcome target.

- `condition_threshold` becomes `sufficiency_threshold`.
- `condition_threshold_actual` becomes `sufficiency_threshold_actual`.

This also makes the two packages agree: NSCA already used these names for the
same quantity, and previously had to read SCAtools' differently named column.

## Retired names still work

- `sca_legacy_names()` returns the map from retired names to their
  replacements.
- `sca_table()` and `sca_thresholds()` gain `legacy = TRUE`, which appends the
  retired names as duplicate columns. Both retired *columns* belong to the
  threshold table, so in practice the argument adds columns in
  `sca_thresholds()` only; `sca_table()` accepts it for symmetry and returns
  the table unchanged.
- `sca_extract()` accepts a retired name with a warning. Asking it for a
  threshold-table column now explains that such a column is defined per outcome
  level and points to `sca_thresholds()`, instead of failing with a message
  about an unknown NCA parameter.

## Also

- `sca_terms()` is a new machine-readable glossary linking each term of the
  framework to the name this package uses for it.
- `sca_rescale()` was exported in 0.2.1 but never documented. It now has a help
  page, on `?sca_scales`.
- `sca_output()` gains `inequality`, which it passes to `sca_thresholds()`.
  Previously the strictness of the printed rules could not be controlled from
  `sca_output()` at all.

# SCAtools 0.2.1

- Adds `sca_rescale()`, the scale conversion used by `sca_thresholds()`, as a
  public function. Other packages need it to express their own quantities on
  the same scale: a threshold from another source is only comparable with an
  SCA threshold when both went through the same direction-aware mirroring and
  the same percentile strictness rule. Reaching into the internal function
  instead would break whenever this package changes.

# SCAtools 0.2.0

## Threshold tables replace bottleneck tables

- Renames the reporting arguments of `sca_analysis()` from `bottleneck.x` and
  `bottleneck.y` to `threshold.x` and `threshold.y`. A bottleneck is a
  necessity concept: it names the minimum level of a condition required to
  reach an outcome. A sufficiency statement has no bottleneck, so the term is
  no longer used. The old arguments still work and emit a deprecation warning.

- Replaces the engine's `NN` and `NA` markers with a `status` column stated in
  sufficiency terms. The two markers invert under contraposition, so copying
  them would reverse the reader's conclusion:

  | Engine cell | Necessity reading | Sufficiency reading | `status` |
  |---|---|---|---|
  | `NN` | no minimum is required | no attainable condition value reaches this outcome level | `no_threshold` |
  | `NA` | the requirement lies outside the scope | the threshold falls outside the scope, so the condition restricts nothing | `always_satisfied` |

  The rule text for these rows describes the condition side only. Asserting
  that the outcome holds for every observation would be stronger than the row
  supports, because a case sitting exactly on the boundary can still fail it.

## Percentage, percentile and standard-deviation scales

- Makes `percentage.range`, `percentage.max` and `percentile` reach the rules
  themselves, and adds `sd`. Previously the requested scale never reached the
  rule text: `sca_thresholds()` always wrote the inequality in actual units,
  and the requested scale appeared only in separate display columns that
  carried the engine's conventions unchanged. Asking for percentages therefore
  produced actual-unit rules beside percentage columns that could not be read
  against them.

- SCAtools now requests thresholds from the engine in actual units and applies
  every scale conversion itself. This fixes three defects that follow from the
  engine's own conventions, all of which affected SCAtools 0.1.0:

  - The displayed outcome column ran opposite to the actual outcome for the
    `HH` and `LH` directions. The engine reverses the actual outcome levels
    when the corner is flipped but leaves the displayed percentages ascending,
    so a row labelled `0%` carried the highest outcome value. Both `H`-outcome
    directions were affected, and `percentage.range` was the default.
  - Percentage scales were measured from the low end of the axis while the
    percentile scale was mirrored according to the physical corner. For `HH`
    and `HL` the reported percentile was the share of cases at or above the
    threshold, so a value of `70` meant the 30th percentile.
  - The out-of-range boundary cells used an unmirrored percentile even when the
    body of the same column was mirrored, placing two coordinate systems in one
    column.

- Adds a `convention` argument, and `sca_scales()` to list what is available.
  Under `"absolute"` (the default) `0` sits at the low end of every axis and
  the inequality carries the direction, so numbers are comparable across
  directions. Under `"directional"` `0` sits at the least sufficient end, the
  axis is mirrored for a low-level direction, and every rule reads as "more of
  the sufficient thing".

- Percentile strictness now follows the rule's inequality. A percentile is a
  step function, so pairing a strict `<` rule with `P(X <= t)` dropped the
  observation nearest the threshold once the data were expressed on the same
  scale. Each combination of direction, convention and boundary is now
  truth-preserving, and is covered by `tests/testthat/test-scales.R`.

- `sca_thresholds()` gains `scale`, `outcome_scale` and `convention`
  arguments. Because actual values are retained, a fitted model can be
  re-expressed on any scale without refitting.

## Other changes

- `sca_analysis()` stores the per-condition data and axis bounds it used, after
  the engine's complete-case filtering, so conversions match the sample the
  frontier was fitted on.
- `sca_thresholds()` warns instead of failing silently if the engine does not
  preserve exact threshold values, and states that its fallback is rounded to
  three decimals.
- Adds `stats` and `utils` to `Imports`, which were already in use.
- Moves `sufficiency-logic.md` from `inst/doc/` to `inst/docs/`. `inst/doc` is
  reserved for vignette build output, so `R CMD build` empties it and
  `devtools::check()` deletes its contents from the source tree.

# SCAtools 0.1.0

- Adds explicit HH, LH, HL, and LL sufficiency directions.
- Separates SCA direction numbering from physical empty-corner numbering.
- Adds wrappers for frontier analysis, permutation tests, simulation, and power.
- Adds tidy summaries, threshold rules, and SCA-labelled `ggplot2` graphics.
- Adds normalization and influential-observation diagnostics.
- Uses NCA 5.0.2 or later as the computational engine.
