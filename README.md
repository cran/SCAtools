# SCAtools

Sufficient Condition Analysis (SCA) in R.

## What the method is

A condition is **sufficient** for an outcome when having the condition
guarantees the outcome. In data, a sufficiency claim makes a definite
prediction: the cases that would contradict it do not occur. Plot the condition
X against the outcome Y and those cases sit in one corner of the scatter plot.
"High X is sufficient for high Y" predicts that the lower-right corner -- high X
with low Y -- is empty.

Sufficient Condition Analysis estimates that empty corner. It fits the boundary
of the empty space, measures how large the empty space is, tests whether it is
larger than random pairing of X and Y would produce, and turns the boundary
into thresholds: the level of X that guarantees each level of Y.

A sufficiency claim can run in four directions, and each predicts a different
empty corner:

| Direction | Claim | Empty corner |
|:---:|---|---|
| `HH` | high X is sufficient for high Y | lower-right |
| `LH` | low X is sufficient for high Y | lower-left |
| `HL` | high X is sufficient for low Y | upper-right |
| `LL` | low X is sufficient for low Y | upper-left |

You state the claim; the package finds the corner.

## What it is for

- **Finding the level that guarantees an outcome.** The threshold table answers
  "how much X is enough for Y to reach this level?" for every outcome level, in
  the units of the data or as percentages, percentiles or standard deviations.
- **Seeing what regression does not show.** A regression slope describes how
  the outcome moves *on average*. A condition can leave the average almost
  unchanged and still rule out low outcomes completely. SCA measures that
  second kind of effect, and `sca_compare()` reports both side by side, so a
  study can say which of the two its data show.
- **Comparing conditions.** Several conditions can be analysed in one call,
  each with its own direction, and reported in one table.
- **Planning a study.** `sca_power()` estimates how many observations a
  sufficiency effect of a given size needs.

## Installation

```r
install.packages("SCAtools")
```

## How to use it

The package includes four simulated datasets that show the four ways a
regression effect and a sufficiency effect can combine.

```r
library(SCAtools)

# Configuration II: a flat regression slope, but an empty lower-right corner
dat <- subset(sca_four_configurations, configuration == "II")

# 1. Fit the claim "high X is sufficient for high Y"
fit <- sca_analysis(dat, x = "X", y = "Y", direction = "HH",
                    scope = c(0, 100, 0, 100), test.rep = 1000)
fit                     # summary, with every setting the result rests on

# 2. Read the results
sca_table(fit)          # effect size, p value and support for each condition
sca_thresholds(fit)     # the X level that guarantees each Y level
sca_plot(fit)           # the scatter plot, the frontier and the empty corner

# 3. Set it beside regression
sca_compare(fit)        # here: slope near zero, large sufficiency effect
```

`sca()` runs the same analysis with a permutation test and draws the plot in
one step:

```r
sca(dat, x = "X", y = "Y", direction = "HH")
```

### Checking the result

| Function | What it shows |
|---|---|
| `sca_test_plot()` | the permutation distribution beside the observed effect size |
| `sca_scope()` | the scope each effect size was divided by |
| `sca_scope_sensitivity()` | how the effect size changes with the scope |
| `sca_resample()` | how much the effect size varies across subsamples |
| `sca_outliers()` | which observations hold up the frontier |
| `sca_locationshift()` | whether the frontier is a level shift or a threshold |
| `sca_magnitude()` | conventional bands for an effect size |

## Reading the results

- **Effect size** is the empty area as a share of the scope, from 0 (no empty
  corner) to 1. `effect_size_null` beside it is the value random pairing alone
  produces with the same data.
- **p value** comes from a permutation test that breaks the pairing of X and Y.
- **Thresholds** read as rules such as `X > 38.9 => Y > 30`. `no_threshold`
  marks an outcome level that no value of X reaches; `always_satisfied` marks
  one that every observation in scope already meets.
- **Scales.** A fitted model can be re-expressed without refitting:

  ```r
  sca_thresholds(fit, scale = "actual")      # original units
  sca_thresholds(fit, scale = "percentile")  # percentiles
  sca_thresholds(fit, scale = "sd")          # standard deviations
  ```

  Under the default `absolute` convention 0 is the low end of every axis and
  the inequality carries the direction; under `directional` a low-level axis is
  mirrored so that every rule reads as "more of the sufficient condition".
  `sca_scales()` lists the options.

## Good practice

- **Choose the direction from theory, then plot the data.** Each direction
  predicts a single empty corner, which assumes the relationship is monotone.
- **Report the scope.** The effect size is an area divided by the scope, so
  state whether the scope is the observed range or a theoretical one, and why.
  `sca_scope()` and the printed `Settings` block record it.
- **Report the frontier technique and the number of permutations.** CE-FDH
  (`"ce_fdh"`) follows the data as a step function; CR-FDH (`"cr_fdh"`) fits a
  straight line through it. 1000 or more permutations are usual.
- **Fix a relevance threshold in advance** with `relevance =` if a minimum
  effect size matters for the conclusion.
- **Report thresholds as empirical rules** for the population and design
  studied; how far they generalise depends on the design.

An ordinary least-squares or median regression line can be added with
`reference = "ols"` or `"qr"`. It is drawn for comparison and reported by
`sca_reference()`; it is not part of the sufficiency analysis.

## Learn more

- `?SCAtools` describes the method and gives its references.
- `citation("SCAtools")` gives the references to cite.
- `system.file("docs", "sufficiency-logic.md", package = "SCAtools")` derives
  the four directions step by step, and
  `system.file("examples", "four-directions.R", package = "SCAtools")` runs
  them.
- The NSCA package uses SCAtools to test conditions that are both necessary
  and sufficient.

Source and issue tracker: <https://github.com/youngchanresearcher/SCAtools>

The Traditional Chinese version of this README is installed with the package:

```r
file.show(system.file("docs", "README_zh-TW.md", package = "SCAtools"))
```
