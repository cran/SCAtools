# SCAtools

`SCAtools` is an experimental R package for direction-aware **Sufficiency
Condition Analysis (SCA)** using empty-space frontiers.

The package keeps two ideas separate:

1. the logical sufficiency statement (`HH`, `LH`, `HL`, or `LL`); and
2. the physical corner that must be empty in the original X-Y scatter plot.

| SCA number | Direction | Sufficiency statement | Physical empty corner |
|---:|:---:|---|---:|
| 1 | HH | High X sufficient for High Y | 4 (lower-right) |
| 2 | LH | Low X sufficient for High Y | 3 (lower-left) |
| 3 | HL | High X sufficient for Low Y | 2 (upper-right) |
| 4 | LL | Low X sufficient for Low Y | 1 (upper-left) |

The mapping follows contraposition:

```text
X sufficient for Y  <=>  not-Y necessary for not-X
```

For example, `High X -> High Y` requires the lower-right corner (High X, Low
Y) to be empty. It is equivalent to Low X being necessary for Low Y.

Source and issue tracker:
<https://github.com/youngchanresearcher/SCAtools>

## Installation

Install the released NCA engine and then the local SCAtools source package:

```r
install.packages(c("NCA", "ggplot2"))
install.packages("SCAtools_0.4.3.tar.gz", repos = NULL, type = "source")
```

## Basic use

```r
library(SCAtools)

set.seed(42)
dat <- sca_random(
  n = 100,
  intercepts = 0,
  slopes = 1,
  direction = "HH"
)

fit <- sca_analysis(
  dat,
  x = "X",
  y = "Y",
  direction = "HH",
  ceilings = c("ce_fdh", "cr_fdh"),
  test.rep = 1000
)

fit
sca_table(fit)
sca_thresholds(fit, ceiling = "ce_fdh", inequality = "strict")
sca_plot(fit)
sca_test_plot(fit, ceiling = "ce_fdh")

# Optional diagnostics
sca_normalize(dat)
sca_outliers(dat, "X", "Y", direction = "HH")
```

For several X conditions, `direction` can be a single value or one value per
condition. All directions in one model must refer to the same outcome level:

```r
fit <- sca_analysis(
  my_data,
  x = c("resources", "constraints"),
  y = "performance",
  direction = c("HH", "LH")
)
```

## Important interpretation limits

- `direction` is logical; `empty_corner` is geometric. They are deliberately
  not given the same number.
- The effect size is the fitted empty-zone area divided by the stated X-Y
  scope. It is scope- and frontier-dependent.
- A permutation p-value evaluates whether an empty zone this large is unusual
  after breaking the X-Y pairing. It does not prove a causal mechanism.
- Observational absence of counterexamples does not by itself establish
  deterministic sufficiency. Temporal order, design, measurement quality,
  scope, and out-of-sample validation still matter.
- A fitted threshold is best reported as an empirical frontier rule, not as a
  causal guarantee, unless the research design justifies that stronger claim.
- For continuous variables, strict inequalities are the exact contrapositive
  at the frontier. Inclusive inequalities are available only as an explicit
  reporting convention.
- A row marked `no_threshold` means no attainable condition value reaches that
  outcome level, and `always_satisfied` means every observation in scope
  already lies on the required side. These are the sufficiency readings of the
  engine's `NN` and `NA` markers, whose meanings invert under contraposition.

## Reporting scales

Threshold tables can be reported in actual units, as a percentage of the range
or of the maximum, as percentiles, or in standard deviations. Because actual
values are retained, a fitted model can be re-expressed without refitting:

```r
fit <- sca_analysis(dat, "X", "Y", direction = "LL", threshold.x = "percentile")

sca_thresholds(fit)                        # percentiles
sca_thresholds(fit, scale = "actual")      # original units
sca_thresholds(fit, scale = "sd")          # standard deviations
```

Two conventions are available, and they differ only for low-level directions:

| Convention | `HH` reads | `LL` reads |
|---|---|---|
| `absolute` (default) | `X > 70% => Y > 80%` | `X < 30% => Y < 20%` |
| `directional` | `X > 70% => Y > 80%` | `X > 70% => Y > 80%` |

Under `absolute`, 0 sits at the low end of every axis and the inequality
carries the direction, so numbers are directly comparable across directions.
Under `directional`, 0 sits at the least sufficient end: a low-level axis is
mirrored, the inequality flips with it, and every rule reads as "more of the
sufficient thing". `sca_scales()` lists what is available.

## Implementation and attribution

`SCAtools` delegates frontier estimation, effect-size calculation, threshold
estimation, permutation tests, random-data generation, and power analysis to
NCA 5.0.2 or later. The wrapper maps sufficiency directions to NCA's physical
corners and provides SCA-specific output and graphics. This avoids maintaining
a divergent copy of the statistical engine.

Scale conversion is the one place where `SCAtools` does not delegate.
Thresholds are always requested from the engine in actual units and converted
here. The engine measures its percentage scales from the low end of the axis
but mirrors its percentile scale according to the physical empty corner, and
its out-of-range threshold cells use a third convention again. Because the
physical corner does not correspond to the logical sufficiency direction,
inheriting those conventions produced tables that could not be read
consistently. Doing the conversion here means one stated convention governs
both axes in all four directions.

NCA is distributed under GPL-3-or-later and is authored by Jan Dul and Govert
Buijs. Cite the underlying method and software as appropriate, including:

- Dul, J. (2016). Necessary Condition Analysis (NCA): Logic and methodology of
  “necessary but not sufficient” causality. *Organizational Research Methods,
  19*(1), 10-52. https://doi.org/10.1177/1094428115584005

The installed package also includes `docs/sufficiency-logic.md` and
`examples/four-directions.R` for a fuller derivation and a runnable example.

`citation("SCAtools")` returns both this package and the NCA method article.

## The reference line: OLS is not a frontier

`reference = "ols"` draws an ordinary least-squares regression of the outcome
on the condition -- the line `lm(y ~ x)` returns -- beside the frontier, for
comparison only:

```r
fit <- sca_analysis(
  dat, x = "X", y = "Y", direction = "HH",
  ceilings = "ce_fdh",
  reference = "ols"
)

sca_reference(fit)   # intercept, slope, R-squared; no effect size
```

A regression line describes how the expected outcome moves with the condition.
A sufficiency frontier describes which condition-outcome combinations are
absent. A frontier is fixed by the most extreme observations, a regression line
by the central tendency, so the two are different quantities and are reported
separately: the reference line has no empty zone, no effect size, no
permutation p value and no sufficiency threshold, and never appears as a row of
`sca_table()`.

Before 0.4.1 `"ols"` was the first entry of the default `ceilings`. It is not a
default any more. Passing it in `ceilings` still works and moves it here with a
warning; passing it alone is an error, because no empty space is left to
estimate.

---

The Traditional Chinese version of this README is installed with the package:

```r
file.show(system.file("docs", "README_zh-TW.md", package = "SCAtools"))
```
