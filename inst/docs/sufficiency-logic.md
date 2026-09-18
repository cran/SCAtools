# Sufficiency logic and empty-corner geometry

## 1. Two numbering systems must not be conflated

The physical corners of an ordinary X-Y plot are fixed:

| Physical corner | Location | X region | Y region |
|---:|---|---|---|
| 1 | upper-left | Low X | High Y |
| 2 | upper-right | High X | High Y |
| 3 | lower-left | Low X | Low Y |
| 4 | lower-right | High X | Low Y |

SCA's directional numbering instead describes the antecedent and consequent:

| SCA number | Direction | Statement | Counterexample region | Empty corner |
|---:|:---:|---|---|---:|
| 1 | HH | High X sufficient for High Y | High X and Low Y | 4 |
| 2 | LH | Low X sufficient for High Y | Low X and Low Y | 3 |
| 3 | HL | High X sufficient for Low Y | High X and High Y | 2 |
| 4 | LL | Low X sufficient for Low Y | Low X and High Y | 1 |

Consequently, an analysis can be called “SCA direction 1 (HH)” while its
physical empty region remains “corner 4.” `SCAtools` always reports both fields.

## 2. Contraposition

For propositions A and B:

```text
A is sufficient for B
A -> B
not-B -> not-A
not-A is necessary for not-B
```

Taking A as High X and B as High Y gives:

```text
High X sufficient for High Y
<=> Low X necessary for Low Y
<=> physical corner 4 is empty
```

The complete relation is:

| Sufficiency statement | Equivalent necessity statement | Empty corner |
|---|---|---:|
| High X sufficient for High Y | Low X necessary for Low Y | 4 |
| Low X sufficient for High Y | High X necessary for Low Y | 3 |
| High X sufficient for Low Y | Low X necessary for High Y | 2 |
| Low X sufficient for Low Y | High X necessary for High Y | 1 |

## 3. Continuous thresholds and equality

Suppose an estimated lower frontier supports the empirical statement:

```text
Y <= y0  ->  X <= x0
```

Its exact contrapositive is:

```text
X > x0  ->  Y > y0
```

The complements of `<=` are therefore strict `>` inequalities. Including
equality requires an additional convention about calibration or measurement.
For this reason `sca_thresholds()` defaults to `inequality = "strict"`; the
inclusive option is explicit rather than automatic.

(That argument was called `boundary` before 0.4.0. The name was needed for the
theoretical line an expected empty space is separated by, which is what a
*frontier* estimates, so the argument governing the strictness of a reported
inequality is now `inequality`.)

## 4. What the computation establishes

The NCA engine estimates a frontier and an empty-zone effect size. With the
correct corner mapping, the same geometry can be described through its
logically equivalent sufficiency statement. This establishes a sample-level
empty-space pattern under a specified scope and estimator.

It does not, without further assumptions, establish:

- that X causes Y;
- that every future case will obey the fitted frontier;
- that the chosen empirical or theoretical scope is uniquely correct;
- that measurement error or rare counterexamples are absent; or
- that a bivariate pattern survives controls for common causes.

Recommended neutral wording is “empirical sufficiency frontier pattern.” Use a
stronger causal formulation only when temporal order, identification strategy,
measurement, robustness, and external validation warrant it.

## 5. Computational mapping used by SCAtools

`sca_analysis()` passes the following physical corner to
`NCA::nca_analysis()`:

```r
c(HH = 4L, LH = 3L, HL = 2L, LL = 1L)
```

The frontier coordinates remain on the original X and Y axes. SCAtools does not
renumber the physical corner inside plots. It adds logical direction metadata,
SCA-labelled summaries, original-scale threshold rules, and SCA-labelled plots.

