# SCAtools 0.5.1

SCAtools 0.5.0 was not released on CRAN, so this is the release that brings
the 0.5.0 changes below to CRAN users of 0.4.3. Read *Upgrading from 0.4.3*
first; it collects in one place every change that can break a script written
for that release.

## Changed: the technique column is `frontier` again

0.5.0 renamed the column that names the frontier technique from `frontier` to
`frontier_technique`. It is `frontier` again, in `sca_table()`,
`sca_thresholds()`, `sca_compare()`, `sca_locationshift()`,
`sca_scope_sensitivity()` and `sca_resample()`, which is the name it had in
0.4.3 -- so for this column a script written against the CRAN release needs no
change at all.

The problem 0.5.0 set out to solve was a column that held `"ce_fdh"` under a
name that the same table also used for the boundary itself. `frontier` does not
have that problem: the value identifies which fitted frontier the row reports,
and `frontier_accuracy` beside it is that frontier's accuracy. `boundary_type`
still says whether the boundary is a ceiling or a floor. The plot legend reads
"Frontier" again, and `frontier` is no longer listed by `sca_legacy_names()`.

The arguments `ceilings` and `ceiling` are unchanged.

## Changed: `empty_zone_area` is kept beside `empty_space_area`

0.5.0 renamed `empty_zone_area` to `empty_space_area`. NSCA 0.4.5, the release
on CRAN, reads the old name from `sca_table()`, and fails without it. The table
therefore carries `empty_zone_area` as an exact copy of `empty_space_area` until
a release of NSCA that reads the new name has replaced 0.4.5; it will then be
removed. New code should read `empty_space_area`.

NSCA 0.4.5's own test suite passes against this release.

## Terminology: Sufficient Condition Analysis

The method is *Sufficient Condition Analysis*. 0.5.0 changed the title, and
this release finishes the job in the places it missed: `citation("SCAtools")`,
the README and the help page of `sca()`.

## Documentation

- `Description` in DESCRIPTION, the text on the CRAN page, now says directly
  what the method is, what it is for and how it helps. Where the method comes
  from, with its authors and references, is on the package help page,
  `?SCAtools`, which is no longer marked internal and so appears in the
  reference manual. It now covers the method, what it is for, how to use it
  and how to read the result.
- The README and its Traditional Chinese translation are rewritten around the
  same four questions, with a worked example on `sca_four_configurations`, and
  install from CRAN. Version history now lives only in this file, and help
  pages no longer recount it.
- Every exported function now has a runnable example. Examples that run a
  permutation test use 99 permutations on a subsample so that they finish
  within seconds; `sca()`, whose point is the default 1000 permutations, is
  wrapped in `\donttest{}`. `sca_power()` had lost the example it had in
  0.4.3 when the help pages were regenerated, and has one again.
- Help pages are generated with Markdown enabled. In 0.5.0 code, links and
  emphasis written in Markdown appeared literally in the rendered help --
  backticks and square brackets rather than code and links.
- `citation("SCAtools")` and the help pages cite the two papers that set out
  the method: Chan (in press), *From Necessity to Sufficiency and
  Necessity-and-Sufficiency: Extending Condition Analysis in Degree*, and Chan
  (in press), *Mean Effects Are Not Sufficiency Effects: How Sufficient
  Condition Analysis Complements Regression Analysis*, both University of
  Leeds. `sca_four_configurations` is the data of the second.
- `sca_legacy_names()` said that `sca_extract()` accepts retired names with a
  warning and that retired arguments still work. Neither has been true since
  0.5.0: `sca_extract()` stops and names the replacement, and `boundary`,
  `bottleneck.x` and `bottleneck.y` were removed. The page now says so, and
  lists the 0.5.0 renames as well as those of 0.4.0.
- A stray `tests/testthat/Rplots.pdf` is no longer shipped.

## Upgrading from 0.4.3

Everything that can make a script written for 0.4.3 fail or give a different
number, with the section of 0.5.0 below that explains it.

- `empty_zone_area` is now `empty_space_area`. The old name is still present in
  `sca_table()` for now, and `legacy = TRUE` appends it to both tables.
- `sca()` runs a permutation test by default (`test.rep = 1000`), so it takes
  noticeably longer than `sca_analysis()`.
- `bottleneck.x` and `bottleneck.y` are removed from `sca_analysis()`, and
  `boundary` from `sca_thresholds()` and `sca_rescale()`. Use `threshold.x`,
  `threshold.y` and `inequality`.
- `sca_extract()` stops at once on a retired column name. In 0.4.3 such a call
  warned and then failed anyway, so only the message changes.
- `sca_reference()` returns one row per condition and reference line, with
  `line`, `tau` and `drawn` columns.
- `sca_thresholds()`, `sca_test_plot()` and `sca_output()` default to the first
  frontier actually estimated instead of `"ce_fdh"`.
- Six inputs that were accepted are refused: an unknown frontier technique, a
  `test.p_threshold` outside (0, 1), a negative `test.rep`, a reversed `scope`,
  a `scope` with no width, and the same variable as condition and outcome.
- One outcome may not be given two different theoretical ranges across
  conditions.
- With missing values, both axes of a condition are now bounded by the same
  rows, so effect sizes can differ from 0.4.3 on incomplete data. Complete data
  is unaffected.
- `Imports` gains `quantreg`, which NCA already imports.

# SCAtools 0.5.0

*Not released on CRAN; its changes reached CRAN in 0.5.1.*

A correctness and consistency release. Four of the changes below alter numbers
that earlier versions reported, and two remove arguments, so this is not a
patch release. Read *Breaking* before upgrading a running analysis.


## New: sca_magnitude() / nsca_magnitude() -- the conventional bands, cited

Dul (2016)'s bands for an NCA effect size, and the label a value falls in:

```r
sca_magnitude()                       # the table
sca_magnitude(c(0.04, 0.18, 0.35))    # "small" "medium" "large"
```

`sca_compare()` and `nsca_compare()` gain a `magnitude` column.

**Why this reverses an earlier position.** The package was *already* applying
Dul's lower edge as the default `d_cut` and Cohen's as the default `beta_cut`,
with both conventions written out in a code comment where no user could see
them -- while the joint table printed "magnitude benchmarks are not
established". A convention applied and not disclosed is worse than either
applying it openly or not applying it at all.

They are a **convention to cite**, exactly as a reader of a regression cites
Cohen. Reporting "d = 0.35, large by the conventional bands (Dul, 2016)" is a
citation and a normal one. The table says so in a `source` column, not only in
the help page, because a table gets pasted into a script and a help page does
not.

### Cohen's bands too, and the trap between them

`beta_cut = 0.30` is Cohen (1988), and it was in a code comment for exactly
the same reason Dul's was. Both are out now:

```r
sca_magnitude(scale = "beta")     # Cohen's bands for a standardised coefficient
```

`sca_compare()` and `nsca_compare()` gain `beta_magnitude` beside `magnitude`.

**The two conventions share their edges and shift their labels by one band.**
Both use .10, .30 and .50. Cohen's *medium* runs .30 to .50; Dul's *large*
runs .30 to .50.

```
value    as a coefficient (Cohen)   as an effect size (Dul)
0.05     below small                small
0.15     small                      medium
0.35     medium                     large
0.60     large                      very large
```

This is why the two defaults differ -- `beta_cut = 0.30` and `d_cut = 0.10`
are each the lower edge of *medium* in their own tradition, and those are
different numbers even though the edges are not. A reader comparing a beta
and a d in the same row of `sca_compare()` needs that stated, and a comment
in the source was not stating it.
### The joint indices still get none, and that is a different judgement

`weakest_effect`, `balanced_joint_effect` and `joint_empty_space_coverage` are
this package's own constructions. Citing somebody else's published convention
and inventing one for your own index are not the same act, and the second is
a claim with nothing behind it. `print()` now distinguishes the two cases
instead of denying benchmarks for both.

### Read a band beside the floor

A band says where a number sits on a conventional scale. It does not say how
much of the number the geometry would have produced anyway -- `effect_size_null`
answers that. A `d_nec` of 0.10 labelled *medium* at a floor of 0.055 is a
different finding from the same 0.10 at a floor of 0.007, and both numbers are
now in the same row of `nsca_compare()`.

## Fixed: the reference line had no key

A grey line ran through the middle of the cloud and nothing on the figure said
what it was, except one sentence in the caption. Reported by someone reading a
plot, which is where this kind of defect gets found.

Keeping it out of the *Component* legend was right -- colouring an average
effect as a component would put it in the same key as an empty-space frontier,
and they are not the same kind of object. But "not in that key" had been built
as "no key at all".

It is now labelled on the line itself: `OLS`, or `QR tau=0.5` for a quantile
reference. ggplot2 allows one scale per aesthetic and both are taken, so a
third legend would mean bending colour or linetype into exactly the conflation
the original decision was avoiding. A label where the reader is already looking
avoids both problems.

The labels are staggered along x, because an OLS line and a median line nearly
coincide on symmetric data and two labels in one place is one label with
something hidden under it.

Both packages.

## Verified: the permutation tests are calibrated

Type I error, measured rather than assumed. Independent X and Y, n = 50,
`test.rep = 199`, nominal alpha 0.05, 2500 simulations (SE 0.00436):

```
necessity      reject .0520   95% CI [.0433, .0607]   KS p = .27
sufficiency    reject .0528   95% CI [.0440, .0616]   KS p = .32
IUT (joint)    reject .0080   95% CI [.0045, .0115]
```

Both component p-value distributions are uniform, not merely correct at the
cut. A first run at 500 simulations put sufficiency at .0740 (z = +2.46), which
looked like it might be a real anti-conservative bias; at 2500 it is .0528.
It was noise.

The intersection-union test rejects well below alpha and fails a uniformity
test. That is the design: it rejects only when both components reject, so its
level is at most alpha by construction. `joint_support` is therefore
conservative, and a null result from it should be read with that in mind.

The script and both results ship at the repository root as
`calibration-0.5.0.R` and `calibration-0.5.0-result.txt`.

## Fixed: a regression frontier drawn outside the box it is measured in

`cr_fdh` and the other line techniques were drawn across the full x range with
no clipping, so near a corner the line crossed the grey dashed scope boundary
and kept going.

The areas were never wrong -- the band integral clips the frontier to the
scope, which is why the identity closed and `fit` and `frontier_accuracy` were
1. But a figure in which the frontier leaves the box whose area is being
reported reads as an error to anyone checking the number against the picture,
and "it is only cosmetic" is not something a reader can verify.

The drawn line is now clipped the same way the integral clips it, so it is the
boundary of the region actually measured.

## Fixed: the plot title ran off the edge of the figure

Found by rendering the figures and looking at them, which the test suite
cannot do -- it only ever asked whether a plot was a `ggplot`.

Title, subtitle and caption were all wrapped at 95 characters. But a title is
bold and about half again the size of a caption, so on a long variable name the
first line overflowed the panel and was clipped mid-word:

```
NSCA: High Hours of structured professional development training per year
is necessa|
```

`nsca_shared_plot()` was worse -- its title was not wrapped at all and simply
ran off. Titles now wrap at a width chosen for their own size, found by
rendering rather than by arithmetic on point sizes.

## Fixed: three power curves, one visible line

`nsca_powerplot()` drew the joint curve last, on top of the two components it
is built from. At high power all three coincide, which is the ordinary case --
so the figure showed one line above a legend naming three.

The joint curve is now drawn **first**, underneath, with the two component
curves dashed and dotted over it, and it leads the legend for the same reason
it is drawn first: it is the curve a study is powered on. The x axis also
breaks at the sample sizes actually simulated; it used to break at 50/75/100
for a study run at n = 30/60/120.

## Changed: the plot legend says "Technique"

It said `Frontier`, which is now the name of the boundary itself. The tables
call the estimator `frontier_technique`; the figures now agree.

## New: the configuration label carries its own floor

`sca_compare()` and `nsca_compare()` gain `effect_size_null` and
`effect_size_excess`.

A pure-noise condition at n = 70 came back as configuration II,
*necessity-dominant*, on `d_nec = 0.104` against the conventional cut of 0.10 --
while its independence floor was 0.055, so half of that effect size was the
envelope fitting sample extremes. The label was defensible and the reading was
still misleading.

```
condition   component  effect_size  effect_size_null  effect_size_excess  label
Training    necessity      0.36825           0.07451             0.29374  mean and necessity
Luck        necessity      0.10418           0.05464             0.04955  necessity-dominant
Luck        sufficiency    0.03725           0.08698            -0.04974  neither
```

`Luck`'s excess is *below* the cut on one side and *negative* on the other. The
cut is unchanged -- moving it to the excess would be inventing a criterion --
but the number that qualifies it is now in the same row.

## Internal: SCAtools' NAMESPACE is generated again

It was hand-maintained, so a new `@export` had to be added to it by hand and
`sca_resample()` nearly shipped unexported. The tags and the file were checked
to be exactly in sync -- 31 exports and 5 S3 methods, nothing dropped, nothing
missing -- before roxygen was given ownership.

## Fixed: the last error message that came from the engine

```
before:  The length of scope needs to be equal to the length of x
         or
after:   'scope' has 6 values. With 3 conditions it must have 4 --
         c(xmin, xmax, ymin, ymax), one box for all of them -- or 12,
         four per condition in the order the conditions were given.
```

A list of the wrong length, or with an entry that is not four numbers, is now
refused the same way.

## New: an adjustment that cannot reject now says so

Found by the adversarial pass. Eight conditions, one of them real, seven noise,
`test.rep = 50`. Unadjusted, the real one is supported. Under BH, nothing is --
and not because the evidence was weak.

The arithmetic is hard. A permutation p-value cannot fall below
`1 / (test.rep + 1)`, and the most significant of `k` conditions is multiplied
by `k` under both Bonferroni and Benjamini-Hochberg. So no condition can reach
`alpha` unless

```
k / (test.rep + 1) <= alpha,   that is   test.rep >= k / alpha - 1
```

At `alpha = 0.05` that is 2 conditions on 50 replications, 5 on 100, 10 on 200,
50 on 1000. A researcher who adjusts eight conditions on a low replication
count and sees nothing has been told a fact about `test.rep`, not about their
data -- and nothing in the output said so.

```
Warning: With 8 conditions and test.rep = 50, the smallest p-value the
permutation test can produce is 0.0196, and 'BH' multiplies the most
significant of 8 by 8. No condition can reach alpha = 0.05 however strong the
evidence, so an absence of support here is arithmetic rather than a finding.
Use test.rep >= 159.
```

A warning rather than a refusal: the adjusted p-values are still correct, and a
caller may want them. What they must not do is read the absence of a rejection
as evidence. Silent when nothing is adjusted, and when no test was run.

## Breaking: six inputs that were accepted are now refused

An adversarial pass over 0.5.0 fed both packages degenerate data, hostile
scales and arguments of the wrong shape. Sixteen bad inputs; seven were
accepted, and three of those produced a fit that **misreported itself** — the
failure a user cannot catch by reading the output.

### An unknown frontier technique reached the engine

The worst of them.

```r
sca_analysis(data, "X", "Y", ceilings = "banana")   # used to work
```

Nothing checked the name. `"banana"` was passed to the engine, which fell back
to its own defaults and fitted **ce_fdh and cr_fdh** — while the metadata still
recorded `"banana"`, so `print()` showed `frontiers  banana` above a table of
two techniques nobody had asked for. A misspelled `"ce_fhd"` did the same
thing, silently.

Now refused, with the available names in the message. The list is pinned
against the engine's own by a test, so an NCA release that adds or drops a
technique breaks the suite rather than being quietly accepted.

### A significance threshold outside (0, 1)

`test.p_threshold = 2` was accepted and stored. Every row came back
`significant = TRUE` and the `Settings` block printed `alpha  2` as though it
were a considered choice.

### A negative replication count

`test.rep = -5` was accepted and stored, and printed in the `Settings` block
beside a `p_value` of `NA`.

### A scope reversed or with no width

`scope = c(1, 0, 1, 0)` was accepted. The engine repairs it silently — it
widens whatever it is given until the data fits — so the analysis was correct
while `sca_scope()` reported `declared_xmin 1, declared_xmax 0`, which is not a
scope anybody could put in a methods section. `c(0.5, 0.5, 0, 1)` was likewise
widened back to the observed range.

`scope = c(0, NA, 0, 1)` is **not** affected: `NA` means "no theoretical bound
on this side", which is the documented way to declare half a box.

### A variable used as both condition and outcome

`sca_analysis(data, "X", "X")` returned `d = 0.53`. A variable against itself
is a perfect diagonal, whose empty corner is half the scope whatever the data
contains — a plausible-looking number that is an artefact of the geometry. In
NSCA it is worse: both frontiers land on the same diagonal.

### Error messages now name the argument

`frame[x]` raised R's own `undefined columns selected`, which mentions neither
the argument nor the name that was not found. A caller with eight conditions
had to bisect to find the one they had misspelled.

```
before:  undefined columns selected
after:   'x' names 'Trainng', which is not a column of the data.
         Available: Training, Experience, Support, Performance.
```

Both packages now give the same message for the same mistake, so a caller who
learns one does not have to learn a second set.

## New: sca_table(p.adjust =), a correction you ask for

The `Settings` block added in this release discloses the multiplicity; this is
the means to act on it.

```r
sca_table(fit, p.adjust = "BH")
```

Adds `p_value_adjusted` and `p_adjust_method` beside `p_value`, and re-derives
`significant`, `supported` and `support_status` from the adjusted values. An
adjusted number printed beside a verdict that ignored it would be worse than
not adjusting at all. `relevant` is untouched: an effect size does not shrink
because more tests were run.

`"none"` is the default and leaves the table byte-identical to before. The
package does not pick a method, because which correction fits depends on what
the family is, and that is a property of the design rather than of the data.

**The family is the conditions, within one frontier technique.** Two techniques
are two estimates of one claim, not two hypotheses, so adjusting across them
would inflate `k` with re-measurements and cost power for nothing. Choosing a
technique after seeing both p-values is a real problem, but it is a garden of
forking paths rather than a family-wise rate, and the `Settings` block names it
separately.

## New: what random pairing alone produces

`sca_table()` gains `effect_size_null` and `effect_size_excess`.

The FDH envelope is built from the sample's own extremes, so it reports a
positive empty area even when the condition and the outcome are independent:
with `n` points there is always *some* corner nothing happens to occupy. The
overstatement shrinks with `n`, which is why an effect size from 40 cases and
one from 400 are not on the same scale — and nothing in the output said so.

This is not modelled, it is read. The permutation test already builds the
distribution of the effect size under random pairing and then discards all of
it except the tail probability. Under independence the true empty area is zero,
so the mean of that distribution **is** the overstatement, at this sample size,
this scope and this technique, measured on the data in hand rather than taken
from an asymptotic constant.

```
n= 40   d=0.476   null=0.104   excess=0.372
n=120   d=0.397   null=0.063   excess=0.335
n=400   d=0.369   null=0.054   excess=0.315
```

`effect_size_excess` is a comparison, **not a corrected estimator**. Subtracting
the null mean does not give an unbiased effect size, because the bias under
independence is not the bias under the alternative. What it gives is the part
of the number that random pairing alone does not account for. A condition that
is pure noise can show a positive `effect_size` and a *negative*
`effect_size_excess`, and that is the finding.

Both are `NA` when `test.rep = 0`: with nothing to compare against, a zero
would be a claim.

## New: sca_resample(), and deliberately no confidence interval

Refits one condition on random subsamples of several sizes and reports the
distribution of the effect size at each size.

```
  n   m fraction reps  effect_size  subsample_median  subsample_p05  subsample_p95
150  38   0.2533  120       0.4185            0.4777         0.4539         0.5015
150  75   0.5000  120       0.4185            0.4430         0.4281         0.4672
150 112   0.7467  120       0.4185            0.4287         0.4200         0.4372
150 150   1.0000    1       0.4185            0.4185         0.4185         0.4185
```

The spread at one size is the sampling variability. The trend down the `m`
column is the bias, visible rather than modelled: fewer points leave more of
the corner empty by accident.

**Why no interval.** Two reasons, both specific to frontier estimators rather
than general caution.

The ordinary n-out-of-n bootstrap is *inconsistent* here. Resampling with
replacement duplicates observations, and a duplicated extreme cannot push the
envelope further out than the original did, so the resampled frontier lies
inside the fitted one far more often than chance; the resampling distribution
does not converge to the sampling distribution. This is the familiar result for
DEA and FDH efficiency scores.

Subsampling without replacement, which is what this function does, restores
consistency but leaves a bias with a known direction. Look at the table above:
at `m = 38` the 5th–95th percentile range is `[0.454, 0.502]`, which does not
even contain the full-sample estimate of `0.4185`. A percentile interval read
straight off a subsample distribution sits above the quantity it claims to
cover. Correcting for that needs the estimator's convergence rate, which
depends on the technique and the dimension, and which this package will not
assert.

So the table reports the distribution and stops there — the same restraint as
[sca_scope_sensitivity()], which reports the effect size on every box and
refuses to subtract two of them for you.

The scope is pinned to the one the model used, so only the frontier moves. The
caller's random stream is restored, as everywhere else here.

## Breaking: the technique column is `frontier_technique`

`frontier` held `"ce_fdh"` — the name of the *estimator*, not a frontier. The
word was already doing a second job in the same table, where `boundary_type`
takes the values `"ceiling"` and `"floor"`, and a third in the glossary, where
`frontier` is defined as "the boundary as estimated from data". One word, three
meanings, two of them in adjacent columns.

```
sca_table()             frontier -> frontier_technique
sca_thresholds()        frontier -> frontier_technique
sca_compare()           frontier -> frontier_technique
sca_locationshift()     frontier -> frontier_technique
sca_scope_sensitivity() frontier -> frontier_technique
```

`frontier` now means only the estimated boundary and `ceiling` only "Y bounded
from above". The name also matches the prose, which has always said *frontier
technique*, and matches the argument it comes from: `ceilings = "ce_fdh"`
produces `frontier_technique = "ce_fdh"`.

The **arguments** `ceilings` and `ceiling` are unchanged. They are NCA's names,
this package passes them through, and renaming them is a separate question.

`frontier` is carried by [sca_legacy_names()], so `sca_table(legacy = TRUE)`
and `sca_thresholds(legacy = TRUE)` append it as a duplicate column.
`sca_extract(param = "frontier")` refuses the retired name and says what
replaced it, which is what every other retired column here does: a caller
who is handed an answer never learns that the name moved.

`sca_outliers()` keeps its `ceiling` column, which is NCA's own and means
something else entirely: `"X"` marks a removed observation that was a peer.
Note that for `cols`, `qr`, `c_lp` and `custom` the engine sets it for every
row — those techniques have no peers to test against — so it carries no
information there.

## New: sca_outliers() takes a fitted model

It took `data`, `x`, `y` and `direction`, which meant restating four things the
model already knew -- and therefore being free to restate them differently. A
diagnostic run on `direction = "LH"` against a model fitted on `"HH"` produced
a perfectly plausible table about a corner the analysis never looked at.

```r
fit <- sca_analysis(data, x = c("A", "B"), y = "Y", direction = "HH")
sca_outliers(fit)
```

Every condition is examined unless `x` selects some. Passing `y`, `direction`
or `scope` alongside a model is an error rather than an override, and it is an
error even when the value agrees with the fit: the test is whether the argument
was named, not whether it differs. A rule that accepted the call that happened
to agree and refused the identical call that did not would be harder to explain
than a flat one.

The data-frame form is unchanged apart from the column below.

### The table now names its condition

Both forms gain a leading `condition` column. The model form needs it, because
it examines several conditions and the rows would otherwise be
indistinguishable; the data form gains it so that one function does not return
two shapes. It is the name or the column position the caller supplied, which
the function has always known and never reported.

## Fixed: sca_compare() reported the median slope as an OLS coefficient

`beta` is documented as the standardised least-squares coefficient, and the
configuration III and IV readings say "on average". The function asked
`sca_reference()` for whatever line the model was fitted with, so a model
fitted with `reference = "qr"` put the median slope in that column under the
OLS label -- and, since the configuration classification thresholds `beta`,
could land a condition in a different configuration than the same data fitted
with `reference = "ols"`.

The line is now pinned. Reference lines are computed in this package rather
than by the engine, so asking for one the model did not request costs nothing.

## New: print() discloses how many tests the table contains

A model with `k` conditions runs `k` permutation tests, and nothing in the
output said so. The `Settings` block now reports the count, states that the
p-values are unadjusted, and gives the arithmetic:

```
  multiplicity  3 conditions = 3 p-values, none adjusted
                were the 3 tests independent, P(at least one false positive)
                at alpha 0.05 would be 0.14
```

Disclosure, not adjustment. Which correction fits depends on what the family
is -- whether the conditions were pre-specified, whether one significant
condition would be reported as the finding or all of them together, whether the
analysis is confirmatory at all. Those are properties of the design rather than
of the data, and a package that guessed at them would be making the
methodological claim on the researcher's behalf. `stats::p.adjust()` on the
`p_value` column is one line once the family is known.

Several frontier techniques are a different problem and get a different line:
two techniques are two estimates of one claim, not two hypotheses, so there is
no family-wise rate to report. What there is instead is the temptation to pick
after seeing both, which the block names.

Silent when there is nothing to disclose -- one condition, one technique, or no
permutation test -- so that a block that says something always means something.

## Changed: sca() takes test.seed, and says when the p-value is not pinned

`sca()` runs a permutation test by default -- `test.rep = 1000` -- and had no
way to pin the result, so it produced a reportable-looking p-value that was
different on every run. It now takes `test.seed`, and `print()` says so beneath
the results when the p-values are unpinned rather than only in the settings
block above them.

The default stays at 1000. A quick result should still carry statistical
evidence; what it should not do is carry evidence that cannot be reproduced
without saying so.

## Changed: sca_reference() gains pseudo_r1

Koenker and Machado's R1 for the `"qr"` line, `NA` for `"ols"`, beside an
`r_squared` that is `NA` for `"qr"`. Quantile regression minimises a different
loss, so there is no residual variance to take a share of. Reporting R1 in the
`r_squared` column would be one column carrying two meanings; leaving it out
entirely reported a difference in kind as a missing value. A `pseudo_r1` and an
`r_squared` are not on a common scale and must not be compared.

## New: qr as a reference line, and reference.tau

`reference = "qr"` draws a quantile regression beside the frontier.
`reference.tau` sets its quantile and defaults to `0.5`: the median line, the
central-tendency summary that does not move when one observation does, which is
the reason to draw it beside OLS rather than instead of it.

It is separate from `qr.tau`, and the separation is the feature. A `qr`
*frontier* sits at the edge of the cloud and uses `qr.tau`; a `qr` *reference*
sits in the middle of it and uses `reference.tau`. Both can now be asked for in
one analysis, which the engine cannot express: `NCA::nca_analysis()` takes one
`qr.tau` per call.

Reference lines are therefore computed by this package rather than requested
from the engine. `sca_reference()` already computed the OLS line itself with
`stats::lm()`, so this makes the two consistent and removes the second engine
run that a reference-only line would otherwise have cost.

* `sca_reference()` returns one row per condition **and line**, with `line`,
  `tau` and `drawn` columns. Code that assumed one row per condition needs a
  filter.
* `r_squared` and `beta` are `NA` for `"qr"`. Quantile regression minimises a
  different loss, so there is no residual variance to take a share of, and the
  pseudo-R1 sometimes reported for it is not comparable with an OLS R-squared.
  An empty column beats one that looks the same and means something else.
* `sca_plot()` draws reference lines unkeyed -- OLS dotted, `qr` long-dashed --
  and names them in the caption with the quantile, because a median line and a
  95th-percentile line look identical on paper. They stay out of the frontier
  legend: they are not frontiers, and a shared key said they were.
* `qr` remains a valid frontier. It is now in both sets, so the rule that moves
  a central-tendency line out of `ceilings` had to be narrowed to the lines
  that can *only* be references, which is `ols` alone.

## New: sca_scope_sensitivity()

Refits one condition on a series of boxes and reports the effect size each one
produces. The scope is the denominator and, unlike the frontier, it is chosen
rather than estimated, so a reported `d` is a claim about the data and a claim
about the box at once. This separates them.

The default boxes are the observed range widened by a fraction of the observed
span on each side, because that is a quantity a reader can judge: "a box half
as wide again as the data" means something where a pair of numbers does not.
When the model was fitted on a declared scope, that box is added as a final row
so the caller can see where their own choice sits.

No column reports a change from a baseline. Which row is the baseline is the
caller's question -- the observed range and the scope the model was fitted on
answer different ones -- so the table reports the effect sizes and leaves the
subtraction to the reader. The data does not change between rows: every
difference down the `effect_size` column is the box.

`sca_analysis()` now records `qr.tau` in its metadata, which this needs in
order to rebuild a `qr` frontier at the tau that produced the numbers being
tested rather than at the default.

## Fixed: sca_power() takes test.seed, and reproduces

A power table is the number that decides a sample size, and it was different on
every run. `NCA::nca_power()` has no seed argument, but it does not need one:
it draws a single value off the caller's stream -- `offset <- sample.int(2^24,
1)` -- and derives every replication's seed from it, exactly as the permutation
test does. Fixing the stream before the call therefore fixes the whole
simulation, so `sca_power()` now takes `test.seed` and wraps the engine call in
the same `.sca_with_seed()` that `sca_analysis()` has always used. No change to
NCA was needed.

The restore matters more here than elsewhere. `nca_power()` calls `set.seed()`
inside its own loop, so without it the session would be left seeded from an
arithmetic expression of the loop counters.

This was the last place in either package where a reported number could not be
reproduced.

## Fixed: a constant condition is refused, as a constant outcome already was

A constant outcome has been refused since 0.4.x: it leaves the scope with no
height, so every empty area would be measured against zero. A constant
*condition* does exactly the same thing on the other axis and was not checked.
It returned a fit in which `scope_area` was `0`, every effect size was `NaN`
and every p-value `NA` -- the same degeneracy, refused on one axis and reported
as a number on the other.

It is now refused, with the escape hatch named: a theoretical `scope` with
distinct `xmin` and `xmax` gives the box a width and the areas are defined
again, and the message says that the result then depends on where the constant
sits inside that scope.

The refusal is only for the case where **every** condition is flat, which is
the case where the analysis has nothing to say at all. One flat condition among
several leaves the others estimable, and stopping would throw those away; those
rows keep their coverage warning and their `NaN`, which is the honest answer
for a row and not for the call.

Found by a calibration and stress pass over properties the test suite does not
check -- `stress-0.5.0.R`, which also confirmed that the permutation test is
correctly sized.

## Fixed: na.action = "listwise" reached this package and not the engine

`na.action` was applied in the function that computes the axis bounds, the
coverage and the data the plots draw, and the full data frame was then handed
to the engine. The engine drops incomplete pairs per condition inside its own
loop (`p_create_loop_data()`: `tmp[complete.cases(tmp), ]`), so every effect
size was fitted on that condition's own rows however `na.action` was set.

`"listwise"` therefore changed the denominators and not the numerators, which
is the disagreement it exists to prevent, and the documented behaviour -- every
condition fitted on the same rows -- was not what happened. Complete data was
never affected, which is why it went unnoticed.

The rule is now applied to the frame before the engine sees it, on the selected
columns only: a missing value in a column that is not in the analysis is not
the analysis's business, and dropping its row would silently shrink the sample.
Listwise deletion that leaves no complete row is an error naming `"pairwise"`
as the alternative, rather than an engine failure.

`"pairwise"`, the default, is unchanged.

## Breaking: one outcome column may not be given two theoretical ranges

`scope` may be one `c(xmin, xmax, ymin, ymax)`, a vector of `4 * k` values, or
a list of `k` such vectors. The per-condition forms exist for one reason:
conditions measured on different scales need different X ranges. The outcome
half is a repetition the format requires, not a setting -- there is only ever
one outcome column -- so writing it `k` times only creates `k` chances to write
it differently.

When the Y halves do differ, nothing downstream can represent it. The threshold
table is one table, one row per outcome level and one column per condition, and
a row cannot mean `y = 2` in one column and `y = 20` in the next. NCA resolves
this by reading the level grid from the first segment alone
(`p_low_high()`: `scope[[1]][3]`, computed once before the per-condition loop)
while each effect size stays an area over its own box, so the table and the
effect sizes end up measured against different rectangles. NCA warns and
carries on -- correct there, since its per-condition analyses are independent
and the table is the only shared result.

SCAtools now raises an error instead, naming the condition, the bound and both
values. The fix costs nothing: estimation was never joint, so one call with
several conditions is a loop over several independent single-condition
analyses, and a condition that genuinely needs a different outcome scope can be
fitted in its own call. The only thing a separate call gives up is the shared
threshold table -- which is exactly the thing that cannot hold two outcome
ranges.

`NA` counts as "not declared". A bound given for one condition and left `NA`
for another is the quiet form of this: `NA` means "use the observed range", so
the two conditions land on different boxes without anyone writing two different
numbers. The `4 * k` vector form is the one to watch, because NCA's own
`p_validate_scope_vector()` does not run the check that
`p_validate_scope_list()` does, so that form warns nowhere.

A single `c(xmin, xmax, ymin, ymax)` is replicated to every condition and can
never trigger this.

## New: print() separates the researcher's decisions from the package's

`sca()` exists so that three arguments produce a reported analysis, which means
the package picks the frontier techniques, the number of replications, the
significance level, the reporting scales and the scope on the caller's behalf.
Nothing in the output used to say so, and a reader could not tell which numbers
a researcher had chosen and which had been chosen for them -- the one
distinction a methods section has to make.

`print()` now ends with a `Settings` block listing every one of them, with
`(default)` against each value the call did not name:

    Settings
      frontiers     ce_fdh, cr_fdh  (default)
      test.rep      1000  (default)
      alpha         0.05  (default)
      seed          not fixed  (default)
      relevance     none pre-specified  (default)
      scope         observed range of each condition and the outcome  (default)
      na.action     pairwise  (default)
      thresholds    condition on percentage of range, outcome on percentage of range  (default)
      convention    absolute  (default)

The marks come from the recorded call, so they are what was actually passed
rather than a comparison against formal defaults. `sca()` now records its own
call for this purpose: it forwards all of its arguments to `sca_analysis()` by
name, so `sca_analysis()`'s own `match.call()` reported every one of them as
supplied even when the caller had written three.

`sca_output()` ends with the same block. It assembles a report, and a report
has to say what it was run with; a caller who reaches for it instead of
`print()` should not silently lose that.

The old `Threshold reporting:` header line is gone; it said the same thing
without saying whether anyone had chosen it.

## New: sca_scope() reports the scope every effect size was divided by

Every effect size is an empty area divided by the area of the X-Y scope, so the
scope is half of every number reported. `sca_scope()` returns the declared
bounds, the bounds the engine used, the resulting area and whether the two
differ -- one row per condition.

`sca_table()` gains `scope_area` and `scope_adjusted` beside
`empty_space_area`, so that `effect_size = empty_space_area / scope_area` is
checkable in the table rather than a claim in the documentation. Code that
selects columns by position rather than by name will need updating.

## Fixed: the engine's scope widening is reported, and its global flag removed

`p_loop_data.R` builds the scope it divides by as the declared scope widened to
hold every observation, so a declared bound that excludes a data point is
replaced by the data point. The denominator of every effect size in the
analysis therefore changes while the declared scope -- the thing a researcher
writes down and reports -- does not.

The engine does say so, once per analysis: *"Theorectical scope has been
adjusted to include all observations"*. That sentence names neither the
condition nor the axis nor either bound, and a flag suppresses it after the
first, so an analysis of six conditions in which four were widened produced one
anonymous line. SCAtools now warns per condition and per axis with the declared
bound and the one that replaced it, and records the same in
`metadata$scope_adjustment`, in the `Settings` block and in `sca_scope()`.

The engine writes that suppression flag into the caller's global environment as
`scope_warnings_nca` and does not remove it, so it turned up in `ls()` and in
`save.image()` after any analysis. It is now removed on exit, unless it was
already there before the call.

## Changed: plots wrap their text and stop hiding the empty corner

* Titles, subtitles and captions are wrapped. ggplot2 wraps neither, so a long
  line used to lose its end on a title and -- captions being right-aligned --
  its beginning on a caption, silently and only in the exported file.
* Captions are left-aligned. A wrapped right-aligned block has a ragged left
  edge that no reader tracks back to.
* The corner label is translucent with a thin border. It sits inside the corner
  the claim says is empty, which is exactly where a counterexample would
  appear, so an opaque box there hid the one observation a reader most needs to
  see. `annotate = FALSE` removes it entirely.
* `sca_test_plot()` gains a caption stating what the null is, and the same
  wrapping and alignment.

## Documentation

* The documentation no longer implies that a declared theoretical scope is
  required for an effect size to be interpretable. It is not, and the engine
  says so: `NCA::nca_analysis()` defaults to the observed range. The observed
  range and a declared scope answer different questions and neither is
  privileged. What is hard to defend is a declared scope with no stated
  justification, because the researcher then chose the denominator and a box
  much wider than the data flatters the number -- unobserved space is empty by
  default, which is what the new coverage warning is about. The claim that a
  declared scope makes studies comparable is also gone: comparability follows
  the denominators, and two studies declaring different theoretical scopes are
  no more comparable than two fitted on their own observed ranges. Declaring
  one makes the denominator explicit and checkable, which is a different and
  smaller claim. `sca_four_configurations`' scope section is retitled
  accordingly: it documents the box those particular numbers assume rather
  than prescribing one.

* `qr.tau` now documents the inversion. The engine fits `tau` for an upper
  empty corner and `1 - tau` for a lower one, so the default 0.95 is the
  95th-percentile line where the empty space lies above and the 5th-percentile
  line where it lies below. Sufficiency in the `"HH"` direction has its empty
  space at corner 4 and therefore fits the 5th percentile: a floor, not a
  ceiling. It also records that `frontier_accuracy` for `qr` is `qr.tau` by
  construction rather than evidence of fit, and that an `NA` `fit` is ordinary
  for `qr` because the engine returns `NA` whenever a technique's empty area
  exceeds CE-FDH's.
* `.sca_from_scale()` no longer claims to be the inverse of `.sca_to_scale()`.
  It is on the four affine scales and is not on `percentile`, where the forward
  map is the empirical CDF and the reverse is `stats::quantile()`; the forward
  map is not injective, so no inverse exists.
* `sca_corner_map()` and the README now state that the four directions assume a
  monotone relationship, which is what makes the expected empty space a single
  corner. A curvilinear or U-shaped relationship has no single expected corner,
  so none of the four states the claim -- and the package will still fit a
  frontier and return a number for whichever direction is named.
* The 0.4.1 entry on OLS is amended. It said that being first in the default
  `ceilings` made `ols` the line `sca_extract()` reached for; in fact
  `NCA::nca_summary()` drops `ols` from the parameter matrix before returning
  it, so the consequence was a silent `NA`, not a wrong number.

## Fixed: sca_power() no longer swallows the engine's refusal

* `sca_power()` no longer returns the engine's `NULL`. The engine announces a
  refusal by printing a line and returning NULL, so the reason used to be lost
  and the failure surfaced later as `sca_powerplot()` complaining about missing
  columns. The printed reason is now captured and re-raised as the error.
  `.sca_check_slope()` stays as a convenience that names the caller's own
  direction; the return check covers everything it does not know about.
* An empty condition selection is refused by `sca_plot()`, `sca_thresholds()`
  and `nsca_plot()`. `NULL` already means every condition, so `character(0)`
  could only come from a filter that matched nothing -- and it used to reach
  the end, where it produced "incorrect number of dimensions".

## Fixed: a condition name containing an upper-case Y corrupted two columns

`empty_space` and `relationship` were stored as sentences containing the
literal characters `X` and `Y`, filled in by two successive
`sub(..., fixed = TRUE)` calls. `sub()` replaces only the first match, so once
the condition name had been substituted, a condition whose own name contained
an upper-case `Y` swallowed the second replacement:

    .sca_empty_space_text(4, "SYSTEM", "score")
      "High X with low Y"  ->  "High SYSTEM with low Y"  ->  "High SscoreSTEM with low Y"

`Yield`, `Year`, `YEARS`, `QUALITY`, `SUPPLY` and `SYSTEM` all trigger it. The
two columns appear in `sca_table()`, `sca_thresholds()`, `print()` and the plot
subtitles, so the corrupted text was reaching whatever the reader copied out.
The templates are now `sprintf` format strings: a `%s` placeholder cannot be
hit by the data being substituted into it. The reference tables
`sca_corner_map()` and the NSCA equivalents were never affected, because they
never substitute names.

## Fixed: an engine parameter that disappears no longer becomes NA

Every quantity SCAtools reports is read out of the NCA result **by name** --
`"Effect size"`, `"Ceiling accuracy"`, `"p-value"` and six others. When a name
was not found, `.sca_parameter()` returned `NA_real_`, so a renamed engine
parameter would have emptied a whole column without an error. That failure is
now separated into three cases that are genuinely different:

* **the name is absent** -- SCAtools and NCA disagree about the interface. This
  is now an error, raised once per fit by the new engine contract check rather
  than repeated per cell.
* **the frontier column is absent** -- that technique was not estimated. `NA`,
  quietly.
* **the value is `NA`** -- the engine computed it and there is nothing there. A
  step frontier has no slope and no intercept by construction, so this must
  stay quiet or every default analysis would warn twice per row.

## Fixed: both axes are now bounded by the same rows (breaking with missing data)

`.sca_condition_data()` bounded the condition axis with that condition's
complete pairs but the outcome axis with every row having a finite outcome,
whether or not the condition was observed there. That is neither pairwise nor
listwise deletion but a third rule nobody chose, and since every effect size is
an area over the scope box, the denominator came from more rows than the
numerator. Complete data was never affected, which is why it went unnoticed.

The new `na.action` argument states the rule: `"pairwise"` (the default, and
what the old code intended) or `"listwise"`. Under either, both axes of a
condition are bounded by the same rows.

## New: `relevance`, and four support columns

`sca_analysis()` takes a pre-specified practical-relevance threshold, as
`NSCA::nsca_analysis()` has for both its components since 0.4.0. `sca_table()`
gains `significant`, `relevant`, `supported` and `support_status`, which mirror
the sufficiency side of NSCA's joint table exactly. Significance and relevance
are kept as separate columns on purpose.

## New: `test.seed`, and the caller's random stream is given back

The engine derives its entire permutation sequence from one draw taken from the
caller's stream, so `set.seed()` before a call already made the p-values
reproducible. What it did not do was leave the stream alone afterwards: NCA
calls `set.seed(NULL)` when the test finishes, which reseeds from the clock. A
single analysis therefore replaced the stream the caller was using. `test.seed`
now fixes the sequence and restores the previous stream on exit.

## Breaking

* `sca()` runs a permutation test by default (`test.rep = 1000`), matching
  `NSCA::nsca()`.
* `bottleneck.x` and `bottleneck.y` are removed from `sca_analysis()`, and
  `boundary` from `sca_thresholds()` and `sca_rescale()`. Use `threshold.x`,
  `threshold.y` and `inequality`.
* `sca_extract()` no longer accepts retired column names. Retired names remain
  reachable through `legacy = TRUE` on the tables, which is the one mechanism
  that cannot give a different answer from the current name -- it copies the
  same vector. The promise that `sca_extract()` accepted them "with a warning"
  could never be kept: all three retired names are threshold-table columns,
  which are defined per outcome level and have no single value to return, so
  the call warned and then failed.
* `sca_thresholds()`, `sca_test_plot()` and `sca_output()` default to the first
  frontier actually estimated instead of hard-coded `"ce_fdh"`.
  `sca_analysis(ceilings = "cr_fdh")` followed by `sca_thresholds(fit)` used to
  fail on a model that had been fitted perfectly well.
* `sca_power()` refuses a non-finite slope. The previous check used
  `anyNA()`, which does not catch `Inf`, while its own message promised finite
  numbers.
* `inst/NOTICE` said `SCAtools 0.1.0`, the last survivor of the 0.1.0 cleanup.

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
permutation p value and no threshold table -- and it stood as the *first*
entry of the default `ceilings`, so the package's own default put a
central-tendency line at the head of a list of frontiers.

> **Amended in 0.5.0.** This entry originally continued "which made it the
> line `sca_extract()` reached for when no ceiling was named". That was wrong
> about the mechanism. `NCA::nca_summary()` drops `ols` from the parameter
> matrix before returning it -- `data2[names(data2) != "ols"]`, at
> `nca_summary.R` lines 146-147 in NCA 5.0.2 -- so `ols` was never a column
> that any extractor could resolve, and asking for it returned `NA` rather
> than a regression coefficient. The default was wrong for the reason given
> above; its consequence was a silent `NA`, not a wrong number reported as a
> frontier.

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
