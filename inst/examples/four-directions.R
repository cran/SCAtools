library(SCAtools)

set.seed(42)

# Positive frontier: appropriate for HH (corner 4) and LL (corner 1).
positive <- sca_random(
  n = 100,
  intercepts = 0.1,
  slopes = 0.8,
  direction = "HH"
)

fit_hh <- sca_analysis(
  positive,
  x = "X",
  y = "Y",
  direction = "HH",
  ceilings = c("ce_fdh", "cr_fdh"),
  threshold.x = "percentile",
  threshold.y = "percentage.range"
)

print(fit_hh)
print(sca_plot(fit_hh))

# The same fit, read three ways. Actual values are retained, so changing the
# reporting scale never refits the model.
print(sca_thresholds(fit_hh))
print(sca_thresholds(fit_hh, scale = "actual"))
print(sca_thresholds(fit_hh, scale = "sd"))

# Negative frontier: appropriate for LH (corner 3) and HL (corner 2).
negative <- sca_random(
  n = 100,
  intercepts = 0.9,
  slopes = -0.8,
  direction = "LH"
)

fit_lh <- sca_analysis(
  negative,
  x = "X",
  y = "Y",
  direction = "LH",
  ceilings = c("ce_fdh", "cr_fdh")
)

print(fit_lh)
print(sca_plot(fit_lh))

# LH has a low-level condition, so the two conventions disagree. Under
# "absolute" the rule reads X < 30% => Y > 80%; under "directional" the axis is
# mirrored and the same rule reads X > 70% => Y > 80%.
print(sca_thresholds(fit_lh, convention = "absolute"))
print(sca_thresholds(fit_lh, convention = "directional"))

# Inspect the complete logical-to-geometric mapping, and the available scales.
sca_corner_map()
sca_scales()

