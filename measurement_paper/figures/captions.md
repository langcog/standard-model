# Figure captions (drafts)

**Figure 1. What a log-unit of input is worth, and how to measure it.**
(A) The accumulator model as an equation. A child knows a word once the tokens accumulated pass the word's threshold; λ converts log tokens into log-odds. Four rulers supply independently measured variation in log input, and each arrow points to the term that ruler varies. Estimates are posterior medians with 95% intervals (Section 4). Age and efficiency have no ruler: vocabulary-by-age data give only the products λ(1 + δ) and λσ.
(B) λ as a slope. Probability of knowing a word against tokens heard relative to the word's threshold, at the lowest and highest ruler estimates. One doubling of input past the threshold takes the probability from 50% to 60% at λ = 0.57 and to 69% at λ = 1.16.
(C) Growth curves cannot tell the two apart. Both values of λ are paired with the δ that gives the same age slope, λ(1 + δ) = 10.9 (SM2's English fit), so a child with median input follows the same curve under both (black). They differ in what more input buys: a child hearing four times the input reaches 50% 3.3 months earlier at λ = 1.16 and 1.7 months earlier at λ = 0.57.

Script: `fig1_schematic.R`. The ruler estimates in panel A are typed in, not read from fits; update them if the fits change.

**Figure 2. Without a ruler, λ and δ lie on a ridge; one ruler resolves it.**
Simulated CDI data (600 children × 100 items, ages 16–30 months) from an accumulator with λ = 2.5, δ = 3.4 and an input share of 35%, tuned so that its age slope and child SD on the logit scale match SM2's English fit.
(A) Posterior draws in (λ, δ). The grey curve is every pair with the true age slope. Fixing λ = 1 picks the point δ ≈ 9.8 (black; 95% interval). Freeing λ with no ruler spreads the posterior along the curve (blue). A word-frequency ruler (orange) or measured input for a third of the children (green) puts it at the truth (diamond).
(B) Posterior medians and 95% intervals for λ, δ and the share of between-child variance due to input, with true values dashed. Fixing λ = 1 gives tight intervals around the wrong values. A frequency ruler measured with error (open circles) biases λ down and δ up, toward the fixed-λ answer.

Script: `fig2_simulation.R`, from the fits in `sims/lambda_identifiability/`. Summary values: `fig2_simulation.csv`.
