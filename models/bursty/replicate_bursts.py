"""Which input series reproduces Cychosz et al.'s published burst metrics?

burst_detection_v1.py reads AWC_COUNT from everyday_cds_ods.csv with no sleep or
CDS filter; the modelling script's comment says bursts were "calculated over AWC
words in 5-minute CDS segments with naps removed". Recompute the metrics exactly
as the script does on four candidate series and compare with
burstiness_compiled_results.csv.

Usage: python3 models/bursty/replicate_bursts.py <dir with everyday_cds_ods.csv>
"""
import sys
import numpy as np
import pandas as pd
from scipy import interpolate

d = sys.argv[1]
epochs = pd.read_csv(f"{d}/everyday_cds_ods.csv", index_col=0, low_memory=False)
pub = pd.read_csv(f"{d}/burstiness_compiled_results.csv", index_col=0).set_index("child_id")


def burst_metrics(y):
    """Exactly burst_detection_v1.py: interpolate x10, threshold 3*SD (no median)."""
    y = np.asarray(y, dtype=float)
    if len(y) < 2:
        return np.nan, np.nan
    f = interpolate.interp1d(np.linspace(0, len(y), len(y)), y)
    ynew = f(np.arange(0, len(y), 0.1))
    above = ynew > 3 * np.std(ynew)
    if not above.any():
        return np.nan, np.nan
    runs = np.diff(np.flatnonzero(np.diff(np.r_[0, above.astype(int), 0])))[::2]
    return ynew[above].mean(), runs.mean()


variants = {
    "all epochs, AWC": lambda g: g["AWC_COUNT"],
    "awake epochs, AWC": lambda g: g.loc[g["sleep_pred"] == 0, "AWC_COUNT"],
    "awake epochs, ODS zeroed": lambda g: g.loc[g["sleep_pred"] == 0, "AWC_COUNT_ODS"],
    "awake CDS epochs only": lambda g: g.loc[(g["sleep_pred"] == 0) & (g["cds_pred"] == 1), "AWC_COUNT"],
}
rows = []
for cid, g in epochs.groupby("child_id", sort=False):
    if cid not in pub.index:
        continue
    g = g.sort_values("segment")  # segment order matches 23% of published values exactly; file order matches none
    for name, sel in variants.items():
        amp, dur = burst_metrics(pd.to_numeric(sel(g), errors="coerce").dropna())
        rows.append((cid, name, amp, dur))
res = pd.DataFrame(rows, columns=["child_id", "variant", "amp", "dur"]).set_index("child_id")
res = res.join(pub[["mean_burst_amplitude", "mean_burst_length"]])
summary = res.groupby("variant").apply(lambda r: pd.Series({
    "n": r["amp"].notna().sum(),
    "amp_exact_%": 100 * np.mean(np.isclose(r["amp"], r["mean_burst_amplitude"], rtol=1e-6)),
    "amp_cor": r["amp"].corr(r["mean_burst_amplitude"]),
    "dur_exact_%": 100 * np.mean(np.isclose(r["dur"], r["mean_burst_length"], rtol=1e-6)),
    "dur_cor": r["dur"].corr(r["mean_burst_length"]),
}))
print(summary.round(3).to_string())
