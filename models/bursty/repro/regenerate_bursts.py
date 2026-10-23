"""Regenerate Cychosz et al.'s burst metrics under alternative, documented choices.

Variants (per recording):
  V1  burst_detection_v1.py as shipped: all epochs of everyday_cds_ods.csv, AWC_COUNT,
      file order, threshold 3*SD
  V2a awake CDS epochs (the input the published values match best), file order, 3*SD
  V2b awake CDS epochs, time order (sorted by segment), 3*SD
  V3  awake CDS epochs, time order, the paper's threshold: median + 3*SD
Also two quantity measures:
  q_paper  all awake adult words / total recording hours (as coded)
  q_cds    CDS-epoch words / awake hours

Usage: python3 models/bursty/regenerate_bursts.py <dataframes dir> <out csv>
"""
import sys
import numpy as np
import pandas as pd
from scipy import interpolate

d, out = sys.argv[1], sys.argv[2]
e = pd.read_csv(f"{d}/everyday_cds_ods.csv", index_col=0, low_memory=False,
                usecols=None)
e["AWC_COUNT"] = pd.to_numeric(e["AWC_COUNT"], errors="coerce")


def bursts(y, median_offset=False):
    y = np.asarray(pd.Series(y).dropna(), float)
    if len(y) < 4:
        return np.nan, np.nan, np.nan
    ynew = interpolate.interp1d(np.linspace(0, len(y), len(y)), y)(np.arange(0, len(y), 0.1))
    thr = 3 * np.std(ynew) + (np.median(ynew) if median_offset else 0)
    above = ynew > thr
    if not above.any():
        return np.nan, np.nan, 0
    runs = np.diff(np.flatnonzero(np.diff(np.r_[0, above.astype(int), 0])))[::2]
    return ynew[above].mean(), runs.mean(), len(runs)


rows = []
for cid, g in e.groupby("child_id", sort=False):
    awake = g[g["sleep_pred"] == 0]
    cds = awake[awake["cds_pred"] == 1]
    cds_sorted = cds.sort_values("segment")
    r = dict(child_id=cid)
    for name, series, med in [("V1", g["AWC_COUNT"], False),
                              ("V2a", cds["AWC_COUNT"], False),
                              ("V2b", cds_sorted["AWC_COUNT"], False),
                              ("V3", cds_sorted["AWC_COUNT"], True)]:
        r[f"amp_{name}"], r[f"dur_{name}"], r[f"n_{name}"] = bursts(series, med)
    th = g["total_hrs"].iloc[0]
    r["q_paper"] = awake["AWC_COUNT"].sum() / th
    r["q_cds"] = cds["AWC_COUNT"].sum() / (len(awake) * 5 / 60)
    rows.append(r)
pd.DataFrame(rows).to_csv(out, index=False)
print(f"recordings: {len(rows)}")
