"""Reliability of input metrics in Cychosz et al.'s daylong LENA data.

For each recording, metrics are computed on the whole day and on two interleaved
halves (alternating 1-hour blocks of 5-minute epochs, so both halves span the same
times of day). Split-half correlations across recordings, stepped up with
Spearman-Brown, give single-day reliability. Children with recordings at more than
one timepoint give an age-adjusted test-retest correlation (which also includes
real change between timepoints, so it is a lower bound on reliability).

Metrics
  q_paper   : all awake adult words / total recording hours (their "CDS words/hr" as coded)
  q_cds     : CDS-epoch adult words / awake hours
  burst_amp : mean burst amplitude, their algorithm (x10 interpolation, > 3 SD), awake CDS epochs
  burst_dur : mean burst length, same
  disp      : excess dispersion of CDS words per awake epoch, (var - mean) / mean^2
              (squared CV of the underlying intensity under a gamma-Poisson model; scale-free)
  cv        : SD / mean of CDS words per awake epoch
  ac1       : lag-1 autocorrelation of CDS words across consecutive awake epochs (persistence)

Usage: python3 models/bursty/reliability.py <dataframes dir> <out dir>
"""
import sys
import numpy as np
import pandas as pd
from scipy import interpolate, stats

d, out = sys.argv[1], sys.argv[2]
e = pd.read_csv(f"{d}/everyday_cds_ods.csv", low_memory=False,
                usecols=["child_id", "segment", "sleep_pred", "cds_pred", "AWC_COUNT", "total_hrs"])
e["AWC_COUNT"] = pd.to_numeric(e["AWC_COUNT"], errors="coerce").fillna(0)
e = e.sort_values(["child_id", "segment"])
e["cds_words"] = np.where(e["cds_pred"] == 1, e["AWC_COUNT"], 0.0)
e["half"] = ((e["segment"] - 1) // 12) % 2          # alternating 1-hour blocks
ages = (pd.read_csv(f"{d}/everyday_speech.csv", usecols=["child_id", "age_mos"])
        .drop_duplicates("child_id").set_index("child_id")["age_mos"])


def bursts(y):
    y = np.asarray(y, float)
    if len(y) < 4:
        return np.nan, np.nan
    ynew = interpolate.interp1d(np.linspace(0, len(y), len(y)), y)(np.arange(0, len(y), 0.1))
    above = ynew > 3 * np.std(ynew)
    if not above.any():
        return np.nan, np.nan
    runs = np.diff(np.flatnonzero(np.diff(np.r_[0, above.astype(int), 0])))[::2]
    return ynew[above].mean(), runs.mean()


def metrics(g, total_hrs):
    awake = g[g["sleep_pred"] == 0]
    if len(awake) < 12:
        return None
    awake_hrs = len(awake) * 5 / 60
    x = awake["cds_words"].to_numpy()
    m, v = x.mean(), x.var(ddof=1)
    seg = awake["segment"].to_numpy()
    consec = np.diff(seg) == 1
    ac1 = np.corrcoef(x[:-1][consec], x[1:][consec])[0, 1] if consec.sum() > 10 else np.nan
    amp, dur = bursts(awake.loc[awake["cds_pred"] == 1, "AWC_COUNT"])
    return dict(q_paper=awake["AWC_COUNT"].sum() / total_hrs,
                q_cds=x.sum() / awake_hrs, burst_amp=amp, burst_dur=dur,
                disp=(v - m) / m**2 if m > 0 else np.nan, cv=np.sqrt(v) / m if m > 0 else np.nan,
                ac1=ac1)


rows = []
for cid, g in e.groupby("child_id"):
    th = g["total_hrs"].iloc[0]
    for part, sub in [("full", g), ("A", g[g["half"] == 0]), ("B", g[g["half"] == 1])]:
        # each half covers half the recording, so its rate denominator is half the hours
        r = metrics(sub, th if part == "full" else th / 2)
        if r:
            rows.append(dict(child_id=cid, part=part, **r))
m = pd.DataFrame(rows)
m["age"] = m["child_id"].map(ages)
m.to_csv(f"{out}/recording_metrics.csv", index=False)

names = ["q_paper", "q_cds", "burst_amp", "burst_dur", "disp", "cv", "ac1"]
logged = {"q_paper", "q_cds", "burst_amp", "burst_dur"}
tf = lambda s, k: np.log(s) if k in logged else s

# split-half reliability
A = m[m.part == "A"].set_index("child_id"); B = m[m.part == "B"].set_index("child_id")
full = m[m.part == "full"].set_index("child_id")
res = []
for k in names:
    ab = pd.concat([tf(A[k], k), tf(B[k], k)], axis=1, keys=["a", "b"]).replace([np.inf, -np.inf], np.nan).dropna()
    r = ab["a"].corr(ab["b"])
    res.append(dict(metric=k, n=len(ab), split_half_r=r, reliability_1day=2 * r / (1 + r)))
res = pd.DataFrame(res).set_index("metric")

# age-adjusted test-retest across timepoints (consecutive recordings of the same child)
f = full.copy(); f["kid"] = f.index.str[:4]; f["tp"] = f.index.str[4:]
f = f[f["tp"].str.startswith("TP")].dropna(subset=["age"])
for k in names:
    v = tf(f[k], k).replace([np.inf, -np.inf], np.nan)
    ok = v.notna()
    resid = pd.Series(np.nan, index=f.index)
    resid[ok] = v[ok] - np.polyval(np.polyfit(np.log(f.loc[ok, "age"]), v[ok], 1), np.log(f.loc[ok, "age"]))
    f[k + "_r"] = resid
pairs = []
for kid, g in f.sort_values("tp").groupby("kid"):
    if len(g) > 1:
        a, b = g.iloc[0], g.iloc[1]
        pairs.append({"gap_mo": b["age"] - a["age"], **{k: (a[k + "_r"], b[k + "_r"]) for k in names}})
P = pd.DataFrame(pairs)
for k in names:
    xy = pd.DataFrame(P[k].tolist(), columns=["x", "y"]).dropna()
    res.loc[k, "retest_r_age_adj"] = xy["x"].corr(xy["y"])
    res.loc[k, "retest_n"] = len(xy)
# overlap with the cleaner quantity measure (full day, log scale)
for k in names:
    res.loc[k, "cor_with_log_q_cds"] = tf(full[k], k).replace([np.inf, -np.inf], np.nan).corr(np.log(full["q_cds"]))
res.to_csv(f"{out}/reliability_summary.csv")
print(f"recordings: {full.shape[0]} | test-retest pairs: {len(P)} (median gap {P['gap_mo'].median():.0f} mo)")
print(res.round(3).to_string())
