"""Two-state bout model (lull vs bout) for daylong CDS input, per recording.

Awake 5-minute epochs are modelled as a two-state hidden Markov chain; within a
state, log(1 + CDS words) is Gaussian (LENA counts are estimates, not integers).
Sleep gaps break the series into separate sequences. Per recording this gives
threshold-free burstiness parameters:
  contrast  : mean log words in bouts minus in lulls
  frac_bout : stationary share of epochs in the bout state
  bout_min  : mean bout length, 5 / (1 - P(stay in bout)) minutes
Fitted to the whole day and to two interleaved halves (alternating 1-hour blocks,
as in reliability.py) for split-half reliability; then related to PPVT.

Usage: python3 models/bursty/bout_model.py <dataframes dir> <out dir>
"""
import sys
import numpy as np
import pandas as pd
from scipy.special import logsumexp

d, out = sys.argv[1], sys.argv[2]
e = pd.read_csv(f"{d}/everyday_cds_ods.csv", low_memory=False,
                usecols=["child_id", "segment", "sleep_pred", "cds_pred", "AWC_COUNT"])
e["AWC_COUNT"] = pd.to_numeric(e["AWC_COUNT"], errors="coerce").fillna(0)
e = e[e["sleep_pred"] == 0].sort_values(["child_id", "segment"])
e["y"] = np.log1p(np.where(e["cds_pred"] == 1, e["AWC_COUNT"], 0.0))
e["half"] = ((e["segment"] - 1) // 12) % 2


def split_sequences(seg, y, block=None):
    """Break where segments aren't consecutive (sleep) or, for halves, at 1-hour blocks."""
    brk = np.diff(seg) != 1
    if block is not None:
        brk |= np.diff(block) != 0
    return np.split(y, np.flatnonzero(brk) + 1)


def log_norm(y, mu, sd):
    return -0.5 * ((y[:, None] - mu) / sd) ** 2 - np.log(sd) - 0.5 * np.log(2 * np.pi)


def fit_hmm(seqs, n_iter=200, tol=1e-6):
    yall = np.concatenate(seqs)
    best = None
    for q in (0.5, 0.7):
        mu = np.array([np.quantile(yall, q * 0.5), np.quantile(yall, min(q + 0.25, 0.95))])
        sd = np.full(2, max(yall.std(), 0.3))
        A = np.array([[0.9, 0.1], [0.2, 0.8]]); pi0 = np.array([0.7, 0.3])
        ll_old = -np.inf
        for _ in range(n_iter):
            g_sum = np.zeros(2); x_sum = np.zeros(2); xx_sum = np.zeros(2)
            trans = np.zeros((2, 2)); start = np.zeros(2); ll = 0.0
            for y in seqs:
                B = log_norm(y, mu, sd); T = len(y)
                la = np.empty((T, 2)); lb = np.zeros((T, 2))
                la[0] = np.log(pi0) + B[0]
                for t in range(1, T):
                    la[t] = logsumexp(la[t - 1][:, None] + np.log(A), axis=0) + B[t]
                for t in range(T - 2, -1, -1):
                    lb[t] = logsumexp(np.log(A) + B[t + 1] + lb[t + 1], axis=1)
                lseq = logsumexp(la[-1]); ll += lseq
                g = np.exp(la + lb - lseq)
                if T > 1:
                    xi = (la[:-1, :, None] + np.log(A)[None] + (B[1:] + lb[1:])[:, None, :]) - lseq
                    trans += np.exp(xi).sum(0)
                start += g[0]; g_sum += g.sum(0); x_sum += g.T @ y; xx_sum += g.T @ y**2
            mu = x_sum / np.maximum(g_sum, 1e-9)
            sd = np.sqrt(np.maximum(xx_sum / np.maximum(g_sum, 1e-9) - mu**2, 0.05**2))
            A = (trans + 1e-3) / (trans + 1e-3).sum(1, keepdims=True)
            pi0 = (start + 1e-3) / (start + 1e-3).sum()
            if ll - ll_old < tol * abs(ll):
                break
            ll_old = ll
        if best is None or ll > best[0]:
            best = (ll, mu.copy(), sd.copy(), A.copy())
    ll, mu, sd, A = best
    hi = int(np.argmax(mu)); lo = 1 - hi
    # stationary distribution of the 2-state chain
    p_hi = A[lo, hi] / (A[lo, hi] + A[hi, lo])
    return dict(contrast=mu[hi] - mu[lo], frac_bout=p_hi, bout_min=5 / (1 - A[hi, hi]),
                mu_lull=mu[lo], mu_bout=mu[hi], loglik=ll)


rows = []
for cid, g in e.groupby("child_id"):
    seg, y = g["segment"].to_numpy(), g["y"].to_numpy()
    if len(y) < 24:
        continue
    parts = {"full": split_sequences(seg, y)}
    for h in (0, 1):
        m = g["half"].to_numpy() == h
        parts["A" if h == 0 else "B"] = split_sequences(seg[m], y[m], block=((seg[m] - 1) // 12))
    for part, seqs in parts.items():
        seqs = [s for s in seqs if len(s) > 0]
        rows.append(dict(child_id=cid, part=part, **fit_hmm(seqs)))
bm = pd.DataFrame(rows)
bm.to_csv(f"{out}/bout_params.csv", index=False)

# split-half reliability
names = {"contrast": False, "frac_bout": "logit", "bout_min": "log"}
tf = lambda v, k: np.log(v) if names[k] == "log" else (np.log(v / (1 - v)) if names[k] == "logit" else v)
A = bm[bm.part == "A"].set_index("child_id"); B = bm[bm.part == "B"].set_index("child_id")
full = bm[bm.part == "full"].set_index("child_id")
rm = pd.read_csv(f"{out}/recording_metrics.csv").query("part == 'full'").set_index("child_id")
res = []
for k in names:
    ab = pd.concat([tf(A[k], k), tf(B[k], k)], axis=1, keys=["a", "b"]).replace([np.inf, -np.inf], np.nan).dropna()
    r = ab["a"].corr(ab["b"])
    res.append(dict(metric=k, n=len(ab), split_half_r=r, reliability_1day=2 * r / (1 + r),
                    cor_with_log_q_cds=tf(full[k], k).corr(np.log(rm["q_cds"]).reindex(full.index)),
                    median=full[k].median()))
res = pd.DataFrame(res).set_index("metric")
res.to_csv(f"{out}/bout_reliability.csv")
print(f"recordings fitted: {full.shape[0]}")
print(res.round(3).to_string())

# validity: PPVT on age, sex, maternal education, clean CDS rate, and bout parameters
x = pd.read_csv(f"{out}/analysis_sample.csv").merge(full.reset_index(), on="child_id")
c = lambda v: v - v.mean()


def tstats(cols):
    X = np.column_stack([np.ones(len(x))] + cols); yv = x["PPVT_GSV"].to_numpy()
    XtX = np.linalg.inv(X.T @ X); b = XtX @ X.T @ yv; r = yv - X @ b
    return b, b / np.sqrt(np.diag(XtX) * (r @ r) / (len(yv) - X.shape[1]))


base = [c(x["PPVT_Age"].to_numpy()), c(x["Female"].to_numpy()), c(x["mated"].to_numpy())]
lq = c(np.log(x["q_cds"].to_numpy()))
print(f"\nPPVT models (n = {len(x)}), t statistics for the input terms:")
for label, extra in [("rate only", []),
                     ("rate + contrast", [c(x["contrast"].to_numpy())]),
                     ("rate + frac_bout", [c(tf(x["frac_bout"], "frac_bout").to_numpy())]),
                     ("rate + bout length", [c(np.log(x["bout_min"].to_numpy()))]),
                     ("rate + all three", [c(x["contrast"].to_numpy()), c(tf(x["frac_bout"], "frac_bout").to_numpy()),
                                           c(np.log(x["bout_min"].to_numpy()))])]:
    b, t = tstats(base + [lq] + extra)
    print(f"  {label:20s} t(rate) = {t[4]:5.2f}" + "".join(f", t = {v:5.2f}" for v in t[5:]))
