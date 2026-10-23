"""H0 simulation: how often does Cychosz et al.'s analysis find "burstiness beats
quantity" when vocabulary depends only on a child's stable input rate?

Generative model (calibrated on the 178 L2T children with PPVT, demographics and a
first-timepoint recording; see analysis_sample.csv):
  true log CDS rate   theta_i = mu + 0.141*mated_c - 0.314*log(age)_c + u_i,  u_i ~ N(0, sigma_b)
  recorded day        day_i   = theta_i + eps_i,  eps_i ~ N(0, sigma_day)
                      sigma_b^2 + sigma_day^2 = 0.446^2, reliability rho = sigma_b^2 / 0.446^2
  within-day texture  a real recording drawn from the 20 whose log CDS rate is closest to day_i,
                      rescaled to rate exp(day_i): quantity and burst amplitude scale with
                      the rate; burst duration (scale-free) is the template's. This keeps the
                      real dependence between rate and burst shape and the paper's measurement
                      quirks (sleep hours in the quantity denominator, ODS words in the numerator).
  vocabulary          PPVT = 2.05*age_c + 1.08*female_c + 4.34*mated_c + b * u_i/sigma_b + noise,
                      residual SD set so total residual SD = 14.2 (as observed). Bursts play no role.
Analysis, as in the paper: OLS of PPVT on age, female, mated, log quantity (as coded) and one
burst measure, both log and centered.

Usage: python3 models/bursty/h0_simulation.py <dir with analysis_sample.csv and recording_metrics.csv>
"""
import sys
import numpy as np
import pandas as pd

d = sys.argv[1]
rng = np.random.default_rng(2026)
kids = pd.read_csv(f"{d}/analysis_sample.csv")
rec = pd.read_csv(f"{d}/recording_metrics.csv").query("part == 'full'").dropna(
    subset=["q_paper", "q_cds", "burst_amp", "burst_dur"])
t_lq = np.log(rec["q_cds"].to_numpy())
order = np.argsort(t_lq)
t_lq_sorted = t_lq[order]
T = rec.iloc[order][["q_paper", "q_cds", "burst_amp", "burst_dur"]].to_numpy()

mu = np.log(kids["q_cds"]).mean()
B_AGE, B_FEM, B_ED, RESID = 2.05, 1.08, 4.34, 14.2
TOTAL_SD = 0.446


def tstats(y, X):
    X = np.column_stack([np.ones(len(y)), X])
    XtX_inv = np.linalg.inv(X.T @ X)
    b = XtX_inv @ X.T @ y
    r = y - X @ b
    s2 = r @ r / (len(y) - X.shape[1])
    return b / np.sqrt(np.diag(XtX_inv) * s2)


def one_study(n, rho, b_theta, k=20):
    s = kids.sample(n, replace=True, random_state=rng.integers(1 << 31))
    age_c = (s["PPVT_Age"] - s["PPVT_Age"].mean()).to_numpy()
    fem_c = (s["Female"] - s["Female"].mean()).to_numpy()
    ed_c = (s["mated"] - s["mated"].mean()).to_numpy()
    lage_c = np.log(s["age"].to_numpy()); lage_c -= lage_c.mean()
    sb, sd = np.sqrt(rho) * TOTAL_SD, np.sqrt(1 - rho) * TOTAL_SD
    u = rng.normal(0, sb, n)
    day = mu + 0.141 * ed_c - 0.314 * lage_c + u + rng.normal(0, sd, n)
    # template: one of the k recordings nearest in log CDS rate
    pos = np.clip(np.searchsorted(t_lq_sorted, day), k // 2, len(t_lq_sorted) - k // 2)
    idx = pos - k // 2 + rng.integers(0, k, n)
    f = np.exp(day) / T[idx, 1]
    q_paper, amp, dur = T[idx, 0] * f, T[idx, 2] * f, T[idx, 3]
    noise_sd = np.sqrt(RESID**2 - b_theta**2)
    ppvt = B_AGE * age_c + B_FEM * fem_c + B_ED * ed_c + b_theta * u / sb + rng.normal(0, noise_sd, n)
    c = lambda v: np.log(v) - np.log(v).mean()
    base = [age_c, fem_c, ed_c]
    tA = tstats(ppvt, np.column_stack(base + [c(q_paper), c(amp)]))[-2:]
    tD = tstats(ppvt, np.column_stack(base + [c(q_paper), c(dur)]))[-2:]
    tC = tstats(ppvt, np.column_stack(base + [c(np.exp(day)), c(amp)]))[-2:]   # cleaner CDS-rate quantity
    return tA, tD, tC


rows = []
reps = 2000
for n in (178, 285):
    for rho in (0.5, 0.65):
        for b in (0.0, 2.0, 4.0, 6.0):
            res = np.array([np.concatenate(one_study(n, rho, b)) for _ in range(reps)])
            tq_A, ta_A, tq_D, td_D, tq_C, ta_C = res.T
            rows.append(dict(n=n, rho=rho, b_theta=b,
                             amp_sig=np.mean(ta_A > 1.96), amp_beats_q=np.mean((ta_A > 1.96) & (np.abs(tq_A) < 1.96)),
                             q_sig_A=np.mean(tq_A > 1.96),
                             dur_sig=np.mean(td_D > 1.96), dur_beats_q=np.mean((td_D > 1.96) & (np.abs(tq_D) < 1.96)),
                             amp_beats_cdsq=np.mean((ta_C > 1.96) & (np.abs(tq_C) < 1.96))))
out = pd.DataFrame(rows)
out.to_csv(f"{d}/h0_simulation.csv", index=False)
pd.set_option("display.width", 200)
print((out.set_index(["n", "rho", "b_theta"]) * 100).round(1).to_string())
