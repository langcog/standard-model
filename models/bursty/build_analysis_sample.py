"""Join PPVT, maternal education and sex onto each child's first-timepoint recording.

Uses the L2T participant files from Cychosz et al.'s public data (TP1 and TP2), one
recording per child (the earliest timepoint), as in the paper's vocabulary models.
Run after reliability.py, which writes recording_metrics.csv.

Usage: python3 models/bursty/build_analysis_sample.py <dataframes dir> <out dir>
"""
import sys
import pandas as pd

d, out = sys.argv[1], sys.argv[2]
t1 = pd.read_csv(f"{d}/participantinfo_tp1-2.csv")
t2 = pd.read_csv(f"{d}/ParticipantInfo_TP2.csv")
demo = (t2[["ResearchID", "maternal_education_level_revised", "Female"]]
        .rename(columns={"maternal_education_level_revised": "mated"})
        .drop_duplicates("ResearchID"))
voc = pd.concat([
    t1.assign(child_id=t1.ResearchID + "TP1")[["child_id", "ResearchID", "PPVT_GSV", "PPVT_Age"]],
    t2.assign(child_id=t2.ResearchID + "TP2")[["child_id", "ResearchID", "PPVT_GSV", "PPVT_Age"]],
])
m = pd.read_csv(f"{out}/recording_metrics.csv").query("part == 'full'")
x = m.merge(voc, on="child_id").merge(demo, on="ResearchID")
x["mated"] = pd.to_numeric(x["mated"], errors="coerce")
x = (x.dropna(subset=["PPVT_GSV", "PPVT_Age", "mated", "Female", "burst_amp", "burst_dur"])
      .sort_values("child_id").drop_duplicates("ResearchID"))
cols = ["child_id", "PPVT_GSV", "PPVT_Age", "age", "mated", "Female", "q_paper", "q_cds", "burst_amp", "burst_dur"]
x[cols].to_csv(f"{out}/analysis_sample.csv", index=False)
print(f"children: {len(x)}")
