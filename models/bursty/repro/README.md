# Reproducibility check: Cychosz et al., "Bursty, irregular speech input to preschoolers predicts vocabulary size"

Runs the authors' `1_modeling.Rmd` end to end from their shipped data, then refits
their Table 3 (age) and Table 4 (vocabulary) models with burst metrics regenerated
under documented choices. Code and data: [megseekosh/everyday_speech](https://github.com/megseekosh/everyday_speech)
(commit 4f367d7). The raw per-recording files read by `0_loading.Rmd` are not public,
so the check starts from the shipped `dataframes/`.

```bash
git clone https://github.com/megseekosh/everyday_speech.git && cd everyday_speech && unzip dataframes.zip
cp /path/to/standard-model/models/bursty/repro/*.{R,py} .   # these scripts, run from the clone
Rscript patch_demo_files.R .                      # two Excel-as-CSV files -> real CSV, column names fixed
Rscript run_modeling.R . dataframes_patched       # their Rmd, expression by expression -> modeling_objects.rds
python3 regenerate_bursts.py dataframes burst_variants.csv
Rscript refit_variants.R modeling_objects.rds burst_variants.csv refit_variants.csv
```

`stargazer` is stubbed and the `effects`-based plot chunks fail harmlessly; neither affects the models.

## Result

From the shipped intermediates, Tables 2-4 reproduce exactly. Regenerating the burst
metrics (`refit_variants.csv`, t statistics, 483 recordings):

| Burst metrics | Age → amplitude | Age → duration | Age → quantity (as coded / CDS per awake hour) | Vocabulary ← amplitude | Vocabulary ← duration |
| --- | --- | --- | --- | --- | --- |
| Published intermediate file | −2.05 | −4.23 | −0.59 / −3.30 | 2.10 | 2.03 |
| Shipped script on shipped file | −2.25 | −1.90 | ″ | 1.45 | 2.72 |
| Awake CDS epochs, file order | −1.45 | −2.19 | ″ | 2.12 | 0.78 |
| Awake CDS epochs, time order | −1.06 | −0.51 | ″ | 2.07 | 0.93 |
| Time order + paper's threshold | −1.48 | −0.11 | −0.45 / −3.43 | 1.67 | 1.71 |

## Discrepancies

1. `dialect_demo_info.csv` and `mp_demo_info.csv` are Excel workbooks with a `.csv` extension, and their column names don't match the code.
2. `everyday_cds_ods.csv` is out of time order for every recording; `burst_detection_v1.py` doesn't sort.
3. `burst_detection_v1.py` reads all epochs (sleep and non-CDS included); the published values match awake CDS epochs.
4. The code's threshold is `3*SD`; the paper says median + 3 SD (which leaves 44 recordings with no bursts).
5. "CDS words/hr" is all awake adult words divided by total recording hours (the CDS filter is commented out).
6. "30-second segments" are linear interpolation of 5-minute epochs.
