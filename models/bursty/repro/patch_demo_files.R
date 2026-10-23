# Make a patched copy of everyday_speech's dataframes/ that 1_modeling.Rmd can read.
#
# dialect_demo_info.csv and mp_demo_info.csv are Excel workbooks saved with a .csv
# extension, and their columns are named age_mos_PPVT / female / race / ethnicity
# where the code selects PPVT_Age / Female / Race / Ethnicity. This converts them to
# real CSV with the expected names; every other file is symlinked unchanged.
#
# Usage: Rscript patch_demo_files.R <everyday_speech clone>   (dataframes.zip unzipped in it)
args <- commandArgs(trailingOnly = TRUE)
repo <- args[1]
src <- file.path(repo, "dataframes")
dst <- file.path(repo, "dataframes_patched")
dir.create(dst, showWarnings = FALSE)
for (f in list.files(src)) file.symlink(normalizePath(file.path(src, f)), file.path(dst, f))
for (f in c("dialect_demo_info.csv", "mp_demo_info.csv")) {
  unlink(file.path(dst, f))
  x <- as.data.frame(readxl::read_excel(file.path(src, f)))
  names(x)[match(c("age_mos_PPVT", "female", "race", "ethnicity"), names(x))] <- c("PPVT_Age", "Female", "Race", "Ethnicity")
  write.csv(x, file.path(dst, f), row.names = FALSE)
}
cat("patched data in", dst, "\n")
