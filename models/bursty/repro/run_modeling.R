# Run 1_modeling.Rmd end to end from the shipped dataframes, without stargazer/effects.
# Each top-level expression is evaluated with error trapping (plot chunks that need
# `effects` fail and are logged); key objects are saved for comparison with the paper.
args <- commandArgs(trailingOnly = TRUE)
repo <- args[1]
suppressMessages(knitr::purl(file.path(repo, "1_modeling.Rmd"), output = file.path(repo, "1_modeling_purled.R"),
                             documentation = 0, quiet = TRUE))
setwd(file.path(repo, if (length(args) > 1) args[2] else "dataframes"))
stargazer <- function(...) invisible(NULL)
pdf(NULL)
exprs <- parse(file.path(repo, "1_modeling_purled.R"))
log <- character()
for (i in seq_along(exprs)) {
  txt <- paste(deparse(exprs[[i]])[1], collapse = "")
  if (grepl("library\\(['\"](stargazer|effects)", txt)) next
  res <- tryCatch({ eval(exprs[[i]], envir = globalenv()); NULL },
                  error = function(e) conditionMessage(e))
  if (!is.null(res)) log <- c(log, sprintf("expr %d: %s -> %s", i, substr(txt, 1, 70), substr(res, 1, 120)))
}
writeLines(log, file.path(repo, "run_errors.txt"))
keep <- intersect(c("burst_df2", "burst_df_vocab_model", "cds_df2", "burst_amp_sum", "burst_length_sum",
                    "hrly_wds", "amp_test", "duration_test", "quant_test", "burst_amp_v_sum",
                    "burst_length_v_sum", "amp_test_v", "duration_test_v", "all_speech_quantity_tbl",
                    "researchid", "cds_df"), ls(globalenv()))
saveRDS(mget(setdiff(keep, "cds_df"), envir = globalenv()), file.path(repo, "modeling_objects.rds"))
cat("errors:", length(log), "\n")
