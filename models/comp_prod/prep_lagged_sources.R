# Source shares for the source-weighted lagged onset model (comp_prod_onset_sources.stan).
#
# For each CDI row (child i, noun j, month t), the share of the child's tokens of j in
# months before t that came from each source:
#   read   : book reading (utterance_type == "r"), reference = all other talk
#   absent : object not present (object_present == "n"), reference = present/unsure
#   other  : any speaker but the mother, reference = mother
# Shares are shrunk toward the child's overall share for that source with a = 3
# pseudo-tokens, so nouns with few past tokens don't get extreme shares.
# Exposure (res_lag) and risk sets are exactly those of prep_lagged_nb.R.
#
#   repeat : (with GAP > 0) a token of the same noun within GAP seconds of the previous one
#            in the same recording -- a massed repeat; reference = first token of an episode
#
# Usage: SN_DIR=/path/to/seedlings-nouns [GAP=120] Rscript models/comp_prod/prep_lagged_sources.R
suppressPackageStartupMessages(library(tidyverse))
here <- "models/comp_prod"
sn <- Sys.getenv("SN_DIR", "../seedlings-nouns")
gap <- as.numeric(Sys.getenv("GAP", "0"))
a <- 3
nb <- readRDS(file.path(here, "seedlings_lagged_nb_cp_standata.rds"))
b <- readRDS(file.path(here, "seedlings_nouns_standata.rds"))

tok <- read_csv(file.path(sn, "seedlings-nouns.csv"), show_col_types = FALSE,
                col_select = c(recording_id, audio_video, subject, month, onset, speaker, global_basic_level,
                               is_top_3_hours, object_present, utterance_type),
                col_types = cols(.default = "c")) %>%
  filter(audio_video == "audio", toupper(is_top_3_hours) == "TRUE", speaker != "CHI",
         !str_detect(speaker, "TV$|^TOY$"), global_basic_level %in% b$nouns$match) %>%
  mutate(m = as.integer(month), read = utterance_type == "r", absent = object_present == "n",
         other = speaker != "MOT", onset = as.numeric(onset)) %>%
  arrange(recording_id, global_basic_level, onset) %>%
  group_by(recording_id, global_basic_level) %>%
  mutate(`repeat` = coalesce(onset - lag(onset) <= gap * 1000, FALSE)) %>% ungroup() %>%
  inner_join(b$kids, by = "subject") %>%
  inner_join(b$nouns %>% select(match, j), by = c(global_basic_level = "match"))

srcs <- c("read", "absent", "other", if (gap > 0) "repeat")
cell <- tok %>% group_by(i, j, m) %>%
  summarise(n = n(), across(all_of(srcs), sum), .groups = "drop")
child_share <- tok %>% group_by(i) %>% summarise(across(all_of(srcs), mean, .names = "cs_{.col}"))
cat("overall shares:", paste(sprintf("%s %.3f", srcs, colMeans(tok[srcs])), collapse = ", "), "\n")

rows <- nb$rows %>% select(i, j, age, y, res_lag) %>% left_join(child_share, by = "i")
past <- rows %>% distinct(i, j, age) %>%
  left_join(cell, by = c("i", "j"), relationship = "many-to-many") %>%
  mutate(keep = !is.na(m) & m < age) %>%
  group_by(i, j, age) %>%
  summarise(n_p = sum(n[keep]), across(all_of(srcs), \(v) sum(v[keep]), .names = "{.col}_p"), .groups = "drop")
rows <- rows %>% left_join(past, by = c("i", "j", "age")) %>%
  mutate(across(c(n_p, ends_with("_p")), \(v) coalesce(v, 0)),
         s_read = (read_p + a * cs_read) / (n_p + a),
         s_absent = (absent_p + a * cs_absent) / (n_p + a),
         s_other = (other_p + a * cs_other) / (n_p + a))
if (gap > 0) rows <- rows %>% mutate(s_repeat = (repeat_p + a * cs_repeat) / (n_p + a))
S <- paste0("s_", srcs)
cat("lagged shares across CDI rows (mean, sd):",
    paste(sprintf("%s %.2f (%.2f)", S, colMeans(rows[S]), apply(rows[S], 2, sd)), collapse = "; "), "\n")
# within-child, within-word spread is what identifies the weights
w_sd <- rows %>% group_by(i) %>% mutate(across(all_of(S), \(v) v - mean(v))) %>%
  group_by(j) %>% mutate(across(all_of(S), \(v) v - mean(v))) %>% ungroup() %>%
  summarise(across(all_of(S), sd))
cat("double-centered SD of shares:", paste(sprintf("%s %.3f", S, unlist(w_sd)), collapse = ", "), "\n")

lagS <- function(d) d %>% mutate(across(all_of(S), \(v) lag(v, default = 0), .names = "{.col}_prev"))
rs <- rows %>% arrange(i, j, age) %>% group_by(i, j) %>%
  mutate(event = as.integer(y >= 1), seen = cumsum(event), keep = seen == 0 | (seen == 1 & event == 1)) %>%
  filter(keep) %>%
  mutate(first = as.integer(row_number() == 1), age_prev = lag(age, default = 0),
         res_prev = lag(res_lag, default = 0)) %>% lagS() %>% ungroup()
ps <- rows %>% arrange(i, j, age) %>% group_by(i, j) %>%
  mutate(comp_on = cumsum(y >= 1) > 0, said = cumsum(y == 2)) %>%
  filter(comp_on, said == 0 | (said == 1 & y == 2)) %>%
  mutate(first = as.integer(row_number() == 1), event = as.integer(y == 2), age_prev = lag(age, default = 0),
         res_prev = lag(res_lag, default = 0)) %>% lagS() %>% ungroup()
stopifnot(nrow(rs) == nb$lag$R, nrow(ps) == nb$lag$P)

sd <- nb$lag
sd$K <- length(S)
sd$S_c <- as.matrix(rs[S]); sd$S_cprev <- as.matrix(rs[paste0(S, "_prev")])
sd$S_p <- as.matrix(ps[S]); sd$S_pprev <- as.matrix(ps[paste0(S, "_prev")])
stopifnot(isTRUE(all.equal(sd$res, rs$res_lag)), isTRUE(all.equal(sd$pres, ps$res_lag)))
out <- if (gap > 0) sprintf("seedlings_lagged_sources_rep%d_standata.rds", gap) else "seedlings_lagged_sources_standata.rds"
saveRDS(list(lag = sd, sources = S, a = a, gap = gap), file.path(here, out))
cat(sprintf("comprehension rows %d, production rows %d\n", sd$R, sd$P))
