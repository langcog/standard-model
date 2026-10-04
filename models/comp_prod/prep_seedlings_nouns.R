# Build Stan data for comp_prod_nouns.stan: CDI nouns x SEEDLingS kids, with each
# child's own monthly token counts for each noun (BergelsonLab/seedlings-nouns).
#
# Counts: audio, top-3 most talkative hours per month (comparable across kids and
# months), excluding the child's own speech (CHI) and electronic-only speakers.
# Exposure offset = annotated top-3 duration of that recording.
#
# Usage: SM2_DIR=... SN_DIR=... Rscript models/comp_prod/prep_seedlings_nouns.R

suppressPackageStartupMessages(library(tidyverse))
sm2 <- Sys.getenv("SM2_DIR", "../standard-model-2")
sn <- Sys.getenv("SN_DIR", "../seedlings-nouns")
a0 <- 12

clean <- \(x) x %>% str_remove_all("\\s*\\(.*\\)|\\*") %>% str_extract("^[^/]+") %>%
  str_trim() %>% tolower()

tok <- read_csv(file.path(sn, "seedlings-nouns.csv"), show_col_types = FALSE,
                col_select = c(audio_video, subject, month, speaker, global_basic_level,
                               is_top_3_hours, object_present, utterance_type),
                col_types = cols(subject = "c", month = "c", .default = "?")) %>%
  filter(audio_video == "audio", is_top_3_hours, speaker != "CHI",
         !str_detect(speaker, "TV$|^TOY$"))
hours <- read_csv(file.path(sn, "regions.csv"), show_col_types = FALSE,
                  col_types = cols(subject = "c", month = "c", .default = "?")) %>%
  filter(audio_video == "audio", is_top_3_hours) %>%
  mutate(h = (end - start) / 3.6e6) %>%
  group_by(subject, month) %>% summarise(hours = sum(h), .groups = "drop")

key <- read_csv(file.path(sm2, "data/intermediates/cdi_master_item_key.csv"), show_col_types = FALSE) %>%
  distinct(item, .keep_all = TRUE) %>% select(item, lexical_category, prob)
cdi <- read_csv(file.path(sm2, "data/raw/seedlings/cdi_items_long.csv"), show_col_types = FALSE,
                col_types = cols(subject_id = "c", .default = "?")) %>%
  filter(age <= 18, !is.na(comprehends), !is.na(produces)) %>%
  mutate(y = pmin(comprehends + produces, 2L))

gbl <- unique(tok$global_basic_level)
nouns <- distinct(cdi, item) %>% left_join(key, by = "item") %>%
  filter(lexical_category == "nouns") %>%
  mutate(w = clean(item), w2 = str_remove_all(w, "[ -]"), w3 = str_remove(w, "s$"),
         match = case_when(w %in% gbl ~ w, w2 %in% gbl ~ w2, w3 %in% gbl ~ w3, TRUE ~ NA_character_))
cat(sprintf("CDI nouns: %d | matched to annotated nouns: %d\n", nrow(nouns), sum(!is.na(nouns$match))))
cat("unmatched:", paste(nouns$item[is.na(nouns$match)], collapse = ", "), "\n")
# a basic-level label shared by two CDI items (e.g. chicken animal/food) is ambiguous: drop
nouns <- nouns %>% filter(!is.na(match)) %>% add_count(match, name = "n_cdi") %>% filter(n_cdi == 1) %>%
  mutate(j = row_number())

counts <- tok %>% filter(global_basic_level %in% nouns$match) %>%
  count(subject, month, global_basic_level, name = "n")
kids <- intersect(unique(cdi$subject_id), unique(hours$subject))
kid_ix <- tibble(subject = sort(kids), i = seq_along(kids))
cnt <- hours %>% filter(subject %in% kids) %>%
  cross_join(select(nouns, j, match)) %>%
  left_join(counts, by = c("subject", "month", match = "global_basic_level")) %>%
  mutate(n = replace_na(n, 0L), m = as.integer(month)) %>%
  left_join(kid_ix, by = "subject")
cat(sprintf("count cells: %d | tokens: %d | zero cells: %.0f%% | top-3 hours/recording: %.2f\n",
            nrow(cnt), sum(cnt$n), 100 * mean(cnt$n == 0), mean(hours$hours)))

d <- cdi %>% inner_join(select(nouns, item, j), by = "item") %>%
  inner_join(kid_ix, by = c(subject_id = "subject"))
adm <- distinct(d, i, age) %>% arrange(i, age) %>% mutate(a = row_number())
d <- left_join(d, adm, by = c("i", "age"))

standata <- list(
  N = nrow(d), I = nrow(kid_ix), A = nrow(adm), J = nrow(nouns),
  y = d$y, ii = d$i, jj = d$j, tt = d$a, adm_kid = adm$i, age = adm$age,
  logp_childes = log(nouns$prob) - mean(log(nouns$prob)),
  M = nrow(cnt), cnt_kid = cnt$i, cnt_item = cnt$j, cnt_n = cnt$n,
  log_hours = log(cnt$hours), cnt_age_rel = log(cnt$m / a0),
  log_H = log(12 * 30.44), a0 = a0, grainsize = 2000L)
saveRDS(list(standata = standata, nouns = nouns, kids = kid_ix, adm = adm),
        "models/comp_prod/seedlings_nouns_standata.rds")
cat(sprintf("CDI responses: %d | kids: %d | admins: %d | nouns: %d\n",
            standata$N, standata$I, standata$A, standata$J))
nouns %>% left_join(cnt %>% group_by(j) %>% summarise(tokens = sum(n), kids_heard = n_distinct(i[n > 0])), by = "j") %>%
  arrange(desc(tokens)) %>% select(item, tokens, kids_heard) %>% print(n = 8)
