# Build Stan data for comp_prod.stan from SEEDLingS (44 kids, monthly WG 6-18 mo).
# Inputs come from a clone of langcog/standard-model-2 (set SM2_DIR), which
# vendors the public SEEDLingS files (Egan-Dailey & Bergelson 2025; BergelsonLab/WordExposure).
#
# Usage: SM2_DIR=/path/to/standard-model-2 Rscript models/comp_prod/prep_seedlings.R [n_items]

suppressPackageStartupMessages(library(tidyverse))
sm2 <- Sys.getenv("SM2_DIR", "../standard-model-2")
args <- commandArgs(trailingOnly = TRUE)
n_items <- if (length(args)) as.integer(args[1]) else NA   # optional stratified subsample
a0 <- 12
set.seed(1)

cdi <- read_csv(file.path(sm2, "data/raw/seedlings/cdi_items_long.csv"), show_col_types = FALSE) %>%
  filter(age <= 18, !is.na(comprehends), !is.na(produces)) %>%
  mutate(y = pmin(comprehends + produces, 2L))   # 0 neither, 1 understands, 2 says
stopifnot(!any(cdi$produces == 1 & cdi$comprehends == 0))

key <- read_csv(file.path(sm2, "data/intermediates/cdi_master_item_key.csv"), show_col_types = FALSE) %>%
  select(item, lexical_category, prob) %>% distinct(item, .keep_all = TRUE)

# phonological covariates for the production gap Delta_j
syll <- read_delim("data/EnglishSyllables.csv", delim = ";", show_col_types = FALSE)
clean_word <- \(x) x %>% str_remove_all("\\s*\\(.*\\)|\\*") %>% str_extract("^[^/]+") %>% str_trim()
items <- distinct(cdi, item) %>%
  left_join(key, by = "item") %>%
  mutate(word = clean_word(item),
         n_syll = map_dbl(str_split(word, "[ -]+"), \(w) {
           s <- syll$nrOfSyll[match(tolower(w), syll$word)]
           if (any(is.na(s))) NA_real_ else sum(s)
         }),
         n_char = nchar(str_remove_all(word, "[^A-Za-z]")))
cat(sprintf("items: %d | with CHILDES prob: %.0f%% | with syllables: %.0f%%\n",
            nrow(items), 100 * mean(!is.na(items$prob)), 100 * mean(!is.na(items$n_syll))))
items <- items %>%
  filter(!is.na(prob), !is.na(lexical_category)) %>%
  mutate(n_syll = coalesce(n_syll, round(predict(lm(n_syll ~ n_char, cur_data()), cur_data()))))

if (!is.na(n_items)) {   # stratify on class x log-frequency quartile, as SM2 does
  items <- items %>% group_by(lexical_category, q = ntile(log(prob), 4)) %>%
    slice_sample(prop = n_items / nrow(items)) %>% ungroup() %>% select(-q)
}
items <- items %>% mutate(j = row_number(), cls = as.integer(factor(lexical_category)))

lena <- read_csv(file.path(sm2, "data/raw/seedlings/lena_data.csv"), show_col_types = FALSE) %>%
  filter(month <= 18, !awc_outlier, awc_perhr > 0)

d <- cdi %>% inner_join(select(items, item, j), by = "item") %>%
  mutate(i = as.integer(factor(subject_id)))
kids <- distinct(d, subject_id, i)
adm <- distinct(d, i, age) %>% arrange(i, age) %>% mutate(a = row_number())
d <- d %>% left_join(adm, by = c("i", "age"))
lena <- lena %>% inner_join(kids, by = c(subj = "subject_id"))

X <- with(items, cbind(scale(log(n_syll))[, 1], scale(n_char)[, 1]))
standata <- list(
  N = nrow(d), I = nrow(kids), A = nrow(adm), J = nrow(items),
  C = n_distinct(items$cls), K = ncol(X),
  y = d$y, ii = d$i, jj = d$j, tt = d$a,
  adm_kid = adm$i, age = adm$age,
  cls = items$cls, logp = log(items$prob) - mean(log(items$prob)), X = X,
  V = nrow(lena), rec_kid = lena$i,
  log_awc = log(lena$awc_perhr) - mean(log(lena$awc_perhr)),
  rec_age_rel = log(lena$month / a0),
  log_H = log(12 * 30.44), a0 = a0,
  free_beta_p = 1L, grainsize = 2000L)
saveRDS(list(standata = standata, items = items, kids = kids, adm = adm),
        "models/comp_prod/seedlings_standata.rds")
cat(sprintf("N = %d responses | %d kids | %d admins | %d items | %d LENA days\n",
            standata$N, standata$I, standata$A, standata$J, standata$V))
print(count(d, age, y) %>% pivot_wider(names_from = y, values_from = n, names_prefix = "y"), n = 20)
