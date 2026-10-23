# Refit Cychosz et al.'s Table 3 (age) and Table 4 (vocabulary) models with
# regenerated burst metrics and two quantity measures, using their exact data prep.
suppressMessages({ library(lme4); library(lmerTest); library(dplyr) })
args <- commandArgs(trailingOnly = TRUE)
o <- readRDS(args[1]); v <- read.csv(args[2])
base <- o$burst_df2 %>% select(child_id, ResearchID, age_mos, PPVT_GSV, Female, maternal_education_level,
                               avg_hrly_words, mean_burst_amplitude, mean_burst_length) %>%
  left_join(v, by = "child_id")
cs <- function(x) as.numeric(scale(x, scale = FALSE))
variants <- list(published = c("mean_burst_amplitude", "mean_burst_length"),
                 V1_shipped_code = c("amp_V1", "dur_V1"), V2a_cds_fileorder = c("amp_V2a", "dur_V2a"),
                 V2b_cds_timeorder = c("amp_V2b", "dur_V2b"), V3_paper_threshold = c("amp_V3", "dur_V3"))
quant <- c(as_coded = "avg_hrly_words", cds_awake = "q_cds")
out <- list()
for (vn in names(variants)) for (qn in names(quant)) {
  d <- base %>% mutate(amp = .data[[variants[[vn]][1]]], dur = .data[[variants[[vn]][2]]],
                       q = .data[[quant[[qn]]]]) %>%
    filter(is.finite(log(amp)), is.finite(log(dur))) %>%
    mutate(amp_c = cs(log(amp)), dur_c = cs(log(dur)), q_c = cs(log(q)), age_c = cs(age_mos),
           ed_c = cs(maternal_education_level), Female = as.factor(Female))
  # Table 3: age effects on each input measure
  t_age <- sapply(c(amp = "amp_c", dur = "dur_c", q = "q_c"), function(y)
    coef(summary(lmer(reformulate(c("age_c", "Female", "ed_c", "(1|ResearchID)"), y), data = d)))["age_c", "t value"])
  # Table 4: vocabulary, as in their code (drop Romeo TP3/4)
  dv <- d %>% filter(!(substr(child_id, 4, 4) == "R" & substr(child_id, 7, 7) %in% c("3", "4")))
  fit <- function(b) coef(summary(lmer(reformulate(c(b, "age_c", "Female", "ed_c", "q_c", "(1|ResearchID)"), "PPVT_GSV"), data = dv)))
  fa <- fit("amp_c"); fd <- fit("dur_c")
  out[[length(out) + 1]] <- data.frame(bursts = vn, quantity = qn, n_rec = nrow(dv),
    t_age_amp = t_age["amp"], t_age_dur = t_age["dur"], t_age_q = t_age["q"],
    vocab_t_amp = fa["amp_c", "t value"], vocab_t_q_ampmodel = fa["q_c", "t value"],
    vocab_t_dur = fd["dur_c", "t value"], vocab_t_q_durmodel = fd["q_c", "t value"])
}
res <- do.call(rbind, out); rownames(res) <- NULL
write.csv(res, args[3], row.names = FALSE)
print(cbind(res[1:3], round(res[-(1:3)], 2)))
