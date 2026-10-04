# Observed SES gap in Wordbank (English WS, 16-30 mo), expressed as an
# age-equivalent ratio, and the input elasticity it implies.
#
# On the ability (logit) scale:  theta = a + B log(age) + G_e
# so a gap G_e equals a multiplicative age shift exp(G_e / B). That ratio is
# what a pure accumulator predicts should equal the input ratio r_e / r_ref.
suppressPackageStartupMessages(library(tidyverse))
here <- "models/ses_gap"
a <- readRDS(file.path(here, "wordbank_en_ws_admins.rds")) %>%
  filter(between(age, 16, 30), !is.na(caregiver_education)) %>%
  distinct(child_id, .keep_all = TRUE) %>%                 # one admin per child
  mutate(ed = fct_relevel(factor(caregiver_education), "College"),
         p = (production + 0.5) / 681)                     # 680 WS items
cat("children:", nrow(a), "\n"); print(count(a, ed))

# logit of proportion known ~ log age + education (sum-score proxy for the IRT ability scale)
m <- lm(qlogis(p) ~ log(age) + ed, data = a)
B <- coef(m)[["log(age)"]]
gaps <- broom::tidy(m, conf.int = TRUE) %>% filter(str_starts(term, "ed")) %>%
  transmute(education = str_remove(term, "^ed"), G = estimate, lo = conf.low, hi = conf.high,
            age_ratio = exp(G / B),                         # child "is as old as" ratio x ref
            months_at_24 = 24 * (age_ratio - 1))
cat(sprintf("age slope B = %.2f logit per log-month\n", B))
print(gaps, width = Inf)
write_csv(gaps, file.path(here, "observed_gap.csv"))

# What would an input gap of (r_lo / r_hi) predict?
#   pure accumulator (elasticity 1):   age ratio = r_lo / r_hi
#   SM2 at lambda = 1 (B_sm2 ~ 10.9):  age ratio = (r_lo / r_hi)^(1 / B_sm2)
input_ratio <- c(`HR poor vs professional (CDS)` = 558 / 2043, `30% less input` = 0.7)
tibble(scenario = names(input_ratio), input_ratio = input_ratio,
       pure_accumulator_months_at_24 = 24 * (input_ratio - 1),
       sm2_lambda1_months_at_24 = 24 * (input_ratio^(1 / 10.9) - 1)) %>% print(width = Inf)
