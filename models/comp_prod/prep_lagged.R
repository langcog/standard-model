# Lagged ("filtered") cumulative-exposure data for comp_onset_lagged.stan.
#
# For CDI month t, child i, noun j, exposure is estimated ONLY from recordings
# in months < t, as a gamma-Poisson posterior:
#   r_ij ~ Gamma(k, k / (mu_j * c_i))      prior: word rate x child talkativeness
#   N_ij(<t) | r_ij ~ Poisson(r_ij * H_i(<t))
#   x_ijt = E[log r_ij | past] = digamma(k + N) - log(k / (mu_j c_i) + H)
# Using the posterior mean of log rate is regression calibration (roughly undoes
# the attenuation from 3-hour samples). Rows are restricted to the comprehension
# risk set (months up to and including first "understands"), so exposure after
# a word is learned never enters the likelihood. A "full" control uses all months.
#
# Usage: Rscript models/comp_prod/prep_lagged.R   (needs seedlings_nouns_standata.rds)
suppressPackageStartupMessages({ library(tidyverse); library(MASS, exclude = "select") })
here <- "models/comp_prod"
b <- readRDS(file.path(here, "seedlings_nouns_standata.rds"))
sd <- b$standata
a0 <- sd$a0

cnt <- tibble(i = sd$cnt_kid, j = sd$cnt_item, n = sd$cnt_n, h = exp(sd$log_hours),
              m = round(a0 * exp(sd$cnt_age_rel)))
hours_im <- distinct(cnt, i, m, h)

# prior: word rate mu_j, child talkativeness c_i, shape k from a NB fit on totals
tot <- cnt %>% group_by(i, j) %>% summarise(N = sum(n), H = sum(h), .groups = "drop")
mu_j <- tot %>% group_by(j) %>% summarise(mu = sum(N) / sum(H))
c_i <- tot %>% left_join(mu_j, by = "j") %>% group_by(i) %>%
  summarise(c = sum(N) / sum(mu * H))
tot <- tot %>% left_join(mu_j, by = "j") %>% left_join(c_i, by = "i")
k <- glm.nb(N ~ 1 + offset(log(mu * c * H)), data = tot)$theta
cat(sprintf("gamma prior shape k = %.2f (between-child CV of persistent rates = %.2f)\n", k, 1 / sqrt(k)))

post_log_rate <- function(N, H, mu, c) digamma(k + N) - log(k / (mu * c) + H)

# CDI rows with lagged and full exposure
rows <- tibble(i = sd$ii, j = sd$jj, a = sd$tt, y = sd$y) %>%
  mutate(age = sd$age[a]) %>%
  left_join(mu_j, by = "j") %>% left_join(c_i, by = "i")
past <- rows %>% distinct(i, j, age) %>%
  left_join(cnt %>% select(i, j, m, n), by = c("i", "j"), relationship = "many-to-many") %>%
  group_by(i, j, age) %>%
  summarise(N_past = sum(n[!is.na(m) & m < age]), .groups = "drop") %>%
  left_join(hours_im %>% rename(h_m = h), by = "i", relationship = "many-to-many") %>%
  group_by(i, j, age, N_past) %>% summarise(H_past = sum(h_m[m < age]), .groups = "drop")
rows <- rows %>% left_join(past, by = c("i", "j", "age")) %>%
  left_join(tot %>% select(i, j, N, H), by = c("i", "j")) %>%
  mutate(x_lag = post_log_rate(N_past, H_past, mu, c),
         x_full = post_log_rate(N, H, mu, c))

# decomposition: child level, word level, within (coefficient-1 ruler)
x_kid <- log(c_i$c) - mean(log(c_i$c))
x_word <- log(mu_j$mu) - mean(log(mu_j$mu))
rows <- rows %>% mutate(base = log(mu) + log(c),
                        res_lag = x_lag - base, res_full = x_full - base)

# comprehension risk set: up to and including first "understands"
rs <- rows %>% arrange(i, j, age) %>% group_by(i, j) %>%
  mutate(event = as.integer(y >= 1), seen = cumsum(event),
         keep = seen == 0 | (seen == 1 & event == 1)) %>%
  filter(keep) %>%
  mutate(first = as.integer(row_number() == 1),
         age_prev = lag(age, default = 0),
         res_lag_prev = lag(res_lag, default = 0),
         res_full_prev = lag(res_full, default = 0)) %>%
  ungroup()
cat(sprintf("risk-set rows: %d (of %d) | onsets observed: %d | known at first admin: %d\n",
            nrow(rs), nrow(rows), sum(rs$event), sum(rs$event & rs$first)))
cat(sprintf("sd within-ruler: lagged %.2f vs full %.2f | cor(lagged, full) = %.2f\n",
            sd(rs$res_lag), sd(rs$res_full), cor(rs$res_lag, rs$res_full)))

make_sd <- function(variant) list(
  R = nrow(rs), I = sd$I, J = sd$J,
  kid = rs$i, item = rs$j, age = rs$age, age_prev = rs$age_prev,
  first = rs$first, event = rs$event,
  res = rs[[paste0("res_", variant)]], res_prev = rs[[paste0("res_", variant, "_prev")]],
  x_kid = x_kid, x_word = x_word, a0 = a0)
saveRDS(list(lag = make_sd("lag"), full = make_sd("full"), k = k, rows = rs, all_rows = rows),
        file.path(here, "seedlings_lagged_standata.rds"))
