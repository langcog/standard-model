# Model-implied observed r^2 for the input-vocabulary meta-analysis design
# (Coffey & Snedeker 2026: 4-7% of vocabulary variance from input quantity).
#
# Frank & Marchman's Fig. 5 compares the model's LATENT input share
# (lambda^2 sigma_r^2 / sigma_xi^2, error-free input and ability) with the
# meta-analysis's OBSERVED r^2 (noisy 1-2 day input measures, one CDI sum score
# at one age). Here we simulate the meta-analysis design from SM2's fitted
# English model and compute the r^2 such a study would observe.
#
# Generative model (SM2 long_no_freq_slopes, English, journal entry 45):
#   eta_ij(t) = xi_i + log H + (1 + delta + zeta_i) log(t / a0) - delta_j
#   xi_i = mu_r + lambda * d_i + a_i,  d_i ~ N(0, sigma_r)  (log input deviation)
#   zeta_i = beta_d * d_i / sigma_r + residual   (F&M's input -> acceleration, kappa units per SD)
#   sd(xi) fixed at the fitted sigma_xi, so sigma_a = sqrt(sigma_xi^2 - lambda^2 sigma_r^2)
#   observed log input = d_i + N(0, sigma_w / sqrt(days))   (LENA day-to-day noise)
#   outcome = CDI WS production sum score over the 682 items at age t
#
# Usage: SM2_DIR=/path/to/standard-model-2 Rscript models/meta_r2/model_implied_r2.R
suppressPackageStartupMessages(library(tidyverse))
sm2 <- Sys.getenv("SM2_DIR", "../standard-model-2")
here <- "models/meta_r2"
set.seed(2026)

delta_j <- read_csv(file.path(sm2, "fits/summaries/long_no_freq_slopes_psi.csv"),
                    show_col_types = FALSE)$delta_j_median
par <- list(mu_r = 7.3377, log_H = log(365), a0 = 20,   # a0 = 20 reproduces Wordbank quantiles; 18 runs ~1 logit high
            sigma_xi = 1.85, delta = 9.89, sigma_zeta = 6.23, rho = 0,   # entry 45, EN M_best
            sigma_w = 0.375)                                             # SEEDLingS LENA day noise

simulate_children <- function(n, sigma_r, lambda, beta_d = 0, p = par) {
  d <- rnorm(n, 0, sigma_r)
  sigma_a <- sqrt(p$sigma_xi^2 - lambda^2 * sigma_r^2)
  a <- rnorm(n, 0, sigma_a)
  # total slope SD stays at the fitted sigma_zeta; beta_d carves part of it out of input
  zeta <- beta_d * d / sigma_r + rnorm(n, 0, sqrt(p$sigma_zeta^2 - beta_d^2))
  list(d = d, xi = p$mu_r + lambda * d + a, zeta = zeta)
}
cdi_sum <- function(kids, age, p = par) {
  eta <- outer(kids$xi + p$log_H + (1 + p$delta + kids$zeta) * log(age / p$a0), delta_j, "-")
  rowSums(matrix(rbinom(length(eta), 1, plogis(eta)), nrow = nrow(eta)))
}

# ---- sanity check: simulated vocabulary vs Wordbank English WS ----
wb <- readRDS("models/ses_gap/wordbank_en_ws_admins.rds") %>% filter(between(age, 16, 30))
kids <- simulate_children(5000, sigma_r = 0.44, lambda = 1)
check <- map_dfr(c(18, 24, 30), \(t) {
  sim <- cdi_sum(kids, t) / length(delta_j)
  obs <- wb %>% filter(abs(age - t) <= 1) %>% pull(production) / 680
  tibble(age = t, sim_q25 = quantile(sim, .25), sim_med = median(sim), sim_q75 = quantile(sim, .75),
         wb_q25 = quantile(obs, .25), wb_med = median(obs), wb_q75 = quantile(obs, .75))
})
cat("Sanity check (proportion of items produced):\n"); print(check %>% mutate(across(-age, \(x) round(x, 2))))

# ---- model-implied observed r^2 across the design grid ----
grid <- expand_grid(sigma_r = c(0.35, 0.44, 0.58), lambda = c(0.6, 1.0, 1.2), beta_d = c(0, 0.7),
                    days = c(1, 2), age = c(18, 24, 30))
n <- 8000
res <- grid %>% group_by(sigma_r, lambda, beta_d) %>% group_modify(\(g, key) {
  kids <- simulate_children(n, key$sigma_r, key$lambda, key$beta_d)
  map_dfr(unique(g$age), \(t) {
    y <- cdi_sum(kids, t)
    map_dfr(unique(g$days), \(dd) {
      x <- kids$d + rnorm(n, 0, par$sigma_w / sqrt(dd))
      tibble(age = t, days = dd, r2_obs = cor(x, y)^2)
    })
  })
}) %>% ungroup() %>%
  mutate(latent_share = lambda^2 * sigma_r^2 / par$sigma_xi^2)

write_csv(res, file.path(here, "model_implied_r2.csv"))
cat("\nModel-implied OBSERVED r^2 (%), by input SD sigma_r, lambda, recording days, CDI age:\n")
res %>% mutate(r2 = round(100 * r2_obs, 1)) %>%
  select(sigma_r, lambda, beta_d, days, age, r2) %>%
  pivot_wider(names_from = age, values_from = r2, names_prefix = "age ") %>%
  left_join(res %>% distinct(sigma_r, lambda, latent_share), by = c("sigma_r", "lambda")) %>%
  mutate(`latent share %` = round(100 * latent_share, 1)) %>% select(-latent_share) %>%
  print(n = Inf)
