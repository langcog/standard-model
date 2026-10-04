# Does fixing the logit scale (lambda = 1) manufacture SM2's headline results?
#
# Simulate CDI data from an accumulator whose true noise scale is lambda = 2.5,
# tuned so that on the logit scale it looks like SM2's English fit
# (age slope lambda*(1+delta) ~ 10.9, child SD sigma_xi ~ 1.85, sigma_r = 0.44).
# Then fit:
#   A  lambda = 1, no frequency        (SM2 "no_freq" analogue)
#   B  lambda free, no frequency       (no ruler -> ridge expected)
#   C  lambda free, frequency ruler    (log p_j with coefficient 1)
#   D  lambda free, observed-input ruler (3 noisy recordings for 200 kids)
#   E  lambda free, frequency ruler measured with error (CHILDES != child's input)
#
# Usage: Rscript run_sim.R <fit_id>   (or "sim" to only simulate)

suppressPackageStartupMessages({ library(rstan); library(tidyverse) })
rstan_options(auto_write = TRUE)
here <- "sims/lambda_identifiability"
args <- commandArgs(trailingOnly = TRUE)

# ---- truth ----
truth <- list(lambda = 2.5, delta = 10.9 / 2.5 - 1, sigma_r = 0.44,
              sigma_alpha = sqrt((1.85 / 2.5)^2 - 0.44^2),
              sd_logp = 1.5, sigma_w = 0.5, logp_noise = 1.0)
truth$pi_alpha <- truth$sigma_alpha^2 / (truth$sigma_alpha^2 + truth$sigma_r^2)

simulate_data <- function(seed = 2026, I = 600, J = 100, n_obs_kids = 200, n_rec = 3) {
  set.seed(seed)
  C <- 3
  cls <- rep(1:C, length.out = J)
  log_p <- rnorm(J, 0, truth$sd_logp)
  psi <- c(-0.3, 0.3, 0.8)[cls] + rnorm(J, 0, 0.8)    # token-unit thresholds
  age <- runif(I, 16, 30)
  log_age_rel <- log(age / 20)
  log_r <- rnorm(I, 0, truth$sigma_r)
  log_alpha <- rnorm(I, 0, truth$sigma_alpha)
  d <- expand_grid(kid = 1:I, item = 1:J)
  eta <- truth$lambda * (log_r[d$kid] + log_alpha[d$kid] +
                           (1 + truth$delta) * log_age_rel[d$kid] +
                           log_p[d$item] - psi[d$item])
  d$y <- rbinom(nrow(d), 1, plogis(eta))
  rec_kid <- rep(sample(I, n_obs_kids), each = n_rec)
  list(N = nrow(d), I = I, J = J, C = C, kid = d$kid, item = d$item, y = d$y,
       log_age_rel = log_age_rel, cls = cls, log_p = log_p,
       log_p_noisy = log_p + rnorm(J, 0, truth$logp_noise),
       sigma_r_pin = truth$sigma_r,
       V = length(rec_kid), rec_kid = rec_kid,
       log_r_obs = 7 + log_r[rec_kid] + rnorm(length(rec_kid), 0, truth$sigma_w))
}

specs <- list(
  A = list(use_freq = 0, lambda_free = 0, use_obs = 0, noisy_p = FALSE),
  B = list(use_freq = 0, lambda_free = 1, use_obs = 0, noisy_p = FALSE),
  C = list(use_freq = 1, lambda_free = 1, use_obs = 0, noisy_p = FALSE),
  D = list(use_freq = 0, lambda_free = 1, use_obs = 1, noisy_p = FALSE),
  E = list(use_freq = 1, lambda_free = 1, use_obs = 0, noisy_p = TRUE)
)

sim_path <- file.path(here, "sim_data.rds")
if (!file.exists(sim_path)) saveRDS(list(data = simulate_data(), truth = truth), sim_path)
if (length(args) == 0 || args[1] == "sim") quit(save = "no")

id <- args[1]
sp <- specs[[id]]
sim <- readRDS(sim_path)$data
sd_in <- sim
if (sp$noisy_p) sd_in$log_p <- sim$log_p_noisy
sd_in$log_p <- sd_in$log_p - mean(sd_in$log_p)
sd_in$log_p_noisy <- NULL
sd_in[c("use_freq", "lambda_free", "use_obs")] <- sp[c("use_freq", "lambda_free", "use_obs")]
if (!sp$use_obs) { sd_in$V <- 0; sd_in$rec_kid <- integer(0); sd_in$log_r_obs <- numeric(0) }

mod <- stan_model(file.path(here, "accum_irt.stan"))
fit <- sampling(mod, data = sd_in, chains = 3, cores = 3, iter = 1200, warmup = 600,
                seed = 11, refresh = 200, control = list(adapt_delta = 0.9),
                pars = c("lambda", "delta", "sigma_alpha", "sigma_r", "pi_alpha",
                         "age_slope_logit", "sigma_xi_logit", "tau_c", "mu_c"))
draws <- as.data.frame(fit) %>% select(-lp__) %>% mutate(fit = id)
summ <- summary(fit)$summary
saveRDS(list(draws = draws, summary = summ), file.path(here, paste0("fit_", id, ".rds")))
print(round(summ[c("lambda", "delta", "pi_alpha", "age_slope_logit", "sigma_xi_logit"),
                 c("mean", "2.5%", "97.5%", "n_eff", "Rhat")], 3))
