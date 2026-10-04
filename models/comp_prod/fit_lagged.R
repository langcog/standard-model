# Fit comp_onset_lagged.stan with lagged vs full-data exposure.
# Usage: Rscript models/comp_prod/fit_lagged.R lag|full
suppressPackageStartupMessages({ library(rstan); library(tidyverse) })
rstan_options(auto_write = TRUE)
here <- "models/comp_prod"
variant <- commandArgs(trailingOnly = TRUE)[1]
b <- readRDS(file.path(here, "seedlings_lagged_standata.rds"))
mod <- stan_model(file.path(here, "comp_onset_lagged.stan"))
fit <- sampling(mod, data = b[[variant]], chains = 4, cores = 4, iter = 1000, warmup = 500,
                seed = 3, refresh = 100, control = list(adapt_delta = 0.9),
                init = function() list(lambda = 1, beta_v = 1, beta_w = 1, delta = 2, s = 1,
                                       sigma_alpha = 0.5, sigma_zeta = 0.3, mu_psi = 0, tau_psi = 1))
saveRDS(fit, file.path(here, paste0("fit_lagged_", variant, ".rds")))
pars <- c("lambda", "lambda_child", "lambda_word", "beta_v", "beta_w", "delta", "s",
          "sigma_alpha", "sigma_zeta", "mu_psi", "tau_psi")
summ <- summary(fit, pars = pars)$summary
print(round(summ[, c("mean", "2.5%", "50%", "97.5%", "n_eff", "Rhat")], 3))
write.csv(summ, file.path(here, paste0("summary_lagged_", variant, ".csv")))
check_hmc_diagnostics(fit)
