# Fit comp_prod_onset_sources.stan (source-weighted lagged onset model).
# Usage: Rscript models/comp_prod/fit_lagged_sources.R [tag]   (tag e.g. rep120 -> *_rep120_* files)
suppressPackageStartupMessages({ library(rstan); library(tidyverse) })
rstan_options(auto_write = TRUE)
here <- "models/comp_prod"
tag <- commandArgs(trailingOnly = TRUE)[1]
sfx <- if (is.na(tag)) "" else paste0("_", tag)
b <- readRDS(file.path(here, paste0("seedlings_lagged_sources", sfx, "_standata.rds")))
mod <- stan_model(file.path(here, "comp_prod_onset_sources.stan"))
pars <- c("w", "log_w", "lambda", "lambda_child", "lambda_word", "beta_v", "beta_w", "delta", "s",
          "kappa", "kappa_ratio", "mu_p", "delta_p", "sd_kid", "rho_alpha_prod",
          "sigma_zeta", "mu_psi", "tau_psi", "tau_D")
fit <- sampling(mod, data = b$lag, chains = 4, cores = 4, iter = 1000, warmup = 500,
                seed = 41, refresh = 100, control = list(adapt_delta = 0.9), pars = pars,
                init = function() list(lambda = 0.6, beta_v = 1, beta_w = 1, delta = 10, s = 1,
                                       kappa = 0.4, mu_p = -3, delta_p = 0, log_w = rep(0, b$lag$K),
                                       sd_kid = c(0.5, 0.5), sigma_zeta = 0.3,
                                       mu_psi = 0, tau_psi = 1, tau_D = 1))
saveRDS(fit, file.path(here, paste0("fit_lagged_sources", sfx, ".rds")))
summ <- summary(fit, pars = pars)$summary
rownames(summ)[1:(2 * b$lag$K)] <- c(paste0("w_", sub("s_", "", b$sources)), paste0("log_w_", sub("s_", "", b$sources)))
print(round(summ[, c("mean", "2.5%", "50%", "97.5%", "n_eff", "Rhat")], 3))
write.csv(summ, file.path(here, paste0("summary_lagged_sources", sfx, ".csv")))
check_hmc_diagnostics(fit)
