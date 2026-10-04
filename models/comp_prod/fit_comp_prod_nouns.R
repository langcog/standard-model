# Fit comp_prod_nouns.stan to the bundle built by prep_seedlings_nouns.R.
# Usage: Rscript models/comp_prod/fit_comp_prod_nouns.R [iter] [warmup]
suppressPackageStartupMessages({ library(rstan); library(tidyverse) })
rstan_options(auto_write = TRUE)
here <- "models/comp_prod"
args <- commandArgs(trailingOnly = TRUE)
iter <- if (length(args) >= 1) as.integer(args[1]) else 1000
warmup <- if (length(args) >= 2) as.integer(args[2]) else 500

b <- readRDS(file.path(here, "seedlings_nouns_standata.rds"))
mod <- stan_model(file.path(here, "comp_prod_nouns.stan"))
inits <- function() list(lambda_c = 1, kappa = 0.5, delta_c = 2, delta_p = 0, s = 1, g = 0.02,
                         beta_v = 1, beta_w = 1, beta_c = 0, c0 = -3, trend = 0, phi = 1,
                         sigma_v = 0.3, sigma_w = 1.5, sigma_u = 0.5, sd_kid = c(0.5, 0.5),
                         mu_psi = 0, tau_psi = 1, tau_D = 1)
fit <- sampling(mod, data = b$standata, chains = 4, cores = 4, iter = iter, warmup = warmup,
                seed = 7, init = inits, refresh = 100, control = list(adapt_delta = 0.9))
saveRDS(fit, file.path(here, "fit_comp_prod_nouns.rds"))

scalars <- c("lambda_within", "lambda_child", "lambda_word", "beta_v", "beta_w", "beta_c",
             "delta_c", "s", "g", "kappa", "kappa_ratio", "delta_p", "mu_p",
             "sigma_v", "sigma_w", "sigma_u", "phi", "trend", "sd_kid", "rho_alpha_prod",
             "input_share_c", "sigma_zeta", "mu_psi", "tau_psi", "tau_D")
summ <- summary(fit, pars = scalars)$summary
print(round(summ[, c("mean", "2.5%", "50%", "97.5%", "n_eff", "Rhat")], 3))
write.csv(summ, file.path(here, "summary_comp_prod_nouns.csv"))
check_hmc_diagnostics(fit)
