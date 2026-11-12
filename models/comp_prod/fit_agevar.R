# Age-varying rulers in SEEDLingS: each ruler's logit coefficient is allowed to
# change linearly in log age (centred at a0 = 12 months).
#   onset: comp_prod_onset_agevar.stan on the NB-filtered lagged exposure bundle
#          (prep_lagged_nb.R) -- within child x word, between-child, between-word
#   lena : comp_prod_agevar.stan on the all-rows bundle (prep_seedlings.R) --
#          between-child LENA adult words. The child's log input rate is estimated
#          from the recordings alone and passed as data (see the Stan file header).
# Usage: Rscript models/comp_prod/fit_agevar.R onset|lena
suppressPackageStartupMessages({ library(rstan); library(tidyverse) })
rstan_options(auto_write = TRUE)
here <- "models/comp_prod"
which_model <- commandArgs(trailingOnly = TRUE)[1]
stopifnot(which_model %in% c("onset", "lena"))
age_out <- c(8, 10, 12, 14, 16, 18)
q <- c("mean", "2.5%", "50%", "97.5%", "n_eff", "Rhat")

if (which_model == "onset") {
  sd <- readRDS(file.path(here, "seedlings_lagged_nb_cp_standata.rds"))$lag
  sd$T_out <- length(age_out); sd$age_out <- age_out
  mod <- stan_model(file.path(here, "comp_prod_onset_agevar.stan"))
  fit <- sampling(mod, data = sd, chains = 4, cores = 4, iter = 1000, warmup = 500,
                  seed = 5, refresh = 100, control = list(adapt_delta = 0.9),
                  init = function() list(lambda = 1, beta_v = 1, beta_w = 1, delta = 2, s = 1,
                                         kappa = 0.5, mu_p = -3, delta_p = 0,
                                         sd_kid = c(0.5, 0.5), sigma_zeta = 0.3,
                                         mu_psi = 0, tau_psi = 1, tau_D = 1,
                                         gamma_r = 0, gamma_v = 0, gamma_w = 0, pi_r = 0,
                                         sigma_omega = 0.3))
  pars <- c("gamma_r", "gamma_v", "gamma_w", "pi_r", "sigma_omega",
            "lambda_within_t", "lambda_child_t", "lambda_word_t", "kappa_within_t",
            "lambda", "lambda_child", "lambda_word", "beta_v", "beta_w", "delta", "s",
            "kappa", "kappa_ratio", "mu_p", "delta_p", "sd_kid", "rho_alpha_prod",
            "sigma_zeta", "mu_psi", "tau_psi", "tau_D", "n_floor_comp", "n_floor_prod")
} else {
  b <- readRDS(file.path(here, "seedlings_standata.rds"))
  sd <- b$standata
  sd$T_out <- length(age_out); sd$age_out <- age_out
  # LENA-only measurement model: same structure as the one inside comp_prod.stan
  mm <- lme4::lmer(log_awc ~ rec_age_rel + (1 | rec_kid),
                   data = data.frame(log_awc = sd$log_awc, rec_age_rel = sd$rec_age_rel,
                                     rec_kid = sd$rec_kid))
  re <- lme4::ranef(mm)$rec_kid
  x <- numeric(sd$I); x[as.integer(rownames(re))] <- re[, 1]
  vc <- as.data.frame(lme4::VarCorr(mm))
  cat(sprintf("LENA-only: sigma_r %.3f | sigma_w %.3f | sd of shrunken x_i %.3f | kids with recordings %d of %d\n",
              vc$sdcor[1], vc$sdcor[2], sd(x), nrow(re), sd$I))
  sd$x_lena <- x
  sd$comp_only <- 1L   # see the Stan file header: the two-stage variant does not converge
  sd[c("V", "rec_kid", "log_awc", "rec_age_rel")] <- NULL
  mod <- stan_model(file.path(here, "comp_prod_agevar.stan"))
  fit <- sampling(mod, data = sd, chains = 4, cores = 4, iter = 1000, warmup = 500,
                  seed = 7, refresh = 100, control = list(adapt_delta = 0.9),
                  init = function() list(lambda_c = 1, kappa = 0.5, delta_c = 2, delta_p = 0,
                                         s = 1, g = 0.02, sd_kid = c(0.5, 0.5),
                                         mu_psi = rep(0, sd$C), tau_psi = rep(1, sd$C),
                                         tau_D = 1, gamma_r = 0, sigma_u = 0.3))
  pars <- c("gamma_r", "lambda_lena_t", "lambda_c", "delta_c", "beta_p", "s", "g",
            "sd_kid[1]", "sigma_zeta")
}
saveRDS(fit, file.path(here, paste0("fit_agevar_", which_model, ".rds")))
summ <- summary(fit, pars = pars)$summary
rn <- rownames(summ)
for (p in c("lambda_within_t", "lambda_child_t", "lambda_word_t", "kappa_within_t", "lambda_lena_t"))
  rn <- str_replace(rn, paste0("^", p, "\\[(\\d+)\\]$"),
                    function(m) paste0(p, "[", age_out[as.integer(str_extract(m, "\\d+"))], "mo]"))
rownames(summ) <- rn
print(round(summ[, q], 3))
write.csv(summ, file.path(here, paste0("summary_agevar_", which_model, ".csv")))
draws <- as.data.frame(fit, pars = intersect(c("gamma_r", "gamma_v", "gamma_w", "pi_r"), fit@model_pars))
cat("\nP(gamma > 0):\n"); print(round(colMeans(draws > 0), 3))
check_hmc_diagnostics(fit)
