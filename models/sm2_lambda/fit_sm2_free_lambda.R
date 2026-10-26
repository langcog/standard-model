# Fit the input paper's Fig. 4 joint io+proc model (standard-model-2, English + count bundle,
# rung D'2) with the coefficient on the input deviation d_i free (sm2_io_proc_free_lambda.stan).
# Priors, inits and the bundle follow model/scripts/fit_joint_io_proc_lean.R; the only addition
# is lambda_d ~ N(1, 2). mode "fixed" pins lambda_d at 1 (prior SD 0.001) = the published model.
#
# Bundle: in a standard-model-2 clone, run
#   Rscript model/scripts/prepare_proc_dp_bundle.R
#   Rscript model/scripts/prepare_joint_io_proc_bundle_mm.R
#   Rscript model/scripts/prepare_english_count_bundle.R
# Usage: SM2_DIR=/path/to/standard-model-2 Rscript models/sm2_lambda/fit_sm2_free_lambda.R [free|fixed]
suppressPackageStartupMessages({ library(rstan) })
rstan_options(auto_write = TRUE)
here <- "models/sm2_lambda"
mode <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode)) mode <- "free"
stopifnot(mode %in% c("free", "fixed"))
sm2 <- Sys.getenv("SM2_DIR", "../standard-model-2")
sd <- readRDS(file.path(sm2, "fits/joint_io_proc_english_count_subset_data.rds"))$stan_data
sd$form_start <- as.array(sd$form_start); sd$form_len <- as.array(sd$form_len)   # rstan: length-1 arrays

# as fit_joint_io_proc_lean.R, rung 2, tau 1
sd$gamma_in_prior_sd <- 1 / sd$sigma_r_prior_mean
sd$beta_xi_prior_sd  <- 1 / sd$sigma_rt0_prior_mean
sd$beta_k0_prior_sd  <- 1 / sd$sigma_rt0_prior_mean
sd$delta_prior_sd    <- 10
sd$lambda_d_prior_mean <- 1
sd$lambda_d_prior_sd   <- if (mode == "free") 2 else 0.001

chains  <- as.integer(Sys.getenv("CHAINS", "4"))
threads <- as.integer(Sys.getenv("THREADS", "3"))
iter    <- as.integer(Sys.getenv("ITER", "2000"))
sd$grainsize <- max(1L, as.integer(floor(sd$N / (2 * threads))))
rstan_options(threads_per_chain = threads)
cat(sprintf("mode=%s I=%d N=%d V_obs=%d n_sum=%d; %d chains x %d threads, iter %d\n",
            mode, sd$I, sd$N, sd$V_obs, sd$n_sum, chains, threads, iter))

mod <- stan_model(file.path(here, "sm2_io_proc_free_lambda.stan"))
init <- function() list(tau_s = rep(6.84, sd$S), psi = -0.35, delta = 7,
                        sigma_alpha = 1, sigma_zeta = 1, sigma_rt0 = 0.143, sigma_lwl = 0.24,
                        mu_c = rep(8, sd$C), tau_c = rep(1, sd$C),
                        gamma_in = 0, beta_xi = 0, beta_k0 = 0, lambda_d = 1,
                        sigma_r = 0.44, mu_r_s = sd$mu_r_s_prior_mean, sigma_meas = rep(0.3, sd$n_instr))
pars <- c("lambda_d", "eff_input_xi", "gamma_in", "eff_input_k", "beta_xi", "eff_proc_xi", "beta_k0",
          "eff_proc_k", "delta", "sigma_r", "sigma_alpha", "sigma_zeta", "sigma_rt0", "sigma_xi",
          "share_input_xi", "share_proc_xi", "share_resid_xi", "sigma_meas")
fit <- sampling(mod, data = sd, chains = chains, cores = chains, iter = iter, warmup = iter / 2,
                seed = 1, refresh = 50, init = init, pars = pars,
                control = list(adapt_delta = 0.95, max_treedepth = 10))
saveRDS(fit, file.path(here, sprintf("fit_sm2_%s_lambda.rds", mode)))
summ <- summary(fit, pars = pars)$summary
print(round(summ[, c("mean", "2.5%", "50%", "97.5%", "n_eff", "Rhat")], 3))
write.csv(summ, file.path(here, sprintf("summary_sm2_%s_lambda.csv", mode)))
check_hmc_diagnostics(fit)
