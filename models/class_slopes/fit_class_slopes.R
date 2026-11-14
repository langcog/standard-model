# Class-specific steepness (lambda_c) and acceleration (Delta_c) on SM2's English
# longitudinal Wordbank bundle (3,105 children, 7,329 administrations, 681 words).
#
# Usage: Rscript models/class_slopes/fit_class_slopes.R <mode> [n_kids] [variant] [iter] [param]
#   mode     sim   simulate from the model on the real design and refit (recovery check)
#            fit   fit the real data
#   n_kids   children sampled at random (seed 1); 0 = all                 [600]
#   variant  both | lambda | delta | rasch  (which class terms are free)  [both]
#            diff  nouns only, "classes" = difficulty quartiles (placebo: does item
#                  difficulty alone reproduce the class pattern?)
#   iter     iterations per chain, half warmup                            [1000]
#   param    r | c | nc                                                    [r]
#            r   class_slopes.stan: the model written to sample well (see its header)
#            c   class_slopes_simple.stan, child and word effects centered
#            nc  class_slopes_simple.stan, non-centered (fits kept with the suffix _nc)
#            The three give the same class terms; the level parameters mix only under r.
# The bundle is read from Mike Frank's standard-model-2 repo (not copied here).
suppressPackageStartupMessages({ library(rstan); library(tidyverse) })
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[1] else "fit"
n_kids <- if (length(args) >= 2) as.integer(args[2]) else 600L
variant <- if (length(args) >= 3) args[3] else "both"
iter <- if (length(args) >= 4) as.integer(args[4]) else 1000L
param <- if (length(args) >= 5) args[5] else "r"
here <- "models/class_slopes"
bundle <- Sys.getenv("SM2_BUNDLE", "../standard-model-2/fits/long_subset_data.rds")
a0 <- 18
rstan_options(auto_write = TRUE); options(mc.cores = 4)

b <- readRDS(bundle)
d <- b$df %>% select(ii, aa, jj, age, y = produces)
words <- b$word_info %>% mutate(class = b$class_levels[cc])
set.seed(1)
kids <- if (n_kids > 0) sort(sample(max(d$ii), n_kids)) else sort(unique(d$ii))
d <- filter(d, ii %in% kids)

if (variant == "diff") {              # placebo: nouns split by proportion produced
  nn <- filter(words, class == "nouns")$jj
  d <- filter(d, jj %in% nn)
  q <- d %>% group_by(jj) %>% summarise(p = mean(y)) %>% mutate(cls = ntile(-p, 4))   # 1 = easiest
  words <- words %>% inner_join(q, by = "jj") %>% mutate(class = paste0("nouns_q", cls), cc = cls)
  class_levels <- paste0("nouns_q", 1:4); ref <- 1L
} else {
  class_levels <- b$class_levels; ref <- match("nouns", class_levels)
}
d <- d %>% mutate(i = match(ii, sort(unique(ii))), a = match(aa, sort(unique(aa))), j = match(jj, sort(unique(jj))))
adm <- d %>% distinct(a, i, age) %>% arrange(a)
wj <- words %>% filter(jj %in% d$jj) %>% mutate(j = match(jj, sort(unique(d$jj)))) %>% arrange(j)
sd <- list(N = nrow(d), A = nrow(adm), I = max(d$i), J = nrow(wj), C = length(class_levels), ref = ref,
           aa = d$a, jj = d$j, y = d$y, admin_to_child = adm$i, log_age_rel = log(adm$age / a0), cc = wj$cc,
           free_lambda = as.integer(variant %in% c("both", "lambda", "diff")),
           free_delta = as.integer(variant %in% c("both", "delta", "diff")))
if (param == "r") {      # centering constants: they shift word difficulties only
  pc <- d %>% mutate(c = wj$cc[j]) %>% group_by(c) %>% summarise(p = mean(y)) %>% arrange(c)
  sd$Lbar <- as.numeric(tapply(sd$log_age_rel, sd$admin_to_child, mean))
  sd$L0 <- log(24 / a0)
  sd$t_c <- 1.5 * (qlogis(pc$p[ref]) - qlogis(pc$p))
} else sd$centered <- as.integer(param == "c")
cat(sprintf("%s | %s | children %d | administrations %d | words %d | responses %d\n",
            mode, variant, sd$I, sd$A, sd$J, sd$N))

truth <- NULL
if (mode == "sim") {                  # simulate on the real design, with class terms that differ
  set.seed(2)
  truth <- list(kappa = 8, lambda = c(1.4, 1, 0.8, 1.25), Delta = c(-1, 0, -2, 1.5),
                sigma = c(1.8, 2.5), rho = 0.5)
  S <- diag(truth$sigma) %*% matrix(c(1, truth$rho, truth$rho, 1), 2) %*% diag(truth$sigma)
  u <- MASS::mvrnorm(sd$I, c(0, 0), S)
  mu_c <- c(4, 1, 1.5, 3); bj <- rnorm(sd$J, mu_c[sd$cc], 1.5)
  cj <- sd$cc[sd$jj]; ia <- sd$admin_to_child[sd$aa]; L <- sd$log_age_rel[sd$aa]
  eta <- truth$lambda[cj] * (u[ia, 1] + (truth$kappa + u[ia, 2] + truth$Delta[cj]) * L) - bj[sd$jj]
  sd$y <- rbinom(sd$N, 1, plogis(eta))
  cat(sprintf("simulated: %.1f%% produced\n", 100 * mean(sd$y)))
}

m <- stan_model(file.path(here, if (param == "r") "class_slopes.stan" else "class_slopes_simple.stan"))
init <- if (param == "r") function() list(mu_xi = -3, kappa = 10, m = rnorm(sd$I, 0, .1), k = rnorm(sd$I, 10, .5),
    sigma = c(2, 5), mu_free = rep(0, sd$C - 1), tau_c = rep(1.5, sd$C),
    b_ref_free = rnorm(sum(sd$cc == ref) - 1, 0, .1), b_oth = rnorm(sum(sd$cc != ref), 0, .1)) else
  function() list(kappa = 8, sigma = c(1.5, 2), mu_c = rep(2, sd$C), tau_c = rep(1.5, sd$C),
    b_raw = rnorm(sd$J, if (sd$centered) 3 else 0, .1), z = matrix(rnorm(2 * sd$I, 0, .1), 2))
t0 <- Sys.time()
fit <- sampling(m, data = sd, chains = 4, iter = iter, warmup = iter %/% 2, init = init, seed = 11,
                control = list(adapt_delta = 0.9, max_treedepth = 10), refresh = max(1, iter %/% 20),
                pars = if (param == "r") c("m", "k", "b_ref_free", "b_oth", "mu_free") else c("z", "b_raw"), include = FALSE)
cat(sprintf("elapsed: %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

tag <- sprintf("%s_%s_%s%s", mode, variant, if (n_kids > 0) paste0("n", n_kids) else "all", if (param == "r") "" else paste0("_", param))
pars <- c("kappa", "lambda", "Delta", "age_slope", "accel_ratio", "sigma", "rho", if (param == "r") c("mu_xi", "thresh"), "mu_c", "tau_c")
s <- summary(fit, pars = pars, probs = c(.025, .5, .975))$summary %>% as.data.frame() %>% rownames_to_column("par")
cls_par <- grepl("^(lambda|Delta|age_slope|accel_ratio|thresh|mu_c|tau_c)\\[", s$par)
for (k in seq_along(class_levels))
  s$par[cls_par] <- sub(sprintf("[%d]", k), sprintf("[%s]", class_levels[k]), s$par[cls_par], fixed = TRUE)
s$par <- recode(s$par, `sigma[1]` = "sigma[xi]", `sigma[2]` = "sigma[zeta]")
if (!is.null(truth)) {
  tv <- c(kappa = truth$kappa, setNames(truth$lambda, sprintf("lambda[%s]", class_levels)),
          setNames(truth$Delta, sprintf("Delta[%s]", class_levels)),
          `sigma[xi]` = truth$sigma[1], `sigma[zeta]` = truth$sigma[2], rho = truth$rho)
  s$truth <- tv[s$par]
}
print(s %>% mutate(across(where(is.numeric), \(x) round(x, 3))) %>% select(-se_mean, -sd), row.names = FALSE)
sp <- get_sampler_params(fit, inc_warmup = FALSE)
cat(sprintf("divergences %d | max treedepth hits %d | max rhat %.3f | min n_eff %.0f\n",
            sum(sapply(sp, \(x) sum(x[, "divergent__"]))), sum(sapply(sp, \(x) sum(x[, "treedepth__"] >= 10))),
            max(s$Rhat, na.rm = TRUE), min(s$n_eff, na.rm = TRUE)))
write_csv(s, file.path(here, sprintf("summary_%s.csv", tag)))
saveRDS(fit, file.path(here, sprintf("fit_%s.rds", tag)))
