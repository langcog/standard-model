# Lagged exposure with a burstiness-aware filter, replacing the Poisson filter
# in prep_lagged.R. For child i, noun j:
#   x_ij = r_ij / (mu_j c_i) ~ Gamma(k, k)                persistent rate (relative)
#   N_ijm ~ NegBin(mean = r_ij h_im, size = phi)           month counts, bursty
# k and phi are fit by marginal likelihood (r integrated on a grid); then for each
# CDI month t, E[log r_ij | months < t] (lagged) or | all months (full) is computed
# exactly on the grid. Risk sets and Stan data are built as in prep_lagged*.R.
#
# Usage: Rscript models/comp_prod/prep_lagged_nb.R
suppressPackageStartupMessages(library(tidyverse))
here <- "models/comp_prod"
b <- readRDS(file.path(here, "seedlings_nouns_standata.rds"))
lagged <- readRDS(file.path(here, "seedlings_lagged_standata.rds"))
sd <- b$standata

cnt <- tibble(i = sd$cnt_kid, j = sd$cnt_item, n = sd$cnt_n, h = exp(sd$log_hours),
              m = round(sd$a0 * exp(sd$cnt_age_rel)))
tot <- cnt %>% group_by(i, j) %>% summarise(N = sum(n), H = sum(h), .groups = "drop")
mu_j <- tot %>% group_by(j) %>% summarise(mu = sum(N) / sum(H))
c_i <- tot %>% left_join(mu_j, by = "j") %>% group_by(i) %>% summarise(c = sum(N) / sum(mu * H))
cnt <- cnt %>% left_join(mu_j, by = "j") %>% left_join(c_i, by = "i") %>%
  mutate(m0 = mu * c, pair = paste(i, j)) %>% arrange(pair, m)

# grid over z = log x
z <- seq(-7, 4, length.out = 111)
months <- sort(unique(cnt$m))
pairs <- unique(cnt$pair)
P <- length(pairs); G <- length(z); M <- length(months)
# month log-likelihood array: pair x month x grid (0 where no recording)
idx_p <- match(cnt$pair, pairs); idx_m <- match(cnt$m, months)
month_ll <- function(phi) {
  ll <- array(0, c(P, M, G))
  for (g in seq_len(G)) {
    ll[cbind(idx_p, idx_m, g)] <- dnbinom(cnt$n, size = phi, mu = cnt$m0 * exp(z[g]) * cnt$h, log = TRUE)
  }
  ll
}
log_prior <- function(k) { lp <- k * z - k * exp(z); lp - max(lp) - log(sum(exp(lp - max(lp)))) }
lse <- function(x) { mx <- apply(x, 1, max); mx + log(rowSums(exp(x - mx))) }

negll <- function(par) {
  k <- exp(par[1]); phi <- exp(par[2])
  tot_ll <- apply(month_ll(phi), c(1, 3), sum)              # pair x grid
  -sum(lse(sweep(tot_ll, 2, log_prior(k), "+")))
}
opt <- optim(c(log(1.25), log(0.3)), negll, method = "Nelder-Mead", control = list(reltol = 1e-6))
k <- exp(opt$par[1]); phi <- exp(opt$par[2])
cat(sprintf("persistent shape k = %.2f (CV %.2f; Poisson filter had 1.25) | month burstiness phi = %.3f\n",
            k, 1 / sqrt(k), phi))

# cumulative log-likelihood through each month, pair x month x grid
ll <- month_ll(phi)
cum <- ll
for (mm in 2:M) cum[, mm, ] <- cum[, mm - 1, ] + ll[, mm, ]
lp <- log_prior(k)
post_mean_z <- function(L) {                                # L: rows x grid log-lik
  w <- sweep(L, 2, lp, "+"); w <- exp(w - apply(w, 1, max)); as.vector((w %*% z) / rowSums(w))
}

rows <- lagged$all_rows %>% mutate(pair = paste(i, j), p = match(pair, pairs))
stopifnot(!anyNA(rows$p))
last_before <- findInterval(rows$age - 1e-9, months)        # index of last month < age (0 = none)
L_lag <- matrix(0, nrow(rows), G)
has <- last_before > 0
L_lag[has, ] <- t(sapply(which(has), \(r) cum[rows$p[r], last_before[r], ]))
L_full <- cum[rows$p, M, ]
rows <- rows %>% mutate(res_lag = post_mean_z(L_lag), res_full = post_mean_z(L_full))
cat(sprintf("sd within-ruler: lagged %.2f (Poisson filter 0.74) | full %.2f (0.86) | cor %.2f\n",
            sd(rows$res_lag), sd(rows$res_full), cor(rows$res_lag, rows$res_full)))

# risk sets, exactly as prep_lagged.R / prep_lagged_cp.R
rs <- rows %>% arrange(i, j, age) %>% group_by(i, j) %>%
  mutate(event = as.integer(y >= 1), seen = cumsum(event),
         keep = seen == 0 | (seen == 1 & event == 1)) %>% filter(keep) %>%
  mutate(first = as.integer(row_number() == 1), age_prev = lag(age, default = 0),
         res_lag_prev = lag(res_lag, default = 0), res_full_prev = lag(res_full, default = 0)) %>%
  ungroup()
ps <- rows %>% arrange(i, j, age) %>% group_by(i, j) %>%
  mutate(comp_on = cumsum(y >= 1) > 0, said = cumsum(y == 2)) %>%
  filter(comp_on, said == 0 | (said == 1 & y == 2)) %>%
  mutate(first = as.integer(row_number() == 1), event = as.integer(y == 2),
         age_prev = lag(age, default = 0),
         res_lag_prev = lag(res_lag, default = 0), res_full_prev = lag(res_full, default = 0)) %>%
  ungroup()

make_sd <- function(v) list(
  R = nrow(rs), I = sd$I, J = sd$J, kid = rs$i, item = rs$j, age = rs$age, age_prev = rs$age_prev,
  first = rs$first, event = rs$event,
  res = rs[[paste0("res_", v)]], res_prev = rs[[paste0("res_", v, "_prev")]],
  x_kid = lagged$lag$x_kid, x_word = lagged$lag$x_word, a0 = sd$a0,
  P = nrow(ps), pkid = ps$i, pitem = ps$j, page = ps$age, page_prev = ps$age_prev,
  pfirst = ps$first, pevent = ps$event,
  pres = ps[[paste0("res_", v)]], pres_prev = ps[[paste0("res_", v, "_prev")]])
saveRDS(list(lag = make_sd("lag"), full = make_sd("full"), k = k, phi = phi, rows = rows),
        file.path(here, "seedlings_lagged_nb_cp_standata.rds"))

# model-free preview, as before
f <- function(d, v) { m <- glm(as.formula(paste("event ~", v, "+ log(age) + factor(j) + factor(i)")),
                               family = binomial, data = filter(d, first == 0))
  s <- summary(m)$coefficients[v, ]; sprintf("%s %.3f (se %.3f)", v, s[1], s[2]) }
cat("comprehension hazard:", f(rs, "res_lag"), "|", f(rs, "res_full"), "\n")
cat("production hazard:   ", f(ps, "res_lag"), "|", f(ps, "res_full"), "\n")
