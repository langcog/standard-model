// Cross-sectional log-linear accumulator IRT with an optional free noise scale.
//
//   eta_ij = lambda * [ log r_i + log alpha_i + (1+delta) log(a_i/a0)
//                       + use_freq * log p_j - psi_j ]
//
// lambda is the logit-per-log-token scale (fixed at 1 in SM2). Rulers that
// can identify it:
//   use_freq = 1 : log p_j enters with coefficient 1 (psi_j ~ class prior)
//   use_obs  = 1 : noisy recordings of log r_i (log r_i enters with coef 1)
// With neither, only lambda*(1+delta) and lambda*sd(xi) are identified.
data {
  int<lower=1> N;
  int<lower=1> I;
  int<lower=1> J;
  int<lower=1> C;
  array[N] int<lower=1, upper=I> kid;
  array[N] int<lower=1, upper=J> item;
  array[N] int<lower=0, upper=1> y;
  vector[I] log_age_rel;               // log(a_i / a0)
  array[J] int<lower=1, upper=C> cls;
  vector[J] log_p;                     // centered log frequency
  int<lower=0, upper=1> use_freq;
  int<lower=0, upper=1> lambda_free;
  int<lower=0, upper=1> use_obs;
  real<lower=0> sigma_r_pin;           // used when use_obs == 0
  int<lower=0> V;
  array[V] int<lower=1, upper=I> rec_kid;
  vector[V] log_r_obs;
}
parameters {
  array[lambda_free] real<lower=0> lambda_p;
  real delta;
  vector[I] r_raw;
  vector[I] a_raw;
  real<lower=0> sigma_alpha;
  array[use_obs] real<lower=0> sigma_r_p;
  array[use_obs] real<lower=0> sigma_w_p;
  array[use_obs] real mu_obs_p;
  vector[C] mu_c;
  vector<lower=0>[C] tau_c;
  vector[J] psi_raw;
}
transformed parameters {
  real lambda = lambda_free ? lambda_p[1] : 1.0;
  real sigma_r = use_obs ? sigma_r_p[1] : sigma_r_pin;
  real pi_alpha = square(sigma_alpha) / (square(sigma_alpha) + square(sigma_r));
  vector[I] log_r = sigma_r * r_raw;
  vector[I] log_alpha = sigma_alpha * a_raw;
  vector[J] psi = mu_c[cls] + tau_c[cls] .* psi_raw;
}
model {
  if (lambda_free) lambda_p[1] ~ lognormal(0, 1);
  delta ~ normal(0, 10);
  r_raw ~ std_normal();
  a_raw ~ std_normal();
  sigma_alpha ~ normal(0, 2);
  mu_c ~ normal(0, 5);
  tau_c ~ normal(0, 2);
  psi_raw ~ std_normal();
  if (use_obs) {
    sigma_r_p[1] ~ normal(0, 1);
    sigma_w_p[1] ~ normal(0, 1);
    mu_obs_p[1] ~ normal(0, 5);
    log_r_obs ~ normal(mu_obs_p[1] + log_r[rec_kid], sigma_w_p[1]);
  }
  {
    vector[N] eta = lambda * (log_r[kid] + log_alpha[kid]
                              + (1 + delta) * log_age_rel[kid]
                              + use_freq * log_p[item] - psi[item]);
    y ~ bernoulli_logit(eta);
  }
}
generated quantities {
  // what SM2 would report from these draws, on the logit scale
  real age_slope_logit = lambda * (1 + delta);
  real sigma_xi_logit = lambda * sqrt(square(sigma_r) + square(sigma_alpha));
}
