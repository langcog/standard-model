// Age-varying rulers: comp_prod_onset_lagged.stan with each ruler's logit
// coefficient allowed to change linearly in log age.
//
// Comprehension onset, child i, noun j, CDI month t, l_t = log(t / a0):
//   eta_c = lambda (res_t + beta_v x_kid_i + beta_w x_word_j + log alpha_i - psi_j
//                   + (1 + delta + zeta_i) log((t - s)/a0))
//           + l_t (gamma_r res_t + gamma_v x_kid_i + gamma_w x_word_j + omega_j)
// so the logit change per log-token of each ruler at age t is
//   within child x word : lambda          + gamma_r l_t
//   between children    : lambda beta_v   + gamma_v l_t
//   between words       : lambda beta_w   + gamma_w l_t
// and the base model's values are the ones at a0 (12 months). gamma_v is the
// SEEDLingS analogue of beta_d in eq. 4 of the SM2 input paper (input -> growth
// rate): zeta_i is the child's residual age slope, so gamma_v is the regression
// of that slope on the child's noun input. omega_j ~ N(0, sigma_omega) is a
// word-specific age slope, the item-side counterpart of zeta_i, so gamma_w is not
// credited with every word-by-age interaction.
//
// Production onset given comprehension is the base model's stage 2 plus one term,
// pi_r l_t pres_t: the within-ruler coefficient is kappa + pi_r l_t. (Production
// onsets are sparse before 13 months, so only the within ruler is freed there.)
//
// gamma_r = gamma_v = gamma_w = pi_r = sigma_omega = 0 recovers comp_prod_onset_lagged.stan.
functions {
  // log P(event) for one risk-set row of a monotone crossing process
  real crossing_lp(int event, int first, real eta, real eta_prev) {
    if (first) return event ? log_inv_logit(eta) : log1m_inv_logit(eta);
    real Fc = inv_logit(eta);
    real Fp = inv_logit(eta_prev);
    // exposure estimates can dip month to month: floor the increment
    real h = fmax(Fc - Fp, 1e-6) / (1 - Fp);
    return event ? log(h) : log1m(fmin(h, 1 - 1e-9));
  }
}
data {
  int<lower=1> I;
  int<lower=1> J;
  vector[I] x_kid;
  vector[J] x_word;
  real<lower=0> a0;
  // comprehension risk set
  int<lower=1> R;
  array[R] int<lower=1, upper=I> kid;
  array[R] int<lower=1, upper=J> item;
  vector[R] age;
  vector[R] age_prev;
  array[R] int<lower=0, upper=1> first;
  array[R] int<lower=0, upper=1> event;
  vector[R] res;
  vector[R] res_prev;
  // production risk set
  int<lower=1> P;
  array[P] int<lower=1, upper=I> pkid;
  array[P] int<lower=1, upper=J> pitem;
  vector[P] page;
  vector[P] page_prev;
  array[P] int<lower=0, upper=1> pfirst;
  array[P] int<lower=0, upper=1> pevent;
  vector[P] pres;
  vector[P] pres_prev;
  int<lower=1> T_out;                       // ages (months) at which to report the rulers
  vector<lower=0>[T_out] age_out;
}
transformed data {
  vector[R] l_age = log(age / a0);
  vector[R] l_age_prev;
  vector[P] l_page = log(page / a0);
  vector[P] l_page_prev;
  for (r in 1:R) l_age_prev[r] = first[r] ? 0 : log(age_prev[r] / a0);
  for (q in 1:P) l_page_prev[q] = pfirst[q] ? 0 : log(page_prev[q] / a0);
}
parameters {
  real<lower=0> lambda;
  real beta_v;
  real beta_w;
  real delta;
  real<lower=0, upper=5> s;
  real kappa;
  real mu_p;
  real delta_p;
  matrix[2, I] z_kid;                       // (log alpha, rho)
  vector<lower=0>[2] sd_kid;
  cholesky_factor_corr[2] L_Omega;
  vector[I] zeta_raw;
  real<lower=0> sigma_zeta;
  real mu_psi;
  real<lower=0> tau_psi;
  vector[J] psi_raw;
  real<lower=0> tau_D;
  vector[J] D_raw;
  // age-varying ruler coefficients (logit per log-token, per unit of log age)
  real gamma_r;
  real gamma_v;
  real gamma_w;
  real pi_r;
  vector[J] omega_raw;
  real<lower=0> sigma_omega;
}
transformed parameters {
  matrix[2, I] kidfx = diag_pre_multiply(sd_kid, L_Omega) * z_kid;
  vector[I] log_alpha = kidfx[1]';
  vector[I] rho = kidfx[2]';
  vector[I] zeta = sigma_zeta * zeta_raw;
  vector[J] psi = mu_psi + tau_psi * psi_raw;
  vector[J] Delta = tau_D * D_raw;
  vector[J] omega = sigma_omega * omega_raw;
}
model {
  lambda ~ lognormal(0, 1);
  beta_v ~ normal(1, 1);
  beta_w ~ normal(1, 1);
  delta ~ normal(0, 5);
  s ~ normal(1, 1);
  kappa ~ normal(0, 2);
  mu_p ~ normal(0, 5);
  delta_p ~ normal(0, 5);
  to_vector(z_kid) ~ std_normal();
  sd_kid ~ normal(0, 2);
  L_Omega ~ lkj_corr_cholesky(2);
  zeta_raw ~ std_normal();
  sigma_zeta ~ normal(0, 1);
  mu_psi ~ normal(0, 10);
  tau_psi ~ normal(0, 2);
  psi_raw ~ std_normal();
  tau_D ~ normal(0, 2);
  D_raw ~ std_normal();
  gamma_r ~ normal(0, 2);
  gamma_v ~ normal(0, 2);
  gamma_w ~ normal(0, 2);
  pi_r ~ normal(0, 2);
  omega_raw ~ std_normal();
  sigma_omega ~ normal(0, 1);

  // comprehension onset: lambda * (L - psi)
  {
    vector[R] base = beta_v * x_kid[kid] + beta_w * x_word[item] + log_alpha[kid] - psi[item];
    vector[R] slope = 1 + delta + zeta[kid];
    vector[R] tv = gamma_v * x_kid[kid] + gamma_w * x_word[item] + omega[item];
    for (r in 1:R) {
      real eta = lambda * (res[r] + base[r] + slope[r] * log((age[r] - s) / a0))
                 + l_age[r] * (gamma_r * res[r] + tv[r]);
      real eta_prev = first[r] ? 0 :
        lambda * (res_prev[r] + base[r] + slope[r] * log((age_prev[r] - s) / a0))
        + l_age_prev[r] * (gamma_r * res_prev[r] + tv[r]);
      target += crossing_lp(event[r], first[r], eta, eta_prev);
    }
  }
  // production onset given comprehension
  {
    vector[P] Lpsi = beta_v * x_kid[pkid] + beta_w * x_word[pitem] + log_alpha[pkid] - psi[pitem];
    vector[P] slope = 1 + delta + zeta[pkid];
    vector[P] extra = mu_p + rho[pkid] - Delta[pitem];
    for (q in 1:P) {
      real eta = kappa * (pres[q] + Lpsi[q] + slope[q] * log((page[q] - s) / a0))
                 + extra[q] + (delta_p + pi_r * pres[q]) * l_page[q];
      real eta_prev = pfirst[q] ? 0 :
        kappa * (pres_prev[q] + Lpsi[q] + slope[q] * log((page_prev[q] - s) / a0))
        + extra[q] + (delta_p + pi_r * pres_prev[q]) * l_page_prev[q];
      target += crossing_lp(pevent[q], pfirst[q], eta, eta_prev);
    }
  }
}
generated quantities {
  real lambda_child = lambda * beta_v;      // at a0
  real lambda_word = lambda * beta_w;       // at a0
  real kappa_ratio = kappa / lambda;        // at a0
  real rho_alpha_prod = multiply_lower_tri_self_transpose(L_Omega)[1, 2];
  // each ruler's coefficient at the reporting ages
  vector[T_out] lambda_within_t = lambda + gamma_r * log(age_out / a0);
  vector[T_out] lambda_child_t = lambda * beta_v + gamma_v * log(age_out / a0);
  vector[T_out] lambda_word_t = lambda * beta_w + gamma_w * log(age_out / a0);
  vector[T_out] kappa_within_t = kappa + pi_r * log(age_out / a0);
  // how often the monotonicity floor binds (diagnostic)
  int n_floor_comp = 0;
  int n_floor_prod = 0;
  {
    vector[R] base = beta_v * x_kid[kid] + beta_w * x_word[item] + log_alpha[kid] - psi[item];
    vector[R] tv = gamma_v * x_kid[kid] + gamma_w * x_word[item] + omega[item];
    for (r in 1:R) if (!first[r]) {
      real sl = 1 + delta + zeta[kid[r]];
      real now = lambda * (res[r] + sl * log((age[r] - s) / a0)) + l_age[r] * (gamma_r * res[r] + tv[r]);
      real before = lambda * (res_prev[r] + sl * log((age_prev[r] - s) / a0))
                    + l_age_prev[r] * (gamma_r * res_prev[r] + tv[r]);
      if (now <= before) n_floor_comp += 1;
    }
    for (q in 1:P) if (!pfirst[q]) {
      real sl = 1 + delta + zeta[pkid[q]];
      real now = kappa * (pres[q] + sl * log((page[q] - s) / a0)) + (delta_p + pi_r * pres[q]) * l_page[q];
      real before = kappa * (pres_prev[q] + sl * log((page_prev[q] - s) / a0))
                    + (delta_p + pi_r * pres_prev[q]) * l_page_prev[q];
      if (now <= before) n_floor_prod += 1;
    }
  }
}
