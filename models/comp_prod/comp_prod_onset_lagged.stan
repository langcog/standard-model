// Comprehension AND production onset as threshold crossings on lagged exposure.
//
// Shared log exposure for child i, noun j at CDI month t (recordings before t only):
//   L_t = res_t + beta_v x_kid_i + beta_w x_word_j + log alpha_i
//         + (1 + delta + zeta_i) log((t - s)/a0)
// res_t enters with coefficient 1, so lambda is the within-child, within-word ruler.
//
// Comprehension onset (risk set: months up to first "understands"):
//   F_t = inv_logit(lambda (L_t - psi_j))
//   first admin P = F_t; later admins hazard (F_t - F_prev) / (1 - F_prev)
//
// Production onset given comprehension (risk set: first "understands" through
// first "says"), the comp_prod.stan stage 2 as a crossing probability:
//   G_t = inv_logit(kappa (L_t - psi_j) + mu_p + rho_i + delta_p log(t/a0) - Delta_j)
//   first production-risk admin P = G_t; later admins hazard (G_t - G_prev) / (1 - G_prev)
// Pure "second threshold on the same accumulator": kappa / lambda -> 1, delta_p -> 0.
// Production as maturation gated by comprehension: kappa -> 0, delta_p > 0.
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
}
transformed parameters {
  matrix[2, I] kidfx = diag_pre_multiply(sd_kid, L_Omega) * z_kid;
  vector[I] log_alpha = kidfx[1]';
  vector[I] rho = kidfx[2]';
  vector[I] zeta = sigma_zeta * zeta_raw;
  vector[J] psi = mu_psi + tau_psi * psi_raw;
  vector[J] Delta = tau_D * D_raw;
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

  // comprehension onset: lambda * (L - psi)
  {
    vector[R] base = beta_v * x_kid[kid] + beta_w * x_word[item] + log_alpha[kid] - psi[item];
    vector[R] slope = 1 + delta + zeta[kid];
    for (r in 1:R) {
      real eta = lambda * (res[r] + base[r] + slope[r] * log((age[r] - s) / a0));
      real eta_prev = first[r] ? 0 :
        lambda * (res_prev[r] + base[r] + slope[r] * log((age_prev[r] - s) / a0));
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
                 + extra[q] + delta_p * log(page[q] / a0);
      real eta_prev = pfirst[q] ? 0 :
        kappa * (pres_prev[q] + Lpsi[q] + slope[q] * log((page_prev[q] - s) / a0))
        + extra[q] + delta_p * log(page_prev[q] / a0);
      target += crossing_lp(pevent[q], pfirst[q], eta, eta_prev);
    }
  }
}
generated quantities {
  real lambda_child = lambda * beta_v;
  real lambda_word = lambda * beta_w;
  real kappa_ratio = kappa / lambda;
  real rho_alpha_prod = multiply_lower_tri_self_transpose(L_Omega)[1, 2];
  // how often the monotonicity floor binds (diagnostic)
  int n_floor_comp = 0;
  int n_floor_prod = 0;
  {
    vector[R] base = beta_v * x_kid[kid] + beta_w * x_word[item] + log_alpha[kid] - psi[item];
    for (r in 1:R) if (!first[r]) {
      real sl = 1 + delta + zeta[kid[r]];
      if (res[r] + sl * log((age[r] - s) / a0) <= res_prev[r] + sl * log((age_prev[r] - s) / a0))
        n_floor_comp += 1;
    }
    for (q in 1:P) if (!pfirst[q]) {
      real sl = 1 + delta + zeta[pkid[q]];
      real now = kappa * (pres[q] + sl * log((page[q] - s) / a0)) + delta_p * log(page[q] / a0);
      real before = kappa * (pres_prev[q] + sl * log((page_prev[q] - s) / a0)) + delta_p * log(page_prev[q] / a0);
      if (now <= before) n_floor_prod += 1;
    }
  }
}
