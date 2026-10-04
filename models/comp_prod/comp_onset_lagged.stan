// Comprehension ONSET as accumulator threshold crossing, with lagged exposure.
//
// Each child-word sequence contributes CDI months up to and including the first
// "understands". With F_t = inv_logit(lambda (L_t - psi_j)) the probability the
// threshold has been crossed by month t:
//   first admin:  P(event) = F_t                       (left-censored)
//   later admins: P(event) = (F_t - F_prev) / (1 - F_prev)   (hazard)
// where
//   L_t = res_t + beta_v x_kid_i + beta_w x_word_j + log alpha_i
//         + (1 + delta + zeta_i) log((t - s)/a0)
// res_t is E[log rate_ij | recordings before t] net of the child and word
// baselines; it enters with coefficient 1, so lambda is the within-child,
// within-word ruler, estimated without post-learning exposure.
data {
  int<lower=1> R;
  int<lower=1> I;
  int<lower=1> J;
  array[R] int<lower=1, upper=I> kid;
  array[R] int<lower=1, upper=J> item;
  vector[R] age;
  vector[R] age_prev;
  array[R] int<lower=0, upper=1> first;
  array[R] int<lower=0, upper=1> event;
  vector[R] res;
  vector[R] res_prev;
  vector[I] x_kid;
  vector[J] x_word;
  real<lower=0> a0;
}
parameters {
  real<lower=0> lambda;
  real beta_v;
  real beta_w;
  real delta;
  real<lower=0, upper=5> s;
  vector[I] alpha_raw;
  real<lower=0> sigma_alpha;
  vector[I] zeta_raw;
  real<lower=0> sigma_zeta;
  real mu_psi;
  real<lower=0> tau_psi;
  vector[J] psi_raw;
}
transformed parameters {
  vector[I] log_alpha = sigma_alpha * alpha_raw;
  vector[I] zeta = sigma_zeta * zeta_raw;
  vector[J] psi = mu_psi + tau_psi * psi_raw;
}
model {
  lambda ~ lognormal(0, 1);
  beta_v ~ normal(1, 1);
  beta_w ~ normal(1, 1);
  delta ~ normal(0, 5);
  s ~ normal(1, 1);
  alpha_raw ~ std_normal();
  sigma_alpha ~ normal(0, 2);
  zeta_raw ~ std_normal();
  sigma_zeta ~ normal(0, 1);
  mu_psi ~ normal(0, 10);
  tau_psi ~ normal(0, 2);
  psi_raw ~ std_normal();
  {
    vector[R] base = beta_v * x_kid[kid] + beta_w * x_word[item] + log_alpha[kid] - psi[item];
    vector[R] slope = 1 + delta + zeta[kid];
    vector[R] eta = lambda * (res + base + slope .* log((age - s) / a0));
    for (r in 1:R) {
      if (first[r]) {
        target += event[r] ? log_inv_logit(eta[r]) : log1m_inv_logit(eta[r]);
      } else {
        real eta_prev = lambda * (res_prev[r] + base[r] + slope[r] * log((age_prev[r] - s) / a0));
        // hazard of crossing between the previous and current admin; exposure
        // estimates can dip month to month, so floor the increment
        real Fc = inv_logit(eta[r]);
        real Fp = inv_logit(eta_prev);
        real h = fmax(Fc - Fp, 1e-6) / (1 - Fp);
        target += event[r] ? log(h) : log1m(fmin(h, 1 - 1e-9));
      }
    }
  }
}
generated quantities {
  real lambda_child = lambda * beta_v;
  real lambda_word = lambda * beta_w;
}
