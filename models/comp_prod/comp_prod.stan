// Comprehension -> production as two stages on one accumulator (SEEDLingS sketch).
//
// Response per child i, word j, admin t (continuation-ratio ordinal):
//   y = 0 neither, 1 understands only, 2 understands + says
//   P(y >= 1)         = pi_c                       (comprehension)
//   P(y = 2 | y >= 1) = pi_p                       (production given comprehension)
//
// Stage 1 is the SM2 accumulator in log-token units, with lambda FREE because
// SEEDLingS has per-child observed input: latent log r_i (measurement model on
// ~13 LENA days) enters with coefficient 1, which sets the scale. Frequency is
// a second ruler; beta_p != 1 means the two rulers disagree:
//   L_ijt   = log r_i + log alpha_i + beta_p log p_j + log H
//             + (1 + delta_c + zeta_i) log((t - s)/a0)
//   eta_c   = lambda_c (L_ijt - psi_j)
//   pi_c    = g + (1 - g) inv_logit(eta_c)          g: parent false-alarm floor
//
// Stage 2 (logit scale) nests "production is a second, higher accumulation
// threshold" (kappa ~ lambda_c, delta_p ~ 0) against "production is
// maturational readiness gated by comprehension" (kappa ~ 0, delta_p > 0):
//   eta_p   = kappa (L_ijt - psi_j) + mu_p + rho_i + delta_p log(t/a0) - Delta_j
//   Delta_j ~ N(X_j gamma, tau_D)                   X_j: phonological covariates
//   (log alpha_i, rho_i) ~ MVN(0, diag(s) Omega diag(s))
functions {
  real cp_partial(array[] int y_slice, int start, int end,
                  array[] int ii, array[] int jj, array[] int tt,
                  vector L_kid_t, vector logp, vector psi, vector Delta,
                  vector rho, vector log_age_rel, array[] int adm_kid,
                  real lambda_c, real kappa, real mu_p,
                  real delta_p, real beta_p, real g) {
    real lp = 0;
    for (n in start:end) {
      int a = tt[n];
      int i = adm_kid[a];
      int j = jj[n];
      real Lmj = L_kid_t[a] + beta_p * logp[j] - psi[j];   // log tokens over threshold
      real pc = g + (1 - g) * inv_logit(lambda_c * Lmj);
      real ep = kappa * Lmj + mu_p + rho[i] + delta_p * log_age_rel[a] - Delta[j];
      int yn = y_slice[n - start + 1];
      if (yn == 0) lp += log1m(pc);
      else {
        lp += log(pc);
        lp += (yn == 2) ? log_inv_logit(ep) : log1m_inv_logit(ep);
      }
    }
    return lp;
  }
}
data {
  int<lower=1> N;                      // item responses
  int<lower=1> I;                      // children
  int<lower=1> A;                      // admins (child x month)
  int<lower=1> J;                      // items
  int<lower=1> C;                      // lexical classes
  int<lower=1> K;                      // phonological covariates for Delta_j
  array[N] int<lower=0, upper=2> y;
  array[N] int<lower=1, upper=I> ii;
  array[N] int<lower=1, upper=J> jj;
  array[N] int<lower=1, upper=A> tt;
  array[A] int<lower=1, upper=I> adm_kid;
  vector[A] age;                       // months
  array[J] int<lower=1, upper=C> cls;
  vector[J] logp;                      // centered log CHILDES probability
  matrix[J, K] X;                      // standardized covariates (syllables, ...)
  // observed input: LENA AWC/hr, ~13 monthly recordings per child
  int<lower=1> V;
  array[V] int<lower=1, upper=I> rec_kid;
  vector[V] log_awc;                   // centered
  vector[V] rec_age_rel;               // log(month / a0) of the recording
  real log_H;                          // log waking hours / month
  real<lower=0> a0;
  int<lower=0, upper=1> free_beta_p;   // 1: estimate the frequency elasticity; 0: pin at 1
  int<lower=1> grainsize;
}
transformed data {
  vector[A] log_age_rel = log(age / a0);
}
parameters {
  real<lower=0> lambda_c;
  real delta_c;
  real delta_p;
  real<lower=0, upper=5> s;                     // accumulation onset (months)
  real kappa;
  real mu_p;
  array[free_beta_p] real beta_p_p;
  real<lower=0, upper=0.2> g;
  // children
  vector[I] r_raw;
  real<lower=0> sigma_r;
  real awc_trend;                               // developmental change in AWC
  real<lower=0> sigma_w;                        // day-to-day recording noise
  matrix[2, I] z_kid;                           // (log alpha, rho)
  vector<lower=0>[2] sd_kid;
  cholesky_factor_corr[2] L_Omega;
  vector[I] zeta_raw;
  real<lower=0> sigma_zeta;
  // items
  vector[C] mu_psi;
  vector<lower=0>[C] tau_psi;
  vector[J] psi_raw;
  vector[K] gamma;
  real<lower=0> tau_D;
  vector[J] D_raw;
}
transformed parameters {
  real beta_p = free_beta_p ? beta_p_p[1] : 1.0;
  vector[I] log_r = sigma_r * r_raw;
  matrix[2, I] kid = diag_pre_multiply(sd_kid, L_Omega) * z_kid;
  vector[I] log_alpha = kid[1]';
  vector[I] rho = kid[2]';
  vector[I] zeta = sigma_zeta * zeta_raw;
  vector[J] psi = mu_psi[cls] + tau_psi[cls] .* psi_raw;
  vector[J] Delta = X * gamma + tau_D * D_raw;     // production gap (logit units; / kappa -> log tokens)
  vector[A] L_kid_t;                               // child-and-age part of log exposure
  for (a in 1:A) {
    int i = adm_kid[a];
    L_kid_t[a] = log_r[i] + log_alpha[i] + log_H
                 + (1 + delta_c + zeta[i]) * log(fmax(age[a] - s, 0.1) / a0);
  }
}
model {
  lambda_c ~ lognormal(0, 1);
  delta_c ~ normal(0, 5);
  delta_p ~ normal(0, 5);
  s ~ normal(1, 1);
  kappa ~ normal(0, 2);
  mu_p ~ normal(0, 5);
  if (free_beta_p) beta_p_p[1] ~ normal(1, 1);
  g ~ beta(1, 30);
  r_raw ~ std_normal();
  sigma_r ~ normal(0, 1);
  awc_trend ~ normal(0, 1);
  sigma_w ~ normal(0, 1);
  to_vector(z_kid) ~ std_normal();
  sd_kid ~ normal(0, 2);
  L_Omega ~ lkj_corr_cholesky(2);
  zeta_raw ~ std_normal();
  sigma_zeta ~ normal(0, 1);
  mu_psi ~ normal(0, 10);
  tau_psi ~ normal(0, 2);
  psi_raw ~ std_normal();
  gamma ~ normal(0, 1);
  tau_D ~ normal(0, 2);
  D_raw ~ std_normal();

  // input measurement model: repeated LENA days identify sigma_r vs sigma_w
  log_awc ~ normal(log_r[rec_kid] + awc_trend * rec_age_rel, sigma_w);

  target += reduce_sum(cp_partial, y, grainsize, ii, jj, tt, L_kid_t, logp, psi,
                       Delta, rho, log_age_rel, adm_kid, lambda_c, kappa,
                       mu_p, delta_p, beta_p, g);
}
generated quantities {
  real rho_alpha_prod = multiply_lower_tri_self_transpose(L_Omega)[1, 2];
  // share of logit-scale comprehension variance across children due to input
  real input_share_c = square(sigma_r) / (square(sigma_r) + square(sd_kid[1]));
  // pure-accumulator benchmark for stage 2: kappa / lambda_c -> 1, delta_p -> 0
  real kappa_ratio = kappa / lambda_c;
  // expected proportion understood / said per admin (posterior predictive check)
  vector[A] E_comp = rep_vector(0, A);
  vector[A] E_prod = rep_vector(0, A);
  {
    vector[A] n_a = rep_vector(0, A);
    for (n in 1:N) {
      int a = tt[n];
      int j = jj[n];
      real Lmj = L_kid_t[a] + beta_p * logp[j] - psi[j];
      real pc = g + (1 - g) * inv_logit(lambda_c * Lmj);
      real pp = inv_logit(kappa * Lmj + mu_p + rho[adm_kid[a]] + delta_p * log_age_rel[a] - Delta[j]);
      E_comp[a] += pc;
      E_prod[a] += pc * pp;
      n_a[a] += 1;
    }
    E_comp = E_comp ./ n_a;
    E_prod = E_prod ./ n_a;
  }
}
