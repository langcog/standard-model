// Comprehension -> production for CDI nouns, with each child's OWN exposure to
// each noun (SEEDLingS hand-annotated tokens) as the ruler for lambda.
//
// Count model (top-3 talkative audio hours per month, child speech excluded):
//   n_ijm ~ NegBin(exp(c0 + v_i + w_j + u_ij + trend log(m/a0)) * hours_im, phi)
//     v_i  : child's overall noun-input rate           (between-child)
//     w_j  : word's rate across children                (between-word, SEEDLingS "frequency")
//     u_ij : this child's persistent deviation for this word (within-child, within-word)
// NegBin month-to-month overdispersion keeps burstiness out of u_ij.
//
// Stage 1 (comprehension), log-token units:
//   L_ijt = u_ij + beta_v v_i + beta_w w_j + beta_c logp_childes_j + log alpha_i
//           + (1 + delta_c + zeta_i) log((t - s)/a0)
//   P(understands) = g + (1 - g) inv_logit(lambda_c (L_ijt - psi_j))
// u_ij has coefficient 1: lambda_c is identified from within-child, within-word
// covariation between exposure and knowledge, free of child and word confounds.
// beta_v, beta_w, beta_c are the other rulers' elasticities relative to it
// (pure accumulator: all = 1; beta_c ~ 0 once w_j is in, since w_j is the
// better frequency measure for these children).
//
// Stage 2 (production | comprehension), logit scale, as in comp_prod.stan:
//   eta_p = kappa (L_ijt - psi_j) + mu_p + rho_i + delta_p log(t/a0) - Delta_j
functions {
  real cp_partial(array[] int y_slice, int start, int end,
                  array[] int jj, array[] int tt, array[] int adm_kid,
                  vector L_kid_t, matrix L_kid_word, vector item_part, vector psi,
                  vector Delta, vector rho, vector log_age_rel,
                  real lambda_c, real kappa, real mu_p, real delta_p, real g) {
    real lp = 0;
    for (n in start:end) {
      int a = tt[n];
      int i = adm_kid[a];
      int j = jj[n];
      real Lmj = L_kid_t[a] + L_kid_word[i, j] + item_part[j] - psi[j];
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
  int<lower=1> N;
  int<lower=1> I;
  int<lower=1> A;
  int<lower=1> J;
  array[N] int<lower=0, upper=2> y;
  array[N] int<lower=1, upper=I> ii;
  array[N] int<lower=1, upper=J> jj;
  array[N] int<lower=1, upper=A> tt;
  array[A] int<lower=1, upper=I> adm_kid;
  vector[A] age;
  vector[J] logp_childes;
  // monthly noun counts
  int<lower=1> M;
  array[M] int<lower=1, upper=I> cnt_kid;
  array[M] int<lower=1, upper=J> cnt_item;
  array[M] int<lower=0> cnt_n;
  vector[M] log_hours;
  vector[M] cnt_age_rel;
  real log_H;
  real<lower=0> a0;
  int<lower=1> grainsize;
}
transformed data {
  vector[A] log_age_rel = log(age / a0);
}
parameters {
  // count model
  real c0;
  real trend;
  real<lower=0> phi;
  vector[I] v_raw;
  real<lower=0> sigma_v;
  vector[J] w_raw;
  real<lower=0> sigma_w;
  matrix[I, J] u_raw;
  real<lower=0> sigma_u;
  // learning
  real<lower=0> lambda_c;
  real beta_v;
  real beta_w;
  real beta_c;
  real delta_c;
  real<lower=0, upper=5> s;
  real<lower=0, upper=0.2> g;
  real kappa;
  real delta_p;
  real mu_p;
  matrix[2, I] z_kid;
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
  vector[I] v = sigma_v * v_raw;
  vector[J] w = sigma_w * w_raw;
  matrix[I, J] u = sigma_u * u_raw;
  matrix[2, I] kid = diag_pre_multiply(sd_kid, L_Omega) * z_kid;
  vector[I] log_alpha = kid[1]';
  vector[I] rho = kid[2]';
  vector[I] zeta = sigma_zeta * zeta_raw;
  vector[J] psi = mu_psi + tau_psi * psi_raw;
  vector[J] Delta = tau_D * D_raw;
  vector[J] item_part = beta_w * w + beta_c * logp_childes;
  vector[A] L_kid_t;
  for (a in 1:A) {
    int i = adm_kid[a];
    L_kid_t[a] = beta_v * v[i] + log_alpha[i]
                 + (1 + delta_c + zeta[i]) * log(fmax(age[a] - s, 0.1) / a0);
  }
}
model {
  c0 ~ normal(-3, 3);
  trend ~ normal(0, 1);
  phi ~ gamma(2, 0.5);
  v_raw ~ std_normal();
  sigma_v ~ normal(0, 1);
  w_raw ~ std_normal();
  sigma_w ~ normal(0, 3);
  to_vector(u_raw) ~ std_normal();
  sigma_u ~ normal(0, 1);

  lambda_c ~ lognormal(0, 1);
  beta_v ~ normal(1, 1);
  beta_w ~ normal(1, 1);
  beta_c ~ normal(0, 1);
  delta_c ~ normal(0, 5);
  s ~ normal(1, 1);
  g ~ beta(1, 30);
  kappa ~ normal(0, 2);
  delta_p ~ normal(0, 5);
  mu_p ~ normal(0, 5);
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

  {
    vector[M] mu_cnt;
    for (m in 1:M)
      mu_cnt[m] = c0 + v[cnt_kid[m]] + w[cnt_item[m]] + u[cnt_kid[m], cnt_item[m]]
                  + trend * cnt_age_rel[m] + log_hours[m];
    cnt_n ~ neg_binomial_2_log(mu_cnt, phi);
  }
  target += reduce_sum(cp_partial, y, grainsize, jj, tt, adm_kid, L_kid_t, u, item_part,
                       psi, Delta, rho, log_age_rel, lambda_c, kappa, mu_p, delta_p, g);
}
generated quantities {
  // lambda implied by each ruler (logit per log-token)
  real lambda_within = lambda_c;            // within-child, within-word exposure
  real lambda_child = lambda_c * beta_v;    // between-child noun input
  real lambda_word = lambda_c * beta_w;     // between-word frequency (SEEDLingS)
  real kappa_ratio = kappa / lambda_c;
  real rho_alpha_prod = multiply_lower_tri_self_transpose(L_Omega)[1, 2];
  // share of comprehension variance across kids due to noun input (log-token units)
  real input_share_c = square(beta_v * sigma_v) / (square(beta_v * sigma_v) + square(sd_kid[1]));
}
