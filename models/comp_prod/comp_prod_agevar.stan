// Age-varying LENA ruler: comp_prod.stan with the comprehension coefficient on
// the child's log input rate allowed to change linearly in log age.
//
//   eta_c = lambda_c (L_ijt - psi_j) + log(t/a0) (gamma_r x_i + u_i)
// so the logit change per log-unit of LENA adult words at age t is
//   lambda_c + gamma_r log(t/a0),
// and lambda_c is its value at a0 (12 months). gamma_r is the SEEDLingS analogue
// of beta_d in eq. 4 of the SM2 input paper (input -> growth rate).
//
// Two changes from comp_prod.stan make gamma_r interpretable:
//  * x_i is DATA: the child's log input rate estimated from the LENA recordings
//    alone (random-intercept model, fit_agevar.R). With log r_i latent, as in
//    comp_prod.stan, the CDI rows outvote the ~13 recordings per child and turn
//    log r_i into a free comprehension age slope (gamma_r ~ +-11 to 19, recording
//    noise up from 0.37 to ~0.5); fixing x_i cuts that feedback.
//  * comp_only = 1 (the reported fit) drops stage 2, so the likelihood is
//    comprehension alone and zeta_i is the child's comprehension age slope;
//    gamma_r is then the regression of that slope on input. With stage 2 kept
//    (comp_only = 0), comprehension needs its own child age slope u_i ~ N(0, sigma_u)
//    for gamma_r to mean the same thing, and the production stage then loses
//    identification (kappa trades off against delta_p; Rhat > 4), so that
//    variant is not used.
// gamma_r = 0 with latent log r_i and comp_only = 0, sigma_u = 0 recovers comp_prod.stan.
functions {
  real cp_partial(array[] int y_slice, int start, int end,
                  array[] int ii, array[] int jj, array[] int tt,
                  vector L_kid_t, vector tv_c, vector logp, vector psi, vector Delta,
                  vector rho, vector log_age_rel, array[] int adm_kid,
                  real lambda_c, real kappa, real mu_p,
                  real delta_p, real beta_p, real g, int comp_only) {
    real lp = 0;
    for (n in start:end) {
      int a = tt[n];
      int i = adm_kid[a];
      int j = jj[n];
      real Lmj = L_kid_t[a] + beta_p * logp[j] - psi[j];   // log tokens over threshold
      real pc = g + (1 - g) * inv_logit(lambda_c * Lmj + tv_c[a]);
      real ep = kappa * Lmj + mu_p + rho[i] + delta_p * log_age_rel[a] - Delta[j];
      int yn = y_slice[n - start + 1];
      if (yn == 0) lp += log1m(pc);
      else {
        lp += log(pc);
        if (!comp_only) lp += (yn == 2) ? log_inv_logit(ep) : log1m_inv_logit(ep);
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
  vector[I] x_lena;                    // child's log input rate from LENA alone (centered)
  real log_H;                          // log waking hours / month
  real<lower=0> a0;
  int<lower=0, upper=1> free_beta_p;   // 1: estimate the frequency elasticity; 0: pin at 1
  int<lower=1> grainsize;
  int<lower=0, upper=1> comp_only;     // 1: comprehension likelihood only, no u_i
  int<lower=1> T_out;                  // ages (months) at which to report the ruler
  vector<lower=0>[T_out] age_out;
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
  real gamma_r;                                 // change in the LENA coefficient per unit log age
  vector[I] u_raw;                              // comprehension-specific child age slope
  real<lower=0> sigma_u;
}
transformed parameters {
  real beta_p = free_beta_p ? beta_p_p[1] : 1.0;
  vector[I] log_r = x_lena;
  vector[I] u = sigma_u * u_raw;
  matrix[2, I] kid = diag_pre_multiply(sd_kid, L_Omega) * z_kid;
  vector[I] log_alpha = kid[1]';
  vector[I] rho = kid[2]';
  vector[I] zeta = sigma_zeta * zeta_raw;
  vector[J] psi = mu_psi[cls] + tau_psi[cls] .* psi_raw;
  vector[J] Delta = X * gamma + tau_D * D_raw;     // production gap (logit units; / kappa -> log tokens)
  vector[A] L_kid_t;                               // child-and-age part of log exposure
  vector[A] tv_c = log_age_rel .* (gamma_r * log_r[adm_kid] + (comp_only ? rep_vector(0, A) : u[adm_kid]));
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
  gamma_r ~ normal(0, 2);
  u_raw ~ std_normal();
  sigma_u ~ normal(0, 2);

  target += reduce_sum(cp_partial, y, grainsize, ii, jj, tt, L_kid_t, tv_c, logp, psi,
                       Delta, rho, log_age_rel, adm_kid, lambda_c, kappa,
                       mu_p, delta_p, beta_p, g, comp_only);
}
generated quantities {
  real rho_alpha_prod = multiply_lower_tri_self_transpose(L_Omega)[1, 2];
  // pure-accumulator benchmark for stage 2: kappa / lambda_c -> 1, delta_p -> 0
  real kappa_ratio = kappa / lambda_c;
  vector[T_out] lambda_lena_t = lambda_c + gamma_r * log(age_out / a0);
  // expected proportion understood / said per admin (posterior predictive check)
  vector[A] E_comp = rep_vector(0, A);
  vector[A] E_prod = rep_vector(0, A);
  {
    vector[A] n_a = rep_vector(0, A);
    for (n in 1:N) {
      int a = tt[n];
      int j = jj[n];
      real Lmj = L_kid_t[a] + beta_p * logp[j] - psi[j];
      real pc = g + (1 - g) * inv_logit(lambda_c * Lmj + tv_c[a]);
      real pp = inv_logit(kappa * Lmj + mu_p + rho[adm_kid[a]] + delta_p * log_age_rel[a] - Delta[j]);
      E_comp[a] += pc;
      E_prod[a] += pc * pp;
      n_a[a] += 1;
    }
    E_comp = E_comp ./ n_a;
    E_prod = E_prod ./ n_a;
  }
}
