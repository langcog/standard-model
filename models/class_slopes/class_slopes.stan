// Class-specific steepness and acceleration in SM2's longitudinal growth model.
//
// SM2's Rasch growth model with a per-child slope is
//   logit P(child i produces word j at age t) = xi_i + kappa_i log(t / a0) - b_j .
// Here each lexical class c gets its own loading on the child's growth curve
// (lambda_c) and its own extra age slope (Delta_c):
//   logit P = lambda_c [ xi_i + (kappa_i + Delta_c) log(t / a0) ] - b_j ,
//   (xi_i, kappa_i) ~ bivariate normal with means (mu_xi, kappa).
// The reference class has lambda = 1 and Delta = 0, so
//   lambda_c           = steepness of class c relative to the reference class
//   kappa + Delta_c    = age slope of class c in reference-class units
//   1 + Delta_c/kappa  = (1 + delta_c) / (1 + delta_ref), the ratio of accelerations
// With an absolute lambda for the reference class from a ruler (lambda_ref):
//   lambda_abs_c = lambda_ref lambda_c,   delta_c = (kappa + Delta_c) / lambda_ref - 1 .
// The class loading is applied to the whole child curve, so it scales the age
// slope and the spread across children alike; Delta_c moves the age slope only.
//
// free_lambda = 0 pins every lambda_c at 1; free_delta = 0 pins every Delta_c at 0.
// Both off is SM2's Rasch model with per-child slopes (no frequency term).
//
// This file is the same model as class_slopes_simple.stan, written so that it
// samples well (each child and word has hundreds of responses):
//   - the child's level is m_i, the level at the child's own mean log-age, so that
//     level and slope are not traded off within a child; xi_i = m_i - kappa_i Lbar_i
//   - child and word effects are on their own scale (centered)
//   - the reference class's difficulties sum to zero, which fixes the origin of the
//     ability scale (mu_xi is then free); in the simple file mean(xi) is 0 instead
//   - the class terms are centered: lambda_c multiplies (theta - t_c) and Delta_c
//     multiplies (log age - L0), with t_c and L0 constants passed as data. They only
//     shift the word difficulties b_j.
data {
  int<lower=1> N;                              // responses
  int<lower=1> A;                              // administrations
  int<lower=1> I;                              // children
  int<lower=1> J;                              // words
  int<lower=2> C;                              // lexical classes
  int<lower=1, upper=C> ref;                   // reference class
  array[N] int<lower=1, upper=A> aa;
  array[N] int<lower=1, upper=J> jj;
  array[N] int<lower=0, upper=1> y;
  array[A] int<lower=1, upper=I> admin_to_child;
  vector[A] log_age_rel;                       // log(age / a0)
  array[J] int<lower=1, upper=C> cc;
  int<lower=0, upper=1> free_lambda;
  int<lower=0, upper=1> free_delta;
  vector[I] Lbar;                              // child's mean log(age / a0)
  real L0;                                     // centering constant for the class age term
  vector[C] t_c;                               // centering constants for the class loading
}
transformed data {
  array[N] int idx;                            // position of (admin, class) in phi
  array[C - 1] int other;                      // the non-reference classes
  int J_ref = 0;
  vector[A] dL = log_age_rel - Lbar[admin_to_child];
  for (n in 1:N) idx[n] = (aa[n] - 1) * C + cc[jj[n]];
  {
    int k = 1;
    for (c in 1:C) if (c != ref) { other[k] = c; k += 1; }
  }
  for (j in 1:J) J_ref += (cc[j] == ref);
  array[J_ref] int ref_items;
  array[J - J_ref] int oth_items;
  {
    int r = 1;
    int o = 1;
    for (j in 1:J) {
      if (cc[j] == ref) { ref_items[r] = j; r += 1; } else { oth_items[o] = j; o += 1; }
    }
  }
}
parameters {
  real mu_xi;                                  // mean level at a0, relative to the mean reference-class word
  real kappa;                                  // mean age slope, reference class
  vector[free_lambda ? C - 1 : 0] log_lambda_free;
  vector[free_delta ? C - 1 : 0] Delta_free;
  vector[I] m;                                 // child level at the child's mean log-age
  vector[I] k;                                 // child age slope
  vector<lower=0>[2] sigma;                    // sd of xi and of the child slope
  cholesky_factor_corr[2] L_rho;
  vector[C - 1] mu_free;                       // class mean difficulty (centered scale)
  vector<lower=0>[C] tau_c;                    // class sd of difficulty
  vector[J_ref - 1] b_ref_free;
  vector[J - J_ref] b_oth;
}
transformed parameters {
  vector[C] lambda = rep_vector(1, C);
  vector[C] Delta = rep_vector(0, C);
  vector[C] mu_c = rep_vector(0, C);
  if (free_lambda) lambda[other] = exp(log_lambda_free);
  if (free_delta) Delta[other] = Delta_free;
  mu_c[other] = mu_free;
}
model {
  vector[J] b;
  vector[A] th = m[admin_to_child] + k[admin_to_child] .* dL;
  vector[A * C] phi;
  b[ref_items] = append_row(b_ref_free, -sum(b_ref_free));
  b[oth_items] = b_oth;
  for (c in 1:C) {
    vector[A] ph = lambda[c] * (th - t_c[c] + Delta[c] * (log_age_rel - L0));
    for (a in 1:A) phi[(a - 1) * C + c] = ph[a];
  }
  {
    array[I] vector[2] v;
    for (i in 1:I) v[i] = [m[i] - k[i] * Lbar[i], k[i]]';
    v ~ multi_normal_cholesky([mu_xi, kappa]', diag_pre_multiply(sigma, L_rho));
  }
  mu_xi ~ normal(0, 5);
  kappa ~ normal(8, 4);
  log_lambda_free ~ normal(0, 0.5);
  Delta_free ~ normal(0, 3);
  sigma ~ normal(0, 3);
  L_rho ~ lkj_corr_cholesky(2);
  mu_free ~ normal(0, 5);
  tau_c ~ normal(0, 3);
  b ~ normal(mu_c[cc], tau_c[cc]);
  y ~ bernoulli_logit(phi[idx] - b[jj]);
}
generated quantities {
  real rho = multiply_lower_tri_self_transpose(L_rho)[1, 2];   // cor(xi, child slope)
  vector[C] age_slope = lambda .* (kappa + Delta);             // logit per log-unit of age, by class
  vector[C] accel_ratio = 1 + Delta / kappa;                   // (1 + delta_c) / (1 + delta_ref)
  // ability at which the mean word of class c is at 50%, at the age where log(age/a0) = L0,
  // relative to the mean reference-class word (reference-class logits)
  vector[C] thresh = t_c + mu_c ./ lambda;
}
