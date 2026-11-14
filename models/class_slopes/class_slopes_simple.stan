// Class-specific steepness and acceleration in SM2's longitudinal growth model.
//
// SM2's Rasch growth model with a per-child slope is
//   logit P(child i produces word j at age t) = xi_i + kappa_i log(t / a0) - b_j .
// Here each lexical class c gets its own loading on the child's growth curve
// (lambda_c) and its own extra age slope (Delta_c):
//   logit P = lambda_c [ xi_i + (kappa + zeta_i + Delta_c) log(t / a0) ] - b_j .
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
  int<lower=0, upper=1> centered;              // 1: child and word effects on their own scale
}
transformed data {
  array[N] int idx;                            // position of (admin, class) in phi
  array[C - 1] int other;                      // the non-reference classes
  for (n in 1:N) idx[n] = (aa[n] - 1) * C + cc[jj[n]];
  {
    int k = 1;
    for (c in 1:C) if (c != ref) { other[k] = c; k += 1; }
  }
}
parameters {
  real kappa;                                  // age slope, reference class
  vector[free_lambda ? C - 1 : 0] log_lambda_free;
  vector[free_delta ? C - 1 : 0] Delta_free;
  matrix[2, I] z;                              // child intercept and slope (standardised if !centered)
  vector<lower=0>[2] sigma;                    // sd of xi and zeta
  cholesky_factor_corr[2] L_rho;
  vector[C] mu_c;                              // class mean difficulty
  vector<lower=0>[C] tau_c;                    // class sd of difficulty
  vector[J] b_raw;                             // word difficulty (standardised if !centered)
}
transformed parameters {
  vector[C] lambda = rep_vector(1, C);
  vector[C] Delta = rep_vector(0, C);
  if (free_lambda) lambda[other] = exp(log_lambda_free);
  if (free_delta) Delta[other] = Delta_free;
}
model {
  matrix[2, 2] L_sig = diag_pre_multiply(sigma, L_rho);
  matrix[I, 2] u = centered ? z' : (L_sig * z)';
  vector[A] xi_a = u[admin_to_child, 1];
  vector[A] slope_a = kappa + u[admin_to_child, 2];
  vector[J] b = centered ? b_raw : mu_c[cc] + tau_c[cc] .* b_raw;
  vector[A * C] phi;
  for (c in 1:C) {
    vector[A] th = lambda[c] * (xi_a + (slope_a + Delta[c]) .* log_age_rel);
    for (a in 1:A) phi[(a - 1) * C + c] = th[a];
  }
  kappa ~ normal(8, 4);
  log_lambda_free ~ normal(0, 0.5);
  Delta_free ~ normal(0, 3);
  if (centered) {
    array[I] vector[2] zi;
    for (i in 1:I) zi[i] = z[, i];
    zi ~ multi_normal_cholesky(rep_vector(0, 2), L_sig);
    b_raw ~ normal(mu_c[cc], tau_c[cc]);
  } else {
    to_vector(z) ~ std_normal();
    b_raw ~ std_normal();
  }
  sigma ~ normal(0, 3);
  L_rho ~ lkj_corr_cholesky(2);
  mu_c ~ normal(0, 5);
  tau_c ~ normal(0, 3);
  y ~ bernoulli_logit(phi[idx] - b[jj]);
}
generated quantities {
  real rho = multiply_lower_tri_self_transpose(L_rho)[1, 2];   // cor(xi, zeta)
  vector[C] age_slope = lambda .* (kappa + Delta);             // logit per log-unit of age, by class
  vector[C] accel_ratio = 1 + Delta / kappa;                   // (1 + delta_c) / (1 + delta_ref)
}
