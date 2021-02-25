functions { /* saved as LM_rng.stan in R's working directory */
  matrix LM_rng(int S, matrix X, real alpha_scale, vector beta_scale, real sigma_scale) {
    int N = rows(X); int K = cols(X); matrix[S, N] draws;
    for (s in 1:S) {
      real alpha = normal_rng(0, alpha_scale);
      real sigma = exponential_rng(1) * sigma_scale;
      vector[K] beta; vector[N] mu; // cannot fill until beta is filled
      for (k in 1:K) beta[k] = normal_rng(0, beta_scale[k]);
      mu = alpha + X * beta; // use * for both scalar and matrix multiplication
      for (n in 1:N) {
        real epsilon = normal_rng(0, sigma);
        draws[s, n] = mu[n] + epsilon;
      }
    }
    return draws;
  }
}
