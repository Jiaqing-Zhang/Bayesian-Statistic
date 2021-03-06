functions { /* named poisson_kernel.stan */
  real poisson_kernel(real alpha, real beta,
                      real a_loc, 
                      real a_scale,
                      real b_loc,
                      real b_scale,
                      int[] y, vector x) {
    int N = rows(x); vector[N] x_ = x - mean(x);
    
    real p = normal_lpdf(alpha | a_loc, a_scale);
    real q = normal_lpdf(beta | b_loc, b_scale);  
    vector[N] eta = alpha + beta * x_;
    

      
    return p + q + // priors & log-likelihood
           poisson_log_lpmf(y | eta);
  }
}
