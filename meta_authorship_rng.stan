functions{ /* saved as meta_authorship_rng.stan in R's working directory */
  matrix meta_authorship_rng (int S, int N, vector sigma_study, vector author_sd /*this is the sd for the authorship level. The authorship sd should be size N and the same author or authors will get the same sd*/){
    matrix[S,N] delta; //study_level
    matrix[S,N] draw; //output
    vector[N] author_mu; //author_level
    for (s in 1:S) {
      real tau = fabs(cauchy_rng(0, 0.3)); //for the study_level
      real tau_sq = tau * tau;
      real mu = normal_rng(0, 1); //for the study_level
      
          for (n in 1:N){
            author_mu[n] = normal_rng(0, author_sd[n]); //get the deviates for the authorship level. the deviates for the same author should be the same
            delta[s,n] = normal_rng(mu+author_mu[n], tau);
            draw[s,n] = normal_rng(delta[s,n], sigma_study[n]);
          }
        }
     return draw;
  }
}
