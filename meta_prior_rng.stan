functions{ /* saved as meta_prior_rng.stan in R's working directory */
  matrix meta_prior_rng (int S, int N, vector sigma_study){
    matrix[S,N] delta; //study_level
    matrix[S,N] draw; //output
    for (s in 1:S) {
      real tau = fabs(cauchy_rng(0, 0.3)); //for the study_level
      real tau_sq = tau * tau;
      real mu = normal_rng(0, 1); //for the study_level
      
          for (n in 1:N){
            delta[s,n] = normal_rng(mu, tau);
            draw[s,n] = normal_rng(delta[s,n], sigma_study[n]);
          }
        }
     return draw;
  }
}

