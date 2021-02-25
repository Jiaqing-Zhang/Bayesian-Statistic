functions { /* saved as priormulti_rng.stan in R's working directory */
  vector binormal_rng( real mu_X, real mu_Y, real sigma_X, real sigma_Y, real rho) {
    vector[2] draws; 
    real beta = rho * sigma_Y / sigma_X;          
    real sigma = sigma_Y * sqrt(1 - square(rho)); 
      real x = normal_rng(mu_X, sigma_X);
      real y = normal_rng(mu_Y + beta * (x - mu_X), sigma);
      draws[1] = x; draws[2] = y;
    return draws;
  }
  
  matrix priormulti_rng (int S) {
    int N = 3000;  matrix[S, N] draws;
     
     for(s in 1:S) {
        matrix[3, N] white_inp;
        matrix[3, N] nonwhite_inp;
         
         matrix[3000, 4] sigma;
         int m=1;
         while (m<=3000){
             real sigma_white_1 = cauchy_rng(0, 5) ;
             real sigma_nonwhite_1 = cauchy_rng(0, 5);
             real sigma_white_2 = cauchy_rng(0, 5);
             real sigma_nonwhite_2 = cauchy_rng(0, 5);
            if (sigma_white_1 > 0 &&  sigma_nonwhite_1 >0 &&  sigma_white_2 > 0 && sigma_nonwhite_2 >0) {
          sigma[m, 1] =sigma_white_1;
          sigma[m, 2] =sigma_white_2;
          sigma[m, 3] =sigma_nonwhite_1;
          sigma[m, 4] =sigma_nonwhite_2;
            m += 1;
        }
     }


        for (n in 1:N){
        real mu_white_1 = normal_rng(-14, 4);
        real mu_nonwhite_1 = normal_rng(-14, 4);
        real rho_1 = uniform_rng(-1, 1);
        real mu_white_2 = normal_rng(-14, 4);
        real mu_nonwhite_2 = normal_rng(-14, 4);
        real rho_2 = uniform_rng(-1, 1);
          white_inp[1, n]=binormal_rng(mu_white_1, mu_nonwhite_1, sigma[n,1], sigma[n,3], rho_1)[1]; /*intercept for white unarmed*/
          white_inp[2, n]=binormal_rng(mu_white_2, mu_nonwhite_2, sigma[n,2], sigma[n,4], rho_2)[1]; /*intercept for white armed*/
          white_inp[3, n]=0;
          nonwhite_inp[1, n]=binormal_rng(mu_white_1, mu_nonwhite_1, sigma[n,1], sigma[n,3], rho_1)[2]; /*intercept for nonwhite unarmed*/
          nonwhite_inp[2, n]=binormal_rng(mu_white_2, mu_nonwhite_2,sigma[n,2], sigma[n,4], rho_2)[2]; /*intercept for nonwhite armed*/
          nonwhite_inp[3, n]=0;
          
          draws[s, n] = softmax(nonwhite_inp[,n])[1] / softmax(white_inp[, n])[1];
        }
       
        }
 return draws;
     }
}
