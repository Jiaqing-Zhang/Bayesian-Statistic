functions { /* saved as bipoisson_kernel.stan in R's working directory */
 real lfactorial (int x) {
   if (x < 0) reject ("x must be a non-negative integer");
   return lgamma (x + 1);
 }
 
 
 real bipoisson_kernel(real lambda_1, real lambda_2, real lambda_3, int [,] x, real rate_1, real rate_2, real rate_3) {// x is two-dimensional
  int N = dims(x)[1]; //number of observations on x_1 and x_2
  real p_1 = exponential_lpdf (lambda_1 | rate_1 );
  real p_2 = exponential_lpdf (lambda_2 | rate_2 );
  real p_3 = exponential_lpdf (lambda_3 | rate_3 );
  
  real part1 = -lambda_1-lambda_2-lambda_3; 

  vector [N] part2;
  vector [N] part3;
  vector [N] part4;
  vector [N] part5;

  for (n in 1:N){
    int k1 = x[n, 1];
    int k2 = x[n, 2];
    int min_k = min(k1, k2);
    vector [min_k+1] out;
    
    part2[n]  = k1 * log(lambda_1) - log (lfactorial(k1));
    part3[n]  = k2 * log(lambda_2) - log (lfactorial(k2));

     for (k in 1:min_k) {
        out[k] = (lfactorial(k1) / (lfactorial(k) * lfactorial(k1-k))) * (lfactorial(k2) / (lfactorial(k) * lfactorial(k2-k))) * lfactorial(k) * exp(k * log(lambda_3/(lambda_1 * lambda_2)));
        out[min_k+1] =  (lfactorial(k1) / (lfactorial(0) * lfactorial(k1))) * (lfactorial(k2) / (lfactorial(0) * lfactorial(k2))) * lfactorial(0) * exp(0 * log(lambda_3/(lambda_1 * lambda_2)));
      }

      part4[n]=log_sum_exp(out);
      
      part5[n] = part1 + part2 [n] + part3 [n] + part4 [n];
         
  }
    return p_1 + p_2 + p_3 + sum (part4); //prior and log likelihood
  }
}




