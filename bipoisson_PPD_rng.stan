functions{/*save as bipoisson_PPD_rng_.stan*/
	int[] bipoisson_rng(real lambda_1, real lambda_2, real lambda_3){
		int y_1=poisson_rng(lambda_1);
		int y_2=poisson_rng(lambda_2);
		int y_3=poisson_rng(lambda_3);
		int x_1=y_1 + y_3;
		int x_2=y_2 + y_3; 
		return {x_1, x_2};
  }

  matrix bipoisson_PPD_rng (int S, real rate_1, real rate_2, real rate_3){
		matrix[S, 2] out; //holds S prior predictive draws on x_1 and x_2
		
		for (s in 1:S){
		  real a1 = exponential_rng(rate_1);
		  real a2 = exponential_rng(rate_2);
		  real a3 = exponential_rng(rate_3);
		  out[s, ] = to_row_vector (bipoisson_rng(a1, a2, a3));
		  
		}
	  return out;
  }
}
