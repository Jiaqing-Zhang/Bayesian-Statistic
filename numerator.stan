functions{/* saved as numberator.stan in R's working directory */
  vector numeraror ( vector z, vector chi){
  int k= rows(z);
  vector[k] conditional=normal_ratio_pdf(z, mu_X= -3.7, mu_Y= -1.6, sigma_X =0.74 , sigma_Y =0.8);
  vector[k] likelihood=likelihood(chi, e_j=c(0.25), c_j=c(1.50)) * likelihood(chi, e_j=c(0.35), c_j=c(1.50)) * likelihood(chi, e_j=c(0.20), c_j=c(2.00)) * (likelihood(chi, e_j=c(0.75), c_j=c(4.00)) * likelihood(chi, e_j=c(0.65), c_j=c(4.50)) *likelihood(chi, e_j=c(0.60), c_j=c(4.50));
  
  return (conditional * likelihood);

  }
  }