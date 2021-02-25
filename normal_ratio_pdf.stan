functions { /* saved as normal_ratio_pdf.stan in R's working directory */
  vector normal_ratio_pdf (vector z, real mu_X, real mu_Y, real sigma_X, real sigma_Y){
  int k=rows(z);
  
  vector[k] a=sqrt((1/square(sigma_X))*(z .* z)+(1/square(sigma_Y)));
  vector[k] b=((mu_X/square(sigma_X))*z)+(mu_Y/square(sigma_Y));
  real c=(square(mu_X)/square(sigma_X))+(square(mu_Y)/square(sigma_Y));
  vector[k] d=exp(((b .* b)-(c * (a .* a)))./(2*(a .* a)));
  
  vector[k] cdf=Phi(b ./ a)-Phi(-b ./ a);
  vector[k] part1=((b .* d)./(a .* a .* a))*(1/(sqrt(2 * pi()) * sigma_X * sigma_Y));
  vector[k] part2=(1 ./ ((a .* a)*pi()*sigma_X*sigma_Y)) * (exp(-c/2));

  vector[k] p_z=part1 .* cdf+part2;

  return p_z;
  
  }
}


