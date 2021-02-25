data {//endogeneous and exogeneous knowns
  int<lower=0> N; //number of countries
  vector[N] GDP; //this is the log form of the GDP
  vector[N] popgrowth; //used to calculate x2
  vector[N] invest; //used to calculate x2 //real v; //v should be 1 in this example
  vector[4] loc; //the location for parameter A, alpha, sigma and sigma_y
  vector[2] scal; //the scale for parameter A, alpha
}

transformed data{//for SBC, this allows to draw from the prior and prior predictive distribution.
    real A_ = gamma_rng(loc[1], scal[1]);  //assume that A follows a gamma distribution 
    real sigma_y_ = exponential_rng(1/loc[4]); //assume that the sigma_y follows a exponential distribution
    real alpha_ = uniform_rng(loc[2], scal[2]); //assume that alpha follows a uniform distirbution, and bounded to [0,1]
    real sigma_ = exponential_rng(1/loc[3]);//assume that sigma follows a exponential distribution. This stores the location for sigma. sigma will used to calculate rho.
    real<lower=1> delta_ = 1 / (1- alpha_); //should be larger than 1
    real<upper=1> rho_ = (sigma_ - 1) / sigma_;
    
    vector[N] x2;  //x1, x2  for the model, since x1=1, we could ignore x1.
    for (n in 1: N){
      x2[n] = (popgrowth[n] + 5) / invest[n];
  }
} 

parameters {//exogeneous unknowns
  real<lower=0> A; 
  real<lower=0, upper=1> alpha;
  real<lower=0> sigma;
  real<lower=0> sigma_y; //sigma_y is the standard deviation of y
} 

transformed parameters{
  real delta = 1 / (1-alpha);
  real rho = (sigma - 1) / sigma;
}
model {
  vector[N] mu_lny;
  for (i in 1:N) {
    mu_lny[i] = log(A) - (1 / rho) * log(delta  - delta * alpha * (pow (x2[i],  -rho)));
  }
  //prior of the Bayes rule
  target += gamma_lpdf(A | loc[1], scal[1]);
  target += uniform_lpdf(alpha | loc[2], scal[2]);
  target += exponential_lpdf(sigma | inv(loc[3]));
  target += exponential_lpdf(sigma_y | inv(loc[4]));
  //likelihood of the Bayes rule
  target += normal_lpdf(GDP | mu_lny, sigma_y);
}

generated quantities{//this block is also for doing SBC
  vector[N] log_lik;
  int ranks_[4] ={A > A_, alpha > alpha_, sigma > sigma_, sigma_y > sigma_y_};
  vector[4] pars_;
  pars_[1] = A_;
  pars_[2] = alpha_;
  pars_[3] = sigma_;
  pars_[4] = sigma_y_;
    for (n in 1: N){
    real mu_lny = log(A) - (1 / rho) * log(delta - delta * alpha * (pow (x2[n],  -rho)));
    log_lik[n] = normal_lpdf(GDP[n] | mu_lny, sigma_y);
}
}









 
