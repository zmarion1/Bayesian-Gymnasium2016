data {
  int<lower=0> nObs;
  vector[nObs] biomass;
  vector[nObs] terpenes;
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}

model {
  vector[nObs] mu;
  
  alpha ~ normal(150, 100);
  beta ~ normal(0, 10);
  sigma ~ cauchy(0, 10);
  
  mu <- alpha + beta*terpenes;
  biomass ~ normal(mu, sigma);
}