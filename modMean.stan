data {
  int<lower=0> nObs;
  real muMean;
  real<lower=0> muSD;
  vector[nObs] biomass;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  mu ~ normal(muMean, muSD);
  sigma ~ cauchy(0, 10);
  
  biomass ~ normal(mu, sigma);
}