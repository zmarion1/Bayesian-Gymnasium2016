data {
  int<lower=0> nObs;
  vector[nObs] biomass;
  vector[nObs] terpenes;
}

transformed data {
  vector[nObs] terpenesCent;
  
  terpenesCent <- terpenes - mean(terpenes);
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
  
  mu <- alpha + beta*terpenesCent;
  biomass ~ normal(mu, sigma);
}

generated quantities {
  vector[nObs] newBio;
  
  for(n in 1:nObs) {
    newBio[n] <- normal_rng(alpha + beta*terpenesCent[n], sigma);
  }
}