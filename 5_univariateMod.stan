data {
  int<lower=0> nObs;
  vector[nObs] obs;
  vector[nObs] pred;
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}

transformed parameters {
  vector[nObs] mu;
  
  mu <- alpha + beta*pred;
}

model {
  alpha ~ normal(0, 10);
  beta ~ normal(0, 1);
  sigma ~ cauchy(0, 10);
  
  obs ~ normal(mu, sigma);
}

generated quantities {
  vector[nObs] log_lik;
  
  for(n in 1:nObs) {
    log_lik[n] <- normal_log(obs[n], mu[n], sigma);
  }
}