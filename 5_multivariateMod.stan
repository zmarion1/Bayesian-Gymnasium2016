data {
  int<lower=0> nObs;
  int<lower=0> nPred;
  vector[nObs] obs;
  matrix[nObs, nPred] xMatrix;
}

parameters {
  vector[nPred] beta;
  real<lower=0> sigma;
}

transformed parameters {
  vector[nObs] mu;
  mu <- xMatrix * beta;
}

model {
  beta ~ normal(0, 10);
  sigma ~ cauchy(0, 10);
  
  obs ~ normal(mu, sigma);
}

generated quantities {
  vector[nObs] log_lik;
  vector[nObs] muP1;
  vector[nObs] muP2;
  
  for(n in 1:nObs) {
    log_lik[n] <- normal_log(obs[n], mu[n], sigma);
  }
  
  muP1 <- beta[1] + beta[2]*xMatrix[,2];
  muP2 <- beta[1] + beta[3]*xMatrix[,3];
}


