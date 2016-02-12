##--------------------------------------------------
## models 1P & 1.5P: Prior only for simple hierarchical model
## Likelihood should be commented out.
##--------------------------------------------------

# data {
  # int<lower=0> nObs;
  # int<lower=0, upper=1> obs[nObs];
  # real<lower=0> aMu;
  # real<lower=0> bMu;
  # real<lower=2> kappa;  
# }

# parameters {
  # real<lower=0, upper=1> theta;
  # real<lower=0, upper=1> mu;
# }

# transformed parameters {
  # real<lower=0> aTheta;
  # real<lower=0> bTheta;
  
  # aTheta <- mu * kappa;
  # bTheta <- (1 - mu)*kappa;
# }

# model {
 # mu ~ beta(aMu,bMu); 
 # theta ~ beta(aTheta, bTheta);
 
  # # for(n in 1:nObs) {
    # # obs ~ bernoulli(theta);
  # # }
# }




##--------------------------------------------------
## model 1 & 1.5: Simple Bernoulli hierarchical model with 
## Likelihood included
##--------------------------------------------------

# data {
  # int<lower=0> nObs;
  # int<lower=0, upper=1> obs[nObs];
  # real<lower=0> aMu;
  # real<lower=0> bMu;
  # real<lower=2> kappa;  
# }

# parameters {
  # real<lower=0, upper=1> theta;
  # real<lower=0, upper=1> mu;
# }

# transformed parameters {
  # real<lower=0> aTheta;
  # real<lower=0> bTheta;
  
  # aTheta <- mu * kappa;
  # bTheta <- (1 - mu)*kappa;
# }

# model {
  # mu ~ beta(aMu,bMu); 
 # theta ~ beta(aTheta, bTheta);
 
  # for(n in 1:nObs) {
    # obs ~ bernoulli(theta);
  # }
# }



##--------------------------------------------------
## model2CP: Complete pooling model.
## Likelihood should be commented out if effect of prior is 
## of interest
##--------------------------------------------------

# data {
  # int<lower=0> nObs;
  # int<lower=0, upper=1> obs[nObs];
  # real<lower=0> aMu;
  # real<lower=0> bMu;
  # real<lower=2> kappa;  
# }

# parameters {
  # real<lower=0, upper=1> theta;
  # real<lower=0, upper=1> mu;
# }

# transformed parameters {
  # real<lower=0> aTheta;
  # real<lower=0> bTheta;
  
  # aTheta <- mu * kappa;
  # bTheta <- (1 - mu)*kappa;
# }

# model {
  # mu ~ beta(aMu,bMu); 
 # theta ~ beta(aTheta, bTheta);
 
  # # for(n in 1:nObs) {
    # # obs ~ bernoulli(theta);
  # # }
# }




##--------------------------------------------------
## model 2NP: No pooling model. Two thetas & two mus.
##--------------------------------------------------


# data {
  # int<lower=0> nObs;
  # int<lower=0> nPers;
  # int<lower=1> person[nObs];
  # int<lower=0, upper=1> obs[nObs];
  # real<lower=0> aMu;
  # real<lower=0> bMu;
  # real<lower=0> kappa;
# }

# parameters {
  # vector<lower=0, upper=1>[nPers] theta;
  # vector<lower=0, upper=1>[nPers] mu;
# }

# transformed parameters {
  # vector<lower=0>[nPers] aTheta;
  # vector<lower=0>[nPers] bTheta;

  # for(p in 1:nPers) {
    # aTheta[p] <- mu[p] * kappa;
    # bTheta[p] <- (1-mu[p]) * kappa;
  # }
# }

# model {
  # for(p in 1:nPers) {
    # mu[p] ~ beta(aMu, bMu); 
    # theta[p] ~ beta(aTheta[p], bTheta[p]);
  # }
  # for(n in 1:nObs) {
  	  # obs[n] ~ bernoulli(theta[person[n]]);
  # }
# }



##--------------------------------------------------
## model 2PP: Partial pooling model. Two thetas, one mu.
##--------------------------------------------------



# data {
  # int<lower=0> nObs;
  # int<lower=0> nPers;
  # int<lower=1> person[nObs];
  # int<lower=0, upper=1> obs[nObs];
  # real<lower=0> aMu;
  # real<lower=0> bMu;
  # real<lower=0> kappa;
# }

# parameters {
  # vector<lower=0, upper=1>[nPers] theta;
  # real<lower=0, upper=1> mu;
# }

# transformed parameters {
  # real<lower=0> aTheta;
  # real<lower=0> bTheta;

    # aTheta <- mu * kappa;
    # bTheta <- (1-mu) * kappa;
# }

# model {
  # for(p in 1:nPers) {
    # mu ~ beta(aMu, bMu); 
    # theta[p] ~ beta(aTheta, bTheta);
  # }
  # for(n in 1:nObs) {
  	  # obs[n] ~ bernoulli(theta[person[n]]);
  # }
# }

