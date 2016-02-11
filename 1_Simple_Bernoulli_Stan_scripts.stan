#==============
## below are several models pertaining to the various things we tried. Just comment out all models but the one that is relevant to run.
#==============

#### model 1
data {
  int<lower=0> nObs;
  int<lower=0, upper=1> obs[nObs];
}

parameters {
  real<lower=0, upper=1> theta;
}

model {
  theta ~ beta(1,1);
  for(n in 1:nObs) {
    obs ~ bernoulli(theta);
	}
}



#### prior only model
# data {
  # int<lower=0> nObs;
  # int<lower=0, upper=1> obs[nObs];
# }

# parameters {
  # real<lower=0, upper=1> theta;
# }

# model {
  # theta ~ beta(1,1);
# }




#### model 2 & model 2.5
# data {
  # int<lower=0> nObs;
  # int<lower=0, upper=1> obs[nObs];
  # real<lower=0> a;
  # real<lower=0> b;
# }

# parameters {
  # real<lower=0, upper=1> theta;
# }

# model {
  # theta ~ beta(a,b);
  # for(n in 1:nObs) {
    # obs ~ bernoulli(theta);
  # }
# }



#### models 3a & 3b
# data {
  # int<lower=0> nObs;
  # int<lower=0, upper=1> obs[nObs];
  # real<lower=0> a;
  # real<lower=0> b;
# }

# parameters {
  # real<lower=0, upper=1> theta;
# }

# model {
  # theta ~ beta(a,b);
  # for(n in 1:nObs) {
    # obs ~ bernoulli(theta);
  # }
# }

# generated quantities {
  # int<lower=0, upper=1> yNew[nObs];
  # int<lower=0> waterNew;
	
  # for(n in 1:nObs) {
    # yNew[n] <- bernoulli_rng(theta);
  # }
  # waterNew <- sum(yNew);
# }