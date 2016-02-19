data {
	int<lower=0> nObs;
	int<lower=0> obs[nObs];
	int<lower=0> sizes[nObs];
	int<lower=0> nGroups;
	int<lower=0> groups[nObs];
}

parameters {
	vector<lower=0>[nGroups] theta;
	vector<lower=0, upper=1>[nGroups] mu;
	real<lower=0> kappa;
}

transformed parameters {
	vector<lower=0>[nGroups] alpha;
	vector<lower=0>[nGroups] beta;
	
	for(g in 1:nGroups) {
		alpha[g] <- mu[g] * kappa;
		beta[g] <- (1-mu[g]) * kappa;
  }
}

model {
	for(g in 1:nGroups) {
		mu[g] ~ beta(1,1);
    theta[g] ~ beta(alpha[g], beta[g]);
	}
	kappa ~ cauchy(0, 5);
	
	for(n in 1:nObs) {
		obs[n] ~ binomial(sizes[n], theta[groups[n]]);
	}
}

generated 	quantities {
	vector[nObs] log_lik;
	real diff;
	for(n in 1:nObs) {
		log_lik[n] <- binomial_log(obs[n], sizes[n], theta[groups[n]]);
	}
	diff <- theta[1] - theta[2];
}



