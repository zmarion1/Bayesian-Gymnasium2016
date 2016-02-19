data {
	int<lower=0> nObs;
	int<lower=0> obs[nObs];
	int<lower=0> sizes[nObs];
	int<lower=0> nGroups;
	int<lower=0> groups[nObs];
}

parameters {
	vector<lower=0>[nGroups] theta;
	real<lower=0, upper=1> mu;
	real<lower=0> kappa;
}

transformed parameters {
	real<lower=0> alpha;
	real<lower=0> beta;
	
		alpha <- mu * kappa;
		beta <- (1-mu) * kappa;
}

model {
	
	mu ~ beta(1,1);
  theta ~ beta(alpha, beta);
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



