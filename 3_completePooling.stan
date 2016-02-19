data {
	int<lower=0> nObs;
	int<lower=0> obs[nObs];
	int<lower=0> sizes[nObs];
}

parameters {
	real<lower=0> theta;
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
	kappa ~ cauchy(0, 5);
	
	for(n in 1:nObs) {
		obs[n] ~ binomial(sizes[n], theta);
	}
}

generated 	quantities {
	vector[nObs] log_lik;
	
	for(n in 1:nObs) {
		log_lik[n] <- binomial_log(obs[n], sizes[n], theta);
	}
}



