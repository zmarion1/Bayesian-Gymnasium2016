library(shinystan)
library(rstan)
library(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
setwd("your working directory")


### Create the data (binomially distributed
obs <- c(3, 4, 3, 5, 4, 5, 4, 1, 3, 4, 3, 3, 3, 5, 4)
nObs <- length(obs)
groups <- c(rep(1, 6), rep(2, 9))  # Men vs women
nGroups <- 2
sizes <- rep(6, length(obs))

dat <- list(obs = obs, nObs = nObs, groups = groups, nGroups = nGroups, 
            sizes = sizes)

### Complete pooling model; one theta, one mu
modCP <- stan(file = "completePooling.stan", data = dat, iter = 1000, chains = 4, 
              seed = 2)

launch_shinystan(modCP)

### No pooling model. The mu hyperprior for theta is independent for men
### vs women
modNP <- stan(file = "noPooling.stan", data = dat, iter = 1000, chains = 4, 
              seed = 2)
launch_shinystan(modNP)

### Partial pooling model; different thetas for men and women that share
### common hyperpriors
modPP <- stan(file = "partialPooling.stan", data = dat, iter = 1000, chains = 4, 
              seed = 2)

launch_shinystan(modPP)

### Using the loo package, extract the log likelihoods; This results in a
### matrix where the rows are the iterations and ### the columns are the
### likelihoods for each observation

likCP <- extract_log_lik(modCP)
likNP <- extract_log_lik(modNP)
likPP <- extract_log_lik(modPP)

### To see what wAIC is doing under the hood, we will go step by step in
### calculating the different components. waic is pointwise; uncertainty
### is point-by-point i.e., handles uncertainty for each obs.

### 1) Pr(yi): average likelihood of obs i in training sample We compute
### likelihood of yi for each set of parameters sampled from the
### posterior.  then we average the likelihood for each observation i and
### finally sum over all observations.

### This gives us log pointwise predictive density: lppd =
### sum(log(Pr(yi))) ie., the lppd is the logarithm of the average
### likelihood of each obs.  If multiplied by -2, would be similar to
### deviance

### We compute by averaging samples for each obs, taking log, then adding
### all logs together.  To do with precison, need to do averaging on log
### scale. SO we compute log of sum of exponentiated terms. Then subtract
### the log number of samples.

lppdCP <- rep(NA, ncol(likCP))  # set up empty vector


for (i in 1:ncol(likCP)) {
  lppdCP[i] <- log(sum(exp(likCP[, i]))) - log(nrow(likCP))
}


### 2) pwaic. P(yi) = variance in log-lik for obs i in the sample.  We
### compute the log-likelihood of yi for each sample in the posterior.
### Then take the variance.

pWAIC <- rep(NA, ncol(likCP))

for (i in 1:ncol(likCP)) {
  pWAIC[i] <- var(likCP[, i])
}

### expected log pointwise predictive density is:
elpdCP <- (lppdCP - pWAIC)

### This is how we calculate the standard error for the diff components
### of WAIC. Multiply the std dev of the component by the sqrt of the
### number of observations nObs

elpdsd <- sd(elpdCP) * sqrt(nObs)

### WAIC is -2 x the elpd. It puts the elpd on a deviance scale. Note
### that we can get a wAIC value for each observation, just like we get
### an elpd or an effective no of parameters for each observation. If we
### take the sum of those, it gives us the value for all the data.
waicCP <- -2 * elpdCP
sum(waicCP)

### Of course there is already a built in function to do this. But
### compare what we get when we use the function to when we do it by
### hand. They are identical. All that math was proof of concept to show
### you what is going on under the hood.

cp <- waic(likCP)
np <- waic(likNP)
pp <- waic(likPP)

### Just like comparing two parameters, WAIC is correlated between
### models. So if you want to know the distribution of the diff in WAIC
### between models, you have to compute it as a difference, not look at
### the overlap in uncertainty between individual WAIC values. We can use
### the compare(m1, m2) function for this. The output gives the
### difference as m2-m1. It also gives us Akaike weights should we want
### to do model averaging (rounded).

compare(cp, np)

### if you compare more than 2 models, it just gives a table of the WAIC,
### pWAIC, elpd, associated errors, and weights. It doesn't give you
### pairwise differences.

compare(cp, np, pp)

### As for interpreting the WAIC values, lower is better. There is no
### real hard and fast rule though for whether to reject a model or not
### based on WAIC.  You can interpret the weights as: an estimate of the
### prob. that the model will make the best predictions on the new data,
### conditional on the set of models considered. Think of the weights as
### posterior probabilities of models, conditional on future data.