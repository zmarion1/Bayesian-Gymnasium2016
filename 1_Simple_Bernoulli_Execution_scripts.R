library(shinystan)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
setwd("~/Dropbox/BayesClass")

# create data of globe tosses; 1 = water, 0 = land

obs <- rep(c(1, 0), times = c(3, 5))
nObs <- length(obs)

# put the needed data for stan into a list
dat <- list(obs = obs, nObs = nObs)

# run model with flat (uniform) beta prior
mod1 <- stan(file = "bernoulli1.stan", data = dat, iter = 1000, chains = 4, 
  seed = 3)
# inspect model using shiny stan
launch_shinystan(mod1)


# -------------------------------- run model without the likelihood to
# inspect priors (equiv. to lik = 1)
priorOnly <- stan(file = "bernoulli1.stan", data = dat, iter = 1000, chains = 4, 
  seed = 3)
launch_shinystan(priorOnly)



# -------------------------------- now we do experiment again with
# larger sample size
obsNew <- rep(c(1, 0), times = c(13, 7))
nObsNew <- length(obsNew)

### can make informative prior for beta based off of previous experiment.
### Before we had 3 waters and 5 land. Therefore, we can set a=3 and b=5
### (read chapter 6 of puppy book for more info about beta dist and how
### to parameterize)
a = 3
b = 5

datMod2 <- list(obs = obsNew, nObs = nObsNew, a = a, b = b)

mod2 <- stan(file = "bernoulli1.stan", data = datMod2, iter = 1000, chains = 4, 
  seed = 3)
launch_shinystan(mod2)


# --------------------------------

### another way we can parameterize a beta is in terms of a mean
### (expectation) and concentration (also chapter 6 of puppy book). The
### expectation of a beta (the most likely value for theta) is mu = a/(a
### + b). The concentration (think of as inverse of variance or as the
### precision) is kappa = a + b. As kappa gets bigger, the spread around
### mu gets smaller and tighter. Some algebra, and a = mu*kappa while b =
### (1-mu)*kappa. For our 3 W and 5 L data previously:

mu <- 3/8
kappa <- 8  # could be any no. greater than 2; in this case I used the previous experiment's sample size. Play around with both mu and kappa and see what happens

a <- mu * kappa
b <- (1 - mu) * kappa

datMod2.5 <- list(obs = obsNew, nObs = nObsNew, a = a, b = b)

mod2.5 <- stan(file = "bernoulli1.stan", data = datMod2, iter = 1000, chains = 4, 
  seed = 3)
launch_shinystan(mod2.5)



# --------------------------------

### We can check how well our model is working with a posterior
### predictive check where we simulate new data based off our model's
### posterior and see how well it fits. First I will create a model with
### a very weak (flat) prior centered at theta = 0.5.

mu <- 4/8
kappa <- 2.2  # could be any no. greater than 2; in this case I used the previous experiment's sample size. Play around with both mu and kappa and see what happens

a <- mu * kappa
b <- (1 - mu) * kappa

### to see what prior looks like try:
curve(dbeta(x, shape1 = a, shape2 = b))

## Could also run model with likelihood commented out to see what the
## posterior looks like

datMod3a <- list(obs = obsNew, nObs = nObsNew, a = a, b = b)

mod3a <- stan(file = "bernoulli1.stan", data = datMod3a, iter = 1000, chains = 4, 
  seed = 3)
launch_shinystan(mod3a)

#### actual number of sampled water from globe toss (out of 20)
actual <- sum(obsNew)

simDat3a <- as.data.frame(mod3a, pars = "waterNew")  #extract the new simulated data

# plot histogram of simulated data and place a vertical line where the
# actual data is
hist(simDat3a$waterNew, xlim = c(0, 20))
abline(v = actual, lty = 2, lwd = 3, col = "hotpink")


# --------------------------------

### Now a prior that is informative and very very wrong relative to the
### actual data. Compare the histogram of simulated data in 3a to that of
### 3b.

mu <- 0.1
kappa <- 12  # could be any no. greater than 2; in this case I used the previous experiment's sample size. Play around with both mu and kappa and see what happens

a <- mu * kappa
b <- (1 - mu) * kappa

### to see what prior looks like try:
curve(dbeta(x, shape1 = a, shape2 = b))

## Could also run model with likelihood commented out to see what the
## posterior looks like

datMod3b <- list(obs = obsNew, nObs = nObsNew, a = a, b = b)

mod3B <- stan(file = "bernoulli1.stan", data = datMod3b, iter = 1000, chains = 4, 
  seed = 3)
launch_shinystan(mod3a)

#### actual number of sampled water from globe toss (out of 20)
actual <- sum(obsNew)

simDat3B <- as.data.frame(mod3B, pars = "waterNew")  #extract the new simulated data

# plot histogram of simulated data and place a vertical line where the
# actual data is
hist(simDat3B$waterNew, xlim = c(0, 20))
abline(v = actual, lty = 2, lwd = 3, col = "hotpink")

