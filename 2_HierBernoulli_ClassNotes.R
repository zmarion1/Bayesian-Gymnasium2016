library(shinystan)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
setwd("~/Dropbox/BayesClass")

#### Specify a grey color that is 40% transparent for use in plotting
#### later.
col <- "#38373940"

# create data of globe tosses: 1 = water, 0 = land.
obs <- rep(c(1, 0), times = c(13, 4))
nObs <- length(obs)

# ----------------------------------------------- 
# Mod 1: Simple
# hierarchical model where the bernoulli theta has a beta prior
# determined by mu and kappa. Mu itself has a weak hyperprior
# (beta(2,2)). The dependence of theta on mu is strong (kappa = 100).
# Kappa is fixed and does not have its own prior.
# -----------------------------------------------

# Specify weak hyperpriors on mu, strong concentration prior
aMu <- 2
bMu <- 2
kappa <- 100

curve(dbeta(x, aMu, bMu))  # plots a beta(2,2) density as example.

# Set up data as a list
datMod1 <- list(obs = obs, nObs = nObs, aMu = aMu, bMu = bMu, kappa = kappa)


# run model (mod 1P should be uncommented in the stan code. Every other
# model should be commented out).
mod1P <- stan(file = "2_HierachicalBern_StanCode.stan", data = datMod1, 
    iter = 1000, chains = 4, seed = 3)

# Inspect results via shiny
launch_shinystan(mod1P)

# Extract mu and theta from prior-only model.
muP <- as.matrix(mod1P, par = "mu")
thetaP <- as.matrix(mod1P, par = "theta")

# Note that the prior puts low certainty on mu but high dependence of
# theta on mu
plot(density(muP))  # marginal posterior for mu
plot(density(thetaP))  # marginal posterior for theta

# Note that the prior puts low certainty on mu but high dependence of
# theta on mu. Theta is tightly correlated with mu, and mu has a broad
# posterior dist. across its range.
plot(thetaP, muP, xlim = c(0, 1), ylim = c(0, 1), col = col)


##################### Now run the second model 1 (the one with the likelihood not
##################### commmented out)

mod1 <- stan(file = "2_HierachicalBern_StanCode.stan", data = datMod1, 
    iter = 1000, chains = 4, seed = 3)

# Inspect results via shiny
launch_shinystan(mod1)

# Extract mu and theta from model.
mu <- as.matrix(mod1, par = "mu")
theta <- as.matrix(mod1, par = "theta")

plot(density(mu))  # marginal posterior for mu
plot(density(theta))  # marginal posterior for theta

# Because of the weak prior on mu, the posterior is now driven by the
# data. The peak of the posterior has been shifted towards the max
# likelihood estimate (can calculate by sum(obs)/length(obs)). The
# dependence of theta on mu has not been altered much though because
# kappa is so large.
plot(theta, mu, xlim = c(0, 1), ylim = c(0, 1), col = col)



# ----------------------------------------------- 
# Mod 1.5: Simple
# hierarchical model where the bernoulli theta has a beta prior
# determined by mu and kappa. Mu now has a strong hyperprior
# (beta(20,20)). The dependence of theta on mu is strong (kappa = 5).
# Kappa is fixed and does not have its own prior.
# -----------------------------------------------

aMu <- 20
bMu <- 20
kappa <- 5


datMod1.5 <- list(obs = obs, nObs = nObs, aMu = aMu, bMu = bMu, kappa = kappa)

# The stan model for this is again model 1P. Everything else should be
# commented out
mod1.5P <- stan(file = "2_HierachicalBern_StanCode.stan", data = datMod1.5, 
    iter = 1000, chains = 4, seed = 3)

launch_shinystan(mod1.5P)

# Extract posterior estimates
muP <- as.matrix(mod1.5P, par = "mu")
thetaP <- as.matrix(mod1.5P, par = "theta")

plot(density(muP))  # marginal posterior for mu
plot(density(thetaP))  # marginal posterior for theta

# Now the prior puts high certainty on mu but low certainty on the
# dependence of theta on mu. The posterior for Mu is tightly clustered
# around 0.5, while the posterior for theta is broad.
plot(thetaP, muP, xlim = c(0, 1), ylim = c(0, 1), col = col)


# The stan model for this is again model 1 with the likelihood.
# Everything else should be commented out
mod1.5 <- stan(file = "2_HierachicalBern_StanCode.stan", data = datMod1.5, 
    iter = 1000, chains = 4, seed = 3)

launch_shinystan(mod1.5)

# Extract posterior estimates
muP <- as.matrix(mod1.5, par = "mu")
thetaP <- as.matrix(mod1.5, par = "theta")

plot(density(mu))  # marginal posterior for mu
plot(density(theta))  # marginal posterior for theta

# The marginal posterior for mu has not changed much because there was
# high prior certainty. The dependence of theta on mu has been
# noticablly altered, however. Because the concentration is weak, theta
# is informed more by the data. This diff. is complimentary to the
# first example

plot(theta, mu, xlim = c(0, 1), ylim = c(0, 1), col = col)



# ----------------------------------------------- 
# Now we have two globes, one big and one small. We also have two people 
# tossing them.
# Here we will test 3 models
# -----------------------------------------------

# Set up data
jim <- rep(c(1, 0), each = 5)
adam <- rep(c(1, 0), times = c(7, 3))
obs <- c(jim, adam)
nObs <- length(obs)

# weak priors
aMu <- 2
bMu <- 2
kappa <- 5

# ----------------------------------------------- 
#1) Complete pooling -
# assume that there is only one theta and mu; Samples are modeled under
# one distribution. It assumed there are no differences between globes
# -----------------------------------------------

datMod2CP <- list(obs = obs, nObs = nObs, aMu = aMu, bMu = bMu, kappa = kappa)


# This is the same model structure as the first models. I just
# separated it for clarity.
mod2CP <- stan(file = "2_HierachicalBern_StanCode.stan", data = datMod2, 
    iter = 1000, chains = 4, seed = 3)
launch_shinystan(mod2)

## I would encourage running the model with and without the priors, and
## exploring the marginal and conditional posteriors as we did in the
## first exercise.



# ----------------------------------------------- 
#1) No pooling -
# assume that the two globes represent completely different
# planets/distributions. Each globe and tosser (Jim or Adam) have their
# own thetas and mus. There is no sharing of info or crosstalk.
# -----------------------------------------------

# indexing vector: Jim = 1, Adam =2
person <- rep(c(1, 2), each = 10)

datMod2NP <- list(obs = obs, nObs = nObs, aMu = aMu, bMu = bMu, kappa = kappa, 
    nPers = 2, person = person)

mod2NP <- stan(file = "2_HierachicalBern_StanCode.stan", data = datMod2NP, 
    iter = 1000, chains = 4, seed = 3)

launch_shinystan(mod2NP)

###### Again, I would encourage looking at models with and without the
###### likelihood

# Extract posterior estimates
muNP1 <- as.matrix(mod2NP, par = "mu[1]")
muNP2 <- as.matrix(mod2NP, par = "mu[2]")

thetaNP1 <- as.matrix(mod2NP, par = "theta[1]")
thetaNP2 <- as.matrix(mod2NP, par = "theta[2]")

# Plots of marginal posteriors for mu and theta overlaid
plot(density(muNP1), xlim = c(0, 1))  # marginal posterior for mu1
lines(density(muNP2), lty = 2, col = "blue")  # marginal posterior for mu2

plot(density(thetaNP1), xlim = c(0, 1))  # marginal posterior for theta1
lines(density(thetaNP2), lty = 2, col = "blue")  # marginal posterior for theta2

# Try plotting conditional probabilities as we did above. For example,
# theta 1 v. mu1. Theta1 v. Theta2.


# ----------------------------------------------- 
#1) Partial pooling -
# assume that the globes are different,but come from a common
# planet/distribution. Each globe and tosser (Jim or Adam) have their
# own thetas but share mus. There is sharing of info or crosstalk. Use
# the same data but the mod2 Partial pooling model.
# -----------------------------------------------

mod2PP <- stan(file = "2_HierachicalBern_StanCode.stan", data = datMod2NP, 
    iter = 1000, chains = 4, seed = 3)

launch_shinystan(mod2PP)

###### Again, I would encourage looking at models with and without the
###### likelihood

# Extract posterior estimates
muPP <- as.matrix(mod2PP, par = "mu")

thetaPP1 <- as.matrix(mod2PP, par = "theta[1]")
thetaPP2 <- as.matrix(mod2PP, par = "theta[2]")

# Plots of marginal posteriors for mu and theta overlaid
plot(density(muPP1), xlim = c(0, 1))  # marginal posterior for mu1

plot(density(thetaPP1), xlim = c(0, 1))  # marginal posterior for theta1
lines(density(thetaPP2), lty = 2, col = "blue")  # marginal posterior for theta2

# Try plotting conditional probabilities as we did above. For example,
# theta 1 v. mu1. Or theta 2 v. theta1


