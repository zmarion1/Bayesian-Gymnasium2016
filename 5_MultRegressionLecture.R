library(shinystan)
library(rstan)
library(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
setwd("~/Dropbox/BayesClass/Lecture5")

#### Load data and specify a grey color that is 40% transparent for use in
#### plotting later.  The data are densities of urchins, sea stars, and
#### chitons. Urchins are the response variable while sea stars (a
#### predator of urchins) and chitons (a potential competitor) are
#### explanatory variables.
load("5_marineEx.R")
col <- "#38373930"

seastars <- as.numeric(scale(reefs$seastars))
chitons <- as.numeric(scale(reefs$chitons))
urchins <- reefs$urchins


### ------------------------------------------ 
### First run separate univariate models of the effects of chitons 
### and sea stars on urchins independently 
###------------------------------------------- 

### CHITONS
chitonDat <- list(nObs = nrow(reefs), obs = reefs$urchins, pred = chitons)

modChitons <- stan(file = "univariateMod.stan", data = chitonDat, iter = 1000, 
                   chains = 4, seed = 8675309)
print(modChitons, pars = c("alpha", "beta", "sigma"))

### Extract the results
chitPars <- as.data.frame(modChitons, pars = c("alpha", "beta"))  # posterior parameter estimates
chitMu <- as.matrix(modChitons, "mu")  # posterior mean expected values
chitL <- apply(chitMu, 2, quantile, 0.025)  # lower 95%
chitU <- apply(chitMu, 2, quantile, 0.975)  # upper 95%
oCH <- order(chitons)  # index of chitons for ordering from lowest to highest


##### Sea Stars
starsDat <- list(nObs = nrow(reefs), obs = reefs$urchins, pred = seastars)

modStars <- stan(file = "univariateMod.stan", data = starsDat, iter = 1000, 
                 chains = 4, seed = 8675309)

print(modStars, pars = c("alpha", "beta", "sigma"))

starPars <- as.data.frame(modStars, pars = c("alpha", "beta"))
starMu <- as.matrix(modStars, "mu")
starL <- apply(starMu, 2, quantile, 0.025)
starU <- apply(starMu, 2, quantile, 0.975)
oST <- order(seastars)

### plot data and 95% HDI for posterior of regression line in 2 panels
par(mfrow = c(1, 2))
plot(urchins ~ chitons, type = "n", las = 1, xaxs = "i", main = "chitons")
points(urchins ~ chitons, pch = 19, col = "blue")
polygon(x = c(chitons[oCH], rev(chitons[oCH])), y = c(chitL[oCH], rev(chitU[oCH])), 
        col = col)
abline(a = mean(chitPars$alpha), b = mean(chitPars$beta), col = "blue", 
       lwd = 3, lty = 4)

plot(urchins ~ seastars, type = "n", las = 1, xaxs = "i", main = "seastars")
points(urchins ~ seastars, pch = 19, col = "blue")
polygon(x = c(seastars[oST], rev(seastars[oST])), y = c(starL[oST], rev(starU[oST])), 
        col = col)
abline(a = mean(starPars$alpha), b = mean(starPars$beta), col = "blue", 
       lwd = 3, lty = 4)

### ================================================= 
### The takeaway from this is that, when analyzed separately, 
### both chiton and sea star densities appear to have strong negative 
### effects on urchin densities.
### Both slopes are very different from zero.
### =================================================


### ------------------------------------------------- 
### Now we are going to analyze the effects of chitons and sea stars 
### in one model, i.e., multiple regression 
### -------------------------------------------------

### Set up data, specifically the predictor variables are now part of a
### design matrix
reefMat <- as.matrix(cbind(rep(1, nrow(reefs)), chitons, seastars))
reefDat <- list(nObs = nrow(reefs), obs = urchins, xMatrix = reefMat, nPred = ncol(reefMat))

modReef <- stan(file = "multivariateMod.stan", data = reefDat, iter = 1000, 
                chains = 4, seed = 8675309)

### Extract results
fullPars <- as.data.frame(modReef, pars = "beta")  # posterior parameter estimates
muChitons <- as.matrix(modReef, pars = "muP1")  # posterior mean expected values for chitons at the MEAN value for seastars
muStars <- as.matrix(modReef, pars = "muP2")  # similar to above

starL <- apply(muStars, 2, quantile, 0.025)
starU <- apply(muStars, 2, quantile, 0.975)
chitL <- apply(muChitons, 2, quantile, 0.025)
chitU <- apply(muChitons, 2, quantile, 0.975)

ost <- order(seastars)
och <- order(chitons)

### Plot results again in 2 panel plot. We are not showing the actual
### data because our regression lines are counterfactual...They are
### conditional on imaginary data for the other predictor. By that I mean
### that we are fixing the other predictor at it's mean value.

quartz()
par(mfrow = c(1, 2))
plot(urchins ~ seastars, type = "n", las = 1, xaxs = "i", main = "Sea Stars")
for (i in 1:nrow(fullPars)) {
  abline(a = fullPars[i, 1], b = fullPars[i, 3], col = col)
}
lines(x = seastars[ost], starL[ost], lty = 1, lwd = 2.5, col = "skyblue1")
lines(x = seastars[ost], starU[ost], lty = 1, lwd = 2.5, col = "skyblue1")
abline(a = mean(fullPars[, 1]), b = mean(fullPars[, 3]), col = "skyblue1", 
       lwd = 3, lty = 4)

plot(urchins ~ chitons, type = "n", las = 1, xaxs = "i", main = "Chitons")
for (i in 1:nrow(fullPars)) {
  abline(a = fullPars[i, 1], b = fullPars[i, 2], col = col)
}
lines(x = chitons[och], chitL[och], lty = 1, lwd = 2.5, col = "skyblue1")
lines(x = chitons[ost], chitU[och], lty = 1, lwd = 2.5, col = "skyblue1")
abline(a = mean(fullPars[, 1]), b = mean(fullPars[, 2]), col = "skyblue1", 
       lwd = 3, lty = 4)


### Now we can do model selection
logLikR <- extract_log_lik(modStars)
logLikF <- extract_log_lik(modReef)
waicR <- waic(logLikR)
waicF <- waic(logLikF)
compare(waicR, waicF)

### ------------------------------------------------- 
### The takehome from
### this should be that it can be misleading to analyze results in a
### univariate framework. Other predictors may completely change or mask
### the effects. In our case, analysing both sea stars and chitons
### together has revealed that there is no effect of chitons because they
### are strongly correlated with sea stars. Once sea stars are included,
### the effect disappears. Biologically, this is because there is
### apparent competition between chitons and urchins. They don't actually
### compete for food. Instead, higher chiton densities attract greater
### numbers of sea star predators, which impact both urchins and chitons.
### Had we not measured sea star densities however, we would wrongly
### conclude that urchins and chitons are strong competitors.
### --------------------------------------------------

### For homework now, try using this code to analyze the pond dataset. In
### that dataset, the response variable are numbers of toad tadpoles.
### There are two predictor variables: newt densities, which are keystone
### predators, and treefrog larval densities (hyla), which are
### heterospecifics of the toads.

load("5_pondEx.R")
