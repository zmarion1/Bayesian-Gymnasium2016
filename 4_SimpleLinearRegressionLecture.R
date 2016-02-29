library(shinystan)
library(rstan)
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
setwd("my directory")

col <- "#38373920" # Makes a nice grey color with 80% transparency; for later


###--------------------------------
### Step 1: Data Makin': Dictyota menstrualis is a brown seaweed that produces > 250 dichyol terpenes as chemical deterrents against marine herbivores and biofouling microbes. When herbivore pressure is high, having higher terpene concentrations should lead to higher lifetime biomass. At least that's my story! This script simulates data. You give it the total number of desired observations, the intercept, the slope, and the standard deviation and it makes a dataframe for you. Play with it, changing parameters as you wish to see how the model differs. Also, in the script, terpenes is the x variable. I didn't include arguments to change that, but it would be easy by chaning the mean=50 & sd=3 to whatever you want.
###--------------------------------

seaweedSim <- function(nObs, alpha, beta, sigma) {
  terpenes <- round(rnorm(nObs, mean=50, sd=3), digits=2) 
  eps <- rnorm(nObs, mean=0, sd=sigma)
  biomass <- alpha + beta * terpenes + eps
  out <- data.frame(terpenes, biomass)
  out <- out[order(terpenes),]
  return(out)
}

###--------------------------------------------------------------
### Model 1.1: Estimating the mean (i.e., intercept only model). 
### We are first going to specify weak priors for the mean mu. We could pick uniforms, but that can lead to poor mixing for complicated models. Instead I am centering the prior for mu around the mean of biomass, and then the standard deviaton is set to 20. 20 standard deviations is a lot. Thus we don't have a lot of information about what the value of mu should be. If you change the simulated data, you may want to take this into account for your model to make sense.
###--------------------------------------------------------------

set.seed(20)
sim <- seaweedSim(nObs=50, alpha=0, beta=3, sigma=12)

dat <- list(nObs=dim(sim)[1], biomass=sim$biomass, terpenes=sim$terpenes, muMean=150, muSD=20)

modMean <- stan(file="mod1.stan", data=dat, iter=2000, chains=4, seed=3)
#launch_shinystan(modMean)
print(modMean)

pairs(modMean) 
### this makes a paired plot for the model. When the number of parameters gets big, this becomes unwieldy

###--------------------------------------------------------------
### Model 1.2: Now we are going to change the priors on the variance for the mu parameter. This is a much more informed prior now. 
###--------------------------------------------------------------

dat <- list(nObs=dim(sim)[1], biomass=sim$biomass, terpenes=sim$terpenes, muMean=150, muSD=0.1)

modMean <- stan(file="modMean.stan", data=dat, iter=2000, chains=4, seed=3)
#launch_shinystan(modMean)
print(modMean)

pairs(modMean) 

### Note that the posterior for the mean, mu, is much more tight around 150. Also note that the posterior for sigma has changed, even though we did not change the prior. This is because sigma is being estimated conditional on mu. We can interpret priors as former posterior inference, such as from previous data. We can compute the immplied amount of data in the prior with a simple formula for the sd of a normal:
### sigmaPost = 1/sqrt(nObs)
### This suggests the implied amount of data with our mean is:
### nObs = 1/sigmaPost^2
### So in this case, with a sd prior on mu of 0.1, N=1/0.1^2=100. So that prior is equivalent to previously observing 100 samples with a mean of 150. That's strong.



###---------------------------------------------------------------
### Model 2.1: Linear regression. Now we add a predictor. Before, biomass ~ normal(mu, sigma) where mu was just a scalar; a single value. With a predictor, mu becomes a function:
### mu <- alpha + beta*terpenes
### Now mu is a function of two parameters: the intercept alpha (when terpenes=0), and beta the slope. 
###---------------------------------------------------------------
modReg <- stan(file="modReg.stan", data=dat, iter=2000, chains=4, seed=3)
print(modMean) 
### we can compare the output to an OLS model: 
modOLS <- lm(dat$biomass ~ dat$terpenes)
summary(modOLS)

pairs(modMean) 
### 1) Note that the interecept is negative, saying there is negative biomass when there is an absence of terpenes. Biologically this makes no sense. 
### 2) Note the tight correlation between alpha and beta. This is often a problem, especially for MCMC samplers. Also, note the low effective sample sizes (should be close to 4000, ESS for alpha and beta (from print) are much lower. The following plot will help explain why
# extract posterior for alpha and beta

post <- as.data.frame(modMean, pars=c("alpha","beta")) 

# plot the regression lines for each posterior, and the data points
plot(dat$terpenes, dat$biomass, type="n", ylim=c(-50,200),xlim=c(0,100))
for(i in 1:dim(post)[1]){
  abline(a=post$alpha[i], b=post$beta[i],col=col)
}
points(dat$terpenes, dat$biomass, col="red",pch=16,cex=0.8)

### The reason we get such a tight correlation is that the center of most of the data for biomass and terpenes are far from zero, while the interecept, alpha, is trying to estimate the value of biomass when terpenes = 0. Because most of the data are so far away from zero, any slight change in slope is going to have huge effects on the value for the interecept. This means alpha and beta will be tightly correlated, and sampling will be inefficient. THus we have HUGE variance in the estimate of the intercept.


###---------------------------------------------------------------
### Model 2.2: Linear regression. Now we center. 
### In the model we will use, modCent.stan, we transform the values of terpenes. Now terpenes are centered around the mean by subtracting each value from the mean. Also, we are simulating new data in the generated quantities block, given our model. 
###---------------------------------------------------------------

modCent <- stan(file="modCent.stan", data=dat, iter=2000, chains=4, seed=3)
print(modCent)
pairs(modCent,pars=c("alpha","beta"))

### Now, the intercept represents the value of biomass when terpene is at the mean value. THis is much more interpretable. Also, the effective sample size is higher because the correlation between alpha and beta is removed. Now lets make a pretty plot

post <- as.data.frame(modCent, pars=c("alpha","beta"))
newObs <- as.matrix(modCent,pars="newBio")
xCentered <- dat$terpenes - mean(dat$terpenes)

lower <- as.numeric(apply(newObs,2, quantile, 0.025))
upper <- as.numeric(apply(newObs,2, quantile, 0.975))

plot(xCentered, dat$biomass, type="n", ylim=c(min(lower), max(upper)), xlim=c(min(xCentered),max(xCentered)), xaxs="i")

polygon(x=c(xCentered, rev(xCentered)),y=c(upper,rev(lower)), col="grey80")

for(i in 1:dim(post)[1]) {
  abline(a=post$alpha[i],b=post$beta[i], col=col)
}  
abline(a=mean(post$alpha), b=mean(post$beta), col="red", lwd=3, lty=4) 
points(xCentered, dat$biomass, col="red", pch=16)

  
### There are a few things going on here. First, the smattering of grey lines represents the uncertainty in our estimate of mu. In the first model, mu was just a mean for biomass. Now, mu is a function of terpenes. So the grey lines represent the posterior uncertainty for this function: The mean relationship between biomass and terpenes. Second, the light grey polygon is the 95% posterior prediction interval. This is incorperating the posterior variance in the data (sigma) too. Given our model, this is where we would expect 95% of the data to fall. 

