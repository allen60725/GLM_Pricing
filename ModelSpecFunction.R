# The packages, the model specifications and the function used for simulations of covariates
# are identical for both Part 1 and Part 2 of the simulation study. The standard deviations
# for the random effects were varied between 0 (for fixed effects only), 0.1, 0.2 and 0.4.
#Install.packages("MASS")
#Install.packages("Statmod')
#Install.packages("lme4")
library(MASS)   # for use of negbin
library(nlme)   # for use of inverse gaussian
library(lme4)

# Model specifications 
sim=500 # 500 simulations
years=5  # policies over 5 years
J=1000*years #1000 policies over 5 years
id=seq(1:(J/years)) # id for i=1, ...,1000 policyholiders
v_n=0.2 #standard deviation for random effect in claim counts
v_z=0.2 #standard deviation for random effect in claim sizes
tau=0.2 # choose a tau for negbin
k_gamma=6 #choose a shape parameter k for gamma sizes
lambda_ig=8 #choose a lambda for inverse gaussian sizes

# choose beta_n for claim counts and beta_z for claim sizes #
beta_n= rbind(-2.26,0.32,0.34,0.24,0.71,-0.52,0.22,-0.13,0.12,-0.13,0.49,0.61,-0.42,-0.62,1.20,0.21)
beta_z = rbind(-2.32,0.31,-0.19,0.27,0.31,-0.22,0.43,0.23,0.28,0.26,0.23,0.31,-0.21,0.12,0.64,0.22)
########################################
#### function (simulate covariates) ####
########################################
cov = function() {
   #Covariates x9 and x11 ramin the same for the policyhoders each year
  x0=rep(1, J)
  X1=rnorm(J, 1, 0.2)
  x2=rnorm(J,1, 0.2)
  x3=rnorm(J,1,0.2)
  x4=rnorm(J,1, 0.2)
  X5=rnorm(J,1, 0.2)
  x6=rnorm(J,1, 0.2)
  x7=rnorm(J, 1, 0.2)
  x8=rbinom(J, 1, 0.5)
  x9=rep(rbinom(J/years, 1, 0.5), years) #e.g gende, same for policies over 5 years
#Multinomial with 5 categorial
  X11=t(rmultinom(J/years, 1, prob=c(0.2, 0.3,0.5)))[,-1]
  x11=matrix(rep(t(x11),years),ncol(x11),byrow = TRUE)
  cov16=cbind(x0,1,x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) # matrix with 16 columns
  return(cov16)
}

