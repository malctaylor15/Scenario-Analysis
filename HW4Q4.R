
library(fExtremes)
setwd("C:/Users/board/Desktop/Kaggle/Scenario Analysis")
data <- read.csv("SP_500_Log_Returns_19600610_19871016.csv")

# Create full loss portfolios 
linear_loss <- -1000000* data[[4]]
#full_loss <- 1000000*(exp(data[[4]]) -1)

# Fit GEV parameters to the data 
GEV_fit <- gevFit(linear_loss, block = 125)
GEV_fit
GEV_params <- NULL
GEV_params[1:3] <- c(-0.075, 8529, 3694)

#Slide 24 Parameters xi = 0.3614 mu = 8430 sigma = 3337
qgev(0.95, xi = 0.3614, mu = 8430, beta = 3337)

# Fit the GP parameters to data 
GP_fit <- gpdFit(linear_loss)
GP_fit
GP_params <- NULL
GP_params[1:2] <- c(5.65067e-2, 2.39357e3)

# Slide 37 xi = 0.2622, beta = 2549 
qgpd(0.95, xi = 0.2622, beta = 2549)

# Calculate EWMA mu and sigma 
numb_obs <- length(linear_loss)
M <- 500 

# 
EWMA_mu <- rep(0,(numb_obs - M))
EWMA_mu[1] <- mean(linear_loss[1:M])
for (i in 1:(numb_obs-M)){
  EWMA_mu[i+1]<- EWMA_mu[i]*0.97 + linear_loss[(M+i)]*0.03
}
plot(EWMA_mu, type = 'l')
final_EWMA_mu = EWMA_mu[(numb_obs-M+1)]


# EWMA Sigma 
EWMA_sigma <- rep(0,(numb_obs - M))
EWMA_sigma[1] <- var(linear_loss[1:M])
for (i in 1:(numb_obs-M)){
  EWMA_sigma[i+1] <- 0.97*EWMA_sigma[i] + 0.03*(EWMA_mu[i] - linear_loss[(M+i)])^2
  
}
EWMA_sigma <- sqrt(EWMA_sigma)
plot(EWMA_sigma, type = 'l')
final_EWMA_sigma <- EWMA_sigma[(numb_obs-M+1)]

# VaR levels specified in question
Var_levels = seq(from = 0.99, to = 0.9999,by = 0.000099)

# Compute quantiles for the specified VaR levels given the parameters from distributions above 
Var_reg = quantile(linear_loss, Var_levels)
Var_EWMA = qnorm(Var_levels, mean = final_EWMA_mu, sd = final_EWMA_sigma)
GEV_Vars = qgev(Var_levels, xi = GEV_params[1], mu = GEV_params[2], beta = GEV_params[3])
GP_Vars =  qgpd(Var_levels, xi = GP_params[1], beta = GP_params[2] )


# Aggregate Var's for limits for plotting
all_data <- c(GEV_Vars, GP_Vars, Var_reg, Var_EWMA)

# Plotting
plot(Var_levels, GEV_Vars, ylim = c(min(all_data), max(all_data)),type = 'l', col= 'red', lwd = 4)
lines(Var_levels ,GP_Vars,type = 'l', col = 'blue' , lwd = 4)
lines(Var_levels, Var_reg, type = 'l', col = 'green', lwd =4)
lines(Var_levels, Var_EWMA, type = 'l', col = 'orange', lwd = 4)

legend("topleft", c("GEV", "GP", "VaR Empirical", "VaR Analytical"), lwd = 4, col = c("red", "blue", "green", "orange"))

# VaR at alpha = 0.9999
Var_reg_9 = quantile(linear_loss, 0.9999)
Var_EWMA_9 = qnorm(0.9999, mean = final_EWMA_mu, sd = final_EWMA_sigma)
GEV_Vars_9 = qgev(0.9999, xi = GEV_params[1], mu = GEV_params[2], beta = GEV_params[3])
GP_Vars_9 =  qgpd(0.9999, xi = GP_params[1], beta = GP_params[2] )

Var_reg_9
Var_EWMA_9
GEV_Vars_9
GP_Vars_9
# GEV has highest VaR 0.9999 at 33097


#L' = 99,452 
pVar_reg_9 = max(linear_loss)
pVar_EWMA_9 = pnorm(99452, mean = final_EWMA_mu, sd = final_EWMA_sigma)
pGEV_Vars_9 = pgev(99452, xi = GEV_params[1], mu = GEV_params[2], beta = GEV_params[3])
pGP_Vars_9 =  pgpd(99452, xi = GP_params[1], beta = GP_params[2] )

pVar_reg_9
pVar_EWMA_9
pGEV_Vars_9
pGP_Vars_9

# Need alpha of 1 for all measures... 
