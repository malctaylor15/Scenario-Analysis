setwd("C:/Users/board/Desktop/Kaggle/Scenario Analysis")
Data <- read.csv("MSFT_AAPL_Log_Returns.csv",header=TRUE)

###############################################################################################
###Corporate Risk Management HW3 Q2 Part I###
############################################################################################

#Log-returns
MSFT_Log_R <- Data$MSFT.Log.Return[!is.na(Data$MSFT.Log.Return)] 
APPL_Log_R <- Data$AAPL.Log.Return[!is.na(Data$AAPL.Log.Return)]

#Portfolio parameters
Val_Port <- 1000000 #Portfolio Value
M_Cap_MSFT <- 448.77 
M_Cap_APPL <- 577.1
# W_MSFT <- M_Cap_MSFT/(M_Cap_MSFT+M_Cap_APPL)#0.4383 ?slides
# W_APPL <- 1-W_MSFT #0.5617 ?slides

W_MSFT <- 0.5617
W_APPL <- 0.4383
Alpha <- 0.95 #Confidence
lambda <- 0.97
M <- 500 #Initial days EWMA
K <- 20 #K parameter for K-day VaR
length(MSFT_Log_R)==length(APPL_Log_R) #Check if if both have same # of obs.
n <- length(MSFT_Log_R) #Number of observations

#for-loop 
mu_MSFT <- NULL
mu_APPL <- NULL
sigma_MSFT <- NULL
sigma_APPL <- NULL
cov_sigma<- NULL

# Initialize first numbers for EWMA for mu and covariance
mu_MSFT[1]<- mean(MSFT_Log_R[1:(M-1)])
mu_APPL[1]<- mean(APPL_Log_R[1:(M-1)])
sigma_MSFT[1]<-var(MSFT_Log_R[1:(M-1)])
sigma_APPL[1]<-var(APPL_Log_R[1:(M-1)])
cov_sigma[1]<-cov(MSFT_Log_R[1:(M-1)],APPL_Log_R[1:(M-1)])

# For loop for EWMA mu and covariance matrix
for(i in 1:(n-M-1)){
  previous_day = M+i-1  
  
  #EWMA update for mu 
  mu_MSFT[i+1]<- lambda*mu_MSFT[(i)] +(1-lambda)*MSFT_Log_R[(previous_day)]
  mu_APPL[i+1]<- lambda*mu_APPL[(i)]+(1-lambda)*APPL_Log_R[(previous_day)]
  mu_mat <- matrix(c(mu_MSFT[i+1],mu_APPL[i+1]),nrow=2,ncol=1)
  
  # EWMA update for covariance matrix 
  sigma_MSFT[i+1]<-lambda*sigma_MSFT[(i)] + (1-lambda)*
    (MSFT_Log_R[(previous_day)]-mu_MSFT[i])^2
  sigma_APPL[i+1]<-lambda*sigma_APPL[i] + (1-lambda)*
    (APPL_Log_R[(previous_day)]-mu_APPL[i])^2
  
  cov_sigma[i+1]<-lambda*cov_sigma[i] + (1-lambda)*
    (MSFT_Log_R[(previous_day)]-mu_MSFT[i])*(APPL_Log_R[(previous_day)]-mu_APPL[i])
  
  cov_mat <- matrix(c(sigma_MSFT[i+1],rep(cov_sigma[i+1],2),
                      sigma_APPL[i+1]),nrow=2,ncol=2)
}

#VaR calculations
weight_matrix <- matrix(c(W_MSFT,W_APPL),nrow=2,ncol=1) #MSFT AAPL weight matrix
Port_Dollar <- -Val_Port*weight_matrix # ct
(VaR <- (t(Port_Dollar)%*%mu_mat) + 
   (sqrt(t(Port_Dollar)%*%cov_mat%*%Port_Dollar))*qnorm(Alpha)) #Lin VaR
(K_VaR <- sqrt(K)*VaR) #K-day VaR
(Reg_Cap <- 3*(sqrt(K/2)*VaR)) #Regulatory Capital Change

#sim <- mvrnorm(50000, mu_mat, cov_mat)

#compare solutions with the values on slide 47 Lecture 8

##########################################################################
#########################################################################
####################  Question  3   #####################################
##########################################################################
#########################################################################
library(MASS)
 

multiVar <- function (mu_mat, cov_mat, K, weight_matrix, ValPort, Alpha = 0.95, lambda = 0.97){
  
# Reset mu, covariance matrix for EWMA 
mu_MSFT <- NULL
mu_APPL <- NULL
mu_MSFT[1] <- mu_mat[1]
mu_APPL[1] <- mu_mat[2]

sigma_MSFT <- NULL
sigma_APPL <- NULL
cov_sigma <- NULL
sigma_MSFT[1] <- cov_mat[1,1]
sigma_APPL[1] <- cov_mat[2,2]
cov_sigma[1] <- cov_mat[1,2]

# Initialize return matrix for K days 
Returns_sum1 <- rep(0, K)
Returns_sum2 <- rep(0, K)

# First observations for the shock 
Xtplusdelta = mvrnorm(n=1,mu_mat,cov_mat)
Xtplusdelta[2] = mu_mat[2] - 5* (cov_mat[2,2]) #shock


for(i in 1:(K-1)) { # For each day after the shock 
  
  # Update mu with new observations 
  mu_MSFT[i+1]<- lambda*mu_MSFT[i]+(1-lambda)*Xtplusdelta[1]
  mu_APPL[i+1]<- lambda*mu_APPL[i]+(1-lambda)*Xtplusdelta[2]
  mu_mat <- matrix(c(mu_MSFT[i+1],mu_APPL[i+1]),nrow=2,ncol=1)
  
  # Update EWMA covariance matrix with new observations 
  sigma_MSFT[i+1]<-lambda*sigma_MSFT[i] + (1-lambda)*
    (Xtplusdelta[1]-mu_MSFT[i])^2
  sigma_APPL[i+1]<-lambda*sigma_APPL[i] + (1-lambda)*
    (Xtplusdelta[2]-mu_APPL[i])^2
  cov_sigma[i+1]<-lambda*cov_sigma[i] + (1-lambda)*
    (Xtplusdelta[1]-mu_MSFT[i])*(Xtplusdelta[2]-mu_APPL[i])
  cov_mat <- matrix(c(sigma_MSFT[i+1],rep(cov_sigma[i+1],2),
                      sigma_APPL[i+1]),nrow=2,ncol=2)
  
  # Store returns
  Returns_sum1[i+1] = Returns_sum1[i]+ Xtplusdelta[1] 
  Returns_sum2[i+1] = Returns_sum2[i]+ Xtplusdelta[2]
  
  # Redraw from multivariate distribution with new mu and covariance matrix 
  Xtplusdelta = mvrnorm(n=1,mu_mat,cov_mat)
}

# VaR Calculations 
# weight_matrix <- matrix(c(W_MSFT,W_APPL),nrow=2,ncol=1) #MSFT AAPL weight matrix
Port_Dollar <- -Val_Port*weight_matrix # ct
(VaR <- (t(Port_Dollar)%*%mu_mat) + 
   (sqrt(t(Port_Dollar)%*%cov_mat%*%Port_Dollar))*qnorm(Alpha)) #Lin VaR
(K_VaR <- sqrt(K)*VaR) #K-day VaR
# (Reg_Cap <- 3*(sqrt(K/2)*VaR)) #Regulatory Capital Change
(Loss_kplusdelta = Port_Dollar[1]*Returns_sum1[K] +Port_Dollar[2]*Returns_sum2[K])

return (c(Loss_kplusdelta, K_VaR))
}

M_hat <-  50000
# Pre allocate space 
results <- data.frame(matrix(0,nrow = M_hat, ncol = 2))
names(results)[1:2] <- c("Loss", "K_Var")
# MC

for (sample in 1:M_hat){ 

results[sample,] <- multiVar(mu_mat, cov_mat, K, weight_matrix, ValPort)
}

# Exceedance 
# Kday Var Exceedances
(K_Var_exceedances <- sum(results[[2]] > rep(K_VaR,M_hat)) )
# Regulatory Cap exceedances 
(Reg_cap_exceedances <- sum(results[[2]] > rep(Reg_Cap,M_hat)) )

# Quick Plots... 
hist(results[[2]], breaks = 100)
abline(v = K_VaR, col = "blue", lwd = 3)
abline(v = Reg_Cap, col = "red", lwd = 3) # doesn't even show up -- too far right 

# Frequencies 
(100/M_hat) * K_Var_exceedances
(100/M_hat) * Reg_cap_exceedances

