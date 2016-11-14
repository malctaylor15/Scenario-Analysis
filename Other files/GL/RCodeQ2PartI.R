setwd("C:/Users/board/Desktop/Kaggle/Scenario Analysis/GL")
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
W_MSFT <- M_Cap_MSFT/(M_Cap_MSFT+M_Cap_APPL)#0.4383 ?slides
W_APPL <- 1-W_MSFT #0.5617 ?slides

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

mu_MSFT[M]<- mean(MSFT_Log_R[1:(M-1)])
mu_APPL[M]<- mean(APPL_Log_R[1:(M-1)])
sigma_MSFT[M]<-var(MSFT_Log_R[1:(M-1)])
sigma_APPL[M]<-var(APPL_Log_R[1:(M-1)])
cov_sigma[M]<-cov(MSFT_Log_R[1:(M-1)],APPL_Log_R[1:(M-1)])

for(i in M:n) {
        
        mu_MSFT[i+1]<- lambda*mu_MSFT[i]+(1-lambda)*MSFT_Log_R[i]
        mu_APPL[i+1]<- lambda*mu_APPL[i]+(1-lambda)*APPL_Log_R[i]
        mu_mat <- matrix(c(mu_MSFT[i+1],mu_APPL[i+1]),nrow=2,ncol=1)
      
        sigma_MSFT[i+1]<-lambda*sigma_MSFT[i] + (1-lambda)*
                (MSFT_Log_R[i]-mu_MSFT[i])^2
        sigma_APPL[i+1]<-lambda*sigma_APPL[i] + (1-lambda)*
                (APPL_Log_R[i]-mu_APPL[i])^2
        cov_sigma[i+1]<-lambda*cov_sigma[i] + (1-lambda)*
                (MSFT_Log_R[i]-mu_MSFT[i])*(APPL_Log_R[i]-mu_APPL[i])
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

#compare solutions with the values on slide 47 Lecture 8

