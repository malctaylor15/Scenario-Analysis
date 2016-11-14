setwd("~/Fall 2017 Courses/Corporate Risk Management/HW3/Q2")
Data <- read.csv("MSFT_AAPL_Log_Returns (1).csv",header=TRUE)

#  Corporate Risk Management HW3 Q2# 
#  Historical Simulation

#  removing NAs
MSFT_Log_R <- Data$MSFT.Log.Return[!is.na(Data$MSFT.Log.Return)] # as of 1-Sep
APPL_Log_R <- Data$AAPL.Log.Return[!is.na(Data$AAPL.Log.Return)] # as of 1-Sep

#  Initial Portfolio properties 
Val_Port <- 1000000 #  Portfolio Value
M_Cap_MSFT <- 448.77 
M_Cap_APPL <- 577.1
(W_MSFT <- M_Cap_MSFT/(M_Cap_MSFT+M_Cap_APPL)) # Weight Microsoft
(W_APPL <- 1-W_MSFT) # Weight Apple
Alpha <- 0.95 # Confidence


ct <- -Val_Port*c(W_APPL, W_MSFT)
mu <- c(0.000909, 0.001487)
sigma1 <- matrix(c(0.000152, 0.000131,0.000152,0.000131), nrow= 2, ncol =2)

pt1 = ct %*% mu
pt2 <- t(ct) %*% sigma1 %*% ct
pt2*qnorm(0.95)*0.5 +pt1
