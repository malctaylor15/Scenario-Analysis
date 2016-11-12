
library(fExtremes)
setwd("C:/Users/board/Desktop/Kaggle/Scenario Analysis")
data <- read.csv("SP_500_Log_Returns_19600610_19871016.csv")
GEV_data_lin <- read.csv("max_block_value_lin.csv") # GEV using linear loss operator
GP_data_lin <- read.csv("GP_data_lin.csv") # GP using linear loss operator 

GEV_data_full <-read.csv("max_block_value_full.csv")
GP_data_full <- read.csv("GP_data_full.csv")

linear_loss <- -1000000* data[[4]]
full_loss <- 1000000*(exp(data[[4]]) -1)

GEV_data_full <- GEV_data_full[[2]]
GEV_fit<- gevFit(GEV_data_full) 
GEV_fit

#Slide 24 Parameters xi = 0.3614 mu = 8430 sigma = 3337
qgev(0.95, xi = 0.3614, mu = 8430, beta = 3337)

GP_data_full <- GP_data_full[[2]]
GPD_fit <- gpdFit(GP_data_full)
GPD_fit

# Slide 37 xi = 0.2622, beta = 2549 
qgpd(0.95, xi = 0.2622, beta = 2549)

Var_levels = seq(from = 0.99, to = 0.9999,by = 0.000099)

Var_reg = quantile(linear_loss, Var_levels)
GEV_Vars = qgev(Var_levels, xi = -0.)
GP_Vars =  qgpd(Var_levels, )


again <- gevFit(full_loss, block = 125)
again

