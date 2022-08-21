# Christina Xu, 
source("xufunction.R")
# A. Simple Linear Regression
# Scatterplot
plot(x = top10s$dnce,
     y = top10s$pop, 
     main = 'Popularity vs Danceability',
     xlab ='X', 
     ylab='Y')

# SLR model
M1 <- xufunction(Sy,as.matrix(Sx))

n <- dim(top10s)[1]
Sx <- top10s[1:n,8] #Danceability
Sy <- top10s[1:n,15] #Popularity 

# Beta coefficients
betahats <- M1$betahat

lines(Sx, M1$predicted, lwd = 3, col = 'lightblue')


# ANOVA
SSE <- M1$SSE
MSE <- M1$MSE
SST <- M1$SST
MST <- M1$MST
SSM <- M1$SSM
MSM <- M1$MSM

std.resid <- M1$`std. residual`

# Standardized residual plot
par(mfrow = c(1,1))
plot(Yhat,  
     std.resid,
     xlab = "Predicted Values for Popularity",
     ylab = "Standardized Residuals",
     main = "Residuals")
abline(c(-2,2), col='salmon',lty='longdash',lwd=2 )
abline(-2,0,col='salmon',lty='longdash')
abline(2,0,col='salmon',lty='longdash')


# Standard errors
SE.betahat <- M1$SEbetahat

# r-squared and r-squared adj
r2 <- M1$rsquared
r2adj <- M1$rsquaredadj

# H0: B1 = 0 vs H0 != 0
pvalue <- M1$`p-value`

# B. Quadratic Simple Regression
pairs(top10s[,5:15],pch=19,lower.panel = NULL) # scatterplot matrix

# Scatterplot
plot(top10s$bpm, top10s$dnce, xlab = "Beats per Minute", ylab = "Danceability", main = "Danceability vs BPM")

# Fitting the model
bpm <- as.matrix(top10s$bpm) 
dnce <- top10s$dnce
M2 <- xufunction(dnce, bpm^2) 

lines(bpm, M2$predicted, lwd = 3, col = 'lightblue')

plot(M2$predicted, M2$`std. residual`, 
     xlab = 'Predicted Values for Danceability', 
     ylab = 'Standardized Residuals', 
     main = 'Residuals')
abline(c(-2,2), col='salmon',lty='longdash',lwd=2 )
abline(-2,0,col='salmon',lty='longdash')
abline(2,0,col='salmon',lty='longdash')

betahat2 <- M2$betahat
# options(scipen = 999)
SE.betahat2 <- M2$SEbetahat
r2.2 <- M2$rsquared
r2adj2 <- M2$rsquaredadj
pval2 <- M2$`p-value`





# 
