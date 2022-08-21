library(corrplot)
source('xufunction.R')

top10s <- top10s[-c(1)]

summary(top10s)

data <- top10s[,-c(1:4)]
 
X <- data[,1:9]
Y <- data[,10]

M1 <- xufunction(Y,as.matrix(X))

# EDA
pairs(top10s[,5:14],pch=19,lower.panel = NULL) # scatterplot matrix

data <- data.matrix(top10s[,5:14])
R <- cor(data)
corrplot(R, method='circle')

Y <- top10s$pop
X1 <- top10s$dnce
X2 <- top10s$dB
X3 <- top10s$dur

M3 <- xufunction(Y, cbind(X1,X2,X3))

# Prediction for new data
# X1 = 60, X2 = -8, X3 = 170
M3$betahat[1] + M3$betahat[2]*60 + M3$betahat[3]*-8 + M3$betahat[4]*170


# ANOVA 
SSE <- M3$SSE
SSM <- M3$SSM
SST <- M3$SST

MSE <- M3$MSE
MSM <- M3$MSM
MST <- M3$MST
  
Fstat <- MSM/MSE
pval <- M3$`p-value`

plot(M3$predicted,Y,xlab='Fitted Values', ylab='y')

Standardized MLR
Z1 <- (X1 - mean(X1))/sd(X1)
Z2 <- (X2 - mean(X2))/sd(X2)
Z3 <- (X3 - mean(X3))/sd(X3)

M4 <- xufunction(Y, cbind(Z1,Z2,Z3))

# ANOVA
ZSSE <- M4$SSE
ZSSM <- M4$SSM
ZSST <- M4$SST

ZMSE <- M4$MSE
ZMSM <- M4$MSM
ZMST <- M4$MST

ZFstat <- MSM/MSE
Zpval <- M4$`p-value`

# Multcolinearity
MYvX1c <- xufunction(Y, cbind(X2,X3))
MX1vX1c <- xufunction(X1, cbind(X2,X3))

MYvX2c <- xufunction(Y, cbind(X1,X3))
MX2vX2c <- xufunction(X2, cbind(X1,X3))

MYvX3c <-xufunction(Y, cbind(X1,X2))
MX3vX3c <- xufunction(X3, cbind(X1,X2))

vif1 <- 1/(1-MYvX1c$rsquared)
vif2 <- 1/(1/MYvX2c$rsquared)
vif3 <- 1/(1/MYvX3c$rsquared)

vif <- c(vif1, vif2, vif3)
barplot(vif, horiz = T, main = 'Variance Inflation Factors', 
        names.arg = c('X1', 'X2', 'X3'),
        xlim = c(0, max(6, max(vif))))
abline(v=5, col='coral', lty = 'longdash')


# Added Variable Plots
plot(MYvX1c$residual, MX1vX1c$residual,
     main = 'Added Variable Plot for X1',
     xlab = 'S.Residuals for Y~X1c',
     ylab = 'S.Residuals for X1~X1c')
abline(0,0,lwd=2)
abline(mean(MX1vX1c$residual)-cor(MYvX1c$residual, MX1vX1c$residual)*sd(MX1vX1c$residual)/sd(MYvX1c$residual), 
       cor(MYvX1c$residual, MX1vX1c$residual)*sd(MX1vX1c2residual)/sd(MYvX1c$residual), col = 'seagreen', lwd = 2)

plot(MYvX2c$residual, MX2vX2c$residual,
     main = 'Added Variable Plot for X2',
     xlab = 'S.Residuals for Y~X2c',
     ylab = 'S.Residuals for X2~X2c')
abline(0,0,lwd=2)
abline(mean(MX2vX2c$residual)-cor(MYvX2c$residual, MX2vX2c$residual)*sd(MX2vX2c$residual)/sd(MYvX2c$residual), 
       cor(MYvX2c$residual, MX2vX2c$residual)*sd(MX2vX2c$residual)/sd(MYvX2c$residual), col = 'salmon', lwd = 2)

plot(MYvX3c$residual, MX3vX3c$residual,
     main = 'Added Variable Plot for X3',
     xlab = 'S.Residuals for Y~X3c',
     ylab = 'S.Residuals for X3~X3c')
abline(0,0,lwd=2)
abline(mean(MX3vX3c$residual)-cor(MYvX3c$residual,MX3vX3c$residual)*sd(MX3vX3c$residual)/sd(MYvX3c$residual),
       cor(MYvX3c$residual, MX3vX3c$residual)*sd(MX3vX3c$residual)/sd(MYvX3c$residual), col = 'cadetblue2', lwd = 2)



#MYvZ1c <- xufunction(Y, cbind(Z2,Z3))
#MZ1vZ1c <- xufunction(Z1, cbind(Z2,Z3))

#MYvZ2c <- xufunction(Y, cbind(Z1,Z3))
#MZ2vZ2c <- xufunction(Z2, cbind(Z1,Z3))

#MYvZ3c <-xufunction(Y, cbind(Z1,Z2))
#MZ3vZ3c <- xufunction(Z3, cbind(Z1,Z2))

# Variance Inflation Factors
#vif1 <- 1/(1-MYvZ1c$rsquared)
#vif2 <- 1/(1/MYvZ2c$rsquared)
#vif3 <- 1/(1/MYvZ3c$rsquared)

#vif <- c(vif1, vif2, vif3)
#barplot(vif, horiz = T, main = 'Variance Inflation Factors', 
#        names.arg = c('Z1', 'Z2', 'Z3'),
#        xlim = c(0, max(4, max(vif))))
#abline(v=2.5, col='coral', lty = 'longdash')

data1 <- cbind(Y, X1, X2, X3)

R1 <- cor(data1)
corrplot(R1, method='circle')

# Standardized residual plot
plot(X1,M3$`std. residual`,
     main = 'Residuals', 
     xlab='Danceability', 
     ylab='Standardized Residuals')
abline(-2,0,col='salmon',lty='longdash')
abline(2,0,col='salmon',lty='longdash')

pairs(cbind(Y,X1,X2,X3),pch=19,lower.panel = NULL)
plot(Z1,M4$`std. residual`, 
     main = 'Residuals',
     xlab='Standardized Year',
     ylab = 'Standardized Residuals')
abline(c(-2,2), col='salmon',lty='longdash',lwd=2 )
abline(-2,0,col='salmon',lty='longdash')
abline(2,0,col='salmon',lty='longdash')

yhat <- M3$predicted

predict()
# Time Series Fundamentals 
popbar <- top10s %>%  group_by(year) %>% summarise_at(vars(pop), list(name=mean))
                                                       
plot(popbar$year,dncebar$name,
     main='Evolution of Popularity',
     xlab = 'Year',
     ylab = 'Mean Popularity')






