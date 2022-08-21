# Christina Xu, MA 575 Final Project Part IV
library(tidyverse)
source('xufunction.R')
source('nemolm2(2).r')
#Scatterplot
plot(top10s$year, top10s$pop, main = 'Popularity of Top Spotify Songs from 2010 to 2019',
     xlab = 'Year', ylab = 'Popularity')

popbar <- top10s %>%  group_by(year) %>% summarise_at(vars(pop), list(name=mean))

plot(popbar$year,popbar$name,
     main='Evolution of Top Song Popularity from 2010 to 2019',
     xlab = 'Year',
     ylab = 'Mean Popularity')

# Polynomial Regression
Y <- popbar$name
X1 <- popbar$year

Z1 <- (X1 - mean(X1))/sd(X1)
Z2 <- (X1^2 - mean(X1^2))/sd(X1^2)
Z3 <- (X1^3 - mean(X1^3))/sd(X1^3)

M5 <- nemolm2(Y, cbind(Z1, Z2, Z3),0)
lines(X1, M5$predicted, lwd = 3, col = 'seagreen')

# Linear regression model
plot(popbar$year,popbar$name,
     main='Evolution of Top Song Popularity from 2010 to 2019',
     xlab = 'Year',
     ylab = 'Mean Popularity')

M6 <- xufunction(Y, as.matrix(X1))
lines(X1, M6$predicted, lwd = 3, col = 'coral')

