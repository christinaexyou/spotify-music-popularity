# Christina Xu, Univariate Data Analysis

# Analyzing the danceability of popular songs
# The higher the value, the easier it is to dance to this song

library(tidyverse)

# Checking the range of danceability values
min(top10s$dnce)
#0
max(top10s$dnce)
#97

n <- length(top10s$dnce)
# 603
alpha <- 0.05
df <- n - 1
# 602

# Generating t-critical value 
tcrit <- qt(alpha/2, df, lower.tail = F)
# 1.964

# Generating sample error
sx <- sd(top10s$dnce)
# 13.379
SE <- sx/sqrt(n)
# 0.545

# One sample hypothesis test for the pop. mean
# Based on logic, popular songs should have a high danceability
mu0 <- 70 #claimed value of the mean
xbar <- mean(top10s$dnce)
# 64.380

# Test statistic
tstat <- (xbar-mu0)/SE
# -10.316

# Pvalue
pvalue <- 2*pt(-abs(tstat), df, lower.tail=T)
# 4.384

# Margin of Error
eps <- tcrit*SE
# 1.070

# Confidence Interval
CIL <- xbar - eps
CIU <- xbar + eps
# (63.310, 65.450)

metric_name = c("CI.Lower", "CI.Upper", "Claimed.Value", "T.stat", "T.crit", 
                "pvalue", "alpha")
metric_value = c(CIL, CIU, mu0, tstat, tcrit, pvalue, alpha)

DataSummary <- data.frame(metric_name, metric_value)

hist(top10s$dnce, col='orchid', main = 'Distribution of Danceability Values', xlab = 'Danceability')
# One sample hypothesis test for the pop. standard deviation
sig0 <- 15 # claimed value of the standard deviation

# Chi-squared test statistic for pop. standard deviation
cstat <- sqrt(((n-1)*sx^2/sig0^2))


# Lower and upper critical values for pop. standard deviation
ccritL <- sqrt(qchisq(alpha/2, df, lower.tail = T))
ccritU <- sqrt(qchisq(alpha/2, df, lower.tail = F))

metric_name1 <- c("Lower Tail Bound","Upper Tail Bound", "sig0", "cstat", "alpha")
metric_value1 <- c(ccritL, ccritU, sig0, cstat, alpha)

DataSummary1 <- data.frame(metric_name1, metric_value1)

# Checking the normality of BPM and Energy distributions
qqnorm(top10s$bpm, ylab = "Quantiles of BPM", main = "NQQ Plot for the Beats Per Minute of Top Spotify Songs")
qqline(top10s$bpm, col = "orchid", lwd = 3)
V <- qqnorm(top10s$bpm, plot=FALSE)
cor(V$x,V$y)

qqnorm(top10s$nrgy, ylab = "Quantiles of Energy", main = "NQQ Plot for the Energy of Top Spotify Songs")
qqline(top10s$nrgy, col = "salmon", lwd = 3)
U <- qqnorm(top10s$nrgy, plot=FALSE)
cor(U$x, U$y)

# ---------H. Test for the Difference in Pop. Means----------
# EDA - checking the frequency of genres
table(top10s$top.genre)

pop <- top10s[top10s$top.genre == 'pop',]
dancepop <- top10s[top10s$top.genre == 'dance pop',]

boxplot(pop$bpm, dancepop$bpm, 
        names = c('Pop', 'Dance Pop'), 
        ylab = 'Beats per Minute (bpm)', col = c('lightpink', 'steelblue1'),
        horizontal = TRUE)

alpha1 <- 0.01

xbar1 <- mean(pop$bpm)
xbar2 <- mean(dancepop$bpm)
sd1 <- sd(pop$bpm)
sd2 <- sd(dancepop$bpm)
n1 <- length(pop$bpm)
n2 <- length(dancepop$bpm)

# Welch t-statistic
xbard <- xbar1 - xbar2
wstat <- xbard/sqrt(sd1^2/n1 + sd2^2/n2)

# degrees of freedom
numerator <- ((sd1^2/n1 + sd2^2/n2)^2)
denominator <- (sd1^4/(n1^2*(n1-1)) + sd2^4/(n2^2*(n2-1)))
df <- numerator/denominator

t.test(pop$bpm,dancepop$bpm,alternative = "two.sided", var.equal = FALSE, conf.level = 0.99)

SE1 <- sqrt((sd1^2/n1)+(sd2^2/n2))
wcrit <- qt(alpha1/2, df, lower.tail = F)
meps1 <- wcrit*SE1
pval1 <- 2*pt(-abs(wstat), df = 77.74)

metric_name2 <- c("CI.Lower", "CI.Upper", "T-stat", "T-crit", "pvalue", "alpha")
metric_value2 <- c(xbard - meps1, xbard + meps1, wstat, wcrit, pval1, alpha1)
DataSummary2 <- data.frame(metric_name2, metric_value2)
print(DataSummary2)

# ---------H.Test 2 for the Difference in Pop. Variances--------
alpha2 <- 0.01

fstat <- sd1^2/sd2^2 
fcritL <- qf(alpha2/2, df1 = n1-1, df2 = n2-1, lower.tail = T) 
fcritR <- qf(alpha2/2, df1 = n1-1, df2 = n2 -1, lower.tail = F)

fstatL <- min(fstat, 1/fstat)
fstatR <- max(fstat, 1/fstat)

pval2 <- pf(fstatL, df1 = n1-1, df2 = n2-1, lower.tail = T) + 
  pf(fstatR, df1 = n1-1, df2 = n2-1, lower.tail = F)

metric_name3 <- c("CI_Lower", "CI_Upper", "FstatL", "FstatU", 
                  "FcritL", "FcritU", "pval", "alpha")
metric_value3 <- c(fcritL*fstat, fcritR*fstat, fstatL, fstatR,
                   fcritL, fcritR, pval2, alpha2)
DataSummary3 <- data.frame(metric_name3, metric_value3)
fcritL*fstat
