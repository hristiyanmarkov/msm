# Project for MSM @ FMI-SU
# Hristiyan Markov, f.n. 147

# load the packages needed
require(robustbase)
require(stats)

# load the data
data(aircraft)

# what's inside
head(aircraft)

# Aircraft Data, deals with 23 single-engine aircraft built over the years 1947-1979,
# from Office of Naval Research. The dependent variable is cost (in units of $100,000)
# and the explanatory variables are aspect ratio, lift-to-drag ratio, weight of plane (in pounds)
# and maximal thrust.
# 
# Format
# 
# A data frame with 23 observations on the following 5 variables.
# 
# X1
# Aspect Ratio
# 
# X2
# Lift-to-Drag Ratio
# 
# X3
# Weight
# 
# X4
# Thrust
# 
# Y
# Cost

# plot the data
plot(aircraft)

# plot(aircraft$X1)
# plot(aircraft$X2)
# plot(aircraft$X3)
# plot(aircraft$X4)
# plot(aircraft$X5)

# normal linear regression on all columns
model.lm1 = lm(Y ~., data = aircraft)
summary(model.lm1)
anova(model.lm1,test="F")
plot(model.lm1)

# confidence intervals
coef(model.lm1)
confint(model.lm1, level = 0.95)

# normal linear regression on selected columns
model.lm2 = lm(Y ~ X3 + X4, data = aircraft)
summary(model.lm2)
anova(model.lm2,test="F")
plot(model.lm2)

# confidence intervals
coef(model.lm2)
confint(model.lm2, level = 0.95)

# LTS
model.lts1<-ltsReg(Y ~., data = aircraft)
summary(model.lts1)
plot(model.lts1)

# plot(model.lts1, which = "rindex") # plot of the standardized residuals versus their index
# plot(model.lts1, which = "rqq") # normal Q-Q plot of the standardized residuals
# plot(model.lts1, which = "rdiag") # regression diagnostic plot
# plot(model.lts1, which = "rfit") # plot of the standardized residuals versus fitted values

###################################################################################################

# QR
library(quantreg)

# QR models aircraft cost in relation to aircraft weight
model.qr1 = rq(Y ~ X3, data = aircraft, tau = .25)
model.qr2 = rq(Y ~ X3, data = aircraft)
model.qr3 = rq(Y ~ X3, data = aircraft, tau = .75)
model.lm3 = lm(Y ~ X3, data = aircraft)

# plot
plot(aircraft$Y ~ aircraft$X3, xlab = "aircraft weight", ylab = "aircraft cost", main = "aircraft cost vs. weight")
abline(model.lm3, col="blue", lty=2, lwd = 3)
abline(model.qr1, col="darkgreen", lty=2, lwd = 3)
abline(model.qr2, col="red", lty=2, lwd = 3)
abline(model.qr3, col="orange", lty=2, lwd = 3)

# make it pretty
legend(x = "topleft",c("25%","mean fit", "median fit", "75%"),
       col = c("darkgreen","blue","red","orange"), lty = 1, lwd = 2)
