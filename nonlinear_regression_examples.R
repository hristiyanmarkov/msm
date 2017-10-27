library(car)
data(USPop)
attach(USPop)

#####################

plot(population ~ year, data=USPop, main="(a)")
abline(lm(population ~ year, data=USPop))

#####################
# Using lm and the logit function from the car package,
# we compute starting values for the other two parameters:
# logit(p)=log(p/(1-p))=x^Tb, 0<p<1
#
logit.lm=lm(logit(population/400) ~ year, data=USPop)
summary(logit.lm)
#
# nls(stats) -  Nonlinear Least Squares procedure
#
pop.mod <- nls(population ~ theta1/(1 + exp(-(theta2 + theta3*year))),
     start=list(theta1 = 400, theta2 = -49, theta3 = 0.025),
     data=USPop, trace=TRUE)
summary(pop.mod)

plot(population ~ year, USPop, xlim=c(1790, 2100), ylim=c(0,450))
#
#Evaluate an R expression in an environment constructed from data
#
predicted=predict(pop.mod, data.frame(year=seq(1790, 2100, by=10)))
with(USPop, lines(seq(1790, 2100, by=10),predicted,lwd=2))

# 
points(2010, 307, pch="x", cex=1.3)
abline(h=0, lty=2)
abline(h=coef(pop.mod)[1], lty=2)
abline(h=.5*coef(pop.mod)[1], lty=2)
abline(v= -coef(pop.mod)[2]/coef(pop.mod)[3], lty=2)
plot(USPpop$year, residuals(pop.mod))
abline(h=0, lty=2)

#
# Analytic Derivatives
#
model <- function(theta1, theta2, theta3, year){
     term <- exp(-(theta2 + theta3*year))
     yhat <- theta1/(1 + term)
     gradient <- cbind((1 + term)^-1, # in proper order
      	  	 theta1*(1 + term)^-2 * term,
        	 theta1*(1 + term)^-2 * term * year)
     attr(yhat, "gradient") <- gradient
     yhat
}

USPop.nls.der=nls(population ~ model(theta1, theta2, theta3, year),
     data=USPop, start=list(theta1=400, theta2=-49, theta3=0.025))
summary(USPop.nls.der)

#
# using the deriv function in R to compute a formula for the gradient
#
model2 <- deriv(~ theta1/(1 + exp(-(theta2 + theta3*year))), # rhs of model
     c("theta1", "theta2", "theta3"), # parameter names
     function(theta1, theta2, theta3, year){} # arguments for result
     )
    
model2  # the required function, its derivatives and arguments
#  
USPop.nls.derive=nls(population ~ model2(theta1, theta2, theta3, year),
		data=USPop, start=list(theta1=400, theta2=-49, theta3=0.025))
summary(USPop.nls.derive)

