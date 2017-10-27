#We consider data from an experiment on N uptake reported by Cedergreen and
#Madsen (2002), where the predictor variable is the initial substrate concentration
#and the response variable is the uptake rate. In this type of experiment,
#it is anticipated that the uptake will increase as the concentration increases,
#approaching a horizontal asymptote.
#Kinetics data such as those in L.minor are often described by the Michaelis-Menten model                                                                #
# f(concentration, (K, Vm)) = (Vm * concentration)/(K + concentration)                             
#
# Starting values
#The horizontal asymptote is attained at 17.1, 
#the substrate concentration resulting in an increase from 0 to Vm/2 is 126!

L.minor <- read.table("L.minor.dat",skip=3,header=T)
attach(L.minor)

plot(rate ~ conc, data = L.minor, ylab = "Uptake rate (weight/h)",  
xlab =" Substrate  concentration (mmol  m^-3)")  
#
# nls is from the stats library!
#
#The first argument, rate ~ Vm*conc/(K+conc), is the model formula, where ~ is used to relate the response, rate!
#
L.minor.m1 <- nls(rate ~ Vm * conc/(K + conc), data = L.minor, start = list(K = 20, Vm = 120), trace = TRUE)

summary(L.minor.m1)
coef(L.minor.m1)
logLik(L.minor.m1)
deviance(L.minor.m1)
sqrt(deviance(L.minor.m1)/6)  #standard error of the model - sqrt(RSS/(n-p))

confint(L.minor.m1)
confint(L.minor.m1, "K")
confint(L.minor.m1, "Vm")

#
# Fitted values
#
fitted(L.minor.m1)
#
#cbind(fitted(L.minor.m1),L.minor$rate)
cor(fitted(L.minor.m1),L.minor$rate)

concVal <- with(L.minor, seq(min(conc), max(conc), length.out = 10))
predict(L.minor.m1, data.frame(conc = concVal)) 
             
plot(rate ~ conc, data = L.minor,
	ylim = c(10, 130), ylab = "Uptake rate (weight/h)",
	xlab = "Substrate  concentration (mmol ~ m^-3)")
#
# expresion for model function
#
expMM<- function(conc, K, Vm){  
           Vm*conc/(K+conc)
}
K=coef(L.minor.m1)[1]
Vm=coef(L.minor.m1)[2]
curve(expMM(x, K=K, Vm =Vm), add = TRUE, col="blue")
curve(expMM(x, K, Vm), add = TRUE, col="red")

L.minor.m3 <- nls(rate ~ expMM(conc,K,Vm), data = L.minor, start = list(K = 20, Vm = 120), trace = TRUE)
summary(L.minor.m3)
plot(rate ~ conc, data = L.minor,
	ylim = c(10, 130), ylab = "Uptake rate (weight/h)",
	xlab = "Substrate  concentration (mmol ~ m^-3)")
K=coef(L.minor.m3)[1]	
Vm=coef(L.minor.m3)[2]	

curve(expMM(x, K=K, Vm =Vm), add = TRUE)

#################################################

# Built-in self-starter functions for nls()
#Now we will repeat the model fit using the self-starter function 
#               SSmicmen(),
#which provides a convenient way of fitting the Michaelis-Menten model       
#without having to bother about starting values    

L.minor.m2 <- nls(rate ~ SSmicmen(conc, Vm, K), data = L.minor) 
summary(L.minor.m2)

# Defining a self-starter function for nls()

 
# y=f(x,b,y0)) = y0*exp(x/b)
#The parameter y0 is the initial amount of the radioactive substance (at time x = 0).

expModel <- function(predictor,b, y0) {
		y0*exp(predictor/b)
}
 
