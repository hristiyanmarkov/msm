require(hett)


n = 1000	# numb of obs
set.seed(98375)
A =  runif(n,0,1)	#uniform
B =  runif(n,0,1)	#uniform
C =  runif(n,0,1)	#standard normal
mu = (1 + 4*A + 4*B)	#generate  random distribution parameters
sd = sqrt(exp(1 + 2*A + 2*B + 3*C))
Y = rnorm(n, mu, sd)	#generate regular obsevations

data=cbind(Y=Y, A=A, B=B, C=C)
data=as.data.frame(data)

plot(mu,Y)

lm.mu <- lm(Y ~ A + B,data=data)
print(summary(lm.mu))

	par(mfrow=c(2,2))
	plot(lm.mu)

tfit <- tlm(Y ~ A + B, data = data, start = list(dof = 4))
summary(tfit)
#tsum(tfit$loc.fit, dispersion = 1)
#tsum(tfit$scale.fit, dispersion = 1)


#
# if nu very large then t distribution reduces to the normal distribution
#
tfitN <- tlm(Y ~ A + B,  ~ A + B + C, data = data, start = list(dof = 100))
summary(tfitN)
#tsum(tfitN$loc.fit, dispersion = 1)

tfit2 <- tlm(Y ~ A + B,  ~ A + B + C, data = data, start = list(dof = 3),estDof = TRUE)
summary(tfit2)
#tsum(tfit2$loc.fit, dispersion = 2)
#tsum(tfit2$scale.fit, dispersion = 1)

#
# Score test for heteroscedastic t models
#
tfit1 <- tlm(Y ~ A + B , ~ 1, data = data, start = list(dof = 3),estDof = TRUE)

tfit2 <- tlm(Y ~ A + B, ~ A + B + C, data = data, start = list(dof=3), estDof = TRUE)

tscore(tfit1, tfit2, data = data, scale = TRUE)

###################### stack loss and salinity data
require(robustbase)
require(stats)
data(stackloss)
summary(lm.stack <- lm(stack.loss ~.,data=stackloss))

par(mfrow=c(2,2))
plot(lm.stack)

tfit.stack <- tlm(stack.loss ~., data = stackloss, start = list(dof = 4))
summary(tfit.stack)

tfit.stack <- tlm(stack.loss ~., data = stackloss, start = list(dof = 4),estDof = TRUE)
summary(tfit.stack)

tfit1.stack <- tlm(stack.loss ~.,~., data = stackloss, start = list(dof = 3),estDof = TRUE)
summary(tfit1.stack)


lts.stack <- ltsReg(stack.loss ~., data = stackloss)
summary(lts.stack)
plot(lts.stack, which = c("rindex"))
plot(lts.stack, which = c("rqq"))
plot(lts.stack, which = "rdiag")

mcd.stack<-covMcd(stackloss[,c(2,3)])
plot(mcd.stack,classic=T)

mcd.stack<-covMcd(stackloss[,c(2,4)])
plot(mcd.stack,classic=T)
mcd.stack<-covMcd(stackloss[,c(3,4)])
plot(mcd.stack,classic=T)



#################
data(salinity)
plot(salinity)
summary(lm.sali  <-        lm(Y ~ . , data = salinity))

lts.sali <-    ltsReg(Y ~ . , data = salinity)

par(mfrow=c(2,2))
plot(lts.sali, which = c("rindex"))
plot(lts.sali, which = c("rqq"))
plot(lts.sali, which = "rdiag")

salinity.x <- data.matrix(salinity[, c(1,3)])
c_sal <- covMcd(salinity.x)
plot(c_sal, "tolEllipsePlot")

salinity.x <- data.matrix(salinity[, c(1,2)])
c_sal <- covMcd(salinity.x)
plot(c_sal, "tolEllipsePlot")

salinity.x <- data.matrix(salinity[, c(2,3)])
c_sal <- covMcd(salinity.x)
plot(c_sal, "tolEllipsePlot")


 