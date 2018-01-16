library(quantreg)

data(barro)
fit.int <- rq(y.net ~  1 , data = barro)
fit.lgdp2 <- rq(y.net ~  lgdp2  , data = barro)
anova(fit.int,fit.lgdp2)
fit.fse2 <- rq(y.net ~  fse2  , data = barro)
anova(fit.int,fit.fse2)

fit.gedy2 <- rq(y.net ~  gedy2  , data = barro)
anova(fit.int,fit0)

fit0 <- rq(y.net ~  lgdp2 + fse2 + gedy2 , data = barro)
anova(fit.int,fit0)

fit1 <- rq(y.net ~  lgdp2 + fse2 + gedy2 + Iy2 + gcony2, data = barro)
fit2 <- rq(y.net ~  lgdp2 + fse2 + gedy2 + Iy2 + gcony2, data = barro,tau=.75)
fit3 <- rq(y.net ~  lgdp2 + fse2 + gedy2 + Iy2 + gcony2, data = barro,tau=.25)
anova(fit1,fit0)
anova(fit1,fit2,fit3)
anova(fit1,fit2,fit3,joint=FALSE)
# Alternatively fitting can be done in one call:
fit <- rq(y.net ~  lgdp2 + fse2 + gedy2 + Iy2 + gcony2, 
	  method = "fn", tau = 1:4/5, data = barro)

summary(fit1)
summary(fit[[1]])
#
# Linear quantile regression model
#
file="TminTmax.dat"
data=read.table(file=file,header=T)
attach(data)

head(data)
tail(data)

#Inter-annual qunatile trend (within the year) - daily data
#
data=cbind(data,sin1=sin(2*pi*days/365.25),cos1=cos(2*pi*days/365.25))
#
#Inta-annual qunatile trend (between the years)
#
case=1:nrow(data)
data=cbind(data,sin2=sin(2*pi*case/nrow(data)),cos2=cos(2*pi*case/nrow(data)))


lm.out.1=lm(Tmax ~ sin1+cos1,data=data)
summary(lm.out.1)
anova(lm.out.1,test=F)

lm.out=lm(Tmax ~ sin1+cos1+sin2+cos2,data=data)
summary(lm.out)
anova(lm.out,test=F)
anova(lm.out.1,lm.out)
####################
rq.out.25.1=rq(Tmax ~ sin1+cos1,data=data,tau=0.25) 
summary(rq.out.25.1)

rq.out.25=rq(Tmax ~ sin1+cos1+sin2+cos2,data=data,tau=c(0.25)) 
summary(rq.out.25)
anova(rq.out.25,rq.out.25.1)
####################

rq.out.50=rq(Tmax ~ sin1+cos1+sin2+cos2,data=data,tau=c(0.5)) 
summary(rq.out.50)
rq.out.75=rq(Tmax ~ sin1+cos1+sin2+cos2,data=data,tau=c(0.75)) 
summary(rq.out.75)
rq.out.90=rq(Tmax ~ sin1+cos1+sin2+cos2,data=data,tau=c(0.9)) 
summary(rq.out.90)

plot(days,Tmax)
lines(lm.out$fitted,col="red",lty=2)
lines(rq.out.25$fitted,col="blue",lty=2)
lines(rq.out.75$fitted,col="blue",lty=2)
lines(rq.out.50$fitted,col="green",lty=2)
lines(rq.out.90$fitted,col="yellow",lty=2)

plot(Tmax[1:3000])
lines(rq.out.90$fitted.values[1:3000],col="blue")
lines(lm.out$fitted.values[1:3000],col="red")
lines(rq.out.25$fitted.values[1:3000],col="green")

#
# Monthly plot
#
data=cbind(data,sin3=sin(2*pi*mm/12),cos3=cos(2*pi*mm/12))
rq.out.mm.75=rq(Tmax ~ sin3+cos3,data=data,0.75)  
rq.out.mm.50=rq(Tmax ~ sin3+cos3,data=data,0.50)
rq.out.mm.25=rq(Tmax ~ sin3+cos3,data=data,0.25)
  
plot(mm,Tmax)
points(mm, rq.out.mm.25$fitted.values,col="red")
points(mm, rq.out.mm.50$fitted.values,col="yellow")
points(mm, rq.out.mm.75$fitted.values,col="green")

#
############# Nonlinear quantile regression
# build artificial data with multiplicative error
Dat <- NULL; 
Dat$x <- rep(1:25, 20)
set.seed(1)
#
#         Self-Starting Nls Logistic Model - 
#
#    SSlogis(input, Asym, xmid, scal)=Asym/(1+exp((xmid-input)/scal)
#

Dat$y <- SSlogis(Dat$x, 10, 12, 2)*rnorm(500, 1, 0.1)
plot(Dat)
# fit first a nonlinear least-square regression
Dat.nls <- nls(y ~ SSlogis(x, Asym, mid, scal), data=Dat)
summary(Dat.nls)
lines(1:25, predict(Dat.nls, newdata=list(x=1:25)), col=1)

# fit the median using nlrq
Dat.nlrq <- nlrq(y ~ SSlogis(x, Asym, mid, scal), data=Dat, tau=0.5, trace=TRUE)
summary(Dat.nlrq)
lines(1:25, predict(Dat.nlrq, newdata=list(x=1:25)), col=2)

# the 1st and 3rd quartiles regressions
Dat.nlrq <- nlrq(y ~ SSlogis(x, Asym, mid, scal), data=Dat, tau=0.25, trace=TRUE)
summary(Dat.nlrq)
lines(1:25, predict(Dat.nlrq, newdata=list(x=1:25)), col=3)
Dat.nlrq <- nlrq(y ~ SSlogis(x, Asym, mid, scal), data=Dat, tau=0.75, trace=TRUE)
summary(Dat.nlrq)
lines(1:25, predict(Dat.nlrq, newdata=list(x=1:25)), col=3)

# and finally "external envelopes" holding 95 percent of the data
Dat.nlrq <- nlrq(y ~ SSlogis(x, Asym, mid, scal), data=Dat, tau=0.025, trace=TRUE)
summary(Dat.nlrq)
lines(1:25, predict(Dat.nlrq, newdata=list(x=1:25)), col=4)

Dat.nlrq <- nlrq(y ~ SSlogis(x, Asym, mid, scal), data=Dat, tau=0.975, trace=TRUE)
summary(Dat.nlrq)
lines(1:25, predict(Dat.nlrq, newdata=list(x=1:25)), col=4)
leg <- c("least squares","median (0.5)","quartiles (0.25/0.75)",".95 band (0.025/0.975)")
legend(1, 12.5, legend=leg, lty=1, col=1:4)

##### alternatively
taus=c(0.025,0.25,0.5,0.75,0.975)
for(i in  seq(along = taus)) {
Dat.nlrq[[i]] <- nlrq(y ~ SSlogis(x, Asym, mid, scal), data=Dat, 
		tau=taus[i], trace=TRUE)
}
summary(Dat.nlrq[[1]])
summary(Dat.nlrq[[2]])
summary(Dat.nlrq[[3]])
summary(Dat.nlrq[[4]])
summary(Dat.nlrq[[5]])

#####################Engel data  example from quantreg package
#plot of engel data and some rq lines see KB(1982) for references to data
data(engel)
attach(engel)
plot(income,foodexp,xlab="Household Income",ylab="Food Expenditure",type = "n", cex=.5)
points(income,foodexp,cex=.5,col="blue")
taus <- c(.05,.1,.25,.75,.9,.95)
xx <- seq(min(income),max(income),100)
f <- coef(rq((foodexp)~(income),tau=taus))
yy <- cbind(1,xx)%*%f
for(i in 1:length(taus)){
        lines(xx,yy[,i],col = "gray")
        }
lm.out=lm(foodexp ~ income)
summary(lm.out)        
abline(lm.out,col="red",lty = 2)
abline(rq(foodexp ~ income), col="blue",tau=0.5)
legend(3000,500,c("mean (LSE) fit", "median (LAE) fit"),
	col = c("red","blue"),lty = c(2,1))
#Example of plotting of coefficients and their confidence bands
plot(summary(rq(foodexp~income,tau = 1:49/50,data=engel)))

#
#       open Engel Data example from quantreg package
#
#data(engel) Engel Data log10 data transformation 

plot(engel, log = "xy",  main = "'engel' data  (log - log scale)")
plot(log10(foodexp) ~ log10(income), data = engel,
     main = "'engel' data  (log10 - transformed)")
#taus <- c(.15, .25, .50, .75, .95, .99)
taus <- c(.05,.1,.25,.75,.9,.95)
rqs <- as.list(taus)
for(i in seq(along = taus)) {
  rqs[[i]] <- rq(log10(foodexp) ~ log10(income), tau = taus[i], data = engel)
  lines(log10(engel$income), fitted(rqs[[i]]), col = i+1)
}
legend("bottomright", paste("tau = ", taus), inset = .04,
       col = 2:(length(taus)+1), lty=1)

##################
