#
# This is a procedure that implement the methodology described in [1] using 
# the Least Trimmed Squares (LTS) regression and Robust distances (RD). 
# The  RD are based on  Minimum Covariance Determinant estimator (MCD).
# The computation are carried out by the "rrcov" R package developed by Todorov (2004). 
# The "rrcov" functions "ltsReg" and "mcdCov" implement the  
# the FAST-LTS and FAST-MCD algorithms developed in [2] and [3]. 
#
#  [1]	P.J.Rousseeuw and van Zomeren, B. C. (1990). Unmasking Multivariate Outliers and Leverage Points. 
#	Journal of the American Statistical Association 85, 633-639. 
#
#  [2]	P. J. Rousseeuw and K. van Driessen (1999)A fast algorithm for the minimum covariance determinant estimator. 
#  	Technometrics 41, 212–223. 
#
#  [3]	P. J. Rousseeuw and K. van Driessen (1999) Computing LTS Regression for Large Data Sets, Technical Report, 
#       University of Antwerp, submitted 
#
#  [4]  V. K. Todorov (2004). Scalable Robust Estimators with High Breakdown Point. Reference manual: 
#       http://cran.R-project.org/doc/packages/rrcov.pdf.  
# 

#
#P. J. Rousseeuw and A. M. Leroy (1987) Robust Regression and Outlier Detection; Wiley, page 26, table 2.
#

################################### starsCYG

starsCYG <- read.table("starsCYG.dat",header=T)
require(robustbase)
attach(starsCYG)
plot(starsCYG)
pairs(starsCYG)
boxplot(starsCYG)

lm.starsCYG <- lm(log.light ~ log.Te, data = starsCYG)
summary(lm.starsCYG)
anova(lm.starsCYG,test="F")

coef(lm.starsCYG)
confint(lm.starsCYG, level = 0.95)

plot(log.light ~ log.Te)
abline(lm.starsCYG)
abline(lm.starsCYG[1],lm.starsCYG$coefficients[2])

############################# LTS 
                
#require(robustbase)
lts.starsCYG <- ltsReg(log.light ~ log.Te, data = starsCYG)
summary(lts.starsCYG)
abline(coef(lts.starsCYG),col="blue")
abline(coef(lts.starsCYG)[1],coef(lts.starsCYG)[2],col="blue")

coef(lts.starsCYG)

plot(lts.starsCYG)

#
# Regression diagnostics and outliers classification
#

mcd.starsCYG<-covMcd(starsCYG)
plot(mcd.starsCYG,classic=T)

#
#
# One-step improuvment: Re-weighted Least Squares based on the LTS weights
#
OneStep.lts.starsCYG <- lm(log.light ~ log.Te, data = starsCYG, weight=lts.starsCYG$lts.wt)
summary(OneStep.lts.starsCYG)
anova(OneStep.lts.starsCYG)
plot(starsCYG)
abline(OneStep.lts.starsCYG,col="red")
abline(coef(OneStep.lts.starsCYG),col="red")


par(mfcol=c(2,2),mar=c(3,4,4,1))
plot(OneStep.lts.starsCYG)                    
bringToTop()

#
# Data matrix output of the outliers classification based on robust distances 
# and the LTS output
#
lts.starsCYG$scale 
    xn <- dim(starsCYG)[1]
    xp <- dim(starsCYG)[2]-1
    chi <- qchisq (0.975, xp, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    cval <- sqrt (chi)
RDw <- ifelse (lts.starsCYG$RD <= cval, 1, 0)
mode <- 2 * RDw + lts.starsCYG$lts.wt
#
# BLP - bad leverage point
# GLP - good leverage point
#  VO - vertical outlier
#  OK - regular obsevations
#
mode<-as.matrix(mode)
mode=apply(mode,1,function(x)  c("BLP", "GLP", "VO", "OK")[x+1])
mode=apply(mode,1,function(x)  c("BLP", "GLP", "VO", "OK"))

outliers=cbind(starsCYG,lts.starsCYG$lts.wt,RDw,mode)
print(outliers)

OneStep.lts.starsCYG <- lm(log.light ~ log.Te, data = starsCYG, weight=RDw)
summary(OneStep.lts.starsCYG)
anova(OneStep.lts.starsCYG)
abline(coef(OneStep.lts.starsCYG)[1],coef(OneStep.lts.starsCYG)[2],col="red")

par(mfcol=c(2,2),mar=c(3,4,4,1))
plot(lts.starsCYG, which = "rqq")
plot(lts.starsCYG, which = "rindex")
plot(lts.starsCYG, which = "rfit")
plot(lts.starsCYG, which = "rdiag")


##############Quantile regression   
require(robustbase)
require(quantreg)
data(starsCYG)
attach(starsCYG)
plot(starsCYG)
cst <- covMcd(starsCYG)

###################### Ima greshka !!! 28 February 2011

taus <- c(.05,.1,.25,.75,.9,.95)
xx <- seq(min(log.Te),max(log.Te),100)
for(i in 1:length(taus)){
f <- coef(rq((log.light)~(log.Te),tau=taus, weights=lts.starsCYG$lts.wt))
yy <- cbind(1,xx)%*%f
#        lines(xx,yy[,i],col = "gray")
abline(rq(log.light ~ log.Te), col="blue")
        }
abline(lm(log.light ~ log.Te),col="red",lty = 2)
abline(rq(log.light ~ log.Te), col="blue")

legend(3000,500,c("mean (LSE) fit", "median (LAE) fit"),
    col = c("red","blue"),lty = c(2,1))
lts.stars <- ltsReg(log.light ~ log.Te, data = starsCYG)
abline(lts.stars <- ltsReg(log.light ~ log.Te, data = starsCYG),col="green")

#lm.stars <- lm(log.light ~ log.Te, data = starsCYG)
#summary(lm.stars)
#plot(lm.stars)
#plot(lts.stars) 

#############################
                
require(robustbase)
data(hbk)
pairs(hbk)
plot(hbk)
lm.hbk <- lm(Y ~ ., data = hbk)
summary(lm.hbk)
anova(lm.hbk)

Sum.SQ=359.49
Mean.Sq=Sum.SQ/71
Mean.Sq
print(Mean.Sq)

#Residual standard error
print(sqrt(Mean.Sq))

coef(lm.hbk)

par(mfcol=c(2,2),mar=c(3,4,4,1))
plot.lm(lm.hbk)                    
bringToTop()


lts.hbk <- ltsReg(Y  ~ ., data = hbk)
summary(lts.hbk)

coef(lts.hbk)
plot(lts.hbk)

#
# Regression diagnostics and outliers classification
#

mcd.hbk<-covMcd(hbk)
plot(mcd.hbk,classic=T)


#
# Data matrix output of the outliers classification based on robust distances 
# and the LTS output
#
lts.hbk$scale 
    xn <- dim(hbk)[1]
    xp <- dim(hbk)[2]-1
    chi <- qchisq (0.975, xp, ncp = 0, lower.tail = TRUE, log.p = FALSE)
    cval <- sqrt (chi)
RDw <- ifelse (lts.hbk$RD <= cval, 1, 0)
mode <- 2 * RDw + lts.hbk$lts.wt
#
# BLP - bad leverage point
# GLP - good leverage point
#  VO - vertical outlier
#  OK - regular obsevations
#
mode<-as.matrix(mode)
mode=apply(mode,1,function(x)  c("BLP", "GLP", "VO", "OK")[x+1])

outliers=cbind(hbk,lts.hbk$lts.wt,RDw,mode)
print(outliers)


####################### tellephone calls

require(car)
data(telef)
attach(telef)

# classical LSE
summary(lm.telef <- lm(Year~Calls, data=telef))
anova(lm.telef)
plot(Year~Calls, data=telef)
abline(lm.telef,col="red")
reg.line(lm.telef <- lm(Year~Calls, data=telef))

# robust LTS

ltsReg.telef<- ltsReg(Year~Calls, data=telef)
summary(ltsReg.telef) 

abline(coef(ltsReg.telef), lty=2,col="blue")
plot(ltsReg.telef, which = "all")

plot(ltsReg.telef, which = "rqq")
plot(ltsReg.telef, which = "rindex")
plot(ltsReg.telef, which = "rfit")
plot(ltsReg.telef, which = "rdiag")
