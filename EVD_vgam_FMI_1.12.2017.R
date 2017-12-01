require(VGAM)
require(ismev)
data("portpirie")
##
## 	Port Pirie Sea Levels - GEV Model
## 	
#	par(mfrow=c(3,2))	
	data("portpirie")
	fit1.pp <- vglm(SeaLevel ~ 1, gev, data = portpirie)
# trace = TRUE is a good idea
	plot(depvar(fit1.pp) ~ Year, data = portpirie, col = "blue",
			ylab = "Sea level (m)", main = "(a)")
	matlines(with(portpirie, Year), fitted(fit1.pp), col = 1:2, lty = 2)

#The estimated parameters and their standard errors are

	Coef(fit1.pp) # Only for intercept-only models
#	parameter standard deviations	
	(SEs <- sqrt(diag(vcov(fit1.pp, untransform = TRUE))))

# An approximate 95% confidence interval for $xi$ is given by	
	c(Coef(fit1.pp)["shape"] + 1.96 * c(-1, 1) * SEs["shape"])

##
## diagnostic plots  - PP, QQ, Density and Return levels/periods
##
	par(mfrow=c(2,2))	
	n1=dim(portpirie)[1]
	n <- nobs(fit1.pp)
	params3 <- predict(fit1.pp, untransform = TRUE)
	plot(pgev(sort(depvar(fit1.pp)), loc = params3[, "location"],
	scale = params3[, "scale"], shape = params3[, "shape"]),
	ppoints(n), xlab = "Empirical", ylab = "Model", col = "blue",
	main = "(b)")
 	abline(a = 0, b = 1, col = "gray50", lty = "dashed")

	plot(qgev(ppoints(n), loc = params3[, "location"],
	scale = params3[, "scale"], shape = params3[, "shape"]),
	sort(depvar(fit1.pp)), xlab = "Model", ylab = "Empirical", col = "blue",
	main = "(c)")
	abline(a = 0, b = 1, col = "gray50", lty = "dashed")

	hist(depvar(fit1.pp), prob = TRUE, col = "wheat", breaks = 9, main = "(d)")
	Range <- range(depvar(fit1.pp))
	Grid <- seq(Range[1], Range[2], length = 400)
	lines(Grid, dgev(Grid, loc = params3[1, "location"], scale = params3[1, "scale"],
	shape = params3[1, "shape"]), col = "blue")

	
#Now the estimates of the 10-year,  100-year, 500-year and 1000-year 
#return levels can be obtained by G(x_p)=1-p
# return level (quantile): 1-p=1-1/T
# return period: T=1/p za p=0.1,0.01,0.001,...
# return period: T=10,100,500,1000
#
	fit1.pp@extra$percentiles <- c(90, 99,99.8,99.9)
	head(fitted(fit1.pp, type.fitted = "percentiles"), 1)

#To close up, give return level plots produced by
#	rlplot(fit1.pp, main = "(e)", pcol = "blue")
	rlplot(fit1.pp, main = "(f)", pcol = "blue", log = FALSE)	

######################################## 

file="Annual.max.dat"
data=read.table(file=file,header=T)
attach(data)

formula=Porto ~ s(Year, df=3)+s(NHT,df=3)
formula=Porto ~ s(Year, df=3)+s(PDO,df=3)
formula=Porto ~ s(Year, df=3)+s(AMO,df=3)
formula=Porto ~ s(Year, df=3)+s(NAO,df=3)
(fit1 = vgam(formula, family=gev(lshape = logoff(offset = 0.5),zero=3),
			data=data, maxit=50))


summary(fit1)  # 3 of the p-values for linearity are > 0.20
coef(fit1, matrix=TRUE)
Coef(fit1, matrix=TRUE)
head(fitted(fit1))
sqrt(diag(vcov(fit1)))   # Standard errors


par(mfrow=c(2,2), las=1)
plotvgam(fit1, se=TRUE, lcol="blue", scol="darkgreen",
		ylim=c(-0.5,0.8))


formula=Porto ~ 1

(fit1 = vgam(formula, family=gev(lshape = logoff(offset = 0.5),zero=3),
			data=data, maxit=50))
summary(fit1)
(scale=exp(coef(fit1)[2]))
(shape=exp(coef(fit1)[3])-0.5)
coef(fit1, matrix=TRUE)
Coef(fit1)#, matrix=TRUE)
head(fitted(fit1))

sqrt(diag(vcov(summary(fit1))))   # Standard errors

################################################
require(ismev)
data(fremantle)
fm.gev <- gev.fit(fremantle[,2])
gev.diag(fm.gev)
gev.profxi(fm.gev, -0.5, 0.1)
gev.prof(fm.gev, 100, 1.8, 2.2)
covar <- cbind((fremantle[,1] - 1943)/46, fremantle[,3])
gev.fit(fremantle[,2], ydat = covar, mul = 1)
gev.fit(fremantle[,2], ydat = covar, mul = 1, sigl = 1, siglink = exp)
fm.gev2 <- gev.fit(fremantle[,2], ydat = covar, mul = c(1,2))
gev.diag(fm.gev2)
