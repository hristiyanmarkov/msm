require(ismev)
data(rain)
#
#
# Fitting the GPD Model Over a Range of Thresholds
#
rnfitrange=gpd.fitrange(rain, umin=10, umax=40, nint = 20)
attributes(rnfitrange)

#threshold
u=30	
rnfit=gpd.fit(rain, threshold=u)

rnfit$mle[1]
# the number of threshold exceedances
rnfit$nexc
rnfit$rate
#The covariance matrix
(rnfit$cov)
# scale and shape standard error
rnfit$se
# confidence interval based on delta method
c(rnfit$mle[1] + 1.96 * c(-1, 1) * rnfit$se[1])
c(rnfit$mle[2] + 1.96 * c(-1, 1) * rnfit$se[2])
#
# the data are daily so n_y=365, N=100 => m=365*100
#

# covariance matrix
# hatzeta=rnfit$nexc/dim(rain)
# var.hatzeta=hatzeta(1-hatzeta)/dim(rain)
# diagnostic plot
#
(lr=length(rain))
(hat.zeta=rnfit$nexc/lr)
(var.hat.zeta=hat.zeta*(1-hat.zeta)/lr)
(rnfit$cov)
#
aa=cbind(hat.zeta,0,0)
bb=cbind(0,rnfit$cov)
dd=rbind(aa,bb)


#
# return level x_m - 100 return level
#
N=100
n_y=365 
m=N*n_y
sigma=rnfit$mle[1]	
xi=rnfit$mle[2]
xm=u+(sigma/xi)*((m*hat.zeta)^xi-1)

#
# Produce profile log-likelihoods for shape parameters
#
gpd.profxi(rnfit, -0.02, 0.3)
abline(v=0.0, col="blue")
abline(v=0.245, col="blue")
# m - observations
# x_m - m-observation return level (quantile, value at risk)
# Produce profile log-likelihoods for m year/block return levels 
# (quantile, value at risk), i.e. the profile likelihood is for the value 
# that is exceeded with probability 1/m.
#conf = 0.95 #default
gpd.prof(rnfit, m = 10, 55, 80)
abline(v=58.2, col="blue")
abline(v=75.54, col="blue")

gpd.prof(rnfit, m = 100, 55, 80)

gpd.prof(rnfit, m = 100, 75, 134)
abline(v=78.2, col="blue")
abline(v=134, col="blue")

gpd.prof(rnfit, m = 500, 55, 80)

gpd.prof(rnfit, m = 500, 80, 220)
abline(v=92, col="blue")
abline(v=200, col="blue")


gpd.diag(rnfit)

##################################
data(euroex)
plot(euroex,type='l',xlab='Day Number',ylab='Exchange Rate')
# log-daily returns
euroex.ldr <- log(euroex[2:975])-log(euroex[1:974])
plot(euroex.ldr,type='l',xlab='Day Number',ylab='Log-Daily Return')
#re-scale the data
euroex.ldr <- 100*euroex.ldr
# threshold selection
#mrl.plot(euroex.ldr)
#length(euroex.ldr[euroex.ldr>0.9])
#
gpd.fitrange(euroex.ldr,-1,1.4,nint=100)
# support u=0.9
# 365 days without weekends 250 trading days per year
euroex.gpd <- gpd.fit(euroex.ldr,0.9,npy=250)
# diagnostic plot
gpd.diag(euroex.gpd)

gpd.profxi(euroex.gpd,-0.5,0.3)

# m=10 year period, 
gpd.prof(euroex.gpd, m=10,npy=250,1.75,3.5)
abline(v=1.97, col="red")
abline(v=1.76, col="red")
abline(v=2.85, col="red")

# covariates
time=matrix(1:974,ncol=1)
euroex.gpd2 <- gpd.fit(euroex.ldr,u=0.9,npy=250,
		ydat=time,sigl=1,siglink=exp)
