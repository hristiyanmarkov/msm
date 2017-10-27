Chwirut2=read.table("Chwirut2.dat",skip=1,header=T)
attach(Chwirut2)                       

plot(y ~ x, data = Chwirut2, xlab = "Metal distance", ylab = "Ultrasonic response")

# model f(x,b1,b2,b3)=exp(-b1*x)/(b2+b3*x)
#For x = 0, the model gives the value 1/b2, and thus b2 can be taken to be the reciprocal
# of the response value closest to the y axis, which is roughly  1/100 = 0.01.
#
expFct <- function(x, beta1, beta2, beta3) {  
		exp(-beta1 * x)/(beta2 + beta3 * x)
}

plot(y ~ x, data = Chwirut2, xlab = "Metal distance",
	ylab = "Ultrasonic response", ylim = c(0, 100))                                  
curve(expFct(x, beta1 = 0.1, beta2 = 0.01, beta3 =   1), add = TRUE, lty = 2)
curve(expFct(x, beta1 = 0.1, beta2 = 0.01, beta3 = 0.1), add = TRUE, lty = 3)
curve(expFct(x, beta1 = 0.1, beta2 = 0.01, beta3 = 0.01), add = TRUE, lty = 4)
curve(expFct(x, beta1 = 0.2, beta2 = 0.01, beta3 = 0.01), add = TRUE, lty = 1)
                        
# Estimation 

Chwirut2.m1 <- nls(y ~ expFct(x, beta1, beta2, beta3), data = Chwirut2,                             
		start = list(beta1 = 0.2, beta2 = 0.01, beta3 = 0.01)) 
		
summary(Chwirut2.m1)     
beta1=coef(Chwirut2.m1)[1]
beta2=coef(Chwirut2.m1)[2]
beta3=coef(Chwirut2.m1)[3]

#plot(y ~ x, data = Chwirut2, xlab = "Metal distance",
#	ylab = "Ultrasonic response", ylim = c(0, 100))                                  
curve(expFct(x, beta1 = beta1, beta2 =beta2, beta3 = beta3), add = TRUE,col="blue")

# The result of seq(0.1, 1, by=0.1) is a vector of length 10 (the numbers from 0.1 to 1 in steps of 0.1  
xxx <- with(Chwirut2, seq(min(x), max(x), length.out = 54))
predict(Chwirut2.m1, data.frame(x = xxx)) 

cor(Chwirut2$y,fitted(Chwirut2.m1))   
coef(Chwirut2.m1)
qqplot(Chwirut2$y,fitted(Chwirut2.m1))