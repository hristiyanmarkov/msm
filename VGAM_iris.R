# Example 1: fit a multinomial logit model to iris data
# colnames -  Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# Species - setosa, versicolor, virginica
library(VGAM)
data<-read.table("iris.dat",header=T)
attach(data)
# 
#fit = vglm(Species ~ ., multinomial,data=data)# OK
#
#the next command is doing the same
#
#
# nominal
#
fit = vglm(Species ~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, multinomial, data=data)

#
# ordinal
#
fit = vglm(Species ~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,cumulative(parallel=TRUE), data=data)

coef(fit, matrix=TRUE)
summary(fit)
fitted.values(fit)
 
#
#  predcition of new data set:
#
irisnew<-read.table("irisnew.dat",header=T)
 
#
#  values of  the linear predcitor
#
#(q3 = predict(fit, newdata=irisnew))
#
#  predicted probablities
#
(p3 = predict(fit, type="res", newdata=irisnew))

