require(mda)
data(iris)
attach(iris)

pairs(iris)
boxplot(iris)
x=boxplot(Sepal.Length~as.factor(Species),plot=T)
boxplot(Sepal.Length~as.factor(Species))


irisfit <- fda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
#irisfit <- fda(Species ~ ., data = iris)
irisfit

irisfit$confusion
confusion(irisfit)
##
## Training Misclassification Error: 0.02 ( N = 150 )
##
confusion(irisfit, iris)
coef(irisfit)
plot(irisfit)
par(mfrow=c(1,2))
plot(iris)
#
# Formirane na training and testing (validating) data sets
#

training <- sample(1:nrow(iris),130)
training_iris = iris[training, ]
table(iris$Species[training])
sort(training)

testing_iris=as.data.frame(iris[-training,])
table(testing_iris$Species)

irisfit.training.fda <- fda(Species ~ ., data = training_iris)
irisfit.training.fda

##
##confusion(predict(irisfit.training.fda, training_iris), training_iris$Species)
##
predict.training.fda <- predict(irisfit.training.fda, newdata=training_iris)
confusion(predict.training.fda, training_iris$Species)

prob<-predict(irisfit.training.fda,data=training_iris,type="posterior")
cbind(prob,training_iris[,"Species"])
class<-predict(irisfit.training.fda,data=training_iris,type="class")
cbind(class,training_iris[,"Species"])

predFDA <- predict(irisfit.training.fda,newdata=testing_iris)
confusion(predFDA,testing_iris$Species)
#
# predicted group membership for test data
#
table(iris$Species[-training],predFDA) # table with assignments


