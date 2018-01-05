data(iris)
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
 
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE) 
print(ir.pca)
plot(ir.pca, type = "l")

summary(ir.pca)

predict(ir.pca,newdata=tail(log.ir, 5))
biplot(ir.pca)

########################## wine data
require(ggbiplot)
data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
summary(wine.pca)
print(ggscreeplot(wine.pca))    
print(ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = wine.class, ellipse = TRUE, circle = TRUE))

                                           
print(ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, 
		groups = , ellipse = TRUE, circle = TRUE))

############ Factor analysis of wine data
wines.fa <- factanal(wine, factors =3,  scores = "regression")
wines.fa

f.obj=factanal(wine, factors = 2, rotation = "promax")
print(f.obj$loadings,cutoff = 0.4)


###########iris data 
data(iris)
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
 
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,  scale. = TRUE) 
summary(ir.pca)
print(ggscreeplot(ir.pca))
print(ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species, ellipse = TRUE, circle = TRUE))
plot(ir.pca, type = "l")

summary(ir.pca)

###########aero data 
file ="OBSERV.txt"
data=read.table(file=file,header=T,sep=",")
adata=data[,c(2:103)]
adata=na.omit(adata)

#factanal(adata, factors = 3) # varimax is the default
factanal(adata, factors = 5, rotation = "promax")
# The following shows the g factor as PC1
#prcomp(adata) # signs may depend on platform

adata.pca <- prcomp(adata,  scale. = TRUE)
summary(adata.pca)
print(ggscreeplot(adata.pca))
print(ggbiplot(adata.pca, obs.scale = 1, var.scale = 1,  ellipse = TRUE, circle = TRUE))
plot(adata.pca, type = "l")

summary(adata.pca)


#
#PCA on caret package 
#
require(caret)  
#
# first apply a Box-Cox transformation to correct for skewness, center and scale each variable and then apply PCA                                                                       
#
trans = preProcess(iris[,1:4], method=c("BoxCox", "center", "scale", "pca"))    
PC = predict(trans, iris[,1:4])   

# Retained PCs              
head(PC, 3)   

############################princom
require(MASS)
require(robustbase)
data(stackloss)
(pc.cl  <- princomp(stackloss))
(pc.robase <- princomp(stackloss, covmat = robustbase::covMcd(stackloss)))
summary(pc.cl)
summary(pc.robase)

loadings(pc.cl)  # note that blank entries are small but not zero
loadings(pc.robase)  # note that blank entries are small but not zero
## The signs of the columns are arbitrary
par(mfrow=c(1,2))
plot(pc.cl) # shows a screeplot.
plot(pc.robase) # shows a screeplot.
## The signs of the columns are arbitrary
biplot(pc.cl)
biplot(pc.robase)

###################################

data(iris)
(pc.cl  <- princomp(iris[,1:4]))
(pc.robase <- princomp(iris[,1:4], covmat = robustbase::covMcd(iris[,1:4])))
summary(pc.cl)
summary(pc.robase)

loadings(pc.cl)  # note that blank entries are small but not zero
loadings(pc.robase)  # note that blank entries are small but not zero
## The signs of the columns are arbitrary
par(mfrow=c(1,2))
plot(pc.cl) # shows a screeplot.
plot(pc.robase) # shows a screeplot.
## The signs of the columns are arbitrary
biplot(pc.cl)
biplot(pc.robase)

library(devtools)                 
#install_github("ggbiplot", "vqv")
require(ggbiplot)

g <- ggbiplot(pc.cl, obs.scale = 1, var.scale = 1, 
              groups = ir.species, ellipse = TRUE,  
              circle = TRUE)                        
g <- g + scale_color_discrete(name = '')            
g <- g + theme(legend.direction = 'horizontal',     
               legend.position = 'top')             
print(g)      

g <- ggbiplot(pc.robase, obs.scale = 1, var.scale = 1, 
              groups = ir.species, ellipse = TRUE,  
              circle = TRUE)                        
g <- g + scale_color_discrete(name = '')            
g <- g + theme(legend.direction = 'horizontal',     
               legend.position = 'top')             
print(g)      

#
# let us contaminate iris data by outliers
#
head(iris)
irisO=iris
irisO[1,"Sepal.Width"]=30.5
irisO[2,"Sepal.Width"]=30.0
irisO[3,"Sepal.Length"]=40.7
irisO[4,"Sepal.Length"]=40.6
(pc.cl.O  <- princomp(irisO[,1:4]))
(pc.robase.O <- princomp(irisO[,1:4], covmat = robustbase::covMcd(irisO[,1:4])))

irisO.covMcd=covMcd(irisO[,1:4],alpha=0.9)
irisO.covMcd$mcd.wt
# Todorov & Filzmoser robust principal components package
(pc.robase.O <- princomp(irisO[,1:4], covmat = )
mcd.wt
summary(pc.cl.O)
summary(pc.cl)
summary(pc.robase.O)
summary(pc.robase)

############################### LDA
require(MASS)
data(iris)
head(iris, 3)    

iris.lda <- lda(formula = Species ~ ., data = iris,prior = c(1,1,1)/3)
summary(iris.lda) 
iris.lda$counts 
iris.lda$means 
iris.lda$scaling
iris.lda$svd
prop = iris.lda$svd^2/sum(iris.lda$svd^2)
prop

r2 <- lda(formula = Species ~ ., 
          data = iris, 
          prior = c(1,1,1)/3,
          CV = TRUE)
head(r2$class)
head(r2$posterior, 3)


train <- sample(1:150, 75)

r3 <- lda(Species ~ ., # training model
         iris, 
         prior = c(1,1,1)/3, 
         subset = train)

plda = predict(object = r, # predictions
               newdata = iris[-train, ])
head(plda$posterior, 3) # posterior prob.
head(plda$x, 3) # LD projections

> head(plda$class) # classification result