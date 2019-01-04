library(ggvis)
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill= ~Species)
set.seed(1234)#any number
ind=sample(2,nrow(iris),replace=TRUE, prob=c(0.67, 0.33))
iris.training=iris[ind==1,1:4]
iris.test=iris[ind==2,1:4]           
iris.trainLabels=iris[ind==1,5]
iris.testLabels=iris[ind==2,5]
library(class)
iris_pred=knn(train=iris.training, test=iris.test, cl=iris.trainLabels,k=3)
library(gmodels)
CrossTable(x=iris_pred, y=iris.testLabels,prop.chisq=FALSE)
p=seq(0,1,0.01)
e=-p*log2(p)-(1-p)*log2(1-p)
plot(p,e)
