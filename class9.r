library(mclust)
concreteData=read.csv(file.choose(),sep=",",header = T)
class.d=diabetes$class
table(class.d)
X=diabetes[,-1]
clPairs(X,model$class)
model=Mclust(X)
summary(model)
plot(model,what="BIC")
plot(model,what="density")
BIC=mclustBIC(X)
summary(BIC)
print(model$classification)
