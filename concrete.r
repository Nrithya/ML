library(mclust)
concreteData=read.csv(file.choose(),sep=",",header = T)
ingredients=concreteData[,2:8]
characteristics=concreteData[,9:11]
slump=concreteData[,2:9]
flow=concreteData[,c(2:8,10)]
cs=concreteData[,c(2:8,11)]
table(characteristics$SLUMP.cm)
table(characteristics$FLOW.cm.)
table(characteristics$Compressive.Strength..28.day..Mpa.)
boxplot(characteristics$SLUMP.cm., characteristics$FLOW.cm., main="Variation in Slump and Flow", names=c("Slump in cm", "Flow in cm"), col=c("cyan","yellow"))
boxplot(characteristics$Compressive.Strength..28.day..Mpa., main="Variation in Compressive Strength", xlab="Compressive Strength in MPA", col="green")
clPairs(ingredients,model$classification)
clPairs(ingredients,as.factor(characteristics$FLOW.cm.))
clPairs(ingredients,characteristics$SLUMP.cm.)


clPairs(characteristics)

model=Mclust(slump)
summary(model)
plot(model,what="classification")
plot(model,what="density")
plot(model,what="BIC")
plot(model,what="uncertainty")
BIC=mclustBIC(slump)
ICL=mclustICL(slump)
summary(BIC)
summary(ICL)
plot(BIC)
plot(ICL)

model=Mclust(cs)
summary(model)
plot(model,what="classification")
plot(model,what="density")
plot(model,what="BIC")
plot(model,what="uncertainty")
BIC=mclustBIC(cs)
ICL=mclustICL(cs)
summary(BIC)
summary(ICL)
plot(BIC)
plot(ICL)

