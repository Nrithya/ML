golf=read.csv(file.choose(),header=T,sep = ',')
library(rpart)
attach(golf)
model=rpart(PlayGolf~.,data=golf, method="class", control=rpart.control(minsplit=2, minbucket=1))
printcp(model)
plotcp(model)
plot(model, uniform=TRUE, main="Decision tree for golf data")
text(model, use.n=TRUE, all=TRUE, cex=.8)

