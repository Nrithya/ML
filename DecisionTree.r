
#import libraries
library(rpart)

#import data
golf=read.csv(file.choose(),header=T,sep = ',')

#set search path
attach(golf)

#decision tree
model=rpart(PlayGolf~.,data=golf, method="class", control=rpart.control(minsplit=2, minbucket=1))

#print & plot tree summary
printcp(model)
plotcp(model)

plot(model, uniform=TRUE, main="Decision tree for golf data")
text(model, use.n=TRUE, all=TRUE, cex=.8)

