library(rpart)

tipData=read.csv(file.choose(),header=T,sep = ',')

tipData$Card=as.factor(tipData$Card)
tipData$Joke=as.factor(tipData$Joke)
tipData$None=as.factor(tipData$None)
tipData$Tip=as.factor(tipData$Tip)

tipData$Ad=ifelse(tipData$Ad==1&tipData$Tip==1, "Tip", "No Tip")
tipData$Joke=ifelse(tipData$Joke==1&tipData$Tip==1, "Tip", "No Tip")
tipData$None=ifelse(tipData$None==1&tipData$Tip==1, "Tip", "No Tip")


tip=tipData[2:6]
attach(tip)
model=rpart(Card~., data=tip, method="class",control=rpart.control(minsplit=2, minbucket=1))
printcp(model)
plotcp(model)
rpart.plot(model, uniform=TRUE, main="Decision tree for tip data")
text(model, use.n=TRUE, all=TRUE, cex=.8)

