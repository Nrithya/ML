abalone=read.csv(file.choose(),header=T,sep=",")
barplot(table(credit$DEFAULT))
abalone_small=subset(abalone, (abalone$Rings>2) & (abalone$Rings<24))
abalone_small$Rings=as.factor(abalone_small$Rings)
set.seed(415)
population=sample(nrow(abalone_small), 0.75*nrow(abalone_small))
train=abalone_small[population,]
test=abalone_small[-population,]

library(randomForest)
model=randomForest(train$Rings~., data=train)
pred = predict(model, newdata=test)
table(pred, test$Rings)

