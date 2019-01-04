#import library
library(randomForest)

#import data
credit=read.csv(file.choose(),header=T,sep=",")

#plot
barplot(table(credit$DEFAULT))

#prepare data
credit$DEFAULT=as.factor(credit$DEFAULT)

#sample
set.seed(1234)
population=sample(nrow(credit), 0.75*nrow(credit))

#set test and train set
train=credit[population,]
test=credit[-population,]

#random forest
model=randomForest(train$DEFAULT~., data=train)

#prediction
pred = predict(model, newdata=test)
table(pred, test$DEFAULT)

