credit=read.csv(file.choose(),header=T,sep=",")
barplot(table(credit$DEFAULT))

set.seed(1234)
population=sample(nrow(credit), 0.75*nrow(credit))
train=credit[population,]
test=credit[-population,]
library(randomForest)
model=randomForest(train$DEFAULT~., data=train)
pred = predict(model, newdata=test)
table(pred, test$DEFAULT)
(21+3)/nrow(test)
