golf=read.csv(file.choose(), header=T, sep=",")
library("e1071")
library(caret)
set.seed(1234)
id=sample(2,nrow(golf), prob=c(0.7,0.3), replace=T)
golfTrain=golf[id==1,]
golfTest=golf[id==2,]
model=naiveBayes(PlayGolf~.,data=golfTrain)
pred=predict(model,newdata=golfTest)
confusionMatrix(pred, golfTest$PlayGolf)
cancer=read.csv(file.choose(), header=T, sep=",")
set.seed(1234)
id=sample(2,nrow(cancer), prob=c(0.7,0.3), replace=T)
cancerTrain=cancer[id==1,]
cancerTest=cancer[id==2,]
cancerModel=naiveBayes(Diagnosis~., data=cancerTrain)
prediction=predict(cancerModel, newdata=cancerTest)
confusionMatrix(prediction,cancerTest$Diagnosis)
