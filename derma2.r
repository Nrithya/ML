library(randomForest)
library(gmodels)
dermaData=read.csv(file.choose(),sep='\t',header=T)
dermaData=na.omit(dermaData)
dermaData=subset(dermaData[,],(dermaData$Age != "?"))
dermaData$Age=as.integer(dermaData$Age)
dermaData$AgeCategory=as.factor(ifelse(dermaData$Age<10,1,ifelse(dermaData$Age<20,2,ifelse(dermaData$Age<30,3,ifelse(dermaData$Age<40,4,ifelse(dermaData$Age<50,5,ifelse(dermaData$Age<60,6,ifelse(dermaData$Age<70,7,8))))))))
dermaData_age=dermaData[,c(1:34,36,35)]
dermaData=dermaData[,c(1:33,36,35)]

set.seed(1234)
dermaData$Disease=as.factor(dermaData$Disease)
data=sample(nrow(dermaData), 0.75*nrow(dermaData))

train=dermaData[data,]
test=dermaData[-data,]

model=randomForest(train$Disease~., data=train)
model2 = predict(model, newdata=test)
CrossTable(x=model2, y=test$Disease,prop.chisq = FALSE, prop.r=FALSE, prop.c=FALSE,prop.t=FALSE)
plot(model2)


ind=sample(2,nrow(dermaData_age),replace=TRUE, prob=c(0.75, 0.25))
training=dermaData_age[ind==1,1:34]
testing=dermaData_age[ind==2,1:34]           
trainLabels=dermaData_age[ind==1,36]
testLabels=dermaData_age[ind==2,36]

model3=knn(train=training, test=testing, cl=trainLabels,k=3)

CrossTable(x=model3, y=testLabels, prop.chisq = FALSE, prop.r=FALSE, prop.c=FALSE,prop.t=FALSE )

ind=sample(2,nrow(dermaData),replace=TRUE, prob=c(0.75, 0.25))
training=dermaData[ind==1,1:34]
testing=dermaData[ind==2,1:34]           
trainLabels=dermaData[ind==1,35]
testLabels=dermaData[ind==2,35]

model3=knn(train=training, test=testing, cl=trainLabels,k=3)

CrossTable(x=model3, y=testLabels, prop.chisq = FALSE, prop.r=FALSE, prop.c=FALSE,prop.t=FALSE )




