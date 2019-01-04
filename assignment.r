library(class)
library(gmodels)

overdrawData=read.csv(file.choose(), sep=",", header=T)
overdrawData=overdrawData[,2:5]
overdrawData=na.omit(overdrawData)
overdrawData$DaysDrinkCategory=as.factor(ifelse(overdrawData$DaysDrink<7,0, ifelse(overdrawData$DaysDrink<14,1,2)))
cor(overdrawData)


overdrawData$Sex=as.factor(overdrawData$Sex)
overdrawData$Overdrawn=as.factor(overdrawData$Overdrawn)
overdrawData$DaysDrink=as.integer(overdrawData$DaysDrink)


set.seed(1234)
train = overdrawData[sample(nrow(overdrawData),328),]
test=overdrawData[sample(nrow(overdrawData),109),]

#Logit
model1=glm(formula=Overdrawn~(Age+Sex+DaysDrinkCategory),family = binomial(link="logit"),data=train)
results=predict(model1,newdata = test,type="response")
results=ifelse(results>0.5,1,0)
CrossTable(x=results, y=test$Overdrawn,prop.chisq=FALSE)


#KNN
model2=knn(train=train, test=test, cl=train$Overdrawn,k=3)
CrossTable(x=model2, y=test$Overdrawn,prop.chisq=FALSE)
