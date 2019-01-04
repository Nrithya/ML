crashData=read.csv(file.choose(),header = T, sep=',')
train = crashData[1:15,]
test=crashData[16:20,]
model=glm(Survived~Age+Speed, family=binomial(link="logit"),data=crashData)
#model2=lm(Survived~(Age+Speed), crashData)
results=predict(model,newdata = test,type="response")
results=ifelse(results>0.5,1,0)
misClassification=mean(results!=test$Survived)
accuracy=1-misClassification
