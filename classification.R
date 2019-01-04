library(anytime)
library(randomForest)
library(gmodels)
library(class)

#import data
ted_main_raw=read.csv(file.choose(), header=TRUE, sep=",")
ratings=read.csv(file.choose(), header=TRUE, sep=",")

#remove na
ted_main=na.omit(ted_main_raw)

#create viewer sentiment metric
attach(ratings)
data=ratings[,2:15]
ratings$Sentiment=as.factor(colnames(data)[apply(data, 1, which.max)])
ratings$metric=ifelse(ratings$Sentiment == 'Beautiful' | 
                       ratings$Sentiment == 'Fascinating' |
                       ratings$Sentiment == 'Jaw.dropping' |
                       ratings$Sentiment == 'Ingenious' |
                       ratings$Sentiment == 'Inspiring',
                      4, ifelse(ratings$Sentiment == 'Courageous' |
                                  ratings$Sentiment == 'Funny' |
                                  ratings$Sentiment == 'Informative' |
                                  ratings$Sentiment == 'Persuasive',
                                3, ifelse(ratings$Sentiment == 'OK' |
                                            ratings$Sentiment == 'Longwinded',
                                          2, 1))) 
#create new dataset for classification
view_sent=ratings[,-16]
view_sent$comments=ted_main$comments
view_sent$views=ted_main$views
view_sent$duration=ted_main$duration
view_sent$languages=ted_main$languages

ind=sample(2,nrow(view_sent),replace=TRUE, prob=c(0.75, 0.25))
training=view_sent[ind==1,c(2:15,17:20)]
testing=view_sent[ind==2,c(2:15,17:20)]           
trainLabels=view_sent[ind==1,16]
testLabels=view_sent[ind==2,16]

model=knn(train=training, test=testing, cl=trainLabels,k=3)

CrossTable(x=model, y=testLabels, prop.chisq = FALSE, prop.r=FALSE, prop.c=FALSE,prop.t=FALSE )


set.seed(1234)
view_sent$metric=as.factor(view_sent$metric)
view_sent=view_sent[,-1]

data=sample(nrow(view_sent), 0.60*nrow(view_sent))
train=view_sent[data,]
test=view_sent[-data,]

model=randomForest(train$metric~., data=train)
model2 = predict(model, newdata=test)
CrossTable(x=model2, y=test$metric,prop.chisq = FALSE, prop.r=FALSE, prop.c=FALSE,prop.t=FALSE)
plot(model2)

