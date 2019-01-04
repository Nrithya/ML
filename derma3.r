library(cluster)
library(gmodels)
dermaData=read.csv(file.choose(),sep='\t',header=T)
dermaData=na.omit(dermaData)
dermaData=subset(dermaData[,],(dermaData$Age != "?"))
dermaData$Age=as.integer(dermaData$Age)
cor(dermaData)
dermaData$Disease=as.factor(dermaData$Disease)



ggplot(dermaData, aes(Age,Thinning,color=Disease))+geom_point()+labs(title="Disease Cluster")

set.seed(123)
kmodel=kmeans(dermaData[,c(1:34)],6)
Cluster=as.factor(kmodel$cluster)
CrossTable(x=kmodel$cluster, y=dermaData$Disease,prop.chisq = FALSE, prop.r=FALSE, prop.c=FALSE,prop.t=FALSE)
ggplot(dermaData, aes(Age,Thinning,color=Cluster))+geom_point()+labs(title="K-Means Cluster")

#Agglomerative
predictor=dermaData[,1:34]
response=dermaData[,35]
model4=agnes(x=predictor, diss=FALSE,stand=TRUE, method="complete")
DendClusters=as.dendrogram(model4)
groups=cutree(model4,k=6)
CrossTable(x=groups, y=dermaData$Disease,prop.chisq = FALSE, prop.r=FALSE, prop.c=FALSE,prop.t=FALSE)
Cluster=as.factor(groups)
ggplot(dermaData, aes(Age,Thinning,color=Cluster))+geom_point()+labs(title="Agglomerative Cluster")
plot(DendClusters, main="Agglomerative Model")
rect.hclust(model4, k=6, border="red")
accuracy=cbind.data.frame(dermaData$Disease,Cluster)
accuracy$pred=ifelse(accuracy$`dermaData$Disease`==1 & Cluster==2 , 1,ifelse(accuracy$`dermaData$Disease`==2 & Cluster==6,1,ifelse(accuracy$`dermaData$Disease`==3 & Cluster==3,1,ifelse(accuracy$`dermaData$Disease`==4 & Cluster==4,1,ifelse(accuracy$`dermaData$Disease`==5 & Cluster==1, 1,ifelse(accuracy$`dermaData$Disease`==6 & Cluster==5,1,0))))))
model_accuracy=sum(accuracy$pred)/nrow(accuracy)*100
#Divisive
model5=diana(dermaData[,1:34],metric="manhattan", stand=TRUE)
DendClusters=as.dendrogram(model5)
groups=cutree(model5,k=6)
Cluster=as.factor(groups)
ggplot(dermaData, aes(Age,Thinning,color=Cluster))+geom_point()+labs(title="Divisive Cluster")
table(groups, dermaData$Disease)
plot(DendClusters,main="Divisive Model")
rect.hclust(model5, k=6, border="red")
accuracy=cbind.data.frame(dermaData$Disease,Cluster)
accuracy$pred=ifelse(accuracy$`dermaData$Disease`==1 & Cluster==2 , 1,ifelse(accuracy$`dermaData$Disease`==2 & Cluster==6,1,ifelse(accuracy$`dermaData$Disease`==3 & Cluster==3,1,ifelse(accuracy$`dermaData$Disease`==4 & Cluster==4,1,ifelse(accuracy$`dermaData$Disease`==5 & Cluster==1, 1,ifelse(accuracy$`dermaData$Disease`==6 & Cluster==5,1,0))))))
model_accuracy=sum(accuracy$pred)/nrow(accuracy)*100




