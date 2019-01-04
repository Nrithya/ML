#load library
library(ggplot2)
library(scales)
library(gmodels)
library(cluster)

options(scipen=5)


#import data
ted_main_raw=read.csv(file.choose(), header=TRUE, sep=",")
ratings=read.csv(file.choose(), header=TRUE, sep=",")
clust=read.csv(file.choose(), header=TRUE, sep=",")
#remove na
ted_main=na.omit(ted_main_raw)

#extract required videos based on tag
tag_tech=subset(ted_main,grepl( "'technology'",ted_main$tags, fixed=TRUE))
tag_cul=subset(ted_main,grepl( "'culture'",ted_main$tags, fixed=TRUE))

#assign labels 
tag_clust=rbind(tag_tech,tag_cul)
tag_clust$dup=duplicated(tag_clust)
repeated=subset(tag_clust,tag_clust$dup==TRUE)
final=tag_clust[!(tag_clust$url %in% repeated$url),]
final$label=ifelse(grepl( "'technology'",final$tags, fixed=TRUE),01, 10)
repeated$label=11
final=rbind(final,repeated)
final$label=as.factor(final$label)

#create curated data with ratings
final=final[,c(17,1,3,6,18,20)]
clust_data=merge(final,ratings, by="url")
clust_data$label=as.factor(clust_data$label)
clust_data_trim=subset(clust_data,views<30000000)
clust_trim=subset(clust,views<30000000 & comments<4000)
clust_trim$label=as.factor(clust_trim$label)
ggplot(clust_data_trim, aes(views,comments,color=label))+geom_point()+labs(title="Tag Cluster")+xlab("Views")+ylab("Comments")

#Agglomerative
predictor=clust_trim[,2:19]
response=clust_trim[,20]
model4=agnes(x=predictor, diss=FALSE,stand=TRUE, method="complete")
DendClusters=as.dendrogram(model4)
groups=cutree(model4,k=3)
CrossTable(x=groups, y=clust_trim$label,prop.chisq = FALSE, prop.r=FALSE, prop.c=FALSE,prop.t=FALSE)
Cluster=as.factor(groups)
ggplot(clust_trim, aes(views,comments,color=Cluster))+geom_point()+labs(title="Agglomerative Cluster")
plot(DendClusters, main="Agglomerative Model")
rect.hclust(model4, k=3, border="red")
accuracy=cbind.data.frame(clust_trim$label,Cluster)
accuracy$pred=ifelse(accuracy$`clust_trim$Disease`==1 & Cluster==2 , 1,ifelse(accuracy$`clust_trim$Disease`==2 & Cluster==6,1,ifelse(accuracy$`clust_trim$Disease`==3 & Cluster==3,1,ifelse(accuracy$`clust_trim$Disease`==4 & Cluster==4,1,ifelse(accuracy$`clust_trim$Disease`==5 & Cluster==1, 1,ifelse(accuracy$`clust_trim$Disease`==6 & Cluster==5,1,0))))))
model_accuracy=sum(accuracy$pred)/nrow(accuracy)*100

#Divisive
model5=diana(clust_trim[,2:19],metric="manhattan", stand=TRUE)
DendClusters=as.dendrogram(model5)
groups=cutree(model5,k=3)
Cluster=as.factor(groups)
ggplot(clust_trim, aes(views,comments,color=Cluster))+geom_point()+labs(title="Divisive Cluster")
table(groups, clust_trim$label)
plot(DendClusters,main="Divisive Model")
rect.hclust(model5, k=3, border="red")
accuracy=cbind.data.frame(clust_trim$Disease,Cluster)
accuracy$pred=ifelse(accuracy$`clust_trim$Disease`==1 & Cluster==2 , 1,ifelse(accuracy$`clust_trim$Disease`==2 & Cluster==6,1,ifelse(accuracy$`clust_trim$Disease`==3 & Cluster==3,1,ifelse(accuracy$`clust_trim$Disease`==4 & Cluster==4,1,ifelse(accuracy$`clust_trim$Disease`==5 & Cluster==1, 1,ifelse(accuracy$`clust_trim$Disease`==6 & Cluster==5,1,0))))))
model_accuracy=sum(accuracy$pred)/nrow(accuracy)*100

