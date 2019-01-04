airlineData=read.csv(file.choose(),header=T, sep=",")
library(cluster)
cor(airlineData[,2:8])
pairs(airlineData[,2:5])
pairs(airlineData[,c(2,6:8)])
labs= airlineData[,1]
#entire set
clusters=agnes(x=airlineData[,2:8], diss=FALSE,stand=TRUE, method="complete")

DendClusters=as.dendrogram(clusters)
rownames(DendClusters)=labs
plot(DendClusters, labels=labs)

#split set
clusters=agnes(x=airlineData[,c(2,6:8)], diss=FALSE,stand=TRUE, method="complete")
DendClusters=as.dendrogram(clusters)
plot(DendClusters)
DendClusters$labels=airlineData$airline
model=diana(airlineData[,2:5],metric="manhattan", stand=TRUE)
plot(model)
identify(model)




