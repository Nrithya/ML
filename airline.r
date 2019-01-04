#import libraries
library(cluster)

#import data
airlineData=read.csv(file.choose(),header=T, sep=",")

#correlation
cor(airlineData[,2:8])

#scatterplot pairs
pairs(airlineData[,2:5])
pairs(airlineData[,c(2,6:8)])

#labels
labs= airlineData[,1]

#agglomerative clustering
clusters=agnes(x=airlineData[,c(2,6:8)], diss=FALSE,stand=TRUE, method="complete")
DendClusters=as.dendrogram(clusters)
plot(DendClusters)
DendClusters$labels=airlineData$airline

#divisive clustering
model=diana(airlineData[,2:5],metric="manhattan", stand=TRUE)
plot(model)
identify(model)




