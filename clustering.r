survivalData=read.csv(file.choose(),header=T, sep=",")
library(cluster)
predictor=survivalData[,1:dim(survivalData)[2]-1]
response=survivalData[,dim(survivalData)[2]]
clusters=agnes(x=predictor, diss=FALSE,stand=TRUE, method="complete")
DendClusters=as.dendrogram(clusters)
plot(DendClusters)


diabData=read.csv(file.choose(),header=T, sep=",")
diabData_clean=subset(diabData, Glucose>0)
model=diana(diabData_clean,metric="manhattan", stand=TRUE)
plot(model)

library(ggplot2)
ggplot(iris, aes(Petal.Length,Petal.Width,color=Species))+geom_point()
set.seed(123)
irisCluster=kmeans(iris[,3:4],5)
table(irisCluster$cluster, iris$Species)
ggplot(iris,aes(Petal.Length, Petal.Width, color=as.factor(irisCluster$cluster))) + geom_point()

d0=diabData_clean[,'Class']==0
d1=diabData_clean[,'Class']==1
p_db=sum(d1)/(sum(d0)+sum(d1))

gluData=diabData_clean[,'Glucose']
gluData.density=density(gluData)
gluData.d0.density=density(gluData[d0])
gluData.d1.density=density(gluData[d1])
gluData.d0.f=approxfun(gluData.d0.density$x,gluData.d0.density$y)
gluData.d1.f=approxfun(gluData.d1.density$x,gluData.d1.density$y)
plot(density(gluData))
lines(density(gluData[d0]), col='blue',xlab='glucose',ylab='estimate p(glu)')
lines(density(gluData[d1]), col='red',xlab='glucose',ylab='estimate p(glu)')
