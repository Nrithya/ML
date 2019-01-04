#import libraries
library(data.table)
library(neuralnet)

#import data
steel_data=read.csv(file.choose(), header=TRUE,sep=",")

#process data
steel_data=steel_data[,c(1:11,14:34)]
pastry_data=steel_data[,c(1:26)]
zscratch_data=steel_data[,c(1:25,27)]
kscatch_data=steel_data[,c(1:25,28)]
stain_data=steel_data[,c(1:25,29)]
dirt_data=steel_data[,c(1:25,30)]
bump_data=steel_data[,c(1:25,31)]
other_data=steel_data[,c(1:25,32)]
data=stain_data

#scale data
max=apply(data,2,max)
min=apply(data,2,min)
scaled=as.data.frame(scale(data,center=min,scale=max-min))
index=sample(1:nrow(data),round(0.75*nrow(data)))

#train and test set
training=scaled[index,]
testing=scaled[-index,]
names=names(scaled)

#neural networks
f=as.formula(paste("Stains~", paste(names[!names %in% "Stains"], collapse="+")))
nn=neuralnet(f,data=training,hidden=c(5,4),linear.output=T)
plot(nn)

#predict
test_nn=compute(nn,testing[,1:25])
y=test_nn$net.result*(max(data$Stains)-min(data$Stains))+min(data$Stains)
b=(testing$Stains)*(max(data$Stains)-min(data$Stains))+min(data$Stains)
MSE=sum((b-y)^2)/nrow(testing)
CrossTable(x=y, y=b,prop.chisq = FALSE, prop.r=FALSE, prop.c=FALSE,prop.t=FALSE)

