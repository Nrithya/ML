library(data.table)
library(neuralnet)
data=fread("http://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data")

max=apply(data,2,max)
min=apply(data,2,min)
scaled=as.data.frame(scale(data,center=min,scale=max-min))
index=sample(1:nrow(data),round(0.75*nrow(data)))
training=scaled[index,]
testing=scaled[-index,]
names=names(scaled)
f=as.formula(paste("V7~", paste(names[!names %in% "V7"], collapse="+")))
nn=neuralnet(f,data=training,hidden=c(4,3),linear.output=T)
plot(nn)
test_nn=compute(nn,testing[,1:6])
y=test_nn$net.result*(max(data$V7)-min(data$V7))+min(data$V7)
b=(testing$V7)*(max(data$V7)-min(data$V7))+min(data$V7)
MSE=sum((b-y)^2)/nrow(testing)
nn=neuralnet(f,data=training,hidden=c(4,3),linear.output=T)
