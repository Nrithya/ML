library(depmixS4)
library(quantmod)
airdata_raw=read.csv(file.choose(),header=TRUE,sep=",")
airdata=na.omit(airdata_raw[,1:15])
airdata=subset(airdata, (airdata[,3]> -199) & (airdata[,4]> -199) & (airdata[,5]> -199) & (airdata[,6]> -199) & (airdata[,7]> -199) & (airdata[,8]> -199) & (airdata[,9]> -199) & (airdata[,10]> -199) & (airdata[,11]> -199) & (airdata[,12]> -199) & (airdata[,13]> -199) & (airdata[,14]> -199) & (airdata[,15]> -199))

airdata$DateTime=paste(airdata$Date ,airdata$Time)

airdata$DateTime=as.POSIXlt(airdata$DateTime, format = "%m/%d/%Y %H:%M:%S")
newdata=airdata[,c(16,3:15)]
airdataTS=as.xts(newdata[,2], order.by = newdata[,1])
airdataTS_no=as.numeric(airdataTS)
plot(airdataTS)


hmm=depmix(airdataTS_no~1,family=gaussian(),nstates=2, data=data.frame(airdataTS_no=airdataTS_no))
hmmfit=fit(hmm,verbose=FALSE)
post_probs=posterior(hmmfit)

layout(1:2)
plot(airdataTS_no, type='l', main="Air Pollution", xlab = "Time", ylab="CO Level")
matplot(post_probs[,-1],type='l', main="Air Pollution", xlab = "Time", ylab="CO Level")


