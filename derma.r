library(ggplot2)

dermaData=read.csv(file.choose(),sep='\t',header=T)
dermaData=subset(dermaData[,],(dermaData$Age != "?"))
dermaData$Age=as.integer(dermaData$Age)
dermaData$AgeCategory=as.factor(ifelse(dermaData$Age<10,1,ifelse(dermaData$Age<20,2,ifelse(dermaData$Age<30,3,ifelse(dermaData$Age<40,4,ifelse(dermaData$Age<50,5,ifelse(dermaData$Age<60,6,ifelse(dermaData$Age<70,7,8))))))))
dermaData_age=dermaData[,c(1:34,36,35)]
dermaData=lapply(dermaData[,c(1:33,35,34)],as.factor)

clinicalData=subset(dermaData[,c(1:11,34,35)],(dermaData$Age != "?"))
pathoData=subset(dermaData[,c(12:33,35)])

#1
cor(as.integer(clinicalData$Age),clinicalData$Disease)
qplot(as.integer(clinicalData$Age),geom="histogram",binwidth = 0.5,main = "Histogram for Age", xlab = "Age",fill=I("blue"))
qplot(clinicalData$Disease,geom="histogram",binwidth = 0.5,main = "Histogram for Disease", xlab = "Disease",fill=I("red"))

model1=lm(Disease~as.integer(Age), clinicalData)
summary(model1)
plot(as.integer(clinicalData$Age),clinicalData$Disease,main="Linear Model (Age vs Disease)",xlab="Age",ylab="Disease", col="red", pch=16)
abline(model1,col="blue",lwd=3)

cost=function(X,y,theta)
{
  sum((X %*% theta-y)^2)/(2*length(y))
}

theta=matrix(c(0,0), nrow=2)
num_iterations=15000
alpha=0.00185
cost_history=double(num_iterations)
theta_history=list(num_iterations)
X=cbind(1,matrix(as.integer(clinicalData$Age)))
y=clinicalData$Disease

for(i in 1:num_iterations){
  error=(X %*% theta -y)
  delta=t(X)%*% error/length(y)
  theta=theta-alpha*delta
  cost_history[i]=cost(X,y,theta)
  theta_history[[i]]=theta }


plot(as.integer(clinicalData$Age),clinicalData$Disease,main="Gradient Descent (Age vs Disease)",xlab="Age",ylab="Disease", col="red", pch=16)
for(i in seq(1,num_iterations, by=1000)){
  abline(coef=theta_history[[i]],lty=5)
}

abline(coef=theta_history[[15000]],col="blue",lwd=3)

plot(cost_history, type='line', main="Cost Function", ylab='Cost', xlab='# of iterations')

#2.



