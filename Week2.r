regressionData=read.csv(file.choose(), header=T, sep=',')
model=lm(y~x, data=regressionData)
summary(model)
attach(regressionData)
plot(x,y, main="linear reg")
abline(model)

cost=function(X,y,theta)
{
  (sum(X%*%theta-y)^2)/(2*length(y))
}

theta=matrix(c(0,0), nrow=2)
num_iterations=60
alpha=0.01
cost_history=double(num_iterations)
theta_history=list(num_iterations)
X=cbind(1,matrix(x))

for(i in 1:num_iterations){
  error=(X %*% theta -y)
  delta=t(X)%*% error/length(y)
  theta=theta-alpha*delta
  cost_history[i]=cost(X,y,theta)
  theta_history[[i]]=theta }

plot(x,y,main="Gradient Descent")
abline(coef=theta_history[[1]])
abline(coef=theta_history[[2]])
abline(coef=theta_history[[3]])
for(i in c(1, num_iterations, by=10)){
  abline(coef=theta_history[[i]])
}
plot(cost_history, type='line', main="Cost Function", ylab='cost', xlab='iterations')

