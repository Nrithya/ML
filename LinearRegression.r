#import libraries
library(ggplot2)

#import data
ratings=read.table(file.choose(), header=T, sep='\t')
summary(ratings)

#correlation
cor(ratings[2:5])

#plot
ggplot(ratings,aes(x=ambience,y=rating))+geom_point(color="orange")+stat_smooth(method="lm")
ggplot(ratings,aes(x=(food+service+ambience),y=rating))+geom_point()+geom_smooth()

#linear regression
lm(rating~(food+service+ambience), ratings)
