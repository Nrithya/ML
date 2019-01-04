library(scatterplot3d)
library(ggplot2)
hateData=read.csv(file.choose(),sep='\t',header = TRUE,fileEncoding="UTF-8-BOM")
hateData_na=na.omit(hateData)
cor(hateData_na[,3:13])
attach(hateData)
colors=c("#ce1414", "#E69F00", "#56B4E9","#228B22")
colors=colors[as.numeric(region)]
model1=lm(hate_crimes_per_100k_splc~(gini_index+median_household_income), hateData)
income=scatterplot3d(gini_index,median_household_income,hate_crimes_per_100k_splc,pch=16,color=colors, angle=240, main="Hate Crimes 2016 Regression", xlab="Gini Index", ylab="Median household income",zlab="# of crimes reported")
income$plane3d(model1)

model2=lm(avg_hatecrimes_per_100k_fbi~(gini_index+median_household_income), hateData)
income=scatterplot3d(gini_index,median_household_income,avg_hatecrimes_per_100k_fbi,pch=16,color=colors, angle=240, main="Average Hate Crimes Regression", xlab="Gini Index", ylab="Median household income",zlab="Avg of crimes reported")
income$plane3d(model2)

model3=lm(hate_crimes_per_100k_splc~(share_white_poverty+share_non_citizen), hateData)
income=scatterplot3d(share_white_poverty,share_non_citizen,hate_crimes_per_100k_splc,pch=16,color=colors, angle=240, main="Hate Crimes Prediction with Race", xlab="White population in poverty", ylab="Non-citizen population",zlab="# of crimes reported")
income$plane3d(model3)

model4=lm(hate_crimes_per_100k_splc~(share_population_with_high_school_degree+share_unemployed_seasonal), hateData)
income=scatterplot3d(share_population_with_high_school_degree,share_unemployed_seasonal,hate_crimes_per_100k_splc,pch=16,color=colors, angle=240, main="Hate Crimes Prediction with Development", xlab="High school degree holders", ylab="Seasonally unemployed",zlab="# of crimes reported")
income$plane3d(model4)


ggplot(data=hateData, aes(x=state, y=hate_crimes_per_100k_splc, fill=region)) +  geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90))+ labs(title= "Reported hate crime numbers per State(After 2016 election)", fill="Region")+xlab("States")+ylab("# of crimes reported/100k") 
ggplot(data=hateData, aes(x=state, y=avg_hatecrimes_per_100k_fbi, fill=region)) +  geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90))+ labs(title= "Reported hate crime numbers per State(FBI reports 2010-15)", fill="Region")+xlab("States")+ylab("Avg # of crimes/100k") 
boxplot(hate_crimes_per_100k_splc, main="Distribution of Hate Crimes 2016", col="red", names=c("# of hate crimes/100k"))


plot(avg_hatecrimes_per_100k_fbi,hate_crimes_per_100k_splc, col="red",pch=16, main="Hate crime distribution", xlab= "Average hate crimes/100k (2010-15)", ylab="# of hate crimes/100k (2016)")
