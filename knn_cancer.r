library(scatterplot3d)
library(gmodels)
library(class)

cancerData=read.csv(file.choose(),header=T, sep=",")
cancerData$Diagnosis_bin=ifelse(cancerData$Diagnosis=='M',1,0)
cor(cancerData[3:13])

cancerType <- c("#E69F00", "#56B4E9")
cancerType=cancerType[as.numeric(cancerData$Diagnosis)]
scatterplot3d(cancerData$ConPoints,cancerData$Perimeter,cancerData$Radius,pch=16, type="p",  angle=70, color=cancerType)


set.seed(1234)#any number
ind=sample(2,nrow(cancerData),replace=TRUE, prob=c(0.75, 0.25))
cancerData.training=cancerData[ind==1,3:12]
cancerData.test=cancerData[ind==2,3:12]           
cancerData.trainLabels=cancerData[ind==1,2]
cancerData.testLabels=cancerData[ind==2,2]

cancer_pred=knn(train=cancerData.training, test=cancerData.test, cl=cancerData.trainLabels,k=7)

CrossTable(x=cancer_pred, y=cancerData.testLabels,prop.chisq=FALSE)

