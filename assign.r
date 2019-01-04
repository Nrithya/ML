Psy=read.csv(file.choose(),header=T,sep=',')
Perry=read.csv(file.choose(),header=T,sep=',')
LMFAO=read.csv(file.choose(),header=T,sep=',')
Eminem=read.csv(file.choose(),header=T,sep=',')
Shakira=read.csv(file.choose(),header=T,sep=',')

spamData=rbind(Psy,Perry,LMFAO,Eminem)
spamData$CLASS=as.factor(spamData$CLASS)
spamData$LENGTH=as.factor(nchar(as.character(spamData$CONTENT)))

Shakira$CLASS=as.factor(Shakira$CLASS)

spamModel=naiveBayes(CLASS~AUTHOR+LENGTH+CONTENT, data=spamData)
prediction=predict(spamModel, newdata=Shakira)
confusionMatrix(prediction,Shakira$CLASS)

