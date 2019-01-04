#load library
library(anytime)
library(plyr)
library(ggplot2)
library(scales)

#import data
ted_main_raw=read.csv(file.choose(), header=TRUE, sep=",")
ratings=read.csv(file.choose(), header=TRUE, sep=",")
#remove na
ted_main=na.omit(ted_main_raw)

#convert Unix date
ted_main$published_date=anytime(ted_main$published_date)
ted_main$film_date=anytime(ted_main$film_date)

#tag dictionary with frequency
tags_raw=paste(unlist(ted_main$tags), collapse =" ")
tags<-gsub("\\[","",tags_raw)
tags_split=strsplit(tags,"]|,")
freq=as.data.frame(table(tags_split))
most_tags=head(arrange(freq,desc(Freq)), n =25)
ggplot(most_tags, aes(x=reorder(tags_split,-Freq),y=Freq, fill= as.factor(Freq))) +geom_bar(stat = "identity")+theme(axis.text.x=element_text(angle=90))+labs(title = "Top 25 Themes", x="Themes", y="No of Tags")+scale_y_continuous(labels=comma)+scale_fill_discrete(name = "Count")

tag_tech=subset(ted_main,grepl( "'technology'",ted_main$tags, fixed=TRUE))
tag_tech$label=01
tag_sci=subset(ted_main,grepl( "'science'",ted_main$tags, fixed=TRUE))
tag_gi=subset(ted_main,grepl( "'global issues'",ted_main$tags, fixed=TRUE))
tag_gi$label=10
tag_cul=subset(ted_main,grepl( "'culture'",ted_main$tags, fixed=TRUE))
tag_cul$label=10
tag_des=subset(ted_main,grepl( "'design'",ted_main$tags, fixed=TRUE))

tag_clust=rbind(tag_tech,tag_cul)
tag_clust$dup=duplicated(tag_clust)
repeated=subset(tag_clust,tag_clust$dup==TRUE)
final=tag_clust[!(tag_clust$url %in% repeated$url),]
final$label=ifelse(grepl( "'technology'",final$tags, fixed=TRUE),01, 10)
final$label=as.factor(final$label)
repeated$label=11
final=rbind(final,repeated)
final=final[,c(17,1,3,6,18,20)]
clust_data=merge(final,ratings, by="url")
write.table(clust_data, "D:/17_Fall/Machine Learning/Project/ted-talks/clusterdata.csv", sep=",")

#general exploration
median_comment=median(ted_main$comments)
mean_comment=mean(ted_main$comments)
median_view=median(ted_main$views)
mean_view=mean(ted_main$views)

#most viewed
most_viewed=head(arrange(ted_main,desc(views)), n =25)
ggplot(most_viewed, aes(x=reorder(main_speaker,-views),y=views, fill= most_viewed$speaker_occupation)) +geom_bar(stat = "identity")+theme(axis.text.x=element_text(angle=90))+labs(title = "Top 25 Most Viewed TED Talks", x="Main Speaker", y="Number of views")+scale_y_continuous(labels=comma)+scale_fill_discrete(name = "Speaker Occupation")
tags_raw=paste(unlist(most_viewed$tags), collapse =" ")
tags<-gsub("\\[","",tags_raw)
tags_split=strsplit(tags,"]|,")
freq=as.data.frame(table(tags_split))
plot_tags=subset(freq,freq$Freq>3) 
ggplot(plot_tags, aes(x=reorder(tags_split,-Freq),y=Freq, fill=tags_split)) +geom_bar(stat = "identity")+theme(axis.text.x=element_text(angle=90))+labs(title = "Themes for the Top 25 Most Viewed TED Talks", x="Themes", y="Count")+scale_y_continuous(labels=comma)


#most commented
most_commented=subset(ted_main,comments>999)
ggplot(most_commented, aes(x=reorder(title,-comments),y=comments, fill= most_commented$comments)) +geom_bar(stat = "identity")+theme(axis.text.x=element_text(angle=90))+labs(title = "Most Commented TED Talks", x="TED Talk Titles", y="Number of comments")+scale_y_continuous(labels=comma)
tags_raw=paste(unlist(most_commented$tags), collapse =" ")
tags<-gsub("\\[","",tags_raw)
tags_split=strsplit(tags,"]|,")
freq=as.data.frame(table(tags_split))
plot_tags=subset(freq,freq$Freq>3) 
ggplot(plot_tags, aes(x=reorder(tags_split,-Freq),y=Freq, fill=tags_split)) +geom_bar(stat = "identity")+theme(axis.text.x=element_text(angle=90))+labs(title = "Themes most discussed", x="Themes", y="Count")+scale_y_continuous(labels=comma)

