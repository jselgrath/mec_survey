rm(list = ls(all = TRUE))


library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert) 


Data<-read.csv("C:/Users/timot/Desktop/OPC_Analysis/Combined_Data_8.5.24.csv")
Data<-Data[c(105:114)]
Data[Data==""]<-NA
Data<-as.data.frame(t(Data))
Names<-Data$V1


Data<-read.csv("C:/Users/timot/Desktop/OPC_Analysis/Combined_Data_8.5.24.csv")
Data$Format<-str_detect(Data$Q32, "PAPER_DATA_ENTRY")
Data$Format<-ifelse(Data$Format==TRUE, "Paper", "Digital")
Headers<-Data[1,]

##Remove Digital Responses (i.e., tablet or phone where people spent less than 500 seconds on the Survey)
Data$Format<-ifelse(Data$Format=="Digital" & Data$Duration_Seconds < 500, "Bad_Digital", Data$Format)
Data<-Data[which(!Data$Format=="Bad_Digital"),]

##Remove Paper Data Entries of Low (i.e., 3) Quality
Paper_Data<-Data[grepl("PAPER_DATA_ENTRY", Data$Q32), , drop = FALSE]
Bad_Paper_Data<-Data[grepl("DATA_QUALITY: 3", Data$Q32), , drop = FALSE]
Bad_Data<-as.list(Bad_Paper_Data$ResponseId)

###Remove Data where people answered less than 60% of the questions
Data<-Data[-which(Data$ResponseId %in% Bad_Data), ]
Data$Progress<-as.numeric(Data$Progress)
Data<-Data[which(Data$Progress > 60),]

Data_Format<-rbind(Headers, Data)
Data<-Data_Format[c(105:114)]
Data[Data==""]<-NA
Sample_N<-sqrt(as.numeric(nrow(Data)))
Data<-as.data.frame(t(Data))
Data<-Data[c(-1)]
row.names(Data)<-Names

###Find the Mean Rank of Each Prompt
Data2<-data.frame(apply(Data, 2, as.numeric))
Means<-as.data.frame(rowMeans(Data2, na.rm=TRUE))
row.names(Means)<-Names
names(Means)[1]<-"Mean_Rank"
Means$Prompt<-row.names(Means)

##Find the SE of Each prompt
SD<-as.data.frame(apply(Data2, 1, sd, na.rm = TRUE))
names(SD)[1]<-"SD"
SD$SE<-SD$SD/Sample_N
Means$SE<-SD$SE

###Order the Prompts from Highest Priority to Lowest
Means<-Means[order(-Means$Mean_Rank),]
Means$Priority<-row.names(Means)
Means$Priority<-as.factor(Means$Priority)
Order<-Means$Priority
Means$Priority <- ordered(Means$Priority, levels=Order)

##Plot Global Mean Rank +/- S.E.
ggplot(Means, aes(x=Priority, y=Mean_Rank)) +geom_bar(stat='identity', fill='grey66', color="black") + 
  geom_errorbar(aes(ymin=Mean_Rank-SE, ymax=Mean_Rank+SE), width=.2,
                position=position_dodge(.9)) + coord_flip() + theme_bw()


### GET Mean Priorities for Different Categorical Variables

##If you are doing a discrete, mutually exclusive category (where respondents can only select on response), start here

##Unique<-as.data.frame(unique(Data_Format$Q27))
##names(Unique) <- c("Group")
##List<-as.list(as.character(Unique$Group))


##If you are using a variable where respondents can select multiple options, you need to start here and make your own list
List<-as.list(c("Hispanic or Latino", "White", "Asian", "Black or African American", "Native Hawaiian or Other Pacific Islander",	"American Indian or Alaska Native",
                "Middle Eastern or North African"))
All_Groups<-NULL

###Start Loop

for (i in 1:length(List)){
  Demographic_Group = List[i]
  ##Use this line of code rather than the next if working with mutually exclusive categories
  ##Single_Group<-Data_Format[which(Data_Format$Q27==Group),]
  Single_Group<-Data_Format[grepl(Demographic_Group, Data_Format$Q24), , drop = FALSE]
  Sample_Number<-nrow(Single_Group)
  Single_Group<-Single_Group[c(105:114)]
  Single_Group[Single_Group==""]<-NA
  Sample_N<-sqrt(as.numeric(nrow(Single_Group)))
  Single_Group<-as.data.frame(t(Single_Group))
  Single_Group<-Single_Group[c(-1)]
  row.names(Single_Group)<-Names
  
  Data2<-data.frame(apply(Single_Group, 2, as.numeric))
  Single_Means<-as.data.frame(rowMeans(Data2, na.rm=TRUE))
  row.names(Single_Means)<-Names
  names(Single_Means)[1]<-"Mean_Rank"
  Single_Means$Group<-Demographic_Group
  Single_Means$Prompt<-row.names(Single_Means)
  Single_Means$Sample_Number<-Sample_Number
  print(Demographic_Group)
  All_Groups<-rbind(All_Groups, Single_Means)
}

###No need to plot up these groups if they are retained
All_Groups<-All_Groups[which(!All_Groups$Group==""),]
All_Groups<-All_Groups[which(!All_Groups$Group=="Choose not to answer"),]


All_Groups<-merge(All_Groups, Means, by="Prompt", all.x=TRUE)
All_Groups$Anomaly<-All_Groups$Mean_Rank.x-All_Groups$Mean_Rank.y
All_Groups$Prompt<-as.character(All_Groups$Prompt)
All_Groups$Prompt<-as.factor(All_Groups$Prompt)
All_Groups$Group<-as.character(All_Groups$Group)
All_Groups$Anomaly<-All_Groups$Anomaly*(-1)
All_Groups$Sample_Number<-as.character(All_Groups$Sample_Number)
All_Groups$Color<-ifelse(All_Groups$Anomaly<0, "Lower Priority than Average", "Higher Priority than Average")

ggplot(All_Groups, aes(x=Prompt, y=Anomaly, fill=Color)) + geom_bar(stat='identity') +
  geom_text(aes(label=Sample_Number), y=0, x=4) +facet_wrap(~Group) + coord_flip()
