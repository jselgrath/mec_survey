rm(list = ls(all = TRUE))

library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert) 



###Load in Data file, Change Path Name to where it is stored on your machine
Data<-read.csv("C:/Users/timot/Desktop/OPC_Analysis/Combined_Data_8.5.24.csv")

###Data Cleaning

##Tag Data that are Paper Survey Entries
Data$Format<-str_detect(Data$Q32, "PAPER_DATA_ENTRY")
Data$Format<-ifelse(Data$Format==TRUE, "Paper", "Digital")

##Save row with question Specific Prompts for late
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


###Add Question Specific Header Info Back In
Data<-rbind(Headers, Data)
##Retain Columns w/ Data you Want to Work With
Data<-Data[c(68:76)]

##Transform Data so that each column is a survey response and each row is a prompt
Data<-as.data.frame(t(Data))
names(Data)[1]<-"Prompt"
row.names(Data)<-Data$Prompt
Data<-Data[c(-1)]


##Tabulate response categories per Prompt
Test<-as.data.frame(apply(Data, MARGIN=1,table))
Test<-Test[c(-1),]
Test<-as.data.frame(t(Test))

##Find Total Number of Responses Per Prompt and Use it to Transform Counts into Percentages
Test$Total<-rowSums(Test)
Test[1]<-Test[1]/Test$Total
Test[2]<-Test[2]/Test$Total
Test[3]<-Test[3]/Test$Total
Test[4]<-Test[4]/Test$Total
Test[5]<-Test[5]/Test$Total
Test<-Test[c(-6)]
Test$Prompt<-row.names(Test)


###Transform from wide to long for plotting
Totals_Long<-pivot_longer(Test,cols=c("Disagree", "Agree", "Strongly agree", "Strongly disagree", "Neither agree nor disagree"), 
                          names_to="Level_Agreement",
                          values_to="Count")

###Rename middle category "Neutral"
Totals_Long$Level_Agreement<-ifelse(Totals_Long$Level_Agreement=="Neither agree nor disagree", "Neutral",Totals_Long$Level_Agreement)

##Order Factors and make Disagree and Strongly disagree percentages negative show up of the left side of the y-axis
Totals_Long$Level_Agreement <- factor(Totals_Long$Level_Agreement, levels = c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree"))
Totals_Long$Count<-ifelse(Totals_Long$Level_Agreement=="Disagree", Totals_Long$Count*-1, Totals_Long$Count)
Totals_Long$Count<-ifelse(Totals_Long$Level_Agreement=="Strongly disagree", Totals_Long$Count*-1, Totals_Long$Count)

###Make Neutral percentages 1/2 positive and 1/2 negative so it straddles the 0 of the y-axis
Totals_Long_Neutral<-Totals_Long[which(Totals_Long$Level_Agreement=="Neutral"),]
Neutral_Pos<-Totals_Long_Neutral
Neutral_Pos$Count<-Neutral_Pos$Count*.5
Neutral_Neg<-Totals_Long_Neutral
Neutral_Neg$Count<-Neutral_Neg$Count*(-.5)
Totals_Long<-Totals_Long[which(!Totals_Long$Level_Agreement=="Neutral"),]
Totals_Long<-rbind(Neutral_Pos, Neutral_Neg, Totals_Long)
Totals_Long$Level_Agreement<- ordered(Totals_Long$Level_Agreement, levels = c("Strongly agree", "Agree", "Strongly disagree", "Disagree", "Neutral"))

##Order Prompts so the highest level of "Strongly agree" is at the top
Factor_Order<-Totals_Long[which(Totals_Long$Level_Agreement=="Strongly agree"),]
Factor_Order<-Factor_Order[order(Factor_Order$Count),]
Order<-Factor_Order$Prompt
Totals_Long$Prompt <- ordered(Totals_Long$Prompt, levels=Order)


##Plot it iup
ggplot(Totals_Long, aes(x=Prompt, y=Count, fill=Level_Agreement)) + geom_bar(stat='identity') + 
  scale_fill_manual(values = c("#ffb000", "#fe6100", "#dc267f", "#785ef0", "#648fff"), breaks=c('Strongly agree', 'Agree', 'Neutral', 'Disagree', "Strongly disagree"))+ coord_flip()+
  ylab("% of Respondents") + xlab("") +theme_bw()







##START OVER FOR Climate_Observations
rm(list = ls(all = TRUE))

Data<-read.csv("Combined_Data_8.5.24.csv")
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

Data<-rbind(Headers, Data)
Data<-Data[c(81:91)]
Data<-as.data.frame(t(Data))
names(Data)[1]<-"Prompt"
row.names(Data)<-Data$Prompt
Data<-Data[c(-1)]


Test<-as.data.frame(apply(Data, MARGIN=1,table))
Test<-Test[c(-1),]
Test<-as.data.frame(t(Test))
Test$Total<-rowSums(Test)

Test[1]<-Test[1]/Test$Total
Test[2]<-Test[2]/Test$Total
Test[3]<-Test[3]/Test$Total
Test[4]<-Test[4]/Test$Total
Test[5]<-Test[5]/Test$Total
Test<-Test[c(-6)]
Test$Prompt<-row.names(Test)


Totals_Long<-pivot_longer(Test,cols=c("Decrease", "Increase", "No change observed", "Strong increase", "Strong decrease"), 
                          names_to="Level_Agreement",
                          values_to="Count")

Totals_Long$Level_Agreement <- factor(Totals_Long$Level_Agreement, levels = c("Strong increase", "Increase", "No change observed", "Decrease", "Strong decrease"))
Totals_Long$Count<-ifelse(Totals_Long$Level_Agreement=="Decrease", Totals_Long$Count*-1, Totals_Long$Count)
Totals_Long$Count<-ifelse(Totals_Long$Level_Agreement=="Strong decrease", Totals_Long$Count*-1, Totals_Long$Count)

Totals_Long_Neutral<-Totals_Long[which(Totals_Long$Level_Agreement=="No change observed"),]
Neutral_Pos<-Totals_Long_Neutral
Neutral_Pos$Count<-Neutral_Pos$Count*.5
Neutral_Neg<-Totals_Long_Neutral
Neutral_Neg$Count<-Neutral_Neg$Count*(-.5)


Totals_Long<-Totals_Long[which(!Totals_Long$Level_Agreement=="No change observed"),]
Totals_Long<-rbind(Neutral_Pos, Neutral_Neg, Totals_Long)
Totals_Long$Level_Agreement<- ordered(Totals_Long$Level_Agreement, levels = c("Strong increase", "Increase", "Strong decrease", "Decrease", "No change observed"))


ggplot(Totals_Long, aes(x=Prompt, y=Count, fill=Level_Agreement)) + geom_bar(stat='identity') + 
  scale_fill_manual(values = c("#ffb000", "#fe6100", "#dc267f", "#785ef0", "#648fff"), breaks=c("Strong increase", "Increase", "No change observed", "Decrease", "Strong decrease"))+ coord_flip()+
  ylab("% of Respondents") + xlab("") +theme_bw()


###START OVER FOR Climate Concerns

rm(list = ls(all = TRUE))

Data<-read.csv("Combined_Data_8.5.24.csv")
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

Data<-rbind(Headers, Data)
Data<-Data[c(92:102)]
Data<-as.data.frame(t(Data))
names(Data)[1]<-"Prompt"
row.names(Data)<-Data$Prompt
Data<-Data[c(-1)]


Test<-as.data.frame(apply(Data, MARGIN=1,table))
Test<-Test[c(-1),]
Test<-as.data.frame(t(Test))
Test$Total<-rowSums(Test)

Test[1]<-Test[1]/Test$Total
Test[2]<-Test[2]/Test$Total
Test[3]<-Test[3]/Test$Total
Test<-Test[c(-4)]
Test$Prompt<-row.names(Test)


Totals_Long<-pivot_longer(Test,cols=c("Not at all concerned", "Somewhat concerned", "Very concerned"), 
                          names_to="Level_Agreement",
                          values_to="Count")

Totals_Long$Level_Agreement <- factor(Totals_Long$Level_Agreement, levels = c("Not at all concerned", "Somewhat concerned", "Very concerned"))
Totals_Long$Count<-ifelse(Totals_Long$Level_Agreement=="Not at all concerned", Totals_Long$Count*-1, Totals_Long$Count)

Totals_Long_Neutral<-Totals_Long[which(Totals_Long$Level_Agreement=="Somewhat concerned"),]
Neutral_Pos<-Totals_Long_Neutral
Neutral_Pos$Count<-Neutral_Pos$Count*.5
Neutral_Neg<-Totals_Long_Neutral
Neutral_Neg$Count<-Neutral_Neg$Count*(-.5)

Totals_Long<-Totals_Long[which(!Totals_Long$Level_Agreement=="Somewhat concerned"),]
Totals_Long<-rbind(Neutral_Pos, Neutral_Neg, Totals_Long)
Totals_Long$Level_Agreement<- ordered(Totals_Long$Level_Agreement, levels = c("Very concerned", "Not at all concerned", "Somewhat concerned"))


ggplot(Totals_Long, aes(x=Prompt, y=Count, fill=Level_Agreement)) + geom_bar(stat='identity') + 
  scale_fill_manual(values = c("#ffb000",  "#dc267f",  "#648fff"), breaks=c("Very concerned", "Somewhat concerned", "Not at all concerned"))+ coord_flip()+
  ylab("% of Respondents") + xlab("") +theme_bw()
