rm(list = ls(all = TRUE))

library('sf')
library('raster')
library('ggplot2')
library('viridis')
library('stringr')




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

##Only Retain Columns where Zip Code info is reported and an adjacent column to facilitate aggregating
Data<-Data[c(124:125)]
names(Data)[2]<-"ZIP_CODE"

##Find the total number for each zip code
Zip_Totals<-aggregate(Q21_4~ZIP_CODE, FUN=length, data=Data)
Zip_Totals$ZIP_CODE<-as.numeric(Zip_Totals$ZIP_CODE)

###Read in ZipCode Shapefile and set Projection to WGS84
CA_Zips<-read_sf('C:/Users/timot/Desktop/OPC_Analysis/California_Zip_Codes.shp')
CA_Zips<-st_transform(CA_Zips, crs=4326)

##Merge Simple Features Spatial File with Zip Code Totals File
CA_Zips<-merge(CA_Zips, Zip_Totals, by="ZIP_CODE", all.x=TRUE)
names(CA_Zips)[7]<-"Respondent_Number"

##Plot All CA
ggplot()+
  geom_sf(data=CA_Zips, aes(fill=Respondent_Number), lwd=.5)+
  scale_fill_viridis(option = "D") 

###Zoom in Monterrey
ggplot()+
  geom_sf(data=CA_Zips, aes(fill=Respondent_Number), lwd=.5)+
  scale_fill_viridis(option = "D")+ylim(36,38)  + xlim(-123,-121)

###Zoom in Santa Barbara
ggplot()+
  geom_sf(data=CA_Zips, aes(fill=Respondent_Number), lwd=.5)+
  scale_fill_viridis(option = "D")+ylim(33.5,35)  + xlim(-121,-117)
