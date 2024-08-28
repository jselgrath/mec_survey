# make plots for aug 16 SAC meeting
library(dplyr)
library(ggplot2)
library(sf)
library(raster)

setwd("/Users/emmagee/Documents/opc_2324/")

# # combine ucsc, online, ucsb, and sf datasets
ucsc<-read.csv("sac_plots/ucsc_0813/24/2024_August 13, 2024_10.23.csv", stringsAsFactors = F)
ucsc<-ucsc[-c(1,2),]
# internet<-read.csv("sac_plots/internet.csv", stringsAsFactors = F)
# internet<-internet[-c(1,2),]
ucsb<-read.csv("sac_plots/ucsb_0813.csv", stringsAsFactors = F)
ucsb<-ucsb[-c(1,2),]
# sf<-read.csv("sac_plots/sf_0813.csv", stringsAsFactors = F)
# sf<-sf[-c(1,2),]
# 
# surv<-rbind(ucsc, ucsb, sf[,names(ucsc)])
# 
# # fix column names that exist in ucsc/ucsb data but not internet and sf
# survnames<-names(surv)
# sfnames<-names(sf)
# innames<-names(internet)
# sfnames[!is.element(sfnames, survnames)]
# innames[!is.element(innames, survnames)]
# 
# # fix internet column names to match others
# names(internet)[which(innames == "Q5a.")]<-"Q5a"
# names(internet)[which(innames == "Q5b.2_4")]<-"Q5b.2._4"
# names(internet)[which(innames == "Q5b.2_5")]<-"Q5b.2._5"
# names(internet)[which(innames == "Q5b.2_6")]<-"Q5b.2._6"
# names(internet)[which(innames == "Q19.")]<-"Q18."
# names(internet)[which(innames == "Q24")]<-"Q24."
# names(internet)[which(innames == "Q24a.")]<-"Q24a"
# names(internet)[which(innames == "Q25")]<-"Q25."
# names(internet)[which(innames == "Q27")]<-"Q27."
# names(internet)[which(innames == "Q28")]<-"Q28."
# names(internet)[which(innames == "Q31.")]<-"Q31"
# names(internet)[which(innames == "Q33a.")]<-"Q33._1_TEXT"
# internet$Q33.<-""
# 
# surv<-rbind(surv, internet[,names(surv)])
# 
# write.csv(surv, "all_survey_data_0813.csv", row.names=F)

surv<-read.csv("all_survey_data_0813.csv", stringsAsFactors = F)

# age
# extract birth years from demo data
b<-filter(surv, nchar(Q26) == 4)
survage<-filter(b, grepl("[[:digit:]][[:digit:]][[:digit:]][[:digit:]]", 
                   Q26))
survage$age<-2024-as.numeric(survage$Q26)

ggplot(survage, aes(age)) + geom_histogram(binwidth=5, col="black", fill="#28bfd9") + 
  theme_bw() + theme(panel.grid = element_blank(), text = element_text(size = 24)) +
  xlab("Age") + ylab("Frequency") + ggtitle("Participant ages") +
  scale_x_continuous(breaks=seq(15,85,5))
ggsave("sac_plots/ages.png", width=10, height=4.5, units="in")

# race
# just get all the races marked (i.e., people who marked 2 races will be double counted)
races<-strsplit(surv$Q24., ",") %>% unlist()

# idk what's going on but i'm going to assume all the 10's are meant to be 9s because there
# are only 9 choices
races[races == "10"]<-"9"
ggplot() + geom_bar(aes(races), col="black", fill="#28bfd9") + 
  theme_bw() + theme(panel.grid = element_blank(), text = element_text(size = 24),
                     axis.text.x=element_text(angle=35, vjust=.8, hjust=0.8, size=16),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ggtitle("Participant racial composition") +
  scale_x_discrete(labels=c("American Indian\n or Alaska Native", "Asian",
                            "Black or African\n American", "Hispanic or Latino",
                            "Middle Eastern or\n North African", 
                            "Native Hawaiian\n or Other \n Pacific Islander",
                            "White", "Other", "Choose not to\n answer"))
ggsave("sac_plots/races.png", width=10, height=4.5, units="in")


# household income
unique(surv$Q27.)
survhi<-filter(surv, Q27. != "" & !is.na(Q27.))
ggplot(survhi, aes(Q27.)) + geom_bar(col="black", fill="#28bfd9") + 
  theme_bw() + theme(panel.grid = element_blank(), text = element_text(size = 24),
                     axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8, size=16)) +
  xlab("Income") + ylab("Frequency") + ggtitle("Annual household income") +
  scale_x_discrete(labels=c("Less than $59,999", "$60,000 to $119,000",
                            "$120,000 to $179,999", "$180,000 to $239,999",
                            "$240,000 or more", "Choose not to answer"))
ggsave("sac_plots/incomes.png", width=10, height=4.5, units="in")

# education level
unique(surv$Q28.)
surved<-filter(surv, Q28. != "")
ggplot(surved, aes(Q28.)) + geom_bar(col="black", fill="#28bfd9") + 
  theme_bw() + theme(panel.grid = element_blank(), text = element_text(size = 24),
                     axis.text.x=element_text(angle=35, vjust=.8, hjust=0.8, size=16),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ggtitle("Highest level of participant education") +
  scale_x_discrete(labels=c("Some high school or\n high school graduate",
                            "Some college (<=\n Associate's Degree",
                            "College graduate", "Graduate degree",
                            "Vocational degree", "Other", "Choose not to\n answer"))
ggsave("sac_plots/education.png", width=10, height=4.5, units="in")


# heatmap of zipcodes
zips<-surv %>%
  group_by(Q22) %>%
  summarise(zipcount=n())

CA_Zips<-read_sf('sac_plots/ca_zips/California_Zip_Codes.shp')
CA_Zips<-st_transform(CA_Zips, crs=4326)

CA_Zips<-left_join(CA_Zips, zips, by=c("ZIP_CODE"="Q22"))

ggplot() +
  geom_sf(data=CA_Zips, aes(fill=zipcount), lwd=.1, col="black") +
  scale_fill_viridis_c(option = "D", na.value = "gray96", name="Number of\n respondents") +
  theme_bw() + theme(panel.grid=element_blank())
ggsave("sac_plots/allca.png", height=7, width=7, units="in")

# sb focus
ggplot() +
  geom_sf(data=CA_Zips, aes(fill=zipcount), lwd=.1, col="black") +
  scale_fill_viridis_c(option = "D", na.value = "gray96", name="Number of\n respondents") +
  theme_bw() + theme(panel.grid=element_blank()) +
  ylim(33.5,35.5) + xlim(-121,-117)
ggsave("sac_plots/sb.png", width=7, height=3.5, units="in")

# sf bay area focus
ggplot() +
  geom_sf(data=CA_Zips, aes(fill=zipcount), lwd=.1, col="black") +
  scale_fill_viridis_c(option = "D", na.value = "gray96", name="Number of\n respondents") +
  theme_bw() + theme(panel.grid=element_blank()) +
  ylim(36.5,38.5)  + xlim(-123,-120.5)
ggsave("sac_plots/sf_bay.png", width=7, height=6, units="in")


# familiarity with sanctuaries
survsanct<-filter(surv, Q18. != "")
tot=nrow(survsanct)
survsanct<- survsanct %>%
  group_by(Q18.) %>%
  summarise(pct=n()/tot)
  
ggplot(survsanct, aes(x=rev(Q18.), y=pct, fill=as.numeric(Q18.))) + 
  geom_bar(col="black", stat="identity") + 
  theme_bw() + theme(panel.grid = element_blank(), text = element_text(size = 24),
                     axis.title.y = element_blank(), axis.title.x = element_blank(),
                     legend.position = "none") +
  ggtitle("How familiar are you with National Marine\n Sanctuaries?") +
  scale_x_discrete(labels=rev(c("Not familiar at all", "Slightly familiar",
                            "Moderately familiar", "Very familiar",
                            "Extremely familiar"))) +
  coord_flip() +
  geom_text(aes(label = scales::percent(pct)), hjust = -0.1) +
  scale_fill_gradient(low = "#e2f9fd", high = "#28bfd9")
ggsave("sac_plots/sanct_knowledge.png", width=10, height=4.5, units="in")

# sanctuary familiarity - low income
survsanct<-filter(surv, Q18. != "" & Q27. == "1")
tot=nrow(survsanct)
survsanct<- survsanct %>%
  group_by(Q18.) %>%
  summarise(pct=n()/tot)

ggplot(survsanct, aes(x=rev(Q18.), y=pct, fill=as.numeric(Q18.))) + 
  geom_bar(col="black", stat="identity") + 
  theme_bw() + theme(panel.grid = element_blank(), text = element_text(size = 24),
                     axis.title.y = element_blank(), axis.title.x = element_blank(),
                     legend.position = "none") +
  ggtitle("How familiar are you with National Marine\n Sanctuaries?") +
  scale_x_discrete(labels=rev(c("Not familiar at all", "Slightly familiar",
                                "Moderately familiar", "Very familiar",
                                "Extremely familiar"))) +
  coord_flip() +
  geom_text(aes(label = scales::percent(pct)), hjust = -0.1) +
  scale_fill_gradient(low = "#e2f9fd", high = "#28bfd9")
ggsave("sac_plots/sanct_knowledge_lowincome.png", width=10, height=4.5, units="in")

# just ucsc data
survsanct<-filter(ucsc, Q18. != "")
nrow(survsanct)
tot=nrow(survsanct)
survsanct<- survsanct %>%
  group_by(Q18.) %>%
  summarise(pct=n()/tot)

ggplot(survsanct, aes(x=rev(Q18.), y=pct, fill=as.numeric(Q18.))) + 
  geom_bar(col="black", stat="identity") + 
  theme_bw() + theme(panel.grid = element_blank(), text = element_text(size = 24),
                     axis.title.y = element_blank(), axis.title.x = element_blank(),
                     legend.position = "none") +
  ggtitle("Monterey Bay") +
  scale_x_discrete(labels=rev(c("Not familiar at all", "Slightly familiar",
                                "Moderately familiar", "Very familiar",
                                "Extremely familiar"))) +
  coord_flip() +
  geom_text(aes(label = scales::percent(pct)), hjust = -0.1) +
  scale_fill_gradient(low = "#e2f9fd", high = "#28bfd9")
ggsave("sac_plots/sanct_knowledge_ucsc.png", width=13, height=4, units="in")

# ucsb
survsanct<-filter(ucsb, Q18. != "")
tot=nrow(survsanct)
survsanct<- survsanct %>%
  group_by(Q18.) %>%
  summarise(pct=n()/tot)

ggplot(survsanct, aes(x=rev(Q18.), y=pct, fill=as.numeric(Q18.))) + 
  geom_bar(col="black", stat="identity") + 
  theme_bw() + theme(panel.grid = element_blank(), text = element_text(size = 24),
                     axis.title.y = element_blank(), axis.title.x = element_blank(),
                     legend.position = "none") +
  ggtitle("Santa Barbara") +
  scale_x_discrete(labels=rev(c("Not familiar at all", "Slightly familiar",
                                "Moderately familiar", "Very familiar",
                                "Extremely familiar"))) +
  coord_flip() +
  geom_text(aes(label = scales::percent(pct)), hjust = -0.1) +
  scale_fill_gradient(low = "#e2f9fd", high = "#28bfd9")
ggsave("sac_plots/sanct_knowledge_ucsb.png", width=13, height=4, units="in")

# make an sb zip code plot with same aspect ratio as monterey and just ucsb data
zipsb<-ucsb %>%
  group_by(Q22) %>%
  summarise(zipsb=n())

CA_Zips<-left_join(CA_Zips, zipsb, by=c("ZIP_CODE"="Q22"))

ggplot() +
  geom_sf(data=CA_Zips, aes(fill=zipsb), lwd=.1, col="black") +
  scale_fill_viridis_c(option = "D", na.value = "gray96", name="Number of\n respondents") +
  theme_bw() + theme(panel.grid=element_blank()) +
  ylim(32.9,35.7) + xlim(-121,-117.7)
ggsave("sac_plots/sb_square.png", width=7, height=6, units="in")

# just ucsc data
zipsc<-ucsc %>%
  group_by(Q22) %>%
  summarise(zipsc=n())

CA_Zips<-left_join(CA_Zips, zipsc, by=c("ZIP_CODE"="Q22"))

ggplot() +
  geom_sf(data=CA_Zips, aes(fill=zipsc), lwd=.1, col="black") +
  scale_fill_viridis_c(option = "D", na.value = "gray96", name="Number of\n respondents") +
  theme_bw() + theme(panel.grid=element_blank()) +
  ylim(36.5,37.9)  + xlim(-122.6,-121)
ggsave("sac_plots/monterey_bay.png", width=7, height=6, units="in")

# plot results of microaggression questions
surv1<-filter(surv, Q14._1 != "")
ggplot(surv1, aes(Q14._1)) + geom_histogram(binwidth=1, col="black", fill="#28bfd9") + 
  theme_bw() + theme(panel.grid = element_blank(), text = element_text(size = 24),
                     axis.title.x = element_blank()) +
  ylab("Frequency") + ggtitle("Treated w/ same amount of respect") +
  scale_x_discrete(labels=c("Never", "Rarely", "Sometimes", "Often",
                            "Always"))










