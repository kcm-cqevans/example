rm(list=ls(all=TRUE))

library(here)
library(dplyr)
library(stringr)
library(tidyr)
library(hablar)
library(tidyverse)
library(writexl)
library(openxlsx)
library(XML)
library(survey)
library(haven)
library(jtools)
library(remotes)
library(ggsurvey)
library(svyVGAM)
library(srvyr)
library(epiDisplay)
library(egg)
library(sf)             # For loading shapefiles
library(mapview)
library(stm)
library(tm)
#library(NLTK)
library(quanteda)
library(topicmodels)
library(stminsights) 
library(wordcloud)
load("Data_Merged_2022-2023.RData")

data<- data %>%  filter(wave<=20224) %>% 
  mutate(wave=case_when(
    wave==20222 ~ "2022, Q2",
    wave==20223 ~ "2022, Q3", 
    wave==20224 ~ "2022, Q4"
  )) %>% 
  mutate(rider = case_when(
    ridestat<=0~"Not current rider",
    ridestat==1~"Current rider")) %>% 
  mutate(rider_num = case_when(
    rider=="Not current rider" ~0,
    rider=="Current rider" ~100)) %>% 
  mutate(freqrider = case_when(transit_freq=="1-2 trips" ~ "Infrequent Rider",
                               transit_freq=="3-5 trips" | transit_freq=="6-10 trips" ~"Somewhat Frequent Rider",
                               transit_freq=="10-29 trips" | transit_freq=="More than 30 trips" ~ "Frequent Rider")) %>% 
  mutate(region=case_when(region=="Seattle" ~ "Seattle/North King County",
                          region=="East" ~ "East King County",
                          region=="South" ~ "South King County"))

data$freqrider <- factor(data$freqrider, levels=c("Infrequent Rider", "Somewhat Frequent Rider", "Frequent Rider"))


svy <- data %>%
  as_survey_design(1, weight = wgt)


blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_text(size=15, color="black"),
    axis.title.y = element_text(size=12, color="black"),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=18, face="bold"), 
    legend.text = element_text(size=12) ,
    plot.margin = margin(0, 0, 0, 0, "cm")
  )


#Figure - sample size
pdf(file=here("AnnualReportFigures/SampleSize.pdf"),  width=5,height=5)
tab1(data$wave, col="#006666", main="", ylab="Number of respondents", cex=0.8, cex.axis=0.8)
dev.off()


#Figure - ridership pie
out_riderpie<-svy %>%
  group_by(rider) %>%
  summarize(proportion = 100*survey_mean(),
            total = survey_total())

pdf(file=here("AnnualReportFigures/RiderStatus_pie.pdf"), width=3,height=4)
ggplot(out_riderpie, aes(x="", y=proportion, fill=rider)) +
  labs(title="Ridership in King County") +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual("", values=c( "#AC95DA",  "#3A2761")) +
  blank_theme + theme(axis.text.x=element_blank(), 
                      text = element_text(size = 8),  
                      legend.title = element_text(size=20), 
                      legend.text = element_text(size=8), 
                      plot.margin = margin(0,0,0,0, "cm"),
                      legend.position = ("bottom"),
                      plot.title = element_text(size=12, face="bold"), 
                      legend.key.height = unit(0.4, 'cm')) +
  geom_text(aes(label = paste(round(proportion, digits=1), "%")),
            position = position_stack(vjust = 0.5), color=("white"), size=3) + labs(x="", y="")

dev.off()


#Figure 3 - Ridership across demographics
  #Income
      out_riderincome <- svyby(design = svy,
                   formula = ~rider_num, 
                   by = ~income_5cat, 
                   FUN = svymean, 
                   na.rm = TRUE, 
                   keep.names = FALSE)
      # Construct a bar plot of average satisfaction overall by region
      riders_income<-ggplot(data =out_riderincome , mapping = aes(x=income_5cat, y=rider_num)) +
        labs(title="Household Income", y="", x="")+
        stat_summary( fun=mean, geom="bar", fill="goldenrod") +
        geom_text(aes(label = paste(round(rider_num, digits=1), "%")),
                  nudge_y = 2, size=3) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
        theme_classic()
  #Age
      out_riderage <- svyby(design = svy,
                   formula = ~rider_num, 
                   by = ~agecat, 
                   FUN = svymean, 
                   na.rm = TRUE, 
                   keep.names = FALSE)
      riders_age<-ggplot(data =out_riderage , mapping = aes(x=agecat, y=rider_num)) +
        labs(title="Age Group", y="", x="")+
        stat_summary( fun=mean, geom="bar", fill="#3A2761") +
        geom_text(aes(label = paste(round(rider_num, digits=1), "%")),
                  nudge_y = 2, size=3) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
        theme_classic()

  # Rent_own status
      out_riderrent <- svyby(design = svy,
                   formula = ~rider_num, 
                   by = ~rentown, 
                   FUN = svymean, 
                   na.rm = TRUE, 
                   keep.names = FALSE)
      riders_rent<-ggplot(data =out_riderrent , mapping = aes(x=rentown, y=rider_num)) +
        labs(title="Home Ownership Status", y="", x="")+
        stat_summary( fun=mean, geom="bar", fill="#3A2761") +
        geom_text(aes(label = paste(round(rider_num, digits=1), "%")),
                  nudge_y = 2, size=3) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
        theme_classic()
      
  # Region  
      out_riderregion <- svyby(design = svy,
                   formula = ~rider_num, 
                   by = ~region, 
                   FUN = svymean, 
                   na.rm = TRUE, 
                   keep.names = FALSE)
      riders_region<-ggplot(data =out_riderregion , mapping = aes(x=region, y=rider_num)) +
        labs(title="Subregion", y="", x="")+
        stat_summary( fun=mean, geom="bar", fill="goldenrod") +
        geom_text(aes(label = paste(round(rider_num, digits=1), "%")),
                  nudge_y = 2, size=3) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
        theme_classic()
  #complete figure for figure 3  
      plot<-ggarrange(riders_age, riders_income, riders_region, riders_rent,  ncol = 2, nrow = 2)
      
pdf(file=here("AnnualReportFigures/RiderDemos.pdf"),  width=8,height=5)
plot
dev.off()

#Figure - Frequency of Transit Use [NOT IN REPORT]
out_transitfreq <-svy %>%
  filter(rider_num==100) %>% 
  group_by(transit_freq) %>%
  summarize(prop = 100*survey_prop(proportion=TRUE))

transit_freq<-ggplot(data = out_transitfreq, aes(x = transit_freq , y = prop)) +
  labs(title="Current Transit Riders: Frequency of Use",  y="", x="") +
  stat_summary( fun=mean, geom="bar", fill="goldenrod") +
  geom_text(aes(label = paste(round(prop, digits=1), "%")),
            nudge_y=1, size=6) +
  theme_classic() + 
  theme(axis.text.x=element_text(size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y = element_text(size=16)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

#Figure 5- Trip purpose
purposedata <- data %>%
  #filter(freqrider=="Infrequent Rider") %>% 
  filter(rider_num==100) %>% 
  dplyr::select(uniqueid, wgt, freqrider, starts_with("purpose_")) %>% 
  dplyr::select(!c(purpose_other_txt, purpose_dk, purpose_primtrip))

purposedata <- reshape(purposedata, direction = "long",
                       idvar = c("uniqueid", "wgt"),
                       varying = 4:15,
                       v.names = c("value"),
                       sep = "")
purposedata<- purposedata %>% 
  mutate(trippurpose = case_when(time== 1 ~ "Work",
                                 time == 2 ~ "School",
                                 time == 3 ~ "Child care",
                                 time == 4 ~ "Shopping",
                                 time == 5 ~ "Fun, social",
                                 time ==6 ~ "Special events",
                                 time==7 ~ "Airport",
                                 time==8 ~ "Medical appt",
                                 time==9 ~ "Social services",
                                 time==10 ~ "Judicial",
                                 time==11 ~ "Other",
                                 time==12 ~ "All trips",)) %>% 
  mutate(value= value*100)
purposedata <- purposedata %>%
  as_survey_design(1, weight = wgt) 

out_purposedata<-purposedata %>%
  filter(!is.na(freqrider)) %>% 
  group_by(freqrider, trippurpose, value) %>%
  summarize(prop = 100*survey_prop(proportion = TRUE)) %>% 
  filter(value==100)

pdf(file=here("AnnualReportFigures/TripPurpose.pdf"), width=5.5,height=4)
ggplot(out_purposedata, aes(x = trippurpose, y = prop, fill=freqrider)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x="", y="", title="Trip Purpose") +
  geom_text(aes(label = ifelse(prop >= 10, paste0(sprintf("%.0f", prop),"%"),"")),
            position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.25, hjust=.4, size=2.2) +
  theme_classic() + 
  #scale_fill_manual("", values=c("#66A3A3", "#338585", "#006666")) +
  scale_fill_manual("", values=c("#FF6600", "#0066FF", "#744EC2")) +
  theme(axis.text.x=element_text(size=7, color="black"),
        axis.text.y=element_text(size=7, color="black"),
        axis.title.y = element_text(size=7), text = element_text(size = 7),
        legend.position = "bottom",
        legend.text = element_text(size=7),
        plot.title=element_text(size=12, face="bold"),
        legend.key.height = unit(0.4, 'cm')) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 9))
dev.off()
  
  
  
  
  #geom_bar(stat="identity", position = "dodge") +   
  #labs(title="Purpose of Transit Trip", y="", x="")+
  #geom_text(aes(label = paste(round(prop, digits=1), "%")), size=4, nudge_y=1.3) +
  #stat_summary( fun=mean, geom="bar", fill="#3A2761") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
  #theme_classic()


svymean(~commute, subset(svy, freqrider=="Frequent Rider" & rider_num==100))
svymean(~comm_days, subset(svy, freqrider=="Frequent Rider" & rider_num==100 & commute=="Yes"))

svymean(~todtravel_pmpeak, subset(svy, freqrider=="Frequent Rider" & rider_num==100))
svymean(~todtravel_saturday, subset(svy, freqrider=="Frequent Rider" & rider_num==100))
svymean(~todtravel_sunday, subset(svy, freqrider=="Frequent Rider" & rider_num==100))
svymean(~todtravel_midday, subset(svy, freqrider=="Frequent Rider" & rider_num==100))

svymean(~transit_allneeds, subset(svy, freqrider=="Frequent Rider" & rider_num==100))


svymean(~income_5cat, subset(svy, freqrider=="Frequent Rider" & rider_num==100))


summary(svyglm(kids_hh~as.factor(freqrider), design=svy))

#Figure 6 - Time of Travel 
todtravel <- data %>%
  #filter(freqrider=="Infrequent Rider") %>% 
  filter(rider_num==100) %>% 
  filter(!is.na(freqrider)) %>% 
  dplyr::select(uniqueid, wgt, freqrider, starts_with("todtravel_")) %>% 
  dplyr::select(!c(todtravel_missing))

todtravel_long <- reshape(todtravel, direction = "long",
                       idvar = c("uniqueid", "wgt"),
                       varying = 4:10,
                       v.names = c("value"),
                       sep = "")
todtravel_long<- todtravel_long %>% 
  mutate(todtravel = case_when(time== 1 ~ "AM Peak (5a-9a)",
                                 time == 2 ~ "Midday (9a-3p)",
                                 time == 3 ~ "PM Peak (3p-7p)",
                                 time == 4 ~ "Night (7p-10p)",
                                 time == 5 ~ "Late night (10p-5a)",
                                 time ==6 ~ "Saturday",
                                 time==7 ~ "Sunday")) %>% 
  mutate(value= value*100)
todtravel_long <- todtravel_long %>%
  as_survey_design(1, weight = wgt) 

todtravel_long$todtravel <- factor(todtravel_long$todtravel, 
                          levels=c("AM Peak (5a-9a)", "Midday (9a-3p)", "PM Peak (3p-7p)",
                                   "Night (7p-10p)", "Late night (10p-5a)", "Saturday", "Sunday" ))


todtravel_long<-todtravel_long %>%
#  filter(!is.na(freqrider)) %>% 
  group_by(todtravel, freqrider, value) %>%
  summarize(prop = 100*survey_prop(proportion = TRUE)) %>% 
  filter(value==100)

pdf(file=here("AnnualReportFigures/TODTravel.pdf"), width=5,height=4)
ggplot(todtravel_long, aes(x = todtravel, y = prop, fill=freqrider)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x="", y="", title="Time of Day of Travel") +
  geom_text(aes(label = ifelse(prop >= 10, paste0(sprintf("%.0f", prop),"%"),"")),
            position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.25, hjust=.4, size=2.2) +
  theme_classic() + 
  #scale_fill_manual("", values=c("#66A3A3", "#338585", "#006666")) +
  scale_fill_manual("", values=c("#FF6600", "#0066FF", "#744EC2")) +
  theme(axis.text.x=element_text(size=7, color="black"),
        axis.text.y=element_text(size=7, color="black"),
        axis.title.y = element_text(size=7), text = element_text(size = 7),
        legend.position = "bottom",
        legend.text = element_text(size=7),
        plot.title=element_text(size=12, face="bold"),
        legend.key.height = unit(0.4, 'cm')) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 9))
dev.off()




# Figure - satisfaction with service elements

satdata <- data %>% 
  filter(ridestat_metro==1 & rider_num==100) %>% 
  dplyr::select(uniqueid, wgt, starts_with("sat_"))
satdata_long <- reshape(satdata, direction = "long",  idvar = c("uniqueid","wgt"), 
                        varying = 3:30, v.names = c("value"), sep = "")

satdata_long<- satdata_long %>% 
  mutate(satisfaction = case_when(time== 1 ~ "Metro overall",
                                  time == 2 ~ "Metro Bus overall",
                                  time == 3 ~ "Frequency of service",
                                  time == 4 ~ "Ontime perf.",
                                  time == 5 ~ "Travel time",
                                  time == 6 ~ "Clean: stops",
                                  time == 7 ~ "Clean: onboard",
                                  time == 8 ~ "Comfort of bus",
                                  time == 9 ~ "Seating: onboard",
                                  time == 10 ~ "Seating: stop",
                                  time == 11 ~ "Transfer wait time",
                                  time == 12 ~ "Drivers' operation",
                                  time == 13 ~ "Safety onboard, day",
                                  time == 14 ~ "Safety waiting, day",
                                  time == 15 ~ "Safety onboard, night",
                                  time == 16 ~ "Safety waiting, night",
                                  time == 17 ~ "Trip plan: overall",
                                  time == 18 ~ "Trip plan: ease of use",
                                  time == 19 ~ "Trip plan: accurate info",
                                  time == 20 ~ "Trip plan: accurate timing",
                                  time == 21 ~ "Trip plan: multimodal",
                                  time == 22 ~ "Safety: getting to/from transit",
                                  time == 23 ~ "Ontime perf.: to stop",
                                  time == 24 ~ "Ontime perf.: to destination",
                                  time == 25 ~ "ORCA payment: overall",
                                  time == 26 ~ "ORCA payment: loading value",
                                  time == 27 ~ "Info: planned changes",
                                  time == 28 ~ "Info: unplanned changes")) %>% 
  filter(!is.na(value)) 

satdata_long<- satdata_long %>%   
  filter(value!="Does not apply to me") %>% 
  filter(value!="No opinion") %>% 
  filter(satisfaction!="Ontime perf.: to destination" & satisfaction!="Ontime perf.: to stop") %>% 
  mutate(response = fct_relevel(value,  
                      c("Very dissatisfied", "Somewhat dissatisfied", 
                        "Somewhat satisfied", "Very satisfied")))

svy <- satdata_long %>%
  as_survey_design(1, weight = wgt)


#Figure
out<-svy %>%
  group_by(satisfaction, response) %>%
  summarize(prop = 100*survey_prop(proportion = TRUE))


pdf(file=here("AnnualReportFigures/Satisfaction.pdf"), width=15,height=12)
ggplot(data = out, aes(x = satisfaction , y = prop, fill=response)) +
  labs(x="", y="")+
  geom_bar(stat="identity") + 
  geom_text(aes(label=ifelse(prop >= 0.07, paste0(sprintf("%.0f", prop),"%"),"")),
            position=position_stack(vjust=0.5), colour="white") +
  scale_fill_manual("", values=c(  "#FF0000", "#FF6666","#338585", "#006666")) +
  #scale_y_continuous(labels = scales::percent) +
  labs(y="", x="") + theme_classic() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  theme(axis.text.x=element_text(size=10, color="black"),
      axis.text.y=element_text(size=10, color="black"),
      axis.title.y = element_text(size=7), text = element_text(size = 7),
      legend.position = "bottom",
      legend.text = element_text(size=10),
      plot.title=element_text(size=12, face="bold"),
      legend.key.height = unit(0.4, 'cm')) 
dev.off()
 



# Figure - satisfaction with service elements

bhv_data <- data %>% 
  dplyr::select(uniqueid, wgt, starts_with("bhv_")) %>% 
  dplyr::select(-c(bhv_tom, bhv_tomCoded, bhv_positive, bhv_pos_news))
bhv_data_long <- reshape(bhv_data, direction = "long",  idvar = c("uniqueid","wgt"), 
                        varying = 3:6, v.names = c("value"), sep = "")

bhv_data_long<- bhv_data_long %>% 
  mutate(behavior = case_when(time== 1 ~ "Enjoy using transit",
                                  time == 2 ~ "Transit is safe",
                                  time == 3 ~ "Transit is important",
                                  time == 4 ~ "Consider sustainability")) %>% 
                                  #time == 5 ~ "Hear positive things",
                                  #time == 6 ~ "Hear positive news"
  filter(!is.na(value)) 


bhv_svy <- bhv_data_long %>%
  as_survey_design(1, weight = wgt)


#Figure
out_bhv_svy<-bhv_svy %>%
  group_by(behavior, value) %>%
  summarize(prop = 100*survey_prop(proportion = TRUE))


pdf(file=here("AnnualReportFigures/Attitudes.pdf"), width=5,height=4)
ggplot(data = out_bhv_svy, aes(x = reorder(behavior, desc(-prop)) , y = prop, fill=value)) +
  labs(x="", y="", title="Attitudes about Public Transit in King County")+
  geom_bar(stat="identity") + 
  geom_text(aes(label=ifelse(prop >= 10, paste0(sprintf("%.0f", prop),"%"),"")),
            position=position_stack(vjust=0.5), colour="white", size=3) +
  scale_fill_manual("", values=c("#006666", "#338585",  "#FF6666", "#FF0000","#CCCCCC")) +
  #scale_y_continuous(labels = scales::percent) +
  blank_theme + theme_classic() + 
   theme(legend.position = "bottom",
         plot.title=element_text(size=12, face="bold")) + guides(fill=guide_legend(ncol=3)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))
dev.off()


barrier <- data %>% 
  filter(freqrider=="Infrequent Rider") %>% 
  dplyr::select(barrier_verb, barrierCoded) %>% 
  filter(!is.na(barrier_verb)) 
#Getting rid of missing text data
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs = VCorpus(VectorSource(barrier$barrier_verb))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
#transforming all to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)


docs <- tm_map(docs, removeWords, c("on", "at", "to", "that", "it",
                                    "the", "a", "and", "about",
                                    "of", "i", "is", "light", "rail", "public", "transportation",
                                    "transit")) 
# Remove punctuation
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming --- this looks for roots of words, like walk(ed, ing)
docs <- tm_map(docs, stemDocument)


#Here - create a function that will look at x-word phrases. Here, the number of words in a phrase
#is set to 3, but you can change it 2, 4. Whatever
NgramTokenizer <-function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)


#Creating a Document Term Matrix out of the responses
#This just puts things in a format that then can be sorted and and analyzed
#Note that you're using the NgramTokenizer that we use above. 
dtm <- TermDocumentMatrix(docs, control=list(tokenize = NgramTokenizer, removePunctuation=TRUE, 
                                             stemming=TRUE))
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#looking at 20 most common n-word phrases, can look at however many.
head(d, 20)
wordcloud(d$word,d$freq,max.words=50,random.order = F)



incentive <- data %>% 
  filter(freqrider=="Somewhat Frequent Rider") %>% 
  dplyr::select(int_oe) %>% 
  filter(!is.na(int_oe)) 
#Getting rid of missing text data
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs = VCorpus(VectorSource(incentive$int_oe))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
#transforming all to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)


docs <- tm_map(docs, removeWords, c("on", "at", "to", "that", "it",
                                    "the", "a", "and", "about",
                                    "of", "i", "is", "light", "rail", "public", "transportation",
                                    "transit")) 
# Remove punctuation
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming --- this looks for roots of words, like walk(ed, ing)
docs <- tm_map(docs, stemDocument)


#Here - create a function that will look at x-word phrases. Here, the number of words in a phrase
#is set to 3, but you can change it 2, 4. Whatever
NgramTokenizer <-function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)


#Creating a Document Term Matrix out of the responses
#This just puts things in a format that then can be sorted and and analyzed
#Note that you're using the NgramTokenizer that we use above. 
dtm <- TermDocumentMatrix(docs, control=list(tokenize = NgramTokenizer, removePunctuation=TRUE,  stemming=TRUE))
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#looking at 20 most common n-word phrases, can look at however many.
head(d, 20)
wordcloud(d$word,d$freq,max.words=50,random.order = F)


svymean(~rrexper_reliability, subset(svy, rruser==1))










