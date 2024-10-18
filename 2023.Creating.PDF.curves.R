#2023.01
#Alene Onion with input from Tony Olsen and Michael Dumelle
#creating PDF curves for lake reports

library(tidyverse)
library(huxtable)
library(ggplot2)
library(lubridate)
library(spsurvey) #This code was written for spsurvey v5.0.0

#Read in data ##################################
# rm(list=setdiff(ls(), c("newdata")))
# setwd("C:/Users/amonion/New York State Office of Information Technology Services/BWAM - Lakes Database/Current")
# source("new_database/Reading.LMAS.Data.R")
setwd("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/Probability.Sampling")
rm(list=setdiff(ls(), c("newdata")))


#Data Prep###########################
lab<-newdata %>%
  filter(SAMPLE_DATE>'2020-01-01') %>%
  mutate(combined=paste(CHARACTERISTIC_NAME,
                        INFORMATION_TYPE,
                        RSLT_RESULT_SAMPLE_FRACTION,
                        sep = "_"))  %>%
  select(LAKE_HISTORY_ID,
         SAMPLE_DATE,
         combined,
         RSLT_RESULT_VALUE,
         RSLT_LABORATORY_QUALIFIER,
         RSLT_VALIDATOR_QUALIFIER,
         RSLT_PROFILE_DEPTH) %>%
  mutate(RSLT_RESULT_VALUE=ifelse(!is.na(RSLT_LABORATORY_QUALIFIER)&(RSLT_LABORATORY_QUALIFIER=="U"|RSLT_LABORATORY_QUALIFIER=="UE"),"0",RSLT_RESULT_VALUE),
         RSLT_RESULT_VALUE=as.numeric(RSLT_RESULT_VALUE)) %>%
  filter(!is.na(RSLT_RESULT_VALUE),
         is.na(RSLT_VALIDATOR_QUALIFIER)|(RSLT_VALIDATOR_QUALIFIER!="R"),
         combined %in% c("ARSENIC_OW_TOTAL",
                         "IRON_OW_TOTAL",
                         "MAGNESIUM_OW_TOTAL",
                         "MANGANESE_OW_TOTAL",
                         "NITROGEN, NITRITE_OW_TOTAL",
                         'CHLOROPHYLL A_OW_TOTAL',
                         'PHOSPHORUS, TOTAL_OW_TOTAL',
                         "TRUE COLOR_OW_TOTAL",
                         "MICROCYSTIN_OW_NA",
                         "SULFATE (AS SO4)_OW_TOTAL",
                         "NITROGEN, NITRATE (AS N)_OW_TOTAL",
                         "NITROGEN, NITRATE-NITRITE_OW_TOTAL",
                         "NITROGEN, KJELDAHL, TOTAL_OW_TOTAL",
                         "NITROGEN, TOTAL_OW_TOTAL",
                         "NITROGEN, AMMONIA (AS N)_OW_TOTAL",
                         "DISSOLVED OXYGEN_DP_NA",
                         "PH_DP_NA",
                         "TRUE COLOR_OW_TOTAL",
                         "PH_DP_NA",
                         "SPECIFIC CONDUCTANCE_DP_NA",
                         "TEMPERATURE_DP_NA",
                         "DEPTH, SECCHI DISK DEPTH_SD_NA",
                         "CALCIUM_OW_TOTAL",
                         "CHLORIDE_OW_TOTAL",
                         "CHLOROPHYLL A (PROBE) CONCENTRATION, CHLOROPHYTE (GREEN ALGAE)_OW_NA",
                         "CHLOROPHYLL A (PROBE) CONCENTRATION, CRYPTOPHYTA (CRYPTOPHYTES)_OW_NA",
                         "CHLOROPHYLL A (PROBE) CONCENTRATION, CYANOBACTERIA (BLUEGREEN)_OW_NA",
                         "CHLOROPHYLL A (PROBE) CONCENTRATION, DINOPHYTA (DIATOMS)_OW_NA",
                         "CHLOROPHYLL A (PROBE) CONCENTRATION, TOTAL_OW_NA",
                         "ALKALINITY, TOTAL (AS CACO3)_OW_TOTAL")) %>%
  select(LAKE_HISTORY_ID,SAMPLE_DATE,combined,RSLT_RESULT_VALUE,RSLT_PROFILE_DEPTH) %>%
  distinct(LAKE_HISTORY_ID,SAMPLE_DATE,combined,RSLT_PROFILE_DEPTH,.keep_all = TRUE) %>%
  rename(LAKE_ID=LAKE_HISTORY_ID,
         chemical_name=combined,
         result_value=RSLT_RESULT_VALUE,
         profile_depth=RSLT_PROFILE_DEPTH)

# dissolved oxygen:
# keep DO values between 0 and 2m depth
lab2 <- lab %>% 
  mutate(keep=case_when(
    !(chemical_name=="DISSOLVED OXYGEN_DP_NA") ~ "no",
    chemical_name=="DISSOLVED OXYGEN_DP_NA" & profile_depth <= 2 ~ "yes",
    chemical_name=="DISSOLVED OXYGEN_DP_NA" & profile_depth > 2 ~ "no")) %>%
  filter(keep!="no") %>%
  select(-keep)

# mean DO for each lake each date
DO <- aggregate(lab2$result_value,list(lab2$LAKE_ID,lab2$SAMPLE_DATE),FUN=mean) %>% 
  mutate(chemical_name="DISSOLVED OXYGEN_epi") %>% 
  rename(LAKE_ID=Group.1,
         SAMPLE_DATE=Group.2,
         result_value=x)

#thermocline
thermocline<-lab %>% 
  filter(chemical_name=='TEMPERATURE_DP_NA') %>% 
  mutate(thermocline=NA)
LID<-thermocline$LAKE_ID[1]
date<-thermocline$SAMPLE_DATE[1]
depth<-thermocline$profile_depth[1]
temp<-thermocline$result_value[1]

for(i in seq(nrow(thermocline))){
  current<-thermocline[i,]
  if(current$LAKE_ID==LID&current$SAMPLE_DATE==date){
    depth=current$profile_depth
    if((temp-current$result_value)>1){
      thermocline$thermocline[i]<-current$profile_depth
    }
    temp=current$result_value
  }
  else{
    LID=current$LAKE_ID
    date=current$SAMPLE_DATE
    depth=current$profile_depth
    temp=current$result_value
  }
}

thermocline<-thermocline %>%  #pull the lowest value for all
  group_by(LAKE_ID,SAMPLE_DATE) %>% 
  mutate(thermocline=min(thermocline, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(thermocline=ifelse(thermocline==Inf,NA,thermocline)) %>% 
  select(LAKE_ID,SAMPLE_DATE,thermocline,profile_depth) %>% 
  filter(!is.na(thermocline)) %>%   #remove those without a thermocline
  distinct()

#epilimentic means for ph and spconductance
epi<-lab %>% 
  inner_join(thermocline,by=c('LAKE_ID','SAMPLE_DATE','profile_depth')) %>% 
  filter(chemical_name %in% c("PH_DP_NA","SPECIFIC CONDUCTANCE_DP_NA")) %>% 
  mutate(result_value = ifelse(chemical_name=="PH_DP_NA",(10^result_value),result_value)) %>% 
  distinct() %>% 
  arrange(LAKE_ID,SAMPLE_DATE,chemical_name,profile_depth) 

epi<-epi %>% 
  filter(profile_depth<=thermocline) %>% 
  distinct() %>% 
  group_by(LAKE_ID,SAMPLE_DATE,chemical_name) %>% 
  summarize(Mean=mean(result_value,na.rm=TRUE),
            n=n()) %>% 
  filter(n>2) %>% 
  select(LAKE_ID,SAMPLE_DATE,chemical_name,Mean)

epi<-epi %>% 
  mutate(Mean=ifelse(chemical_name=="PH_DP_NA",log10(Mean),Mean)) %>% 
  rename(result_value=Mean) %>% 
  mutate(chemical_name=case_when(
    chemical_name=="PH_DP_NA"~"PH_epi",
    chemical_name=="SPECIFIC CONDUCTANCE_DP_NA"~"SPECIFIC CONDUCTANCE_epi"
  ))

# read in site data
sites<-read.csv("Probability_Based_Sites_2020_2021.csv")
sites<-sites %>% 
  rename(siteID=SITE_ID,
         xcoord=LON_DD83,
         ycoord=LAT_DD83,
         Eval_Status=EvalStatus) %>% 
  filter(Eval_Status!="") %>%    #removing sites that we haven't yet evaluated
  mutate(Eval_Status=trimws(Eval_Status))

# restrict to only the data in the probability study and spread the data
att<-merge(lab,epi,by=c("LAKE_ID","SAMPLE_DATE","chemical_name"),all=TRUE) %>% 
  mutate(result_value=ifelse(is.na(result_value.x),result_value.y,result_value.x)) %>% 
  select(-result_value.x,-result_value.y)
att<-merge(att,DO,by=c("LAKE_ID","SAMPLE_DATE","chemical_name"),all=TRUE)%>% 
  mutate(result_value=ifelse(is.na(result_value.x),result_value.y,result_value.x)) %>% 
  filter(!chemical_name %in% c("DISSOLVED OXYGEN_DP_NA","PH_DP_NA","SPECIFIC CONDUCTANCE_DP_NA","TEMPERATURE_DP_NA")) %>% 
  select(-result_value.x,-result_value.y)

att<-merge(att,sites,by=c('LAKE_ID'),all.y = TRUE) %>% distinct()
att<-att %>% spread(chemical_name,result_value,fill = NA)

# Create a Target/NonTarget status variable
att<-att %>% 
  mutate(statusTNT="Target",
         statusTNT=ifelse(Eval_Status=="NonTarget","NonTarget",statusTNT))

# remove duplicates (multiple samples of same lake)
att <- att %>%
  mutate(SAMPLE_DATE = ymd(SAMPLE_DATE)) %>% # convert to date
  mutate(month = month(SAMPLE_DATE)) %>% # extract month from date
  group_by(siteID) %>%
  mutate(has_aug = 8 %in% month) %>% # annotate as having date in august
  filter(has_aug & month == 8 |
           !has_aug) %>% # filter only august dates from sites with or all from those without
  slice_max(n = 1,
            order_by = SAMPLE_DATE,
            with_ties = F) %>% # change with_ties to TRUE to discard NA values
  select(-has_aug)

#Adjust weights ###############
att<-att %>% 
  ungroup() %>% 
  mutate(WgtAdj=case_when(
    PROB_CAT=="(1,4]"~(1828/29),
    PROB_CAT=="(4,10]"~(2490/15),
    PROB_CAT=="(10,20]"~(1003/18),
    PROB_CAT=="(20,50]"~(616/20),
    PROB_CAT==">50"~(500/33)))

#creating pdf #########################
#first for one parameter
probDF<-ash1_wgt(att$CHLOROPHYLL_ug_L,wgt=att$WgtAdj)
probDF<-data.frame(x=probDF$x,y=probDF$y)
probDF<-probDF %>% 
  rename(results=x,
         probability=y) %>% 
  mutate(parameter="CHLOROPHYLL_ug_L")
#now for all the rest

#testing a plot######################
library(ggplot2)
ggplot(probDF %>% filter(parameter=="CHLOROPHYLL_ug_L"),aes(x=results,y=probability))+
  geom_line()