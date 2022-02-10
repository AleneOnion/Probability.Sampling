library(tidyverse)

#load data

land.cover <- read.csv("Statewide_Lake_Watershed_NLCD_2016_percentages_NHD_LakeID.csv")
sites <- read.csv("Probability_Based_Sites_2020_2021.csv") %>% filter(EvalStatus!="") %>% rename(Lake_ID=LAKE_ID,GNIS_Name=GNIS_NAME)

# setwd("~/OneDrive - New York State Office of Information Technology Services/Rscripts/Current")
# source("new_database/Reading.LMAS.Data.R")
# setwd("~/OneDrive - New York State Office of Information Technology Services/Rscripts/Probability.Sampling/")
# 
# rm(list=setdiff(ls(), c('newdata',"land.cover","sites","att")))

newdata1 <- newdata %>% 
  filter(LAKE_HISTORY_ID %in% unique(sites$Lake_ID)) %>% 
  select(LAKE_HISTORY_ID,LCIFD_MAX_SOUND_DEPTH) %>% 
  mutate(DEPTHMAX=as.numeric(LCIFD_MAX_SOUND_DEPTH)) %>% 
  rename(Lake_ID=LAKE_HISTORY_ID) %>% 
  group_by(Lake_ID) %>% 
  summarise(max(DEPTHMAX,na.rm=T)) %>% 
  rename(DEPTHMAX=`max(DEPTHMAX, na.rm = T)`) %>% 
  ungroup() %>% 
  distinct()

att <- merge(sites,land.cover,by=c("Lake_ID"),all.x=T)

att <- merge(att,newdata1,by="Lake_ID",all.x=T) %>% mutate(DEPTHMAX=case_when(is.infinite(DEPTHMAX)~0,
                                                                              is.finite(DEPTHMAX)~DEPTHMAX,
                                                                              EvalStatus!="TargetSampled"~0))
att$DEPTHMAX[att$DEPTHMAX == 0] <- NA
att <- att %>% 
  mutate(pct_ag = VALUE_81+VALUE_82) %>% 
  mutate(pct_nat = VALUE_41+VALUE_42+VALUE_43+VALUE_52+VALUE_71+VALUE_90+VALUE_95) %>% 
  mutate(pct_notnat = 100-pct_nat) %>% 
  mutate(pct_dev = VALUE_21+VALUE_22+VALUE_23+VALUE_24) %>% 
  mutate(Wshed_Acres = AreaSqKm*247.105) %>% 
  mutate(WgtAdj=case_when(
    PROB_CAT=="(1,4]"~(1828/29),
    PROB_CAT=="(4,10]"~(2490/15),
    PROB_CAT=="(10,20]"~(1003/18),
    PROB_CAT=="(20,50]"~(616/20),
    PROB_CAT==">50"~(500/33)))
  

#Conduct analysis on continuous variables

myvars <- c("DEPTHMAX","LkAcres","Wshed_Acres","pct_ag","pct_nat","pct_notnat","pct_dev")
  
# 1. Max depth
# 2. Lake Acres
# 3. Watershed acres
# 4. % agricultural watershed
# 5. % natural watershed
# 6. % developed watershed

library(spsurvey)

analysis <- cont_analysis(
  dframe = att,
  vars = c("pct_ag","pct_notnat","pct_dev"),
  subpops = ,
  siteID = "SITE_ID",
  weight = "WgtAdj",
  xcoord = "LON_DD83",
  ycoord = "LAT_DD83")

#Plot cumulative distribution function

analysis$CDF <- analysis$CDF %>%
  # group_by(Indicator) %>%
  # filter(Value != max(Value)) %>%
  # ungroup()
  # filter(Value<10000) %>% 
  # mutate(Value=case_when(
  #   Indicator=="LkAcres"~log(Value),
  #   Indicator=="Wshed_Acres"~log(Value),
  #   Indicator=T~Value
  # )) %>% 
  # mutate(Estimate.P=case_when(
  #   Indicator=="pct_notnat"~100-Estimate.P,
  #   Indicator=T~Estimate.P),
  #   LCB95Pct.P=case_when(
  #     Indicator=="pct_notnat"~100-LCB95Pct.P,
  #     Indicator=T~LCB95Pct.P),
  #   UCB95Pct.P=case_when(
  #     Indicator=="pct_notnat"~100-UCB95Pct.P,
  #     Indicator=T~UCB95Pct.P
  #   )) %>% 
  mutate(Indicator=case_when(
    Indicator=="DEPTHMAX"~"Lake depth (m)",
    Indicator=="LkAcres"~"Lake area (log(acres))",
    Indicator=="pct_ag"~"% Agriculture",
    Indicator=="pct_dev"~"% Developed",
    Indicator=="pct_nat"~"% Natural",
    Indicator=="pct_notnat"~"% Natural",
    Indicator=="Wshed_Acres"~"Watershed area (log(acres))"
  ))
  
ggplot(analysis$CDF,aes(x=Value,y=Estimate.P,ymin=LCB95Pct.P,ymax=UCB95Pct.P,fill=Indicator))+
  geom_point(aes(color=Indicator,shape=Indicator))+
  geom_line(aes(color=Indicator))+
  geom_ribbon(alpha=0.2)+
  ylim(0,100)+
  labs(y="Percent of lakes",x="Percent of agricultural & developed cover")
  # facet_wrap(.~Indicator, scales="free")
