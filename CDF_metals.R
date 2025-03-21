library(tidyverse)

#load data
# retrieve raw data from database
setwd("~/OneDrive - New York State Office of Information Technology Services/Rscripts/Current")
source("new_database/Reading.LMAS.Data.R")
setwd("~/OneDrive - New York State Office of Information Technology Services/Rscripts/Probability.Sampling/")

rm(list=setdiff(ls(), c('newdata')))

lab<-newdata %>%
  filter(SAMPLE_DATE>'2020-01-01') %>%
  filter(CHARACTERISTIC_NAME %in% c('ALUMINUM','ARSENIC','COPPER','IRON','MAGNESIUM','NICKEL','ZINC')) %>% 
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
         is.na(RSLT_VALIDATOR_QUALIFIER)|(RSLT_VALIDATOR_QUALIFIER!="R")) %>%
  select(LAKE_HISTORY_ID,SAMPLE_DATE,combined,RSLT_RESULT_VALUE,RSLT_PROFILE_DEPTH) %>%
  distinct(LAKE_HISTORY_ID,SAMPLE_DATE,combined,RSLT_PROFILE_DEPTH,.keep_all = TRUE) %>%
  rename(LAKE_ID=LAKE_HISTORY_ID,
         chemical_name=combined,
         result_value=RSLT_RESULT_VALUE,
         profile_depth=RSLT_PROFILE_DEPTH)

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
att<-merge(lab,sites,by=c('LAKE_ID'),all.y = TRUE) %>% distinct()
att<-att %>% spread(chemical_name,result_value,fill = NA)

att<-att %>% filter((Eval_Status=="Target_Sampled"&!is.na(`CHLOROPHYLL A_OW_TOTAL`)&!is.na(`PHOSPHORUS, TOTAL_OW_TOTAL`)) |
                      (Eval_Status!="Target_Sampled") |
                      (LAKE_ID %in% c("0703UWBXXX1","0801KAY0984A","1203MET0821","0602LUD0099","0801GUL0969"))) %>% 
  distinct()

# remove fields in the sites table that aren't relevant
att<-att %>% 
  select(-Accessible,-Comments,-Contact,-STATUS)


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

att<-att %>% 
  ungroup() %>% 
  mutate(WgtAdj=case_when(
    PROB_CAT=="(1,4]"~(1828/29),
    PROB_CAT=="(4,10]"~(2490/15),
    PROB_CAT=="(10,20]"~(1003/18),
    PROB_CAT=="(20,50]"~(616/20),
    PROB_CAT==">50"~(500/33)))

rm(list=setdiff(ls(), c('newdata',"att","sites")))

#Conduct analysis on continuous variables

myvars <- c("ARSENIC_BS_TOTAL","ARSENIC_OW_TOTAL","IRON_BS_TOTAL","IRON_OW_TOTAL","MAGNESIUM_BS_TOTAL","MAGNESIUM_OW_TOTAL")

library(spsurvey)

analysis <- cont_analysis(
  dframe = att,
  vars = myvars,
  subpops = ,
  siteID = "siteID",
  weight = "WgtAdj",
  xcoord = "xcoord",
  ycoord = "ycoord")

#Plot cumulative distribution function

analysis$CDF <- analysis$CDF %>%
  mutate(Indicator=case_when(
    Indicator=="ARSENIC_BS_TOTAL"~"Arsenic, bottom (ug/L)",
    Indicator=="ARSENIC_OW_TOTAL"~"Arsenic, open water (ug/L)",
    Indicator=="IRON_BS_TOTAL"~"Iron, bottom (ug/L)",
    Indicator=="IRON_OW_TOTAL"~"Iron, open water (ug/L)",
    Indicator=="MAGNESIUM_BS_TOTAL"~"Magnesium, bottom (ug/L)",
    Indicator=="MAGNESIUM_OW_TOTAL"~"Magnesium, open water (ug/L)",
  ))

ggplot(analysis$CDF,aes(x=Value,y=Estimate.P,ymin=LCB95Pct.P,ymax=UCB95Pct.P,fill=Indicator))+
  geom_point(aes(color=Indicator))+
  geom_line(aes(color=Indicator))+
  geom_ribbon(alpha=0.2)+
  ylim(0,100)+
  labs(y="Percent of lakes",x="")+
  facet_wrap(.~Indicator, scales="free")+
  theme(legend.position = "none")
