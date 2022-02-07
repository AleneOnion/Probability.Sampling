rm(list=setdiff(ls(), c("newdata")))
#read in the database if you need it
#(but you might not)
setwd("C:/Users/leneo/Dropbox/Alene/Rscripts/Current")
source("new_database/Reading.LMAS.Data.R")
setwd("C:/Users/leneo/Dropbox/Alene/Rscripts/Probability.Sampling")


library(tidyverse)
# Load the package
library("stayCALM")
# Load the LMAS data
data("lmas_df")

sites<-read.csv("Probability_Based_Sites_2020_2021.csv")
sites<-sites %>% 
  filter(EvalStatus=="Target_Sampled ") %>% 
  mutate(LAKE_ID=tolower(LAKE_ID),
         probability="yes") %>% 
  select(LAKE_ID,probability,SITE_ID) %>% distinct()

# Filter the data
new_df <- lmas_df %>%
  mutate(LAKE_ID=gsub("_.*","",site_id)) %>% 
  filter(date>'2020-01-01') %>% 
  arrange(LAKE_ID,date,parameter) 
new_df<-merge(sites,new_df,by=c('LAKE_ID'),all.x=TRUE)
#remove these because indicates no data are available
new_df<-new_df %>% filter(!is.na(site_id)) %>% distinct()
#identify classification
class<-newdata %>% 
  mutate(seg_id=LOCATION_PWL_ID,
         LAKE_ID=LAKE_HISTORY_ID) %>% 
  select(LAKE_ID,seg_id,LOCATION_WATERBODY_CLASSIFICATION) %>% distinct()
new_df<-merge(new_df,class,by=c('LAKE_ID','seg_id'),all.x=TRUE)

#filling class a pwls in non-class a waters
#we pull existing class c pwl ids and then switch them back at the end
#first identify which need to be switched
new_df %>% filter(is.na(seg_id)) %>% distinct() %>% select(LAKE_ID,SITE_ID,site_id,seg_id) %>% distinct()
#then identify that number of pwl ids from the database
new<-head(newdata %>% filter(LOCATION_WATERBODY_CLASSIFICATION=="C") %>% 
       select(LAKE_HISTORY_ID,LOCATION_PWL_ID) %>% distinct(),10)
new<-new %>% mutate(LAKE_ID=tolower(LAKE_HISTORY_ID)) %>% 
  rename(seg_id=LOCATION_PWL_ID) %>% select(LAKE_ID,seg_id)
#make sure none of these are already in the data frame
merge(new,new_df,by=c('LAKE_ID','seg_id'),all.x=TRUE)
#now assign the false pwl ids
new_df<-new_df %>% 
  mutate(seg_id=ifelse(LAKE_ID=="0301canxxx1","0102-0035",seg_id),
         seg_id=ifelse(LAKE_ID=="0502milxxx1","0102-0043",seg_id),
         seg_id=ifelse(LAKE_ID=="0703uwbxxx1","0103-0010",seg_id),
         seg_id=ifelse(LAKE_ID=="1006uwbxxx1","0104-0066",seg_id),
         seg_id=ifelse(LAKE_ID=="1203uwb0798e","0105-0034",seg_id),
         seg_id=ifelse(LAKE_ID=="1302hem0103h","0201-0073",seg_id),
         seg_id=ifelse(LAKE_ID=="1601uwbxxx1","0201-0064",seg_id),
         seg_id=ifelse(LAKE_ID=="1702uwb0159","0201-0048",seg_id))
#check now that everything has a PWL ID
new_df %>% filter(is.na(seg_id)) %>% distinct() %>% select(LAKE_ID,SITE_ID,site_id,seg_id) %>% distinct()


#now make sure that there is only one sample per location/parameter
#keep the sample collected closest to the date 8/1/2020
new_df<-new_df %>% 
  mutate(sep=abs(date-as.Date('2020-08-01'))) %>% 
  group_by(LAKE_ID,parameter) %>% 
  mutate(minsep=min(sep)) %>% 
  ungroup() %>% 
  filter(minsep==sep) %>% 
  select(-minsep,-sep)

#write the data frame for Sabrina to use later
write.csv(new_df,file="for.sabrina.probability.data.filtered.csv",row.names=FALSE)

# Extract the package root with base R functions.
# This directory will provide relative paths between machines.
root.dir <- gsub("(stayCALM)(.*$)", "\\1", getwd())

#export files as csv
# This argument is supplied to the Rmarkdown code chunk options.
export.logical <- TRUE

#Evaluate water quality standard violations but do not run assessment logic dictated by the CALM.
wqs_violations <- stayCALM::wqs_violations(
  .data = new_df,
  .period = from_today("10 years"),
  .targeted_assessment = TRUE,
  .wipwl_df = stayCALM::wipwl_df,
  .wqs_df = stayCALM::nysdec_wqs,
  .tmdl_df = stayCALM::tmdl_df
)
exceedances<-wqs_violations$exceedance_summary
#pull in lake id
lakes<-new_df %>% select(LAKE_ID,seg_id) %>% distinct()
exceedances<-merge(exceedances,lakes,by=c('seg_id'),all=TRUE)
exceedances<-exceedances %>% select(-seg_id) %>% 
  filter(use=="fishing",
         parameter!="phosphorus",
         parameter!="chlorophyll_a") %>% 
  mutate(n_exceedances=ifelse(n_exceedances>0,1,n_exceedances)) 

#write the data frame for Sabrina to use later
write.csv(exceedances,file="for.sabrina.probability.data.excursions.csv",row.names=FALSE)
