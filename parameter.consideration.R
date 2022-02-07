library(tidyverse)

#Calculating excursion rates for parameters in smas dataset we are considering

smas_all<-stayCALM::smas_df
smas_all<-smas_all %>% 
  subset(!is.na(seg_id))
# Extract the package root with base R functions.
# This directory will provide relative paths between machines.
root.dir <- gsub("(stayCALM)(.*$)", "\\1", getwd())
library(stayCALM)
# This argument is supplied to the Rmarkdown code chunk options.
export.logical <- TRUE
wqs <- stayCALM::nysdec_wqs %>%
  dplyr::filter(
    !(
      parameter %in% "phosphorus" & type %in% c("aquatic_chronic",
                                                "health_water-source")
    ),
    !grepl("bap", parameter),
    !parameter %in% "chlorophyll_a"
  )
#trying out the new script
wqs_violations <- stayCALM::wqs_violations(
  .data = smas_all,
  .period = stayCALM::from_today("10 years"),
  .targeted_assessment = TRUE,
  .wipwl_df = stayCALM::wipwl_df,
  .wqs_df = wqs,
  .tmdl_df = stayCALM::tmdl_df
)

param_study<-wqs_violations$violation_data

param_study %>% 
  filter(substr(assessment_id,11,14)=="merc") %>% 
  mutate(result=ifelse(result>0,1,0)) %>% 
  group_by(result) %>% 
  summarize(n=n()) %>% 
  ungroup()
param_study %>% 
  filter(substr(assessment_id,11,14)=="merc") %>% 
  group_by(attaining_wqs) %>% 
  summarize(n=n()) %>% 
  ungroup()

