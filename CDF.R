##### EXAMPLE #####

# #To conduct analysis on a continuous variable:
# 
# analysis <- cont_analysis(
#   dframe = att,
#   vars = myvars,
#   subpops = ,
#   siteID = "siteID",
#   weight = "WgtAdj",
#   xcoord = "xcoord",
#   ycoord = "ycoord")
# 
# #Creates 3 estimations in list: CDF, percentiles, means.
# 
# #To plot a cumulative distribution function:
# 
# ggplot(analysis$CDF,aes(x=Value,y=Estimate.P,color=Study,shape=Study))+
#   geom_line()+
#   geom_line(aes(y=UCB95Pct.P,alpha=0.2),linetype = 2)+ #can use geom_ribbon() instead
#   geom_line(aes(y=LCB95Pct.P,alpha=0.2),linetype = 2)+
#   ylim(0,100)+
#   facet_wrap(.~Indicator, scales="free")



##---- NEW HAMPSHIRE ----

nh.att <- nh.att %>% 
  mutate(TN_UG.L=TN_UG.L/1000)
myvars <- c("CHLOROPHYLL.A.x","TN_UG.L","PHOSPHORUS.ug.L.x","CHLORIDE")

#continuous variable analysis, spits out CDF, percentiles, means
analysis <- cont_analysis(
  dframe = nh.att,
  vars = myvars,
  subpops = ,
  siteID = "SITE_ID",
  weight = "WGT_TP_EXT",
  xcoord = "X_ALBERS",
  ycoord = "Y_ALBERS")

NH<-analysis$CDF %>% 
  select(Indicator,Value,Estimate.P,LCB95Pct.P,UCB95Pct.P) %>% 
  mutate(Study="NH")

##### NORTHERN APPLACHIAN  #####

myvars <- c("CHLA","TN","TP","CHLORIDE","OXYGEN")

#continuous variable analysis, spits out CDF, percentiles, means
analysis <- cont_analysis(
  dframe = nap.att,
  vars = myvars,
  subpops = "include",
  siteID = "SITE_ID",
  weight = "WGT_TP_EXTENT",
  xcoord = "XCOORD",
  ycoord = "YCOORD")

NAP<-analysis$CDF %>% 
  filter(Subpopulation=="yes") %>% 
  select(Indicator,Value,Estimate.P,LCB95Pct.P,UCB95Pct.P) %>% 
  mutate(Study="NAP")

##### NATIONAL  #####
#Load in att from Probability.Sampling.NLA.NYS.2021

#continuous variable analysis, spits out CDF, percentiles, means
analysis <- cont_analysis(
  dframe = nla.att,
  vars = myvars,
  subpops = "include",
  siteID = "SITE_ID",
  weight = "WGT_TP_EXTENT",
  xcoord = "XCOORD",
  ycoord = "YCOORD")

NLA<-analysis$CDF %>% 
  filter(Subpopulation=="yes") %>% 
  select(Indicator,Value,Estimate.P,LCB95Pct.P,UCB95Pct.P) %>% 
  mutate(Study="NLA")

# ggplot(analysis$CDF,aes(x=Value,y=Estimate.P,ymin=LCB95Pct.P,ymax=UCB95Pct.P,fill=Indicator))+
#   geom_line()+
#   geom_ribbon(alpha=0.5)+
#   ylim(0,100)+
#   facet_wrap(.~Indicator, scales="free")

##### ALL #####

all <- rbind(NY,NH,NAP,NLA) %>%
  mutate(Indicator=case_when(
    Indicator=="CHLO"~"CHLORIDE (mg/L)",
    Indicator=="CHLORIDE" ~ "CHLORIDE (mg/L)",
    Indicator=="CHLA"~"CHLOROPHYLL (ug/L)",
    Indicator=="CHLOROPHYLL"~"CHLOROPHYLL (ug/L)",
    Indicator=="CHLOROPHYLL_A_OW_TOTAL"~"CHLOROPHYLL (ug/L)",
    Indicator=="TN"~"NITROGEN (mg/L)",
    Indicator=="NITROGEN"~"NITROGEN (mg/L)",
    Indicator=="TP"~"PHOSPHORUS (ug/L)",
    Indicator=="PHOSPHORUS"~"PHOSPHORUS (ug/L)",
    Indicator=="OXYGEN"~"DISSOLVED_OXYGEN (mg/L)",
    Indicator=="DISSOLVED_OXYGEN"~"DISSOLVED_OXYGEN (mg/L)",
    Indicator=="DISSOLVED_OXYGEN_epi"~"DISSOLVED_OXYGEN (mg/L)",
    Indicator=="PHOSPHORUS_TOTAL_OW_TOTAL"~"PHOSPHORUS (ug/L)",
    Indicator=="NITROGEN_TOTAL_OW_TOTAL"~"NITROGEN (mg/L)",
    Indicator=="CHLORIDE_OW_TOTAL"~"CHLORIDE (mg/L)",
    Indicator=="CHLOROPHYLL.A.x"~"CHLOROPHYLL (ug/L)",
    Indicator=="TN_UG.L"~"NITROGEN (mg/L)",
    Indicator=="PHOSPHORUS.ug.L.x"~"PHOSPHORUS (ug/L)",
    Indicator=TRUE~Indicator
  )) %>% 
  group_by(Indicator,Study) %>% 
  ungroup() %>% 
  filter((Indicator=="CHLOROPHYLL (ug/L)"&Value<50)|
           (Indicator=="CHLORIDE (mg/L)"&Value<250)|
           (Indicator=="NITROGEN (mg/L)"&Value<2.5)|
           (Indicator=="PHOSPHORUS (ug/L)"&Value<150)|
           Indicator=="DISSOLVED_OXYGEN (mg/L)")

table <- data.frame(Indicator=c("CHLOROPHYLL (ug/L)","DISSOLVED_OXYGEN (mg/L)","NITROGEN (mg/L)","PHOSPHORUS (ug/L)"),
                    Low=c(4.52,3,0.428,16),
                    High=c(8.43,5,0.655,27.9))

ggplot(all,aes(x=Value,y=Estimate.P,color=Study,shape=Study))+
  geom_line()+
  # geom_line(aes(y=UCB95Pct.P,alpha=0.2),linetype = 2)+
  # geom_line(aes(y=LCB95Pct.P,alpha=0.2),linetype = 2)+
  ylim(0,100)+
  geom_vline(data=table,aes(xintercept=Low),linetype="dashed")+
  geom_vline(data=table,aes(xintercept=High),linetype="dashed")+
  facet_wrap(.~Indicator, scales="free")
