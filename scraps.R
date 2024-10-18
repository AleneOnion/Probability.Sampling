sites<-read.csv("junk.csv")
library(ggplot2)
ggplot(sites, aes(x=Sample_Pop)) +
  geom_bar(aes(y=Estimate),stat = "identity", position = position_dodge())+
  geom_errorbar(aes(ymin=Lower95, ymax=Upper95), width=.2,
                position=position_dodge(.9))+
  theme(axis.title.x=element_blank())+
  labs(title="Number of Lakes > 6.5 acres that could be sampled in NYS",y="# Lakes")


lab<-read.csv("LCI.2020.qaqcd-2020-11.30_with_lakeID.csv")
lab<-lab %>% 
  mutate(sys_sample_code=substr(sys_sample_code,nchar(sys_sample_code),nchar(sys_sample_code))) %>% 
  filter(sys_sample_code!=2) %>% 
  mutate(chemical_name=paste(chemical_name,result_unit,sep="_"),
         #converting non-detects to 0
         result_value=ifelse(validator_qualifiers=="U",0,result_value)) %>% 
  select(LAKE,sample_date) %>% 
  distinct() %>% 
  rename(LAKE_ID=LAKE) %>% 
  mutate(purpose=NA,
         purpose=ifelse(LAKE_ID %in% c('1402KIA0044','1402LOU0040','1402RE10003','1402EAS0055','1404RUS0410','1401HUN0216','1401SUP0104','1404BEA0401','1401CHE0106','1402HAW0005A','1401MOU0114','1402MOR0050','1402EVE0050A','0905ROC0231','1101MOR0101','0906MUD0003','0903BLA0029A','0906MUD0005','0903EAT0248','0906RED0010','0905SYL0088','0906HIC0004','0903FOL0186','0906MUS0008','0905LOW0061','0903EAG0306','0902BAR0262','0905MOO0071','0903LSI0182','0903BLU0307','0402DEW5227','0403ROC0155','0404NUN0084A','0402GOD0017','0402UWB5160','0402MIL0016'),"Intensive",purpose),
         purpose=ifelse(LAKE_ID %in% c('1201SPE0760','0104GRE5309','1104PAR0432','1301PET0161G','0903UWBXXX1','1006UWBXXX1','1104HAR0680','1004FER0222','0905CRY0289','1104DUR0641A','0903OTT0049','1005SUN0440','0903SOU0017A','1104BRA0347','1104GRE0127','0903BRI0178','1303ORA0239D','1305SIL0378','1310BLA0N80B','1302HEM0103H','1401SWA0211','1601UWBXXX1','0603CAY0008','1202ENG0589','0602LUD0099','0502MILXXX1','0603SPE0013','0801OTT0926','0801GUL0969','1104FOU0325','1203MET0821','0801KAY0984A','1104ALG0276A','0703UWBXXX1','0704CAN0286'),"ProbabilitySampling",purpose),
         purpose=ifelse(LAKE_ID=="1402GUY0014","Routine",purpose),
         purpose=ifelse(LAKE_ID %in% c('0303SOU0001','0303HYD0391','0303NOR1041','0303STM0042','0302SIL0065','0303COA0048','0303LOR0052','0301UWB0190A','0301UWB0153D','0301WAT0166','0302MIL0092','0301MID0177A','0301GOO0167O','0302DEE0142','0105SIL0157','0104UWB','0102UWB5211','0105EMU0144','0104HIR5501','0104CRY0147','0103UWB5479','0104SUC0132','0105FRE0160','0102FAU0021A','0102HAR5128','1201UWB5549','0703ROM5276','1201MIRXXX1','1202UWB5180','1201UWBXXX3','1202TAN0655D','1202RIP0656A','1202COL0649','1201FEA0571','1201VAN0689','1202COB0583','1202ECH0605','1201ECA0703A','1203GLA0859','0104CLE0100'),"Screening",purpose),
         purpose=ifelse(LAKE_ID %in% c('1501SWA0984A ','1501DEF0977A  ','1501CON0984   ','1306STU0453A','0703ONE0026_SHACK','0703ONE0026_B109','0703ONE0026_3MILE','1301WIC0183A','1501MOM1010','1305COL0404','1301MEL0331','1311WAT0270','1005WIL0455A','1005UWB0454','1005BUT5350','1203HIN0799','1005BUT0452','1501CON0984','1501DEF0977A','1501SWA0984A'),"SpecialStudies",purpose)) %>% 
  select(LAKE_ID,sample_date,purpose) %>% 
  distinct() %>% 
  group_by(purpose) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  rename(y2020=n) %>% 
  mutate(y2021=y2020,
         y2021=ifelse(y2020=="113","70",y2021)) #%>% 
  #gather(year,Samples,-purpose)
  
library(ggplot2)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")  )
#ggplot(lab,aes(x="",y=Samples,fill=purpose))+
#  geom_bar(width=1,stat="identity")+
#  coord_polar("y",start=0)+
#  blank_theme +
#  theme(axis.text.x=element_blank()) +
#  facet_wrap(~year)
ggplot(lab,aes(x="",y=y2020,fill=purpose))+
    geom_bar(width=1,stat="identity")+
    coord_polar("y",start=0)+
    blank_theme +
    theme(axis.text.x=element_blank())
ggplot(lab,aes(x="",y=y2021,fill=purpose))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  blank_theme +
  theme(axis.text.x=element_blank())

#streams design
design<-read.delim("RandomSites_LI2008v2.tab")

#lakes design
lakes<-read.csv("Probability_Based_Sites_2020.csv")


rmarkdown::render("Probability.Results.Rmd")

filter(LAKE_ID %in% c('1701LFR0662','1404OQU0383','1402WOL0037','1401ANA0251','1310QUE0057','1308ROB0902','1204MAD1051','1104SCH0374','1104SAC0314','1102BAB1109','0906BUT0054','0906BON0024','0906BLA0001','0801SEC0782B','0706OWA0212','0705DUC0222','0705COM0333','0703TUS0153A','0703DER0139A','0703CAZ0153','0602SON0072','0602PET0078','0602MOR0152','0602MEL0039','0602LEB0153','0602HAT0155','0602EAT0163','0602EAR0146','0602CRO0073','0602BRA0154','0601CAN0392','0403SIL0115','0402CON0067','0302SOD0096','0202FIN0153','0202CHA0122','0201CUB0115'))



#Plotting data frame distribution as well as NYS sample distribution collected since 2010
frame<-att %>% select(PROB_CAT,AREA_CAT) %>% rename(size_category=AREA_CAT) %>% distinct() %>% 
  mutate(Probability_Sites=ifelse(size_category=="(1,4]",((1828/6437)*100),size_category),
         Probability_Sites=ifelse(size_category=="(4,10]",((2490/6437)*100),Probability_Sites),
         Probability_Sites=ifelse(size_category=="(10,20]",((1003/6437)*100),Probability_Sites),
         Probability_Sites=ifelse(size_category=="(20,50]",((616/6437)*100),Probability_Sites),
         Probability_Sites=ifelse(size_category==">50",((500/6437)*100),Probability_Sites),
         Long_Term_Trend_Sites=ifelse(size_category=="(1,4]",(0),size_category),
         Long_Term_Trend_Sites=ifelse(size_category=="(4,10]",(2.7),Long_Term_Trend_Sites),
         Long_Term_Trend_Sites=ifelse(size_category=="(10,20]",(16.2),Long_Term_Trend_Sites),
         Long_Term_Trend_Sites=ifelse(size_category=="(20,50]",(13.5),Long_Term_Trend_Sites),
         Long_Term_Trend_Sites=ifelse(size_category==">50",(67.6),Long_Term_Trend_Sites),) %>% 
  select(-PROB_CAT) %>% 
  gather(study,percentage,-size_category) %>% 
  arrange(study,size_category) %>% 
  mutate(percentage=as.numeric(percentage),
         size_category=factor(size_category,levels=c("(1,4]","(4,10]","(10,20]","(20,50]",">50")))


forplot2<-forplot %>% 
  spread(percent,results) %>% 
  rename(percent=pct) %>% 
  filter(Indicator=="Trophic Status from Phosphorus") %>% 
  mutate(study="probability") %>% 
  add_row(Indicator="Trophic Status from Phosphorus",Category="eutrophic",study="Long_Term_Trend",percent=((8/37)*100)) %>% 
  add_row(Indicator="Trophic Status from Phosphorus",Category="mesotrophic",study="Long_Term_Trend",percent=((13/37)*100)) %>% 
  add_row(Indicator="Trophic Status from Phosphorus",Category="oligotrophic",study="Long_Term_Trend",percent=((16/37)*100)) %>% 
  mutate(pct_lcb=ifelse(is.na(pct_lcb),percent,pct_lcb),
         pct_ucb=ifelse(is.na(pct_ucb),percent,pct_ucb))


setwd("C:/Users/leneo/Dropbox/Alene/Rscripts/Current")
source("new_database/Reading.LMAS.Data.R")
setwd("C:/Users/leneo/Dropbox/Alene/Rscripts/Probability.Sampling")


newdata %>% filter(LAKE_HISTORY_ID=="0902SAI0156A") %>% select(SAMPLE_DATE) %>% distinct()
0902SAI0156A
0101RES5772


filtered_exceedances %>% group_by(seg_id,parameter) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  filter(n>1)

sites<-sites %>% 
  mutate(LAKE_ID=ifelse(LAKE_ID=="0101RES5772","1301CRO1033",LAKE_ID),
         LAKE_ID=ifelse(LAKE_ID=="0902SAI0156A","0902WHI0158",LAKE_ID))
write.csv(sites,file="Probability_Based_Sites_2020_2021.csv",row.names=FALSE)


new_df<-lmas_df %>% filter(seg_id=="1306-0037") %>% distinct()
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
  filter(use=="fishing") %>% 
  mutate(n_exceedances=ifelse(n_exceedances>0,1,n_exceedances))

#write the data frame for Sabrina to use later
write.csv(exceedances,file="for.sabrina.probability.data.excursions.csv",row.names=FALSE)

#see if all 30 lakes are in the probability data set
junk<-newdata %>% 
  filter(substr(SAMPLE_DATE,1,4)>2019,
         CHARACTERISTIC_NAME=="PHOSPHORUS, TOTAL",
         LAKE_HISTORY_ID %in% c('0902DEE0073','1302CAR0062A','0403RUS0146','0704CAN0286','1104BRA0347','1203UWB0798E','1203GLA0859',
         '0801DAR0750','0902WHI0158','1201ECA0703A','0902SPI0264','1203DEE0911','1004NIC0314','0801SAN0436','0801MEI0420','0903UPR0239',
         '1201KLO0708','1301CRO1033','0903SHA0324','1501GRE1026','0602STE0101','0602WHI5358','1401RIO0079A','1404RUS0410','1301CAN0168A',
         '0601ARN0362','0301CANXXX1','1104GRE0127','1104PAR0432','0701CRO0185'))
length(unique(junk$LAKE_HISTORY_ID))


sites<-sites %>% select(siteID,ycoord,xcoord,LAKE_ID) %>% distinct()


score<-analysis$CDF
junk<-att %>% select(CHLOROPHYLL_ug_L,NITROGEN_mg_L,PHOSPHORUS_mg_L,DISSOLVED_OXYGEN_mg_L,ALKALINITY_mg_L,CHLORIDE_mg_L,Ammonia_mg_L,Arsenic_mg_L,Iron_mg_L,Magnesium_mg_L,Manganese_mg_L,Nitrate_mg_L,Nitrate_Nitrite_mg_L,Nitrite_mg_L,PH_epi,Sulfate_mg_L,Specific_conductivity_epi,Calcium_mg_L,Color,Secchi_depth) %>% distinct()



#creating pdf
probDF<-ash1_wgt(att$CHLOROPHYLL_ug_L,wgt=att$WgtAdj)
probDF<-data.frame(x=probDF$x,y=probDF$y)
probDF<-probDF %>% 
  rename(CHLOROPHYLL_ug_L=x,
         probability=y)
library(ggplot2)
ggplot(probDF,aes(x=CHLOROPHYLL_ug_L,y=probability))+
  #geom_point()+
  geom_line()


unknown
0301CANXXX1_DH
2021-09-27
open_water
not_applicable
chlorophyll-a-concentration_chlorophyte_green-algae


0704CAN0286_DH
0902DEE0073_DH
# 1302CAR0062A_DH


junk<-obt |> 
  filter(SITE_CODE %in% c("0104GRE5309_DH","0701CRO0185_DH","0704CAN0286_CSL1","0902DEE0073_DH","1104BRA0347_DH","1301CRO1033_DH",
                          "1302CAR0062A_DH"),
         SAMPLE_LOCATION %in% c("open_water","depth_profile","secchi_depth"),
         EVENT_DATETIME >= as.POSIXct("2020-01-01 00:00"),
         EVENT_DATETIME <= as.POSIXct("2022-01-01 00:00"),
         !RESULT_QUALIFIER %in% c("R", "T"),
         SAMPLE_LAB!="SUNYESF") |> 
  select(SITE_CODE,EVENT_DATETIME,SAMPLE_LOCATION,FRACTION,PARAMETER_NAME,UNIT) |> distinct()
  
"0104GRE5309_DH", no TP must have been rejected
"0701CRO0185_DH", it is there but multiple days, filtering for DO and phosph will help
"0704CAN0286_CSL1",
"0902DEE0073_DH",
"1104BRA0347_DH",
# 1301CRO1033_DH", #it is there but multiple days, filtering for DO and phosph will help
# "1302CAR0062A_DH"

junk2<-df |> filter(SITE_CODE %in% c("0104GRE5309_DH","0701CRO0185_DH","0704CAN0286_CSL1","0902DEE0073_DH",
                                     "1104BRA0347_DH","1301CRO1033_DH","1302CAR0062A_DH"))|> 
  select(SITE_CODE,EVENT_DATETIME,SAMPLE_LOCATION,FRACTION,PARAMETER_NAME,UNIT) |> distinct() |> 
  filter(PARAMETER_NAME %in% c('dissolved_oxygen','phosphorus'))

junk<-df |> filter(SITE_CODE=="1302CAR0062A_DH") |> 
  select( WIPWL,
          WATERBODY_NAME,
          SITE_CODE,
          EVENT_ID,
          REPLICATE,
          EVENT_DATETIME,
          SAMPLE_LOCATION,
          FRACTION,
          PARAMETER_NAME,
          UNIT,
          RESULT_VALUE,
          QUANTITATION_LIMIT,
          RESULT_QUALIFIER,
          RESULT_QUALIFIER_DESCRIPTION
  ) |> distinct()
junk<-df |> select(EVENT_DATETIME) |> distinct() |> arrange(EVENT_DATETIME)

parameters<-df |> select(PARAMETER_NAME,FRACTION) |> distinct() |> arrange(PARAMETER_NAME,FRACTION)


# [1] "SITE_CODE"                                            "EVENT_DATETIME"                                      
# [3] "alkalinity(total)"                                    "arsenic(total)"                                      
# [5] "calcium(total)"                                       "chloride(total)"                                     
# [7] "chlorophyll-a-concentration_chlorophyte_green-algae"  "chlorophyll-a-concentration_cryptophyta_cryptophytes"
# [9] "chlorophyll-a-concentration_cyanobacteria_bluegreen"  "chlorophyll-a-concentration_dinophyta_diatoms"       
# [11] "chlorophyll-a(total)"                                 "dissolved_organic_carbon"                            
# [13] "dissolved_oxygen"                                     "hardness(total)"                                     
# [15] "iron(total)"                                          "kjeldahl_nitrogen(total)"                            
# [17] "manganese(total)"                                     "microcystin"                                         
# [19] "nitrate-nitrite(total)"                               "ph"                                                  
# [21] "ph_for_color_analysis"                                "phosphorus(dissolved)"                               
# [23] "phosphorus(total)"                                    "secchi_disk_depth"                                   
# [25] "specific_conductance"                                 "sulfate(total)"                                      
# [27] "temperature"                                          "total_organic_carbon"                                
# [29] "true_color(total)"                                    "dissolved_oxygen_wqs"                                
# [31] "ph_high_wqs"                                          "ph_low_wqs"                                          
# [33] "xcoord"                                               "ycoord"                                              
# [35] "Eval_Status"                                          "PROB_CAT" 


newdata |> filter(RSLT_ANALYTICAL_METHOD_ID=="bbe Fluoroprobe User Manual") |> 
  select(CHARACTERISTIC_NAME,RSLT_RESULT_TYPE,RSLT_ANALYTICAL_METHOD_ID) |> distinct()
obt |> filter(LAB_ANALYTICAL_METHOD=="bbe Fluoroprobe User Manual") |> 
  select(PARAMETER_NAME,SAMPLE_SOURCE) |> distinct()

junk<-df |> filter(PARAMETER_NAME=="chlorophyll-a-concentration_total") |> select(PARAMETER_NAME,SAMPLE_SOURCE) |> distinct() |> arrange(PARAMETER_NAME)

#removing infrequent parameters
parameters<-df |> select(WATERBODY_CODE,WATERBODY_NAME,EVENT_DATETIME,PARAMETER_NAME,FRACTION) |> distinct() |> 
  mutate(year=substr(EVENT_DATETIME,1,4)) |> 
  group_by(PARAMETER_NAME,FRACTION,year) |> 
  summarise(n=n()) |> 
  ungroup() |> 
  spread(key=year,value=n)



write.csv(df,file="junk.df.csv",row.names=FALSE)
junk<-read.csv("junk.df.csv")

df<-read.csv("junk.df.csv")
dbrief<- df|> select(siteID,SITE_CODE,WgtAdj,xcoord,ycoord,phos_trophic,chla_trophic) |> distinct() 
dbrief<-as_tibble(dbrief)

vars <- c("phos_trophic", "chla_trophic")

brief<-att |> select(siteID,WgtAdj,xcoord,ycoord,phos_trophic,chla_trophic) |> distinct()

#analysis
CatExtent <- cat_analysis(
  dframe=dbrief,
  vars=vars, 
  subpops = , #for example could separate by area category or year
  siteID = "siteID",
  weight = "WgtAdj", #name will probably change in future
  xcoord = "xcoord",
  ycoord = "ycoord")

"0701UWB5036"


junk<-att |> filter(chemical_name=="MICROCYSTIN_OW_NA",!is.na(result_value)) |> select(LAKE_ID,SAMPLE_DATE) |> distinct()

newdata |> filter(LOCATION_HISTORY_ID=="0104GRE5309_DH",SAMPLE_DATE=="2020-09-30",CHARACTERISTIC_NAME=="MICROCYSTIN") |> 
  select(LOCATION_HISTORY_ID,INFORMATION_TYPE,RSLT_RESULT_VALUE,RSLT_RESULT_SAMPLE_FRACTION,RSLT_VALIDATOR_QUALIFIER) |> distinct()

rmarkdown::render("2024.09.Probability.Sampling.BASE_onion.Rmd")

read.csv("junk.chla.albany.csv")

chl<-chl |> 
  mutate(datetime=gsub("\\ .*","",datetime)) |> 
  mutate(datetime=as.Date(datetime,format="%m/%d/%Y"))

ggplot( chl 
        # |> mutate(value=log(value))
          ,aes(x=datetime,y=value)) +
  geom_point()+
  labs(title="Microcystin Detection",y="chl a")
  
  +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank(),legend.title=element_blank())


obt |> 
  filter(WATERBODY_TYPE == "lake",!is.na(RESULT_VALUE)|!is.na(RESULT_QUALIFIER),
         PARAMETER_NAME=="sodium") |> 
  mutate(year=substr(EVENT_DATETIME,1,4)) |> 
  select(year) |> distinct() |> arrange(year)

junk<-obt |> 
  filter(
         WATERBODY_TYPE!="lake",
        # SAMPLE_ORGANIZATION %in% c('Citizens Statewide Lake Assessment Program (CSLAP)','CSLAP'),
         PUBLIC_WATER_SUPPLY=="TRUE") |> 
  select(WATERBODY_CODE,WATERBODY_NAME,SITE_CODE,WATERBODY_TYPE) |> distinct()


df |> 
  filter(PARAMETER_NAME=="dissolved_organic_carbon") |> 
  mutate(year=substr(EVENT_DATETIME,1,4)) |> 
  select(year,PARAMETER_NAME,UNIT) |> 
  distinct() |> arrange(desc(year))


obt |> filter(WATERBODY_TYPE!="lake",PARAMETER_NAME=="specific_conductance") |> select(PARAMETER_NAME,UNIT) |> distinct()


obt |> select(WATERBODY_NAME,WIPWL,MUNICIPALITY,COUNTY,BASIN_NAME)

obt |> filter(WATERBODY_CODE=="0705SEN0369") |> select(WIPWL,SITE_DESCRIPTION) |> distinct()

obt |> filter(WATERBODY_CODE=="1203THE0850A",
              PARAMETER_NAME=="true_color") |> 
  select(EVENT_DATETIME,RESULT_VALUE,METHOD_DETECTION_LIMIT,RESULT_QUALIFIER) |> 
  distinct() 
junk<-obt |> filter(WATERBODY_CODE=="1202UWB0579") |> 
  select(PARAMETER_NAME) |> distinct() |> 
  arrange(PARAMETER_NAME)
 
obt |> filter(WATERBODY_CODE=="1202UWB0579",
              PARAMETER_NAME=="phosphorus") |> 
  select(EVENT_DATETIME,RESULT_VALUE,METHOD_DETECTION_LIMIT,RESULT_QUALIFIER,SAMPLE_LOCATION) |> 
  distinct() 
