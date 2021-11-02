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
