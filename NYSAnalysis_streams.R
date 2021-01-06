# Program:  NYSAnalysis.R
# Purpose: Population estimation for New York Long Island and Lake Champlain Stream studies
# Programmer: Tony Olsen
# Date: February 17, 2011

#### Lake Champlain Basin Analysis
# Read evaluation data file
att <- read.delim("Champlain_RandomSites_2008v2.tab")
head(att)

# look at status variable
summary(att$Eval_Status)
# Create a Target/NonTarget status variable
att$statusTNT <- att$Eval_Status
levels(att$statusTNT) <- list(Target=c("Target_Sampled", "T_NoAccess" ,"T_Other"),
                              NonTarget=c("NT_Bay", "NT_Canal", "NT_NonWadeable",
                                          "NT_Other", "NT_Wetland"))
   
addmargins(table(att$Eval_Status,att$statusTNT))
      
# Adjust sample weights to account for use of oversample sites
sum(att$wgt)
framesize_LC <- c("Lake Champlain"=2736.6)
att$WgtAdj <- adjwgt(rep(TRUE, nrow(att)), att$wgt, att$Basin, framesize_LC)
sum(att$WgtAdj)

# Estimate TNT extent
sites <- data.frame(siteID=att$siteID, Use=rep(TRUE, nrow(att)))
subpop <- data.frame(siteID=att$siteID,
                     Basin=att$Basin)
design <- data.frame(siteID=att$siteID,
                   wgt=att$WgtAdj,
                   xcoord=att$xcoord,
                   ycoord=att$ycoord)
dataTNT <- data.frame(siteID=att$siteID, StatusTNT=att$statusTNT)
TNTextent <- cat.analysis(sites, subpop, design, dataTNT,
                          popsize=list(Basin=framesize_LC))
TNTextent

# Estimate Target extent reasons
sites <- data.frame(siteID=att$siteID, Use=att$statusTNT == "Target")
dataTarget <- data.frame(siteID=att$siteID, TargetReason=att$Eval_Status)
TargetExtent <- cat.analysis(sites, subpop, design, dataTarget)
TargetExtent

#  Estimate assessment result
sites <- data.frame(siteID=att$siteID, Use=att$Eval_Status == "Target_Sampled")
datacat <- data.frame(siteID=att$siteID, 
              WQ_Assessment=att$wqa,
              MAX_ISD=att$MAX_ISD,
              P_Trophic=att$P_Trophic,
              N_Trophic=att$N_Trophic,
              Habitat_Assessment=att$Habitat.Assessment
              )
CatExtent <- cat.analysis(sites, subpop, design, datacat)
CatExtent

# Estimate continuous variables cdf
sites <- data.frame(siteID=att$siteID, Use=att$statusTNT == "Target")
datacont <- data.frame(siteID=att$siteID, 
                       BAP=att$bap,
                       NBI_P=att$NBI.P,
                       NBI_N=att$NBI.N,
                       HMA=att$HMA
                       )
Cont_Est <- cont.analysis(sites, subpop, design, datacont)

cont.cdfplot("Lake Champlain_Cont_Data_CDF_Estimates.pdf", Cont_Est$CDF,
    cdf.page=1, height=6, width=6, ylbl.r="Stream Length (km)")

comb <- rbind(TNTextent, TargetExtent, CatExtent)
row.names(comb)  <- 1:nrow(comb)
write.table(comb, file="Lake Champlain Category Estimates 20110217.csv", sep=",", col.names=NA)
write.table(Cont_Est$CDF, file="Lake Champlain CDF Estimates.csv",
            sep=",", col.names=NA)
write.table(Cont_Est$Pct, file="Lake Champlain PCT Estimates.csv",
            sep=",", col.names=NA)            

            
###############
#### Long Island Basin Analysis
# Read evaluation data file
att_li <- read.delim("RandomSites_LI2008v2.tab")
head(att_li)

# look at status variable
summary(att_li$Eval_Status)
# Create a Target/NonTarget status variable
att_li$statusTNT <- att_li$Eval_Status
levels(att_li$statusTNT) <- list(Target=c("Target_Sampled", "Target_No_Access"),
                              NonTarget=c("NT_Tidal","NT_Wetland"),
                              Duplicate="Target_Sampled_LT")
   
addmargins(table(att_li$Eval_Status,att_li$statusTNT))
      
# Adjust sample weights to account for use of oversample sites
sum(att_li$wgt)
framesize_LI <- c("Atlantic Ocean/Long Island Sound"=884.3)
Use <- att_li$statusTNT == "Target" | att_li$statusTNT == "NonTarget"
att_li$WgtAdj <- adjwgt(Use, att_li$wgt, att_li$Basin, framesize_LI)
sum(att_li$WgtAdj)

# Estimate TNT extent
sites <- data.frame(siteID=att_li$siteID, Use=Use)
subpop <- data.frame(siteID=att_li$siteID,
                     Basin=att_li$Basin)
design <- data.frame(siteID=att_li$siteID,
                   wgt=att_li$WgtAdj,
                   xcoord=att_li$xcoord,
                   ycoord=att_li$ycoord)
dataTNT <- data.frame(siteID=att_li$siteID, StatusTNT=att_li$statusTNT)
TNTextent_LI <- cat.analysis(sites, subpop, design, dataTNT,
                          popsize=list(Basin=framesize_LI))
TNTextent_LI

# Estimate Target extent reasons
sites <- data.frame(siteID=att_li$siteID, Use=att_li$statusTNT == "Target")
dataTarget <- data.frame(siteID=att_li$siteID, TargetReason=att_li$Eval_Status)
TargetExtent_LI <- cat.analysis(sites, subpop, design, dataTarget)
TargetExtent_LI

#  Estimate assessment result
sites <- data.frame(siteID=att_li$siteID, Use=att_li$Eval_Status == "Target_Sampled")
datacat <- data.frame(siteID=att_li$siteID, 
              WQ_Assessment=att_li$wqa,
              MAX_ISD=att_li$MAX_ISD,
              P_Trophic=att_li$P_Trophic,
              N_Trophic=att_li$N_Trophic,
              Habitat_Assessment=att_li$HabitatAssessment
              )
CatExtent_LI <- cat.analysis(sites, subpop, design, datacat)
CatExtent_LI

# Estimate continuous variables cdf
sites <- data.frame(siteID=att_li$siteID, Use=att_li$statusTNT == "Target")
datacont <- data.frame(siteID=att_li$siteID, 
                       BAP=att_li$bap,
                       NBI_P=att_li$NBI.P,
                       NBI_N=att_li$NBI.N)
Cont_Est_LI <- cont.analysis(sites, subpop, design, datacont)

cont.cdfplot("Long_Island_Cont_Data_CDF_Estimates.pdf", Cont_Est_LI$CDF,
    cdf.page=1, height=6, width=6, ylbl.r="Stream Length (km)")

comb_LI <- rbind(TNTextent_LI, TargetExtent_LI, CatExtent_LI)
row.names(comb_LI)  <- 1:nrow(comb_LI)
write.table(comb_LI, file="Long Island Category Estimates 20110217.csv", sep=",", col.names=NA)
write.table(Cont_Est_LI$CDF, file="Long Island CDF Estimates.csv",
            sep=",", col.names=NA)
write.table(Cont_Est_LI$Pct, file="Long Island PCT Estimates.csv",
            sep=",", col.names=NA)            
