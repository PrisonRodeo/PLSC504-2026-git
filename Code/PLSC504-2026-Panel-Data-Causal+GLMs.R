#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Introduction                                        ####
#
# PLSC 504
#
# Panel Data: Causal Inference + GLMs
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr", "haven", "psych", "sandwich", "countrycode", "wbstats","ggplot2",
     "lme4", "plm", "gtools", "boot", "plyr", "dplyr", "texreg", "statmod",
     "tibble", "pscl", "naniar", "ExPanDaR", "stargazer", "prais", "nlme",
     "tseries", "pcse", "panelView", "performance", "pgmm", "dynpanel",
     "OrthoPanels", "peacesciencer", "corrplot", "rgenoud", "WDI",
     "MatchIt", "Matching", "did", "optmatch", "Synth", "quickmatch",
     "cobalt", "modelsummary", "marginaleffects", "car", "jstable",
     "glmmML", "bife", "pglm", "geepack", "fixest", "xtsum")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Set display options:

options(scipen = 99) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# setwd yo'self, if you care to, or set up a project...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# World Development Indicators data (yet again)...    ####

# Get the data:

WDI<-read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/main/Data/WDI26b.csv")

# Add a "Post-Cold War" variable:

WDI$PostColdWar <- with(WDI,ifelse(Year<1990,0,1))

# log GDP Per Capita:

WDI$lnGDPPerCap<-log(WDI$GDPPerCapita)

# Now make a positive-only aid received variable:

WDI$AidReceived<-WDI$NetAidReceived
WDI$AidReceived<-ifelse(WDI$AidReceived>0,WDI$AidReceived,0)

# ... and log it:

WDI$lnAidReceived<-log(WDI$AidReceived+1)

# Keep a numeric year variable (for -panelAR-):

WDI$YearNumeric<-WDI$Year

# Add WBLI + parental leave data:

wbli<-read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/main/Data/WBLI-PPL.csv")

WDI<-merge(WDI,wbli,by=c("ISO3","Year"),all.x=TRUE,all.y=TRUE)
WDI$PaidParentalLeave<-ifelse(WDI$PaidParentalLeave=="Yes",1,0)

# Fix Nauru:

WDI<-WDI[WDI$country!="Nauru",]

# Make the data a panel dataframe:

WDI<-pdata.frame(WDI,index=c("ISO3","Year"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Descriptive statistics on the WDI data              ####

describe(WDI,fast=TRUE,ranges=FALSE,check=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Some Regressions...                                 ####
#
# First, a simple preliminary investigation...
#
# Create logged Child Mortality variable:

WDI$lnCM <- log(WDI$ChildMortality)

# T-test (not reproduced):

t.test(lnCM~PaidParentalLeave,data=WDI)

# Bivariate regression:

BIV<-lm(lnCM~PaidParentalLeave,data=WDI)

# OLS:

OLS<-lm(lnCM~PaidParentalLeave+lnGDPPerCap+lnAidReceived+GovtExpenditures,data=WDI)

# Fixed Effects... One-way:

FE.1way<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+lnAidReceived+GovtExpenditures,
             data=WDI,effect="individual",model="within")

# Two-way:

FE.2way<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+lnAidReceived+GovtExpenditures,
             data=WDI,effect="twoway",model="within")


FE.LDV<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+lnAidReceived+GovtExpenditures+
            lag(ChildMortality),data=WDI,effect="individual",model="within")

# A nice table:

MortTable1 <- stargazer(BIV,OLS,FE.1way,FE.2way,FE.LDV,
                    title="Models of log(Child Mortality)",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,p=c(0.05),
                    dep.var.caption="",omit.stat=c("f","ser"),
                    covariate.labels=c("Paid Parental Leave","ln(GDP Per Capita)",
                                       "ln(Aid Received)",
                                       "Government Expenditures",
                                       "Lagged Child Mortality"),
                    column.labels=c("Bivariate OLS","OLS","One-Way FE",
                                    "Two-Way FE","FE w.Lagged Y"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="ChildMortTable1-26.tex")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INSTRUMENTAL VARIABLES!                             ####
#
# We'll instrument Paid Parental Leave with the
# Women in Legislature variable:

with(WDI,t.test(WomenInLegislature~PaidParentalLeave))

pdf("IVBoxplot-26.pdf",7,5)
par(mar=c(4,4,2,2))
boxplot(WomenInLegislature~PaidParentalLeave,data=WDI,
        xlab="Paid Parental Leave",
        ylab="Percent of Legislative Seats Held By Women")
dev.off()

# IV regressions (one-way FE and RE):

FE.IV<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+
              lnAidReceived+GovtExpenditures |
              .-PaidParentalLeave+WomenInLegislature,
              data=WDI,effect="individual",model="within")


RE.IV<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+
             lnAidReceived+GovtExpenditures |
             .-PaidParentalLeave+WomenInLegislature,
             data=WDI,effect="individual",model="random")

# A nice table:

IVTable<-stargazer(OLS,FE.1way,FE.IV,RE.IV,
                   title="IV Models of log(Child Mortality)",
                   column.separate=c(1,1),align=TRUE,
                   dep.var.labels.include=FALSE,p=c(0.05),
                   dep.var.caption="",omit.stat=c("f","ser"),
                   covariate.labels=c("Paid Parental Leave","ln(GDP Per Capita)",
                                      "ln(Aid Received)",
                                      "Government Expenditures"),
                   column.labels=c("OLS","One-Way FE","FE w/IV","RE w/IV"),
                   header=FALSE,model.names=FALSE,
                   model.numbers=FALSE,multicolumn=FALSE,
                   object.names=TRUE,notes.label="",
                   out="ChildMortTable2-26.tex")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simple "RDD"...                                     ####
#
# Pull out *only* those countries that, at some
# point during the observed periods, instituted
# a paid parental leave policy:

PPLs<-WDI
PPLs<-PPLs %>% group_by(ISO3) %>%
  filter(any(PaidParentalLeave==1))
PPLs$YearNumeric<-as.numeric(as.character(PPLs$YearNumeric)) # fix

# Plot the trends by value of PaidParentalLeave: 

pdf("PPL-Plot-26.pdf",7,5)
par(mar=c(4,4,2,2))
with(PPLs[PPLs$PaidParentalLeave==0,],
               plot(YearNumeric,log(ChildMortality),
               pch=20,col="darkorange",xlab="Year",
               ylim=c(0.4,5.3)))
with(PPLs[PPLs$PaidParentalLeave==1,],
               points(YearNumeric,log(ChildMortality),
               pch=17,col="forestgreen"))
legend("topright",bty="n",pch=c(20,17),
       col=c("darkorange","forestgreen"),
       legend=c("No Paid Parental Leave",
                "Paid Parental Leave"))
dev.off()

# Create a better trend variable:

PPLs$Time<-PPLs$YearNumeric-1950

# Create interaction term:

PPLs$PPLxTime<-PPLs$PaidParentalLeave * PPLs$Time

# REGRESSION TIME 
#
# Complete-data observations only...

PPLs.C <- na.omit(
  PPLs[,c(
    "lnCM",
    "PaidParentalLeave",
    "Time",
    "PPLxTime",
    "lnGDPPerCap",
    "lnAidReceived",
    "GovtExpenditures",
    "ISO3",
    "Year"
  )]
)

PPLs.C <- pdata.frame(
  PPLs.C,
  index=c("ISO3","Year")
)

# OLS...

RDD.OLS1<-lm(lnCM~PaidParentalLeave+Time+PPLxTime,data=PPLs.C)

RDD.OLS2<-lm(lnCM~PaidParentalLeave+Time+PPLxTime+lnGDPPerCap+
               lnAidReceived+GovtExpenditures,data=PPLs.C)

# FE models...
# 
# PPLs<-pdata.frame(PPLs,index=c("ISO3","Year")) # make panel data

RDD.1way.1<-plm(lnCM~PaidParentalLeave+Time+PPLxTime,data=PPLs.C,
                effect="individual",model="within")

RDD.1way.2<-plm(lnCM~PaidParentalLeave+Time+PPLxTime+lnGDPPerCap+
                lnAidReceived+GovtExpenditures,
                data=PPLs.C,effect="individual",model="within")

RDD.2way.1<-plm(lnCM~PaidParentalLeave+PPLxTime,data=PPLs.C,
                effect="twoway",model="within")

RDD.2way.2<-plm(lnCM~PaidParentalLeave+PPLxTime+lnGDPPerCap+
                  lnAidReceived+GovtExpenditures,
                  data=PPLs.C,effect="twoway",model="within")

# TABLE TIME...


RDDs<-list("OLS #1"=RDD.OLS1,"OLS #2"=RDD.OLS2,
           "One-Way FE #1"=RDD.1way.1,"One-Way FE #2"=RDD.1way.2,
           "Two-Way FE #1"=RDD.2way.1,"Two-Way FE #2"=RDD.2way.2)

modelsummary(RDDs,output="RDDMortRegs-26.tex",stars=TRUE,
             title="RDD Models of log(Child Mortality)",
             fmt=4,gof_map=c("nobs","r.squared","adj.r.squared"),
             coef_rename=c("(Intercept)","Paid Parental Leave",
                           "Time (1950=0)","Paid Parental Leave x Time",
                           "ln(GDP Per Capita)","ln(Aid Received)",
                           "Government Expenditures"))

# Note that you could also use -texreg-:
# 
# texreg(list(RDD.OLS1,RDD.OLS2,RDD.1way.1,RDD.1way.2,
#             RDD.2way.1,RDD.2way.2),file="RDDMortRegs-25A.tex",
#             caption="",label="")
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DiD...                                              ####
#
# Finally, a differences-in-differences (DiD) example. This
# uses the -did- package, which deals with DiD models where
# there are more than two "periods" and/or staggered
# "treatment" timings.
#
# Clean up the data a bit...

WDI$ID<-as.numeric(WDI$ISO3)

WDI <- WDI %>%
  group_by(ID, ISO3, YearNumeric) %>%
  summarise(
    lnCM = mean(lnCM, na.rm=TRUE),
    PaidParentalLeave = max(PaidParentalLeave, na.rm=TRUE),
    across(
      c(lnGDPPerCap, lnAidReceived, GovtExpenditures),
      ~mean(.x, na.rm=TRUE)
    ),
    .groups="drop"
  )

WDI <- WDI %>%
  group_by(ID) %>%
  mutate(
    YearPPL = ifelse(
      any(PaidParentalLeave==1),
      min(YearNumeric[PaidParentalLeave==1]),
      2025
    )
  ) %>%
  ungroup()

DiD.fit1 <- att_gt(
  yname = "lnCM",
  gname = "YearPPL",
  idname = "ID",
  tname = "YearNumeric",
  allow_unbalanced_panel = TRUE,
  xformla = ~1,
  data = WDI,
  est_method = "reg"
)

# Event study object:

DiD.ev1 <- aggte(DiD.fit1,type="dynamic",na.rm=TRUE)

# Make an event study plot:

pdf("DiDEventPlot1-26.pdf",7,4)
par(mar=c(4,4,2,2))
ggdid(DiD.ev1,xgap=10)
dev.off()

# Total group effects:

DiD.grp1<-aggte(DiD.fit1,type="group",na.rm=TRUE)
summary(DiD.grp1)

# Plotted:

pdf("DiDGroupEffects1-26.pdf",5,6)
ggdid(DiD.grp1)
dev.off()

# Finally, add the three control variables:

DiD.fit2<-att_gt(yname = "lnCM",gname = "YearPPL",idname = "ID",
                 tname = "YearNumeric",allow_unbalanced_panel = TRUE,
                 xformla = ~lnGDPPerCap+lnAidReceived+GovtExpenditures,
                 data = WDI, est_method = "reg")

# Event study object:

DiD.ev2 <- aggte(DiD.fit2,type="dynamic",na.rm=TRUE)

# Make an event study plot:

pdf("DiDEventPlot2-26.pdf",7,4)
par(mar=c(4,4,2,2))
ggdid(DiD.ev2,xgap=10)
dev.off()

# Total group effects:

DiD.grp2<-aggte(DiD.fit2,type="group",na.rm=TRUE)
summary(DiD.grp2)

# Plotted:

pdf("DiDGroupEffects2-26.pdf",5,6)
ggdid(DiD.grp2)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Generalized" models                                ####
#
# WDI "plus" data...
#
# Pull the ("original") WDI data...

WDI<-read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/main/Data/WDI26a.csv")

# Add a "Post-Cold War" variable:

WDI$PostColdWar <- with(WDI,ifelse(Year<1990,0,1))

# Keep a numeric year variable:

WDI$YearNumeric<-WDI$Year

# summary(WDI)
#
# Add some political data... (note: this is why
# you loaded the -peacesciencer- and 
# -countrycode- packages above):

create_stateyears(system="gw",
                  subset_years=c(1960:2021)) %>%
  add_ccode_to_gw() %>%
  add_ucdp_acd(type="intrastate", only_wars = FALSE) %>%
  add_ucdp_onsets() %>%
  add_democracy() -> DF

DF$Year<-DF$year
DF$ISO3<-countrycode(DF$gwcode,"gwn","iso3c")
nc<-ncol(DF)
nd<-nc-1
ne<-nc-2 # kludgey af

DF<-DF[,c(nc,nd,1:ne)] # order variables
DF<-DF[order(DF$ISO3,DF$Year),] # sort data

# Merge:

Data<-merge(WDI,DF,by=c("ISO3","Year"),
            all=TRUE)

Data<-Data[order(Data$ISO3,Data$Year),] # sort
rm(DF) # clean up

# Zap some missingness...

Data<-Data[is.na(Data$ISO3)==FALSE,]

# Make the data a panel dataframe:

Data<-pdata.frame(Data,index=c("ISO3","Year"))

# Prep some variables:

Data$PopMillions<-Data$Population/1000000
Data$GDPPerCapita<-Data$GDP/Data$Population
Data$CivilWar<-Data$ucdpongoing
Data$OnsetCount<-Data$sumonset1
Data$POLITY<-(Data$polity2+10)/2
Data$POLITYSquared<-Data$POLITY^2

# Summary statistics on the variables we're using...

vars<-c("ISO3","Year","YearNumeric","country","CivilWar",
        "OnsetCount","LandArea","PopMillions","UrbanPopulation",
        "GDPPerCapita","GDPPerCapGrowth","PostColdWar",
        "POLITY","POLITYSquared")

DF<-Data[vars]

describe(DF,skew=FALSE)

# Make panel data:

DF<-pdata.frame(DF,index=c("ISO3","Year"))

# Describe the within- and between-unit variation:

WB.DF<-xtsum(DF,na.rm=TRUE,return.data.frame=TRUE)

stargazer(WB.DF,summary=FALSE,rownames=FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Logits...                                           #### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pooled logit model of civil war:

Logit<-glm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+log(GDPPerCapita)+
             GDPPerCapGrowth+PostColdWar+POLITY+POLITYSquared,data=DF,family="binomial")

summary(Logit)

# Fixed Effects logit model:

FELogit<-bife(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+log(GDPPerCapita)+
                GDPPerCapGrowth+PostColdWar+POLITY+POLITYSquared|ISO3,data=DF,model="logit")

summary(FELogit)

# Alternative Fixed Effects logit w/feglm:

FELogit2<-feglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+log(GDPPerCapita)+
                  GDPPerCapGrowth+PostColdWar+POLITY+POLITYSquared|ISO3,data=DF,family="binomial")

summary(FELogit2)

# Random Effects:

RELogit<-pglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+log(GDPPerCapita)+
                GDPPerCapGrowth+PostColdWar+POLITY+POLITYSquared|ISO3,data=DF,family=binomial,
              effect="individual",model="random",method="bfgs")

summary(RELogit)

# A table:

texreg(list(Logit,FELogit,FELogit2,RELogit),
       custom.model.names=c("Logit","FE Logit","FEs+Robust","RE Logit"),
       custom.coef.names=c("Intercept","ln(Land Area)","ln(Population)",
                           "Urban Population","ln(GDP Per Capita)","GDP Growth",
                           "Post-Cold War","POLITY","POLITY Squared",
                           "Estimated Sigma"), 
       stars=0.05,caption=" ",label=" ",
       file="BinaryFERE-26.tex")

# Example of marginal effects on quadratic POLITY variable,
# using the -marginaleffects- package...
#
# Re-fit FE model, using an explicit squared term for POLITY:

FELogit3<-feglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+log(GDPPerCapita)+
                  GDPPerCapGrowth+PostColdWar+POLITY+I(POLITY^2)|ISO3,data=DF,
                family="binomial")

pred <- avg_predictions(
  FELogit3,
  newdata=datagrid(POLITY=seq(0,10,by=1),
                   grid_type="counterfactual"),
  by="POLITY",vcov=FALSE
)

ggplot(pred,aes(x=POLITY,y=estimate)) +
  geom_line(linewidth=1.4) +
  labs(x="POLITY",y="Pr(Civil War)") +
  ylim(0,0.4) +
  theme_classic(base_size=11)

ggsave("POLITY-Marginal-Effects-26.pdf",width=6,
       height=5,units="in",dpi=300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generalized Estimating Equations (GEEs)             ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Back to the binary Civil War variable...
#
# Zap all the missingness, to make our life
# a little easier:

DF<-DF[complete.cases(DF),]

# 
#
# We're going to include a model that uses the "unstructured"
# correlations structure...This is a *bad* idea with a big T, 
# so let's subset the data by extracting five semi-recent
# years and just looking at those:

DF$flag<-ifelse(as.numeric(as.character(DF$Year))<2018 & 
                  as.numeric(as.character(DF$Year))>2012,1,0)
DF5<-DF[DF$flag==1,]


# Now, we'll compare the modelsusing the five-year data, and
# also dropping the -PostColdWar- variable, since it has no
# variation...

# Independence

GEE.ind5<-geeglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+
                   log(GDPPerCapita)+GDPPerCapGrowth+POLITY+
                   POLITYSquared,data=DF5,id=ISO3,family="binomial",
                 corstr="independence")
summary(GEE.ind5)

# Exchangeable

GEE.exc5<-geeglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+
                   log(GDPPerCapita)+GDPPerCapGrowth+POLITY+
                   POLITYSquared,data=DF5,id=ISO3,family="binomial",
                corstr="exchangeable")
summary(GEE.exc5)

# AR(1)

GEE.ar15<-geeglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+
                   log(GDPPerCapita)+GDPPerCapGrowth+POLITY+
                   POLITYSquared,data=DF5,id=ISO3,family="binomial",
                 corstr="ar1")

# Unstructured:

GEE.unstr<-geeglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+
                    log(GDPPerCapita)+GDPPerCapGrowth+POLITY+
                    POLITYSquared,data=DF5,id=ISO3,family="binomial",
                  corstr="unstructured")
summary(GEE.unstr)

# Put them in a nice table, using -modelsummary- (which
# works nicely with -geeglm-):

GEEs<-list("Independence"=GEE.ind5,"Exchangeable"=GEE.exc5,
           "AR(1)"=GEE.ar15,"Unstructured"=GEE.unstr)

modelsummary(GEEs,output="GEERegs-26.tex",stars=TRUE,
             title="GEE Models of Civil War Onset",
             fmt=3,gof_map=c("nobs","r.squared","adj.r.squared"),
             coef_rename=c("(Intercept)","ln(Land Area)","ln(Population)",
                           "Urban Population","ln(GDP Per Capita)","GDP Growth",
                           "POLITY","POLITY Squared"))

# /fin