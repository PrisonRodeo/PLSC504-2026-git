#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Intro materials...                                   ####
#
# PLSC 504 -- Fall 2026
#
# Panel/TSCS: "Unit effects" and dynamic models
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set working directory (or not, whatever...)

setwd("~/Dropbox (Personal)/PLSC 504") # change as needed

# This code takes a list of packages ("P") and (a) checks for whether
# the package is installed or not, (b) installs it if it is not, and 
# then (c) loads each of them:

P<-c("RCurl","readr","haven","colorspace","foreign","psych","car",
     "lme4","plm","gtools","boot","plyr","dplyr","texreg","statmod",
     "plm","tibble","pscl","naniar","ExPanDaR","stargazer","prais",
     "sf","maps","usmap","mapdata","countrycode","rworldmap",
     "nlme","tseries","panelView","performance","xtsum",
     "modelsummary","marginaleffects","ggplot2")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run ^ this code a few times to make it work - that is, until
# you get all smiley faces :)
#
# Set R Options:

options(scipen = 8) # bias against scientific notation
options(digits = 2) # show fewer decimal places

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# World Development Indicators (WDI) data              ####
#
# The WDI data we'll be using for this session can be 
# accessed by running the "WDI-MakeData.R"script found 
# on the Github repository. You can modify that script 
# to add additional variables if you choose to. The data 
# are also available in ready-to-use format in the "Data" 
# folder on the Github repo.
#
# Get the data:

wdi<-read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/main/Data/WDI26a.csv")

# Add a "Post-Cold War" variable:

wdi$PostColdWar <- with(wdi,ifelse(Year<1990,0,1))

# Add WBLI + parental leave data:

wbli<-read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/main/Data/WBLI-PPL.csv")

wdi<-merge(wdi,wbli,by=c("ISO3","Year"),all.x=TRUE,all.y=TRUE)
wdi$PaidParentalLeave<-ifelse(wdi$PaidParentalLeave=="Yes",1,0)

# Summarize:

describe(wdi,fast=TRUE,ranges=FALSE,check=TRUE)

# Missing data viz:

library(naniar)

pdf("Notes/WDI-Missing-2026.pdf",7,5)
par(mar=c(4,4,4,6))
p <- vis_miss(wdi) +
  theme(
    text = element_text(size = 6),
    plot.margin = margin(t = 20, r = 50, b = 20, l = 20),
    axis.text.x.top = element_text(
      angle = 45,
      hjust = 0,
      vjust = 0
    )
  )
p
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Visualizing WDI data...                            ####

pdf("Notes/PanelWBLIViz26.pdf",7,5)
panelview(WomenBusLawIndex~1,data=wdi,theme.bw=TRUE,
          outcome.type="continuous",type="outcome",
          by.timing=TRUE,index=c("ISO3","Year"),
          main=" ",ylab="Women's Business Law Index",
          legendOff=TRUE)
dev.off()

# Map! (WBLI version):

mapWDI<-with(wdi[wdi$Year==2023,],data.frame(ISO3=ISO3,
                                             WBLI=WomenBusLawIndex))
mapData<-joinCountryData2Map(mapWDI,joinCode="ISO3",
                             nameJoinColumn="ISO3",
                             mapResolution="low")

pdf("Notes/WBLI-Map-26.pdf",8,6)
par(mar=c(1,1,0.1,1))
MAP<-mapCountryData(mapData,nameColumnToPlot="WBLI",
                    mapTitle="",
                    addLegend="FALSE",
                    catMethod = c(0,60,70,80,85,90,95,100),
                    colourPalette=c("darkblue","blue","lightblue","grey64",
                                    "goldenrod1","orange","darkorange3"))
do.call(addMapLegend,c(MAP,legendLabels="all",
                       legendWidth=0.5,digits=2,
                       labelFontSize=0.8,legendMar=4))
dev.off()

# Binary data on Paid Parental Leave:

pdf("Notes/PanelPLeaveViz26.pdf",7,5)
panelview(WomenBusLawIndex~PaidParentalLeave,data=wdi,theme.bw=TRUE,
          by.timing=FALSE,index=c("ISO3","Year"),
          color=c("orange","darkgreen"),
          legend.labs=c("No Paid Leave","Paid Leave"),
          main=" ",ylab="Country Code",axis.lab.gap=c(5,5),
          background="white")
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Unit Effects...                                      ####
#
# FE plot:

i<-1:4
t<-20
NT<-t*max(i)
set.seed(7222009)
df<-data.frame(i=rep(i,t),
               t=rep(1:t,max(i)),
               X=runif(NT))
df$Y=1+2*i+4*df$X+runif(NT)
df<-df[order(df$i,df$t),] # sort

pdf("Notes/FEIntuition26.pdf",7,6)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=i+14,col=i,
              xlim=c(0,1),ylim=c(3,14)))
abline(a=3.5,b=4,lwd=2,lty=1,col=1)
abline(a=5.5,b=4,lwd=2,lty=2,col=2)
abline(a=7.5,b=4,lwd=2,lty=3,col=3)
abline(a=9.5,b=4,lwd=2,lty=4,col=4)
legend("bottomright",bty="n",col=1:4,lty=1:4,
       lwd=2,legend=c("i=1","i=2","i=3","i=4"))
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-=
# Variation: Total, within, and between              ####
#
# Create "panel series" from WDI...

WDI<-pdata.frame(wdi,index=c("ISO3","Year"))
class(WDI)
WBLI<-WDI$WomenBusLawIndex
class(WBLI)

describe(WBLI,na.rm=TRUE) # all variation

pdf("Notes/WBLIAll26.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(WDI$WomenBusLawIndex,na.rm=TRUE),
     main="",xlab="WBL Index",lwd=2)
abline(v=mean(WDI$WomenBusLawIndex,na.rm=TRUE),
       lwd=1,lty=2)
dev.off()

# "Between" variation:

describe(plm::between(WBLI,effect="individual",na.rm=TRUE)) # "between" variation

WBLIMeans<-plm::between(WBLI,effect="individual",na.rm=TRUE)

WBLIMeans<-ddply(WDI,.(ISO3),summarise,
                 WBLIMean=mean(WomenBusLawIndex,na.rm=TRUE))

pdf("Notes/WBLIBetween26.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(WBLIMeans$WBLIMean,na.rm=TRUE),
     main="",xlab="Mean WBLI",lwd=2)
abline(v=mean(WBLIMeans$WBLIMean,na.rm=TRUE),
       lwd=1,lty=2)
dev.off()

# "Within" variation:

describe(Within(WBLI,na.rm=TRUE)) # "within" variation

pdf("Notes/WBLIWithin26.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(Within(WBLI,na.rm=TRUE),na.rm=TRUE),
     main="",xlab="WBLI: Within-Country Variation",
     lwd=2)
abline(v=0,lty=2)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Regression! One-Way Unit Effects models              ####

# Subset (just for descriptives):

vars<-c("ISO3","Year","WomenBusLawIndex","PopGrowth","UrbanPopulation",
        "FertilityRate","GDPPerCapita","NaturalResourceRents","PostColdWar")
smol<-WDI[vars]
smol<-smol[complete.cases(smol),] # listwise deletion
smol$ISO3<-droplevels(smol$ISO3) # drop unused factor levels
smol$lnGDPPerCap<-log(smol$GDPPerCapita)
smol$GDPPerCapita<-NULL
describe(smol[,3:9],fast=TRUE)

# Summary: Between vs. within variation in the predictors:

Xs.df<-xtsum(smol,id="ISO3",t="Year",return.data.frame=TRUE)
stargazer(Xs.df,summary=FALSE)

# Pooled OLS:

OLS<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                 log(GDPPerCapita)+NaturalResourceRents+PostColdWar, 
                 data=WDI,model="pooling")
summary(OLS)

# "Fixed" / within effects:

FE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+log(GDPPerCapita)+
        NaturalResourceRents+PostColdWar,data=WDI,effect="individual",model="within")

summary(FE)

# Make a table:

options(digits = 4)
Table1 <- stargazer(OLS,FE,
                    title="Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    omit.stat="f",
                    out="Notes/UFX11-26.tex")

# Tests for \alpha_i = 0:

pFtest(FE,OLS)
plmtest(FE,effect=c("individual"),type=c("bp"))
plmtest(FE,effect=c("individual"),type=c("kw"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Interpretation...

with(WDI, sd(UrbanPopulation,na.rm=TRUE)) # all variation

WDI<-ddply(WDI, .(ISO3), mutate,
               UPMean = mean(UrbanPopulation,na.rm=TRUE))
WDI$UPWithin<-with(WDI, UrbanPopulation-UPMean)

with(WDI, sd(UPWithin,na.rm=TRUE)) # "within" variation

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Between effects:

BE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                log(GDPPerCapita)+NaturalResourceRents+PostColdWar,data=WDI,
        effect="individual",model="between")

summary(BE)

Table2 <- stargazer(OLS,FE,BE,
                    title="Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="Notes/UFX21-26.tex")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Random effects:

RE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
          PostColdWar,data=WDI,effect="individual",model="random")

summary(RE)

Table3 <- stargazer(OLS,FE,BE,RE,
                    title="Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    omit.stat="f",
                    out="Notes/UFX31-26.tex")

# Model summary (coefficient) plot:
#
# models<-list("OLS"=OLS,"FE"=FE,"BE"=BE,"RE"=RE)
# 
# pdf("FE-BE-RE-ModelPlot-26.pdf",7,5)
# p<-modelplot(models,coef_omit = 'Interc')
# p + geom_vline(xintercept=0)
# dev.off()

# Hausman test:

phtest(FE, RE)  # ugh...

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Separating "within" and "between" effects            ####
# 
# Create "between" and "within" versions of the
# Natural Resource Rents variable:

WDI<-ddply(WDI,.(ISO3),mutate,
             NRR.Between=mean(NaturalResourceRents,na.rm=TRUE))
WDI$NRR.Within<- (WDI$NaturalResourceRents - WDI$NRR.Between) 

# Fit a (standard / OLS) regression model that includes both:

WEBE.OLS<-lm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
           log(GDPPerCapita)+NRR.Within+NRR.Between+PostColdWar,
           data=WDI)

# summary(WEBE.OLS) # not reported

# Nice table:

Table4 <- stargazer(WEBE.OLS,
                    title="BE + WE Model of WBLI",
                    column.separate=c(1,1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Within-Country Nat. Resource Rents",
                                       "Between-Country Nat. Resource Rents",
                                       "Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    omit.stat="f",
                    out="Notes/WEBE1-26.tex")

# While it's pretty obvious that they are different, let's 
# formally test their equality, using a standard F-test (which
# we can do using the -linearHypothesis- command in the -car-
# package, among others):

linearHypothesis(WEBE.OLS,c("NRR.Within=NRR.Between"))

# Extension: The "Mundlak device" (using the minimal data
# frame called "smol" that we created above). First, re-fit 
# the FE model:

FE2<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+lnGDPPerCap+
        NaturalResourceRents+PostColdWar,data=smol,effect="individual",model="within")

# Then create unit-level means of *all* the (time-varying) predictors:

smol$PGBetween<-plm::Between(smol$PopGrowth,effect="individual")
smol$UPBetween<-plm::Between(smol$UrbanPopulation,effect="individual")
smol$FRBetween<-plm::Between(smol$FertilityRate,effect="individual")
smol$GDPBetween<-plm::Between(smol$lnGDPPerCap,effect="individual")
smol$NRRBetween<-plm::Between(smol$NaturalResourceRents,effect="individual")
smol$PCWBetween<-plm::Between(smol$PostColdWar,effect="individual")

# Finally, regress Y on both using plain-old OLS:

MD<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+lnGDPPerCap+NaturalResourceRents+
        PostColdWar+PGBetween+UPBetween+FRBetween+GDPBetween+NRRBetween+PCWBetween,
        data=smol,effect="individual",model="pooling")

summary(MD)

# Compare:

Table5 <- stargazer(FE2,MD,
                    title="FE and Mundlak Device",
                    column.separate=c(1,1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents",
                                       "Post-Cold War",
                                       "Between-Country Population Growth",
                                       "Between-Country Urban Population",
                                       "Between-Country Fertility Rate",
                                       "Between-Country ln(GDP Per Capita)",
                                       "Between-Country Nat. Resource Rents",
                                       "Between-Country Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="Notes/Mundlak-26.tex")

# Testing:

linearHypothesis(MD,c("PGBetween = 0",
                      "UPBetween = 0",
                      "FRBetween = 0",
                      "GDPBetween = 0",
                      "NRRBetween = 0",
                      "PCWBetween = 0"))

# Mundlak's CREM model...
#
# This basically means fitting the same OLS specification as 
# above, but in a RE model.
#
# Create "within" variables:

smol$PGWithin  <- smol$PopGrowth - smol$PGBetween
smol$UPWithin  <- smol$UrbanPopulation - smol$UPBetween
smol$FRWithin  <- smol$FertilityRate - smol$FRBetween
smol$GDPWithin <- smol$lnGDPPerCap - smol$GDPBetween
smol$NRRWithin <- smol$NaturalResourceRents - smol$NRRBetween
smol$PCWWithin <- smol$PostColdWar - smol$PCWBetween

# Model:

MCREM <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+lnGDPPerCap+NaturalResourceRents+
               PostColdWar+PGBetween+UPBetween+FRBetween+GDPBetween+NRRBetween+PCWBetween,
             data=smol,index=c("ISO3","YearNumeric"),effect="individual",model="random",
             random.method = "walhus")

summary(MCREM)

# Mundlak test:

linearHypothesis(
  MCREM,
  c(
    "PGBetween = 0",
    "UPBetween = 0",
    "FRBetween = 0",
    "GDPBetween = 0",
    "NRRBetween = 0",
    "PCWBetween = 0"
  )
)

# Create another table with standard RE and CREM models
# side-by-side...
#
# First, re-fit the standard RE model:

RE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+lnGDPPerCap+NaturalResourceRents+
          PostColdWar,data=smol,effect="individual",model="random")

# summary(RE)
#
# Now, a table:

Table6 <- stargazer(RE,MCREM,
                    title="Random Effects and Mundlak's CREM",
                    column.separate=c(1,1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents",
                                       "Post-Cold War",
                                       "Between-Country Population Growth",
                                       "Between-Country Urban Population",
                                       "Between-Country Fertility Rate",
                                       "Between-Country ln(GDP Per Capita)",
                                       "Between-Country Nat. Resource Rents",
                                       "Between-Country Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="Notes/Mundlak-2-26.tex")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Two-way effects...                                   ####
#
# First, "fixed" effects (that is, within-unit-and-time):

TwoWayFE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
              log(GDPPerCapita)+NaturalResourceRents+PostColdWar,data=WDI,
              effect="twoway",model="within")

summary(TwoWayFE)

# Equivalence to -lm- (note that we omit the Post-Cold War
# variable; in addition, the "-1" in the formula means that
# we're omitting the intercept in the model, so that the
# FE estimates are the unit- and period-specific means):
# 
# TwoWayFE.BF<-lm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
#                 factor(ISO3)+factor(Year)-1,data=WDI)
# 
# summary(TwoWayFE.BF)
#
# Two-way "random" effects:

TwoWayRE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
              log(GDPPerCapita)+NaturalResourceRents+PostColdWar,data=WDI,
              effect="twoway",model="random")

summary(TwoWayRE)

# Here's a nicer table:

Table6 <- stargazer(OLS,FE,BE,RE,TwoWayFE,TwoWayRE,
                    title="Models of WBLI",
                    column.separate=c(1,1,1,1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    column.sep.width="1pt",
                    omit.stat=c("f"),
                    out="Notes/UFX51-26.tex")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Interpretation using modelsummary                    ####
#
# We'll use our OLS and one-way fixed and random (unit) effects 
# models as the example. Here's the no-frills version:

modelsummary(list(OLS,FE,RE),output="Notes/table.tex",stars=TRUE)

# And here's a better one that follows good practices for tables:

models<-list("OLS"=OLS,"Within"=FE,"Random"=RE)

modelsummary(models,output="Notes/MS-Table-26.tex",title="Models of WDBI",
             stars=TRUE,fmt=2,gof_map=c("nobs","r.squared","adj.r.squared"),
             coef_rename=c("PopGrowth"="Population Growth",
                           "UrbanPopulation"="Urban Population",
                           "FertilityRate"="Fertility Rate",
                           "log(GDPPerCapita)"="ln(GDP Per Capita)",
                           "NaturalResourceRents"="Natural Resource Rents",
                           "PostColdWar"="Post-Cold War"))

# Coefficient plot (with 99% CIs):

pdf("Notes/OLS-FE-RE-Coefplot-26.pdf",6,5)
modelplot(models,conf_level=0.99,coef_omit="(Intercept)",
          coef_rename=c("PopGrowth"="Population Growth",
                        "UrbanPopulation"="Urban Population",
                        "FertilityRate"="Fertility Rate",
                        "log(GDPPerCapita)"="ln(GDP Per Capita)",
                        "NaturalResourceRents"="Natural Resource Rents",
                        "PostColdWar"="Post-Cold War"))
dev.off()

# /fin

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# APPENDIX: Dynamics in Panel Data...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Note that for PLSC 504 we won't actually cover dynamics in class;
# however, there are slides at the "back of the deck" that go into
# models for panel data with temporal dynamics, so it seemed like
# a good idea to include the code here too...
#
# Make a numeric year variable (for -panelAR-):

WDI$Year<-as.numeric(as.character(WDI$Year))
WDI$YearNumeric<-as.numeric(WDI$Year)

# Add a "Cold War" variable:

WDI$ColdWar <- with(WDI,ifelse(Year<1990,1,0))

# And zap a duplicate:

WDI<-WDI[WDI$country!="Nauru",]
WDI<-WDI[is.na(WDI$ISO3)==FALSE,]

# summary(WDI)
#
# Make the data a panel dataframe:

WDI<-pdata.frame(WDI,index=c("ISO3","Year"))

# Sort:

WDI<-WDI[order(WDI$ISO3,WDI$Year),]

# Summary statistics:

vars<-c("ISO3","Year","WomenBusLawIndex","PopGrowth","UrbanPopulation",
        "FertilityRate","GDPPerCapita","NaturalResourceRents","ColdWar")
smol<-data.frame(WDI[vars])
smol<-smol[complete.cases(smol),] # listwise deletion
smol$lnGDPPerCap<-log(smol$GDPPerCapita)
smol$GDPPerCapita<-NULL
describe(smol,fast=TRUE)

# How much autocorrelation in those variables?

PG<-pdwtest(PopGrowth~1,data=smol)
UP<-pdwtest(UrbanPopulation~1,data=smol)
FR<-pdwtest(FertilityRate~1,data=smol)
GDP<-pdwtest(lnGDPPerCap~1,data=smol)
NRR<-pdwtest(NaturalResourceRents~1,data=smol)
CW<-pdwtest(ColdWar~1,data=smol)

rhos<-data.frame(Variable=c("Population Growth","Urban Population",
                            "Fertility Rate","GDP Per Capita",
                            "Natural Resource Rents","Cold War"),
                 Rho = c(1-(PG$statistic/2),1-(UP$statistic/2),
                         1-(FR$statistic/2),1-(GDP$statistic/2),
                         1-(NRR$statistic/2),1-(CW$statistic/2)))

stargazer(rhos,summary=FALSE,out="Notes/Rhos-26.tex")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Panel unit root tests...                            ####
#
# Data:

WBLI<-data.frame(ISO3=WDI$ISO3,Year=WDI$Year,
                 WBLI=WDI$WomenBusLawIndex)
WBLI<-na.omit(WBLI)  # remove missing
WBLI<-pdata.frame(WBLI,index=c("ISO3","Year")) # panel data
WBLI.W<-data.frame(split(WBLI$WBLI,WBLI$ISO3)) # "wide" data

purtest(WBLI.W,exo="trend",test="levinlin",pmax=2)
purtest(WBLI.W,exo="trend",test="hadri",pmax=2)
purtest(WBLI.W,exo="trend",test="madwu",pmax=2)
purtest(WBLI.W,exo="trend",test="ips",pmax=2)

# Gather the statistics:

ur1<-purtest(WBLI.W,exo="trend",test="levinlin",pmax=2)
ur2<-purtest(WBLI.W,exo="trend",test="hadri",pmax=2)
ur3<-purtest(WBLI.W,exo="trend",test="madwu",pmax=2)
ur4<-purtest(WBLI.W,exo="trend",test="ips",pmax=2)

urs<-matrix(nrow=4,ncol=5)
for(i in 1:4){
  nom<-get(paste0("ur",i))
  urs[i,1]<-nom$statistic$method
  urs[i,2]<-nom$statistic$alternative
  urs[i,3]<-names(nom$statistic$statistic)
  urs[i,4]<-round(nom$statistic$statistic,3)
  urs[i,5]<-round(nom$statistic$p.value,4)
}

urs<-data.frame(urs)
colnames(urs)<-c("Test","Alternative","Statistic",
                 "Estimate","P-Value")
URTests<-stargazer(urs,summary=FALSE,
                   title="Panel Unit Root Tests: WBLI",
                   out="Notes/URTests-26.tex")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Some regression models, with dynamics...            ####
#
# Lagged -dependent-variable model:

WDI$WBLI.L <- plm::lag(WDI$WomenBusLawIndex,n=1) # be sure to use the
# -plm- version of -lag-

LDV.fit <- lm(WomenBusLawIndex~WBLI.L+PopGrowth+UrbanPopulation+
                FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
                ColdWar,data=WDI)

FD.fit <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                log(GDPPerCapita)+NaturalResourceRents+ColdWar,
              data=WDI,effect="individual",model="fd")

FE.fit <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                log(GDPPerCapita)+NaturalResourceRents+ColdWar,
              data=WDI,effect="individual",model="within")

LDV.FE.fit <- plm(WomenBusLawIndex~WBLI.L+PopGrowth+UrbanPopulation+
                    FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
                    ColdWar,data=WDI,effect="individual",model="within")

# Next: Arellano-Bond model. Do not run, unless
# you are young, and have lots of time on your
# hands...

AB.fit<-pgmm(WomenBusLawIndex~WBLI.L+PopGrowth+UrbanPopulation+
               FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
               ColdWar|lag(WomenBusLawIndex,2:20),data=WDI,
             effect="individual",model="twosteps")

# Table:

texreg(list(LDV.fit,FD.fit,FE.fit,LDV.FE.fit),
       custom.model.names=c("Lagged Y","First Difference","FE","Lagged Y + FE"),
       custom.coef.names=c("Intercept","Lagged WBLI",
                           "Population Growth","Urban Population",
                           "Fertility Rate","ln(GDP Per Capita)",
                           "Natural Resource Rents","Cold War"),
       digits=3,stars=0.05)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Trend things...                               ####
#
# Trend illustration simulation:

set.seed(2719)
Tobs<-40 
X<-cumsum(rnorm(Tobs))+5
u<-rnorm(T,0,2)
T<-1:Tobs
Y<-10+X+u
Yt<-5+X+0.5*T+u

pdf("Notes/TrendPlot-26.pdf",5,6)
par(mar=c(4,4,2,2))
plot(T,Yt,t="l",lwd=2,lty=2,ylim=c(0,35),
     ylab="Y",col="blue")
lines(T,Y,lty=3,lwd=2,col="orange")
lines(T,X,lwd=2,col="black")
legend("topleft",bty="n",lwd=2,lty=c(2,3,1),
       col=c("blue","orange","black"),
       legend=c("Y2","Y1","X"))
dev.off()

# Regressions:

f1<-lm(Y~X)
f2<-lm(Yt~X)
f3<-lm(Yt~X+T)

stargazer(f1,f2,f3,omit.stat="f")

# Make a better trend variable in the WDI data:

WDI$Trend <- WDI$YearNumeric-1950

# FE w/o trend:

FE  <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
             log(GDPPerCapita)+NaturalResourceRents+ColdWar,
           data=WDI,effect="individual",model="within")

# FE with trend:

FE.trend <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+
                  FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
                  ColdWar+Trend,data=WDI,effect="individual",
                model="within")

# FE with trend + interaction:

FE.intx <- plm(WomenBusLawIndex~PopGrowth+
                 UrbanPopulation+FertilityRate+
                 log(GDPPerCapita)+NaturalResourceRents+
                 ColdWar+Trend+ColdWar*Trend,
               data=WDI,effect="individual",model="within")

# A table:

TableTrend <- stargazer(FE,FE.trend,FE.intx,
                        title="FE Models of WBLI",
                        column.separate=c(1,1),align=TRUE,
                        dep.var.labels.include=FALSE,
                        dep.var.caption="",
                        covariate.labels=c("Population Growth","Urban Population",
                                           "Fertility Rate","ln(GDP Per Capita)",
                                           "Natural Resource Rents","Cold War",
                                           "Trend (1950=0)","Cold War x Trend"),
                        header=FALSE,model.names=FALSE,
                        model.numbers=FALSE,multicolumn=FALSE,
                        object.names=TRUE,notes.label="",
                        omit.stat="f",
                        out="Notes/Trendy-26.tex")

# /fin