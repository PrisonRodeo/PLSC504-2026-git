#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# INTRODUCTION                                  ####
#
# Code for PLSC 504 - Fall 2026
#
# Binary- and Nominal-Response Regression 
# Models: Extensions (mostly)
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages, etc.:                               ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("devtools","readr","RCurl","elrm","logistf","VGAM","mlogit",
     "marginaleffects","modelsummary","tinytable","aod","nnet",
     "stargazer","MNP","ggcorrplot","clarify","tidyr",
     "MASS","scales","margins","car","vcd","ggplot2","dfidx",
     "psych")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 10-12 times until you get all smileys. :)
#
# Then be sure to do this too:

devtools::install_github("ManuelNeumann/MNLpred")
library(MNLpred)

# Set working directory:
#
# setwd("~/Dropbox (Personal)/PLSC 504") # <- change as needed...
#                                             or use a project, whatever,
#                                             it's your call.
#
# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Binary Responses: Separation...               ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Table

Yeas<-t(c(rep(0,times=212),rep(1,times=219)))
Dems<-t(c(rep(0,times=178),rep(1,times=253)))
table(Yeas,Dems)

# Simulated Logits:

set.seed(7222009)
X<-runif(100,min=-5,max=5)
X<-X[order(X)]
Z<-runif(100,min=-5,max=5)
Y<-ifelse(plogis(X+Z)>0.5,1,0)
Y2<-ifelse(plogis(X+0.5*Z)>0.5,1,0)
Y3<-ifelse(plogis(X+0.1*Z)>0.5,1,0)
Ysep<-ifelse(plogis(X)>0.5,1,0)
Yfit<-glm(Y~X,family="binomial")
Y2fit<-glm(Y2~X,family="binomial")
Y3fit<-glm(Y3~X,family="binomial")
Ysepfit<-glm(Ysep~X,family="binomial")

# Plots:

pdf("Notes/Separation.pdf",8,7)
par(mar=c(4,4,2,2))
par(mfrow=c(2,2))
plot(X,Y,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Yfit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Yfit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Yfit))[4],digits=2))))
plot(X,Y2,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Y2fit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Y2fit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Y2fit))[4],digits=2))))
plot(X,Y3,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Y3fit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Y3fit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Y3fit))[4],digits=2))))
plot(X,Ysep,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Ysepfit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Ysepfit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Ysepfit))[4],digits=2))))
dev.off()

# Toy data:

rm(X,Y,Z)
set.seed(7222009)
Z<-rnorm(500)
W<-rnorm(500)
Y<-rbinom(500,size=1,prob=plogis((0.2+0.5*W-0.5*Z)))
X<-rbinom(500,1,(pnorm(Z)))
X<-ifelse(Y==0,0,X)

summary(glm(Y~W+Z+X,family="binomial"))
summary(glm(Y~W+Z+X,family="binomial",maxit=100,epsilon=1e-16))

# data<-as.data.frame(cbind(W,X,Y,Z))
# write.dta(data,"SepSim.dta") # for the old Stata example

# Exact logistic regression...
# DO NOT ACTUALLY RUN THIS CODE -- your computer will likely
# freeze up. It's here for example purposes only.
#
# df <- data.frame(one=1,Y=Y,W=W,Z=Z,X=X)
# toy.elrm <- elrm(Y/one~W+Z+X,interest=~X,dataset=df,
#                  r=4,iter=5000,burnIn=1000)
# summary(toy.elrm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pets-as-family data example:

Pets<-read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/master/Data/Pets.csv")

# Summary statistics:

summary(Pets)

# Simple model:

Pets.1<-glm(petfamily~female+as.factor(married)+as.factor(partyid)
            +as.factor(education),data=Pets,family=binomial)
summary(Pets.1)

Pets.2<-glm(petfamily~female+as.factor(married)*female+as.factor(partyid)+
              as.factor(education),data=Pets,family=binomial)
summary(Pets.2)

with(Pets, xtabs(~petfamily+as.factor(married)+female))

# Firth regression:

Pets.Firth<-logistf(petfamily~female+
                      as.factor(married)*female+as.factor(partyid)+
                      as.factor(education),data=Pets,flic=TRUE)
summary(Pets.Firth)

# Profile Firth profile likelihood:

Pets.profile<-profile(Pets.Firth,
     variable="femaleMale:as.factor(married)Widowed",
     firth=TRUE)

# Plot it:

pdf("Notes/PetsProfileL26.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Pets.profile)
abline(v=Pets.Firth$coefficients[15],lty=2,lwd=2)
abline(v=0,lty=3,lwd=1)
dev.off()



#---
# Binary Responses: Rare Events                 ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Rare Event bias figure:

N<-c(.1,5,seq(10,10000,by=10))
p4 <- (0.4-0.5) / (N*(0.4)*(1-0.4))
p1 <- (0.1-0.5) / (N*(0.1)*(1-0.1))
p01 <- (0.01-0.5) / (N*(0.01)*(1-0.01))
p001 <- (0.001-0.5) / (N*(0.001)*(1-0.001))

pdf("Notes/RareEventBiasR26.pdf",7,6)
par(mar=c(4,4,2,2))
plot(N,p4,t="l",lwd=2,lty=1,col="black",xlab="N",
      ylab="Bias in the Intercept",ylim=c(-0.5,0))
lines(N,p1,t="l",lwd=2,lty=2,col="green")
lines(N,p01,t="l",lwd=2,lty=3,col="yellow")
lines(N,p001,t="l",lwd=2,lty=4,col="red")
legend("bottomright",bty="n",
        legend=c(expression(paste(bar(pi),"=0.4")),
                 expression(paste(bar(pi),"=0.1")),
                 expression(paste(bar(pi),"=0.01")),
                 expression(paste(bar(pi),"=0.001"))),
       col=c("black","green","yellow","red"),
       lty=c(1,2,3,4),lwd=2)
dev.off()

# TAPS data:

TAPS<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/main/Data/SomeTAPSData.csv")
TAPS<-subset(TAPS,select=-(BeesOrBear)) # Zap
TAPS<-TAPS[complete.cases(TAPS),] # delete cases w/NAs
TAPS$Age10<-TAPS$Age/10 # rescale Age variable

table(TAPS$RunOutOfGas)
prop.table(table(TAPS$RunOutOfGas))

# Basic logit:

ROGlogit<-glm(RunOutOfGas~Education+Age10+Female+White+Black+Asian+
                     Democrat+GOP+Ideology,data=TAPS,family=binomial)
summary(ROGlogit)

# Zelig: King-Zeng "rare events" logit:

RElogit<-relogit(RunOutOfGas~Education+Age10+Female+White+Black+Asian+
                 Democrat+GOP+Ideology, tau = NULL,
                 case.control = c("prior", "weighting"), # n/a here
                 bias.correct = TRUE, data=TAPS)
summary(RElogit)

# Firth logit, for comparison:

relogit.firth<-logistf(RunOutOfGas~Education+Age10+Female+White+Black+Asian+
                    Democrat+GOP+Ideology,data=TAPS)
summary(relogit.firth)

# Firth logit with FLIC:

relogit.flic<-logistf(RunOutOfGas~Education+Age10+Female+White+Black+Asian+
                         Democrat+GOP+Ideology,data=TAPS,flic=TRUE)
summary(relogit.flic)


# Combine and plot coefficients:

BHats<-data.frame(Logit=coef(ROGlogit))
BHats$RE.KZ<-coef(RElogit)
BHats$RE.Firth<-coef(relogit.firth)
BHats$RE.FLIC<-coef(relogit.flic)
BHT<-data.frame(t(BHats))
BHT$Intercept<-BHT$X.Intercept.
BHT$X.Intercept.<-NULL

pdf("Notes/REStripchart-26.pdf",8,6)
par(mar=c(4,7,2,2))
stripchart(BHT[1,],group.names=colnames(BHT),yaxt="n",
           pch=15,xlim=c(-2,2),
           col="black")
points(BHT[2,],1:10,col="blue",pch=16)
points(BHT[3,],1:10,col="orange",pch=17)
points(BHT[4,],1:10,col="green",pch=17)
abline(v=0,lty=2)
axis(2,at=c(1:10),labels=colnames(BHT),las=1)
legend("bottomright",pch=c(15:18,20),bty="n",
       col=c("black","blue","orange","green"),
       legend=c("Logit","King-Zeng","Firth","Firth w/FLIC"))
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Nominal-Level Responses: Introduction         ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# First, the "small" version of the 1992 U.S. presidential
# election data...

NES92<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/master/Data/ANES92.csv")

# summary:

describe(NES92)

# Three different ways to fit the same model...
#
# #1: Using -vglm-:

NES92.mlogit<-vglm(PresVote~PartyID+Age+White+Female,multinomial,data=NES92)
summary(NES92.mlogit)

# #2: using multinom (change the "baseline" category):

NES92$PresVote2<-factor(NES92$PresVote, 
                        levels = c("3", "1", "2"), 
                        labels = c("Perot", "Bush", "Clinton"))
NES92.mlogit2<-multinom(PresVote2~PartyID+Age+White+Female,data=NES92)
summary(NES92.mlogit2)

# #3: using -mlogit- (requires "reshaping" data):

head(NES92)
AltNES92<-dfidx(NES92,varying=9:11,shape="wide",choice="VotedFor")
head(AltNES92)

NES92.mlogit3<-mlogit(VotedFor~0|PartyID+Age+White+Female,
                      data=AltNES92,reflevel="Perot")
summary(NES92.mlogit3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Conditional logit...                     ####
#
# Data structure, revisited:

head(AltNES92)

# Conditional logistic regression (Feeling Thermomenter
# variable only):

NES92.clogit<-mlogit(VotedFor~FT,data=AltNES92,reflevel="Perot")
summary(NES92.clogit)

# "Full" model w/all predictors:

NES92.clogit2<-mlogit(VotedFor~FT|PartyID+Age+White+Female,
                      data=AltNES92,reflevel="Perot")
summary(NES92.clogit2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Interpretation!                          ####

NES.MNL<-vglm(PresVote~PartyID+Age+White+Female,data=NES92,
              multinomial(refLevel=1)) # Bush is comparison category
summaryvglm(NES.MNL)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Nice tables using -modelsummary- tools...

mod<-list(NES92.mlogit2)
get_estimates(mod[[1]])

modelsummary(mod, shape = term + response ~ statistic)

modelsummary(mod, shape = model + term ~ response)


# Tests:

wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(5,6))
wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(1,3,5,7,9))

# Marginal effects, via -margins-...
#
# Recreate the results from above, using -multinom-:

MNL.alt<-multinom(PresVote2~PartyID+Age+White+Female,data=NES92,
                  Hess=TRUE)
summary(marginal_effects(MNL.alt))

# Odds ratios:

mnl.or <- function(model) { 
  coeffs <- c(t(coef(NES.MNL))) 
  lci <- exp(coeffs - 1.96 * diag(vcov(NES.MNL))^0.5) 
  or <- exp(coeffs) 
  uci <- exp(coeffs + 1.96* diag(vcov(NES.MNL))^0.5) 
  lreg.or <- cbind(lci, or, uci) 
  lreg.or 
} 

mnl.or(NES.MNL)

# In-Sample predicted outcomes / PRE:

NES92$Predictions<-" "
NES92$Predictions<-ifelse(fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,2] 
                          & fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,3],
                          paste("Bush"),NES92$Predictions) # Bush
NES92$Predictions<-ifelse(fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,1] 
                          & fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,3],
                          paste("Clinton"),NES92$Predictions) # Clinton
NES92$Predictions<-ifelse(fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,1] 
                          & fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,2],
                          paste("Perot"),NES92$Predictions) # Perot)

table(NES92$VotedFor,NES92$Predictions)

# In-sample predictions:

hats<-as.data.frame(fitted.values(NES.MNL))
names(hats)<-c("Bush","Clinton","Perot")
attach(hats)

pdf("Notes/InSampleRScatterplotMatrix.pdf",8,7)
spm(~Bush+Clinton+Perot,pch=20,plot.points=TRUE,
    diagonal="histogram",col=c("black","grey"))
dev.off()

pdf("Notes/InSampleMNLPredProbsR.pdf",6,4)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
plot(NES92$PartyID,Bush,xlab="Party ID",pch=20)
plot(NES92$PartyID,Clinton,xlab="Party ID",pch=20)
plot(NES92$PartyID,Perot,xlab="Party ID",pch=20)
par(mfrow=c(1,1))
dev.off()

# Predicted probabilities using -MNLpred-...
#
# Re-fit the -multinom- estimates again, changing 
# "White" to numeric:

NES92$WhiteNum<-ifelse(NES92$White=="White",1,0)
MNL.alt2<-multinom(PresVote2~PartyID+Age+WhiteNum+Female,
                   data=NES92,Hess=TRUE)
# summary(MNL.alt2)

# Predictions:

Hats<-mnl_pred_ova(model=MNL.alt2,data=NES92,
                   x="PartyID",by=0.1,seed=7222009,nsim=500)

# Plotting predicted probabilities & CIs
# (via ggplot; can also be done easily with 
# base R):

cand.labs <- c("Bush", "Clinton", "Perot")
names(cand.labs) <- c("1", "2", "3")

pdf("Notes/MNLPredictedProbabilities.pdf",7,5)
ggplot(data=Hats$plotdata,aes(x=PartyID,y=mean,
                              ymin=lower,ymax=upper)) +
  geom_ribbon(alpha = 0.1) +
  geom_line() + theme_bw() +
  facet_wrap(PresVote2~.,scales="fixed",
             labeller=labeller(PresVote2=cand.labs)) +
  scale_x_continuous(breaks=1:7) +
  labs(y = "Predicted Probabilities",x = "Party Identification")
dev.off()

# Plotting first differences for the WHITE variable:

FDF<-mnl_fd2_ova(model=MNL.alt2,data=NES92,x="WhiteNum",
                 value1=min(NES92$WhiteNum),
                 value2=max(NES92$WhiteNum),nsim=500)

pdf("Notes/MNLFirstDifferences.pdf",7,5)
ggplot(FDF$plotdata_fd,aes(categories, y=mean,
                           ymin=lower,max=upper)) +
  geom_pointrange() + geom_hline(yintercept=0) +
  scale_y_continuous(name="First Difference: White") +
  labs(x = "Candidate") + theme_bw()
dev.off()

# Conditional logit: In-sample predictions:

summary(NES92.clogit2)

CLhats<-predict(NES92.clogit2,AltNES92)

# Plot by candidate:

pdf("Notes/InSampleCLHatsR.pdf",7,6)
par(mar=c(4,4,2,2))
plot(NES92$FT.Bush,CLhats[,2],pch=19,
     col=rgb(100,0,0,100,maxColorValue=255),
     xlab="Feeling Thermometer",
     ylab="Predicted Probability")
points(NES92$FT.Clinton+runif(nrow(CLhats),-1,1),
       CLhats[,3],pch=4,col=rgb(0,0,100,100,maxColorValue=255))
points(NES92$FT.Perot+runif(nrow(CLhats),-1,1),
       CLhats[,1],pch=17,col=rgb(0,100,0,50,maxColorValue=255))
lines(lowess(NES92$FT.Bush,CLhats[,2]),lwd=2,col="red")
lines(lowess(NES92$FT.Clinton,CLhats[,3]),lwd=2,col="blue")
lines(lowess(NES92$FT.Perot,CLhats[,1]),lwd=2,col="darkgreen")
legend("topleft",bty="n",c("Bush","Clinton","Perot"),
       col=c("red","blue","darkgreen"),pch=c(19,4,17))
dev.off()


# fin
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=