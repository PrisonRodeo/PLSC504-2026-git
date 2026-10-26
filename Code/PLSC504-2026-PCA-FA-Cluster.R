#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preliminaries                           ####
#
# PLSC 504 -- Fall 2026
#
# Principal components analysis, factor analysis,
# and cluster analysis (whew, that's a lot...)
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages, etc.:                         ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("RCurl","readr","psych","randomNames","car","ggcorrplot",
     "cluster","pvclust","fpc","mclust","dendextend",
     "circlize","smacof","lme4","plm","gtools","biotools",
     "plyr","texreg","statmod","ltm","plotrix")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(paste0("Package count: ",i)))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 12-15 times until you get "Package count = 22."
#
# NOTE: If you are doing this on a Mac / machine running MacOS,
# you need to install XQuartz (https://www.xquartz.org/) for all
# packages to load. (Having XQuartz on your machine is generally
# a good idea anyway.)
#
# setwd("~/Dropbox (Personal)/PLSC 504/Notes")  # <-- change as necessary...
#
# Options:

options(scipen = 24) # bias against scientific notation
options(digits = 4) # show fewer decimal places

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PCA intuition figure:

set.seed(1048)
Y1 <- rnorm(400)
Y2 <- Y1 + rnorm(400)
fit<-lm(Y2~Y1)

pdf("PCA-Intuition-26.pdf",10,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(Y1,Y2,xlim=c(-4,4),ylim=c(-4,4),pch=20,
     xaxt="n",yaxt="n",xlab="X",ylab="Y")
abline(h=0,lwd=2)
abline(v=0,lwd=2)
plot(Y1,Y2,pch=20,xlim=c(-4,4),ylim=c(-4,4),
     xaxt="n",yaxt="n",xlab="X",ylab="Y")
abline(fit,lwd=2)
abline(a=fit$coefficients[1], b=-(1/fit$coefficients[2]),
       lwd=2)
abline(h=0,lwd=1,lty=2,col="grey")
abline(v=0,lwd=1,lty=2,col="grey")
text(3.5,2.5,"PC1")
text(-3.3,2.5,"PC2")
text(3,-0.3,labels=expression(cos(theta) == L1))
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Larger PCA simulation...                ####
#
# A small dataset with N=20 and K=10 variables. Six
# of the variables (Z1-Z6) are related to the latent
# Z; the other four (X1-X4) are related to the 
# latent X:

N <- 20
set.seed(7222009)
Name <- randomNames(N, which.names="first")
Z <- rnorm(N)
Z1 <- Z + 0.2*rnorm(N)
Z2 <- Z + 0.5*rnorm(N)
Z3 <- Z + 1*rnorm(N)
Z4 <- Z + 1.5*rnorm(N)
Z5 <- Z + 2*rnorm(N)
Z6 <- Z + 3*rnorm(N)

X <- rnorm(N)
X1 <- X + rnorm(N)
X2 <- X + rnorm(N)
X3 <- X + rt(N,5)
X4 <- X + rt(N,5)

df <- data.frame(Z1,Z2,Z3,Z4,Z5,Z6,X1,X2,X3,X4)
rownames(df)<-Name 

# head(df)
corrMat<-cor(df)
# corrMat
#
# Plot the correlations:

pdf("PCA-sim-corr-plot-26.pdf",6,5)
ggcorrplot(corrMat, hc.order="FALSE",
           type="lower",lab=TRUE,
           legend.title="Correlations")
dev.off()

# Flavors of PCA:

PCE <- princomp(df)
PCE

PCS <- prcomp(df,retx=FALSE)
PCS

# Using -principal-

PCSim1 <- principal(df, nfactors=1,rotate="none")
PCSim1

# minimalist plot:

pdf("PCA-loadings-plot-26.pdf",6,5)
plot(PCSim1,ylim=c(-1,1),ylab="Loadings",
     labels=names(PCSim1$communality))
dev.off()

# Scores:

PCSim1$scores

# Two principal components:

PCSim2 <- principal(df, nfactors=2,rotate="none")
PCSim2

PCSim2$scores

# a little scatterplot:

pdf("PCA-scores-scatterplot-26.pdf",6,6)
par(mar=c(4,4,2,2))
plot(PCSim2$scores,xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),
     xlab="PC One",ylab="PC Two",pch="")
text(PCSim2$scores, labels=rownames(df))
abline(h=0,lty=2)
abline(v=0,lty=2)
dev.off()

# # Three principal components:
# 
# PCSim3 <- principal(df, nfactors=3,rotate="none")
# PCSim3
# 
# # scatterplot matrix:
# 
# pdf("PCA-sim-scattermatrix-24.pdf",7,6)
# scatterplotMatrix(PCSim3$scores,col="black",
#                   pch=19,smooth=FALSE,regLine=FALSE,
#                   var.labels=c("PC One","PC Two","PC Three"))
# dev.off()
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Biplots!                                     ####

foo<-prcomp(df)
foo$rotation[,1:2]

pdf("SimulatedBiplot-26.pdf",7,6)
par(mar=c(4,4,4,2))
biplot(foo,main=" ",cex=0.8)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Factor Analysis...                      ####
#
# Return to our simulated data...
#
# FA using simulated data:

FASim1 <- factanal(df,factors=1,scores="regression",
                   rotation="none")
print(FASim1,cutoff=0)

# Plot of factor loadings vs. correlations with "Z"

FA1Ls <- FASim1$loadings[1:10,1]
ZRs<-cor(cbind(Z,df))[2:11,1]

pdf("FAvsRealCorrs-26.pdf",6,5)
par(mar=c(4,4,2,2))
plot(FA1Ls,ZRs,pch="",xlim=c(-0.6,1.1),
     ylim=c(-0.6,1.2),xlab="Factor Loadings",
     ylab="Correlations with Z")
text(FA1Ls,ZRs,labels=names(ZRs))
abline(a=0,b=1,lwd=2)
abline(h=0, lty=2)
dev.off()

# Two-factor model:

FASim2 <- factanal(df,factors=2,scores="regression",
                   rotation="none")
print(FASim2,cutoff=0)

# Three-factor model:

FASim3 <- factanal(df,factors=3,scores="regression",
                   rotation="none")
print(FASim3,cutoff=0)

# scatterplot matrix...

pdf("FA-loadings-scattermatrix-26.pdf",7,6)
scatterplotMatrix(FASim3$loadings[1:10,1:3],col="black",
                  pch=19,smooth=FALSE,regLine=FALSE,
                  var.labels=c("Factor One","Factor Two","Factor Three"))
dev.off()

cor(FASim3$loadings[1:10,1:3])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FA Remix: ANES 2016 feeling thermometers  ####
#
# Data stuff...

ANES <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/master/Data/ANES2016.csv")

Tvars <- c("V162310","V162311","V162312","V162313",
           "V162314","V162078","V162079","V162080",
           "V162081","V162091","V162092","V162093",
           "V162094","V162095","V162096","V162097",
           "V162098","V162099","V162100","V162101",
           "V162102","V162103","V162104","V162105",
           "V162106","V162107","V162108","V162109",
           "V162110","V162111","V162112","V162113")

Therms <- ANES[Tvars]
Therms[Therms==-5] <- NA
Therms[Therms==-6] <- NA
Therms[Therms==-7] <- NA
Therms[Therms==-9] <- NA
Therms[Therms==998] <- NA
Therms[Therms==999] <- NA
Therms <- na.omit(Therms)
colnames(Therms) <- c("Asian-Americans","Hispanics","Blacks",
                      "Illegal Immigrants","Whites","Dem. Pres. Candidate",
                      "GOP Pres. Candidate","Libertarian Pres. Candidate",
                      "Green Pres. Candidate","Dem. VP", "GOP VP",
                      "John Roberts", "Pope Francis",
                      "Christian Fundamentalists","Feminists","Liberals",
                      "Labor Unions","Poor People","Big Business",
                      "Conservatives","SCOTUS","Gays & Lesbians",
                      "Congress","Rich People","Muslims","Christians",
                      "Jews","Tea Party","Police","Transgender People",
                      "Scientists","BLM")

describe(Therms,range=FALSE)

# Factor Analysis, one factor:

FTFA1 <- fa(Therms,nfactors=1,fm="ml",rotate="none")
print(FTFA1)

# Plot:
FA1Ls<-as.numeric(print(FTFA1$loadings,cutoff=0))

pdf("FATherm1-26.pdf",7,4.5)
plot(FA1Ls,rep(0,times=length(FA1Ls)),pch=19,yaxt="n",
     xlim=c(-0.82,0.82),ylim=c(-0.1,1),xlab="Loadings",ylab="")
text(FA1Ls,0.03,labels=colnames(Therms),pos=4,srt=90,offset=0.1)
dev.off()

# Two factors:

FTFA2 <- fa(Therms,nfactors=2,fm="ml",
            rotate="none")
print(FTFA2)

pdf("FATherm2-26.pdf",6,5)
par(mar=c(4,4,2,2))
plot(FTFA2,labels=colnames(Therms),cex=0.6,
     xlim=c(-1.5,1.5),ylim=c(-0.1,0.6),main="",
     xlab="Factor One",ylab="Factor Two")
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CLUSTER ANALYSIS                        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Silly Tick example:

Tick<-c(1,711,0.08)
Arthur<-c(0,588,0.27)
L2<-dist(rbind(Tick,Arthur))
L1<-dist(rbind(Tick,Arthur),method="manhattan")
# Mahalanobis by hand:
Diff<-Arthur-Tick
S <- cov(rbind(Tick,Arthur))
LM <- sqrt(t(Diff)%*%solve(S,tol=1e-30)%*%Diff)
# Check:
sqrt(mahalanobis(Arthur,center=Tick,cov=S,tol=1e-30))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simulation Data                         ####

N <- 20
set.seed(7222009)
Name <- randomNames(N, which.names="first")
X <- 5*rbeta(N,0.5,0.5)
Y <- runif(N,-4,4)
Z <- rbinom(N,1,pnorm(Y/2))

df <- data.frame(Name=Name,X=X,Y=Y,Z=Z)
rownames(df)<-df$Name

z_palette<-c("0"="navy","1"="orange")
plot_colors<-z_palette[as.character(df$Z)]

pdf("ClusterSimPlotOne-26.pdf",6,5)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=NA,xlim=c(-1,6),ylim=c(-4,4)))
with(df, text(X,Y,labels=Name,col=plot_colors,
              cex=0.8))
legend("topright",bty="n",pch=c(20,20),col=c("navy","orange"),
       legend=c("Z=0","Z=1"))
dev.off()

pdf("ClusterSimPlotTwo-26.pdf",3,5)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=NA,xlim=c(-1,6),ylim=c(-4,4)))
with(df, text(X,Y,labels=Name,col=plot_colors,
              cex=0.8))
legend("topright",bty="n",pch=c(20,20),col=c("navy","orange"),
       legend=c("Z=0","Z=1"))
dev.off()

pdf("ClusterSimPlotThree-26.pdf",6,3)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=NA,xlim=c(-1,6),ylim=c(-4,4)))
with(df, text(X,Y,labels=Name,col=plot_colors,
              cex=0.8))
legend("topright",bty="n",pch=c(20,20),col=c("navy","orange"),
       legend=c("Z=0","Z=1"))
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Distances:
#
# CENTER AND RESCALE / STANDARDIZE THE DATA:

ds <- scale(df[,2:4])

DL2 <- dist(ds) # L2 / Euclidean distance
DL1 <- dist(ds,method="manhattan") # L1 / Manhattan distance
DM <- sqrt(D2.dist(ds,cov(ds))) # Mahalanobis distances

Distances<-data.frame(DL1=as.numeric(DL1),
                      DL2=as.numeric(DL2),
                      DM=as.numeric(DM))

# Scatterplot matrix of distances:

pdf("ClusterDistanceComparisons-26.pdf",6,5)
scatterplotMatrix(~DL2+DL1+DM,data=Distances,pch=20,
                  regLine = list(method=lm,lty=1,lwd=2,col="red"),
                  smooth=list(smoother=loessLine,spread=TRUE,
                              lty.smooth=1,lwd.smooth=1.5,
                              lty.spread=3, lwd.spread=1,
                              col.smooth="green"),
                  var.labels=c("Euclidean","Manhattan","Mahalanobis"))
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Agglomerative clustering (Euclidean distance)####

ADL2.s <- hclust(DL2,method="single")
ADL2.c <- hclust(DL2,method="complete")
ADL2.a <- hclust(DL2,method="average")

str(ADL2.s)

pdf("ClusterSimDendrogram-26.pdf",4,6)
par(mar=c(4,4,4,2))
plot(ADL2.s,main="Single Linkage",xlab=" ",cex=0.8)
dev.off()

pdf("Cluster-HCLUST-Sims-26.pdf",8,6)
par(mfrow=c(1,3))
par(mar=c(4,4,4,2))
plot(ADL2.s,main="Single",xlab=" ",cex=0.8)
plot(ADL2.c,main="Complete",xlab=" ",cex=0.8)
plot(ADL2.a,main="Average",xlab=" ",cex=0.8)
dev.off()

# Same thing, with Mahalanobis distance...

ADM.s <- hclust(DM,method="single")
ADM.c <- hclust(DM,method="complete")
ADM.a <- hclust(DM,method="average")

pdf("Cluster-HCLUST-Mahal-26.pdf",8,6)
par(mfrow=c(1,3))
par(mar=c(4,4,4,2))
plot(ADM.s,main="Single",xlab=" ",cex=0.8)
plot(ADM.c,main="Complete",xlab=" ",cex=0.8)
plot(ADM.a,main="Average",xlab=" ",cex=0.8)
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Divisive clustering                     ####

Diana.L2 <- diana(ds,metric="euclidean")
Diana.L2

Diana.L1 <- diana(ds,metric="manhattan")

pdf("DivisiveClusteringDendrograms-26.pdf",8,5)
par(mar=c(4,4,4,2))
par(mfrow=c(1,2))
plot(Diana.L2,which.plots=2,main="Euclidean Distance",
     xlab=" ",cex=0.8)
legend("topleft",bty="n",cex=0.8,
       legend=paste("DC = ",round(Diana.L2$dc,3)))
plot(Diana.L1,which.plots=2,main="Manhattan Distance",
     xlab=" ",cex=0.8)
legend("topleft",bty="n",cex=0.8,
       legend=paste("DC = ",round(Diana.L1$dc,3)))
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# k-Means Clustering                      ####

KM2 <- kmeans(ds,2)
KM2

# Plot:

pdf("KMeansPlot2-26.pdf",6,5)
par(mar=c(4,4,4,2))
clusplot(ds,KM2$cluster,color=TRUE,shade=TRUE,
         labels=2,lines=0,main="K = 2",xlab="First PC",
         ylab="Second PC")
dev.off()

KM3 <- kmeans(ds,3)
KM3

pdf("KMeansPlot3-26.pdf",6,5)
par(mar=c(4,4,4,2))
clusplot(ds,KM3$cluster,color=TRUE,shade=TRUE,
         labels=2,lines=0,main="K = 3",xlab="First PC",
         ylab="Second PC")
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# States 2005 data examples...            ####

States <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/master/Data/States2005.csv")

summary(States)

StS <- data.frame(scale(States[,3:10]))
rownames(StS)<-States$statename

# Agglomerative Clustering:

StSL2 <- dist(StS) # L2 / Euclidean distance

StS.agg <- agnes(StS,metric="euclidean",method="average")

pdf("StatesAggDendrogram-26.pdf",8,6)
par(mar=c(4,4,4,2))
plot(StS.agg,which.plots=2,main="Euclidean Distance / Average Linkage",
     xlab=" ",cex=0.8)
legend("topleft",bty="n",legend=paste("AC = ",round(StS.agg$ac,3)))
dev.off()

# Circular dendrogram using -dendextend-:

StS.dend <- hang.dendrogram(as.dendrogram(StS.agg))
StS.dend <- rotate(StS.dend, 1:50)
StS.dend <- color_branches(StS.dend, k=6)

pdf("StatesCircDendrogram-26.pdf",8,8)
par(mar=c(2,2,2,2))
StS.circ <- circlize_dendrogram(StS.dend,
                                labels_track_height=0.4,
                                dend_track_height=0.4)
dev.off()

# # Kmeans: How many clusters?
# 
# wss <- (nrow(StS)-1)*sum(apply(StS,2,var))
# for (i in 1:15) wss[i] <- sum(kmeans(StS,centers=i)$withinss)
# 
# pdf("StatesKMeansScree.pdf",6,5)
# par(mar=c(4,4,2,2))
# plot(1:15, wss, t="o", xlab="Number of Clusters",pch=20,
#      ylab="Within-Groups Sum of Squares",lwd=2)
# dev.off()

# K-means plot, K=5:

StSKM5 <- kmeans(StS,5)

pdf("StateKMeans5-26.pdf",7,6)
par(mar=c(4,4,2,2))
clusplot(StS,StSKM5$cluster,color=TRUE,shade=TRUE,
         labels=3,lines=0,main=" ",xlab="First PC",
         ylab="Second PC",xlim=c(-4,7))
dev.off()

# /fin