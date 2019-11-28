####	LMMeRE Model Selection	 ####
####	Script Name: LMMwREmodelSelection.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will test the heirarchical (and non-heirarchical) structures within the LMMwRE models to find the optimal model for this method.

####	Script Dependencies	 ####

# Package Dependencies:
library(lme4)
library(ggplot2)
library(nlme)
library(MASS)
library(pscl)
library(stargazer)
library(GLMMadaptive)
library(lattice)
library(lmerTest)
library(ggExtra)
library(downloader)
library(RCurl)

# File Dependencies
url.data="https://raw.githubusercontent.com/leepanter/BIOS6643FinalProject/master/Data/ProjDat.csv.gz"
url.models="https://raw.githubusercontent.com/leepanter/BIOS6643FinalProject/master/Data/ModelFunctions.R"

dat.csv.gz="ProjDat.csv.gz"
models.R="ModelFunctions.R"
download.file(url.data, dat.csv.gz)
dat=read.csv(gzfile("ProjDat.csv.gz"))
source_url(url.models, sha_url(url.models))

# source(file = "/Users/lee/Documents/Lee/School/CU Denver/Fall 2019/BIOS 6643/FinalProject/Modeling/ModelFunctions/ModelFunctions.R")

# # Set Working Directory
# WD="/Users/lee/Documents/Lee/School/CU Denver/Fall 2019/BIOS 6643/FinalProject"
# setwd(WD)

# # Data Dependencies:
# dat=read.table(gzfile("/Users/lee/Documents/Lee/School/CU Denver/Fall 2019/BIOS 6643/FinalProject/Data/ProjDat.RData.gz"))



#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#

####	Pre-processing	 ####

# Define Response, covariate, and class variables to be used throughout the script
dat=dat[,-1]
colnames(dat)=c("cd19", "mala", "subject")
dat=dat[-1,]
#dat$cd19=as.integer(levels(dat$cd19))[dat$cd19]
#dat$mala=as.integer(levels(dat$mala))[dat$mala]
dat$subject=as.factor(dat$subject)


dat$malaOld=dat$mala
dat$mala=dat$mala-67


logcd19=log(dat$cd19+1, base = exp(1))
logmala=log(dat$mala+1, base = exp(1))


dat=groupedData(mala~cd19|subject, data = dat)

logdat=data.frame(logmala, logcd19, subject)
logdat=groupedData(logmala~logcd19|subject, data = logdat)


####	FEs = ~1	 ####

#####	REs = (~1 | subject) 	#####
lmod.LMMeRE.0=lme4::lmer(mala~1+(1|subject),
                         data=dat)
(lmod.LMMeRE.0s=summary(lmod.LMMeRE.0))
(AIC.lmod.LMMeRE.0=AIC(lmod.LMMeRE.0))

loglmod.LMMeRE.0=lme4::lmer(logmala~1+(1|subject),
                         data=logdat)
(loglmod.LMMeRE.0s=summary(loglmod.LMMeRE.0))
(AIC.loglmod.LMMeRE.0=AIC(loglmod.LMMeRE.0))

#####	REs = (~1 | subject) + (0 + cd19 | subject)	#####
lmod.LMMeRE.1=lme4::lmer(mala~1+(1|subject)+(0 + cd19 | subject),
                         data=dat)
(lmod.LMMeRE.1s=summary(lmod.LMMeRE.1))
(AIC.lmod.LMMeRE.1=AIC(lmod.LMMeRE.1))

loglmod.LMMeRE.1=lme4::lmer(logmala~1+(1|subject)+(0 + logcd19 | subject),
                            data=logdat)
(loglmod.LMMeRE.1s=summary(loglmod.LMMeRE.1))
(AIC.loglmod.LMMeRE.1=AIC(loglmod.LMMeRE.1))



####	FEs = ~ cd19	 ####

#####	REs = (~1 | subject) 	#####
lmod.LMMeRE.3=lme4::lmer(mala~cd19+(1|subject),
                         data=dat)
(lmod.LMMeRE.3s=summary(lmod.LMMeRE.3))
(AIC.lmod.LMMeRE.3=AIC(lmod.LMMeRE.3))

loglmod.LMMeRE.3=lme4::lmer(logmala~cd19+(1|subject),
                            data=logdat)
(loglmod.LMMeRE.3s=summary(loglmod.LMMeRE.3))
(AIC.loglmod.LMMeRE.3=AIC(loglmod.LMMeRE.3))

#####	REs = (~1 | subject) + (0 + cd19 | subject)	#####
lmod.LMMeRE.4=lme4::lmer(mala~cd19+(1|subject)+(0 + cd19 | subject),
                         data=dat)
(lmod.LMMeRE.4s=summary(lmod.LMMeRE.4))
(AIC.lmod.LMMeRE.4=AIC(lmod.LMMeRE.4))

loglmod.LMMeRE.4=lme4::lmer(logmala~cd19+(1|subject)+(0 + logcd19 | subject),
                            data=logdat)
(loglmod.LMMeRE.4s=summary(loglmod.LMMeRE.4))
(AIC.loglmod.LMMeRE.4=AIC(loglmod.LMMeRE.4))


####	AIC Comparisons	 ####

# Regular Models
AIC.lmod.LMMeRE.0
AIC.lmod.LMMeRE.1
# AIC.lmod.LMMeRE.2--Skipped in error
AIC.lmod.LMMeRE.3 # lowest
AIC.lmod.LMMeRE.4

# Transformed Models
AIC.loglmod.LMMeRE.0
AIC.loglmod.LMMeRE.1 # Lowest
AIC.loglmod.LMMeRE.3
AIC.loglmod.LMMeRE.4

####	FINAL MODELS	 ####
lmod.LMMwRE=lmod.LMMeRE.3=lme4::lmer(mala~cd19+(1|subject),data=dat)
loglmod.LMMwRE=loglmod.LMMeRE.1=lme4::lmer(logmala~1+
                                             (1|subject)+
                                             (0 + logcd19 | subject),
                                           data=logdat)










