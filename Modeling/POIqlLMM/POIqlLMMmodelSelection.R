####	POIqlLMM Model Selection	 ####
####	Script Name: POIqlLMMmodelSelection.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will compare the Generalized linear Mixed models created using Quasi-Poisson regression to each other to find the best "Quasi-poisson Mixed regresssion model"

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


####	FEs = ~ 1	 ####

#####	REs = ~ (1|subj)	#####
lmod.POIqlLMM.0=glmmPQL(mala~1,
                        random = list(~1|subject),
                        family = poisson,
                        data=dat)
(lmod.POIqlLMM.0s=summary(lmod.POIqlLMM.0))

loglmod.POIqlLMM.0=glmmPQL(logmala~1,
                        random = list(~1|subject),
                        family = poisson,
                        data=logdat)
(loglmod.POIqlLMM.0s=summary(loglmod.POIqlLMM.0))

#####	REs = ~ (1|subj) + (0 + cd19 | subject)	#####
lmod.POIqlLMM.1=glmmPQL(mala~1,
                        random = list(~1|subject, ~0+cd19|subject),
                        family = poisson,
                        data=dat)
(lmod.POIqlLMM.1s=summary(lmod.POIqlLMM.1))

loglmod.POIqlLMM.1=glmmPQL(logmala~1,
                        random = list(~1|subject, ~0+logcd19|subject),
                        family = poisson,
                        data=logdat)
(loglmod.POIqlLMM.1s=summary(loglmod.POIqlLMM.1))


####	FEs = ~ 1	+ cd19 ####

#####	REs = ~ (1|subj)	#####
lmod.POIqlLMM.2=glmmPQL(mala~cd19,
                        random = list(~1|subject),
                        family = poisson,
                        data=dat)
(lmod.POIqlLMM.2s=summary(lmod.POIqlLMM.2))

loglmod.POIqlLMM.2=glmmPQL(logmala~logcd19,
                           random = list(~1|subject),
                           family = poisson,
                           data=logdat)
(loglmod.POIqlLMM.2s=summary(loglmod.POIqlLMM.2))

#####	REs = ~ (1|subj) + (0 + cd19 | subject)	#####
lmod.POIqlLMM.3=glmmPQL(mala~cd19,
                        random = list(~1|subject, ~0+cd19|subject),
                        family = poisson,
                        data=dat)
(lmod.POIqlLMM.3s=summary(lmod.POIqlLMM.3))

loglmod.POIqlLMM.3=glmmPQL(logmala~logcd19,
                           random = list(~1|subject, ~0+logcd19|subject),
                           family = poisson,
                           data=logdat)
(loglmod.POIqlLMM.3s=summary(loglmod.POIqlLMM.3))


xrange=cd19
yobs=dat$mala
ypred.lmod.POIqlLMM.0=predict(lmod.POIqlLMM.)

df.lmod.POIqlLMM.0=data.frame(xrange,yobs, ypred.lmod.POIqlLMM.0)

g=ggplot(df.lmod.POIqlLMM.0, aes(x=xrange))+
  geom_point(aes(y=yobs), color="red")+
  geom_line(aes(y=ypred.lmod.POIqlLMM.0))
g
