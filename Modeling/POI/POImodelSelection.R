####	POI Model Selection	 ####
####	Script Name: POImodelSelection.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will compare the Generalized linear models created using Poisson regression to each other to find the best "poisson regresssion model"

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
#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#

set.seed(123)

####	Pre-processing	 ####
dat=dat[,-1]
colnames(dat)=c("cd19", "mala", "subject")
dat$subject=as.factor(dat$subject)
subject=dat$subject
dat$malaOld=dat$mala
dat$mala=dat$mala-67
logcd19=log(dat$cd19+1, base = exp(1))
logmala=log(dat$mala+1, base = exp(1))
dat=groupedData(mala~cd19|subject, data = dat)
logdat=data.frame(logmala, logcd19, subject)
logdat=groupedData(logmala~logcd19|subject, data = logdat)


####	Intercept Only	 ####
lmod.POI.0=glm(mala~1, data = dat, family = poisson(link = "log"))
(lmod.POI.0s=summary(lmod.POI.0))

loglmod.POI.0=glm(logmala~1, data = logdat, family = poisson(link = "log"))
(loglmod.POI.0s=summary(loglmod.POI.0))

####	Covariate + Intercept	 ####
lmod.POI.1=glm(mala~cd19, data = dat, family = poisson(link = "log"))
(lmod.POI.1s=summary(lmod.POI.1))

loglmod.POI.1=glm(logmala~logcd19, data = logdat, family = poisson(link = "log"))
(loglmod.POI.1s=summary(loglmod.POI.1))
anova(lmod.POI.0, lmod.POI.1, test = "LRT")
# Indicates that there is very strong evidence for the inclusion of the covariate (p<2.2e-16)
anova(loglmod.POI.0, loglmod.POI.1, test = "LRT")
# Indicates that there is Insufficient evidence for the inclusion of the covariate (p=0.1405)



####	Intercept + Subject	 ####
lmod.POI.2=glm(mala~subject, data = dat, family = poisson(link = "log"))
(lmod.POI.2s=summary(lmod.POI.2))

loglmod.POI.2=glm(logmala~subject, data = logdat, family = poisson(link = "log"))
(loglmod.POI.2s=summary(loglmod.POI.2))
anova(lmod.POI.0, lmod.POI.2, test = "LRT")
# Indicates that there is very strong evidence for the inclusion of the subject term (p<2.2e-16)
anova(loglmod.POI.0, loglmod.POI.2, test = "LRT")
# Indicates that there is sufficient evidence for the inclusion of the subject term (p=0.001533)


####	Intercept, covariate, & Subject	 ####
lmod.POI.3=glm(mala~subject+cd19, data = dat, family = poisson(link = "log"))
(lmod.POI.3s=summary(lmod.POI.3))

loglmod.POI.3=glm(logmala~subject+logcd19, data = logdat, family = poisson(link = "log"))
(loglmod.POI.3s=summary(loglmod.POI.3))


anova(lmod.POI.1, lmod.POI.3, test = "LRT")
# Indicates that there is very strong evidence for the inclusion of the subject term (p<2.2e-16)
anova(lmod.POI.2, lmod.POI.3, test = "LRT")
# Indicates that there is very strong evidence for the inclusion of the covariate term (p<2.2e-16)


anova(loglmod.POI.1, loglmod.POI.3, test = "LRT")
# Indicates that there is suffient evidence for the inclusion of the subject term (p=0.001832)
anova(loglmod.POI.2, loglmod.POI.3, test = "LRT")
# Indicates that there is insufficient evidence for the inclusion of the covariate term (p=0.1993)


####	FINAL MODELS	 ####
lmod.POI=glm(mala~subject+cd19, data = dat, family = poisson(link = "log"))
loglmod.POI=glm(logmala~subject, data = logdat, family = poisson(link = "log"))





