####	POIql Model Selection	 ####
####	Script Name: POIqlModelSelection.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will compare the Generalized linear models created using Quasi-Poisson regression to each other to find the best "Quasi-poisson regresssion model"

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
lmod.POIql.0=glm(mala~1, data = dat, family = quasipoisson(link = "log"))
(lmod.POIql.0s=summary(lmod.POIql.0))


loglmod.POIql.0=glm(logmala~1, data = logdat, family = quasipoisson(link = "log"))
(loglmod.POIql.0s=summary(loglmod.POIql.0))


####	Covariate + Intercept	 ####
lmod.POIql.1=glm(mala~cd19, data = dat, family = quasipoisson(link = "log"))
(lmod.POIql.1s=summary(lmod.POIql.1))

loglmod.POIql.1=glm(logmala~logcd19, data = logdat, family = quasipoisson(link = "log"))
(loglmod.POIql.1s=summary(loglmod.POIql.1))

anova(lmod.POIql.0, lmod.POIql.1, test = "LRT")
# Indicates that there is insufficient evidence for the inclusion of the covariate into the null model (p=0.6478)

anova(loglmod.POIql.0, loglmod.POIql.1, test = "LRT")
# Indicates that there is sufficient evidence for the inclusing of the covariate into the null model (p=0.0001722)


####	Intercept + Subject	 ####
lmod.POIql.2=glm(mala~subject, data = dat, family = quasipoisson(link = "log"))
(lmod.POIql.2s=summary(lmod.POIql.2))

loglmod.POIql.2=glm(logmala~subject, data = logdat, family = quasipoisson(link = "log"))
(loglmod.POIql.2s=summary(loglmod.POIql.2))

anova(lmod.POIql.0, lmod.POIql.2, test = "LRT")
# Indicates that there is very strong evidence for the inclusion of the subject term into the null model (p<2.2e-16)

anova(loglmod.POIql.0, loglmod.POIql.2, test = "LRT")
# Indicates that there is very strong evidence for the inclusion of the subject term into the null model (p<2.2e-16)


####	Intercept, covariate, & Subject	 ####
lmod.POIql.3=glm(mala~subject+cd19, data = dat, family = quasipoisson(link = "log"))
(lmod.POIql.3s=summary(lmod.POIql.3))

loglmod.POIql.3=glm(logmala~subject+logcd19, data = logdat, family = quasipoisson(link = "log"))
(loglmod.POIql.3s=summary(loglmod.POIql.3))


anova(lmod.POIql.1, lmod.POIql.3, test = "LRT")
# Indicates that there is very strong evidence for the inclusion of the subject term into the model: mala ~ cd19 (p<2.2e-16)

anova(lmod.POIql.2, lmod.POIql.3, test = "LRT")
# Indicates that there is sufficient evidence for the inclusion of the covariate term into the model: mala ~ subject (p=0.0578)


anova(loglmod.POIql.1, loglmod.POIql.3, test = "LRT")
# Indicates that there is very strong evidence for the inclusion of the subject term in the model: logmala ~ logcd19 (p<2.2e-16)
anova(loglmod.POIql.2, loglmod.POIql.3, test = "LRT")
# Indicates that there is strong evidence to suggest for the inclusion of the covariate term in the model: logmala ~ subject (pvlaue=0.0004442)

####	FINAL MODELS	 ####
lmod.POIql=glm(mala~subject+cd19, data = dat, family = quasipoisson(link = "log"))
loglmod.POIql=glm(logmala~subject+logcd19, data = logdat, family = quasipoisson(link = "log"))





