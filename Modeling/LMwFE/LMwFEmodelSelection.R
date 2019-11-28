####	LMeFE Model Selection	 ####
####	Script Name: LMwFEmodelSelection.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will test the heirarchical structures within the LMwFE models to find the optimal model for this method.

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
#
#
# # Set Working Directory
# WD="/Users/lee/Documents/Lee/School/CU Denver/Fall 2019/BIOS 6643/FinalProject"
# setwd(WD)
#
# # Data Dependencies:
# dat=read.table(gzfile("/Users/lee/Documents/Lee/School/CU Denver/Fall 2019/BIOS 6643/FinalProject/Data/ProjDat.RData.gz"))



#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#

####	Pre-processing	 ####
dat=dat[,-1]
colnames(dat)=c("cd19", "mala", "subject")
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



####	Intercept Only	 ####
lmod.LMwFE.0=lm(mala~1, data=dat)
(lmod.LMwFE.0s=summary(lmod.LMwFE.0))
(AIC.lmod.LMwFE.0=AIC(lmod.LMwFE.0))

loglmod.LMwFE.0=lm(logmala~1, data=logdat)
(loglmod.LMwFE.0s=summary(loglmod.LMwFE.0))
(AIC.loglmod.LMwFE.0=AIC(loglmod.LMwFE.0))



####	Covariate + Intercept only	 ####
lmod.LMwFE.1=lm(mala~cd19, data = dat)
(lmod.LMwFE.1s=summary(lmod.LMwFE.1))
(AIC.lmod.LMwFE.1=AIC(lmod.LMwFE.1))
anova(lmod.LMwFE.0,lmod.LMwFE.1)
#### Addition of covariate alone to lmod.LMwFE.0 not advised (p=0.6439)

loglmod.LMwFE.1=lm(logmala~logcd19, data = logdat)
(loglmod.LMwFE.1s=summary(loglmod.LMwFE.1))
(AIC.loglmod.LMwFE.1=AIC(loglmod.LMwFE.1))
anova(loglmod.LMwFE.0,loglmod.LMwFE.1)
#### Addition of logcov to loglmod.LMwFE.0 advised (p=0.0001685)


####	Intercept + Subject Factor	 ####
lmod.LMwFE.2=lm(mala~subject, data = dat)
(lmod.LMwFE.2s=summary(lmod.LMwFE.2))
(AIC.lmod.LMwFE.2=AIC(lmod.LMwFE.2))
anova(lmod.LMwFE.0,lmod.LMwFE.2)
#### Addition of subject to lmod.LMwFE.0 recommended (p<2.2e-16)

loglmod.LMwFE.2=lm(logmala~subject, data = logdat)
(loglmod.LMwFE.2s=summary(loglmod.LMwFE.2))
(AIC.loglmod.LMwFE.2=AIC(loglmod.LMwFE.2))
anova(loglmod.LMwFE.0,loglmod.LMwFE.2)
#### Addition of subject to loglmod.LMwFE.0 recommended (p<2.2e-16)



####	Covariate + Subject	 ####
lmod.LMwFE.3=lm(mala~subject+cd19, data = dat)
(lmod.LMwFE.3s=summary(lmod.LMwFE.3))
(AIC.lmod.LMwFE.3=AIC(lmod.LMwFE.3))
anova(lmod.LMwFE.0,lmod.LMwFE.3)
anova(lmod.LMwFE.2,lmod.LMwFE.3)
#### Addition of subject and cov to lmod.LMwFE.0 NOT recommended (p=0.08813)

loglmod.LMwFE.3=lm(logmala~subject+cd19, data = logdat)
(loglmod.LMwFE.3s=summary(loglmod.LMwFE.3))
(AIC.loglmod.LMwFE.3=AIC(loglmod.LMwFE.3))
anova(loglmod.LMwFE.0,loglmod.LMwFE.3)
anova(loglmod.LMwFE.2,loglmod.LMwFE.3)
#### Addition of cd19 to loglmod.LMwFE.2 NOT recommended (p0.3728)


####	AIC Comparisions	 ####

#### Regular Models
AIC.lmod.LMwFE.0
AIC.lmod.LMwFE.1
AIC.lmod.LMwFE.2 # very close
AIC.lmod.LMwFE.3 # lowest

#### Transformed Models
AIC.loglmod.LMwFE.0
AIC.loglmod.LMwFE.1
AIC.loglmod.LMwFE.2 # Lowest
AIC.loglmod.LMwFE.3 # very close


####	FINAL MODELS 	 ####

lmod.LMwFE    = lm(mala~subject+cd19, data = dat)
loglmod.LMwFE = lm(logmala~subject, data = logdat)


#-------------------------------------------------------------------------#
####	End Script	 ####
#-------------------------------------------------------------------------#





#-------------------------------------------------------------------------#
#####	Post-Script	#####

####  Notes:

####  Compilation Errors:

####  Execution Errors:

####  Next Scripts to Consider:

#-------------------------------------------------------------------------#
