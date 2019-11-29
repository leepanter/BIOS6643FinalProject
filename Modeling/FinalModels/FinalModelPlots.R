####	Final Model Plots	 ####
####	Script Name: FinalModelPlots.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will plot graphs of:
#   Model vs Original data
#   Residual vs Fitted Values
#   QQ Plots
# For each of the Final Models decided upon in the model selection scripts.

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

cd19=dat$cd19
mala=dat$mala
subject=dat$subject

logcd19=logdat$logcd19
logmala=logdat$logmala


####	LMwFE  ####
lmod.LMwFE=LMwFEmodelFit(mala, cd19, subject )
(lmod.LMwFEs=summary(lmod.LMwFE))


####	Model vs Original Data -- LMwFE	 ####
intercept=rep(coef(lmod.LMwFE)[1], times=15)
subject.intercept=coef(lmod.LMwFE)[2:15]
subject.intercept=c(0, subject.intercept)
intercept=intercept+subject.intercept

slope=coef(lmod.LMwFE)[16]

aCoefs=intercept
bCoefs=rep(slope, times=15)

plot(mala~cd19)
for(i in 1:15)
{
  abline(a=aCoefs[i], b=bCoefs[i], col=(20+i), lty=2, lwd=2)
}


####	Fitted vs Residuals -- LMwFE	 ####
dat$fitted=fitted(lmod.LMwFE)
dat$resid=resid(lmod.LMwFE)

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p


####	Q-Q plot -- LMwFE	 ####
q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q





####	logLMwFE  ####
loglmod.LMwFE=logLMwFEmodelFit(logmala, logcd19, subject )
(loglmod.LMwFEs=summary(loglmod.LMwFE))


####	Model vs Original Data -- logLMwFE	 ####
intercept=rep(coef(loglmod.LMwFE)[1], times=15)
subject.intercept=coef(loglmod.LMwFE)[2:15]
subject.intercept=c(0, subject.intercept)
intercept=intercept+subject.intercept

slope=0

aCoefs=intercept
bCoefs=rep(slope, times=15)

plot(logmala~logcd19)
for(i in 1:15)
{
  abline(a=aCoefs[i], b=bCoefs[i], col=(20+i), lty=2, lwd=2)
}


####	Fitted vs Residuals -- logLMwFE	 ####
dat$fitted=fitted(loglmod.LMwFE)
dat$resid=resid(loglmod.LMwFE)

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p


####	Q-Q plot -- logLMwFE	 ####
q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q






####	LMMwRE  ####
lmod.LMMwRE=LMMwREmodelFit(mala, cd19, subject )
(lmod.LMMwREs=summary(lmod.LMMwRE))


####	Model vs Original Data -- LMMwRE	 ####
intercept=rep(fixef(lmod.LMMwRE)[1], times=15)
subject.intercept=ranef(lmod.LMMwRE)$subj$'(Intercept)'
intercept=intercept+subject.intercept

slope=fixef(lmod.LMMwRE)[2]

aCoefs=intercept
bCoefs=rep(slope, times=15)

plot(mala~cd19)
for(i in 1:15)
{
  abline(a=aCoefs[i], b=bCoefs[i], col=(20+i), lty=2, lwd=2)
}


####	Fitted vs Residuals -- LMMwRE	 ####
dat$fitted=fitted(lmod.LMMwRE)
dat$resid=resid(lmod.LMMwRE)

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p


####	Q-Q plot -- LMMwRE	 ####
q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q




####	logLMMwRE  ####
loglmod.LMMwRE=LMMwREmodelFit(logmala, logcd19, subject )
(loglmod.LMMwREs=summary(loglmod.LMMwRE))
fixef(loglmod.LMMwRE)
ranef(loglmod.LMMwRE)

####	Model vs Original Data -- logLMMwRE	 ####
intercept=rep(fixef(loglmod.LMMwRE)[1], times=15)
subject.intercept=ranef(loglmod.LMMwRE)$subj$'(Intercept)'
intercept=intercept+subject.intercept

slope=fixef(loglmod.LMMwRE)[2]

aCoefs=intercept
bCoefs=rep(slope, times=15)

plot(logmala~logcd19)
for(i in 1:15)
{
  abline(a=aCoefs[i], b=bCoefs[i], col=(20+i), lty=2, lwd=2)
}


####	Fitted vs Residuals -- logLMMwRE	 ####
dat$fitted=fitted(loglmod.LMMwRE)
dat$resid=resid(loglmod.LMMwRE)

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p


####	Q-Q plot -- logLMMwRE	 ####
q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q






















