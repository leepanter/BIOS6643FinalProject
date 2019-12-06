####	Final Project Script	 ####
####	Script Name: FinalProjectScript.R

#-------------------------------------------------------------------------#
####	Description:	 ####
#  This script contains only relevant code that ended up being used on the write-up of my final project.


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

# Set Working Directory
WD="/Users/lee/Documents/GitHub/BIOS6643FinalProject/BIOS6643FinalProject"
setwd(WD)

# Data Dependencies:
load("/Users/lee/Documents/GitHub/BIOS6643FinalProject/BIOS6643FinalProject/Data/ProjDat.RData")

#Variable Dependencies:

# File Dependencies
source(file = "/Users/lee/Documents/Lee/School/CU Denver/Fall 2019/BIOS 6643/FinalProject/Modeling/ModelFunctions/ModelFunctions.R")


#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#
dat$subject=as.factor(dat$subject)
subject=dat$subject

dat$malaOld=dat$mala
dat$mala=dat$mala-67


logcd19=log(dat$cd19+1, base = exp(1))
logmala=log(dat$mala+1, base = exp(1))


dat=groupedData(mala~cd19|subject, data = dat)

logdat=data.frame(logmala, logcd19, subject)
logdat=groupedData(logmala~logcd19|subject, data = logdat)


################################################################################

####	Define LMwFE	 ####
lmod.LMwFE=lm(logmala~subject+logcd19, data = logdat)
(lmod.LMwFEs=summary(lmod.LMwFE))
(AIC.lmod.LMwFE=AIC(lmod.LMwFE))


####	Model vs Original Data -- LMwFE	 ####
intercept=rep(coef(lmod.LMwFE)[1], times=15)
subject.intercept=coef(lmod.LMwFE)[2:15]
subject.intercept=c(0, subject.intercept)
intercept=intercept+subject.intercept

slope=coef(lmod.LMwFE)[16]

aCoefs=intercept
bCoefs=rep(slope, times=15)

plot(logmala~logcd19)
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

################################################################################

####	Define LMMwRE	 ####
lmod.LMMwRE=lme4::lmer(logmala~logcd19+(1|subject),
                            data=logdat, REML = T)
(lmod.LMMwRE=summary(lmod.LMMwRE))
(AIC.lmod.LMMwRE=AIC(lmod.LMMwRE))

####	Model vs Original Data -- LMMwRE	 ####
intercept=rep(lme4::fixef(lmod.LMMwRE)[1], times=15)
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
