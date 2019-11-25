####	Example Analysis ####
####	Script Name: ExampleAnalysis.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script with produce an example analysis of all models on the Example Data generated in the ExampleGraph.R script


####	Script Dependencies	 ####

# Package Dependencies:
library(lme4)
library(ggplot2)
library(nlme)
library(MASS)
library(pscl)
library(stargazer)

# File Dependencies
source(file = "/Users/lee/Documents/Lee/School/CU Denver/Fall 2019/BIOS 6643/FinalProject/Modeling/ModelFunctions/ModelFunctions.R")


# Set Working Directory
WD="/Users/lee/Documents/Lee/School/CU Denver/Fall 2019/BIOS 6643/FinalProject"
setwd(WD)

# Data Dependencies:
load(file="/Users/lee/Documents/Lee/School/CU Denver/Fall 2019/BIOS 6643/FinalProject/ExampleData.Rdata")

# Variable Dependencies:

#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#

####	Pre-processing	 ####

# cd19 defined as covariate variable
cd19=df$X
logcd19=log(cd19+1, base = exp(1))

# mala defined as response variable
mala=df$Y
logmala=log(mala+1, base = exp(1))

# subject defined as group variable
subject=df$group

dat=data.frame(mala, cd19, subject)
dat=groupedData(mala~cd19|subject, data = dat)

logdat=data.frame(logmala, logcd19, subject)
logdat=groupedData(logmala~logcd19|subject, data = logdat)


####	LMwFE  ####
lmod.LMwFE=LMwFEmodelFit(mala, cd19, subject)
summary(lmod.LMwFE[[1]])
summary(lmod.LMwFE[[2]])
AIC(lmod.LMwFE[[1]])
AIC(lmod.LMwFE[[2]])


plot(jitter(mala,2)~jitter(cd19,2), data=dat,
     xlab="X",
     ylab="Y",
     main="Model v Original Data")
abline(lmod.LMwFE[[1]], col=(20), lty=2, lwd=2)


dat$fitted=fitted(lmod.LMwFE[[1]])
dat$resid=residuals(lmod.LMwFE[[1]])

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_jitter(width = 0.1, height = 0.25)+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p

q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q


plot(jitter(logmala, 3)~jitter(logcd19, 3), data=logdat,
     xlab="log(X)",
     ylab="log(Y)",
     main="Model v Original Data")
abline(lmod.LMwFE[[2]], b=slope[2], col=(20), lty=2, lwd=2)


logdat$fitted=fitted(lmod.LMwFE[[2]])
logdat$resid=residuals(lmod.LMwFE[[2]])

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_jitter(width = 0.1, height = 0.25)+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p

q=ggplot(logdat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q


####	LMMwRE  ####
lmod.LMMwRE=LMMwREmodelFit(mala, cd19, subject)
summary(lmod.LMMwRE[[1]])
summary(lmod.LMMwRE[[2]])
AIC(lmod.LMwFE[[1]])
AIC(lmod.LMwFE[[2]])

fixed.intercept=list()
ran.subject.intercept=list()
fixed.slope=list()
ran.subject.slope=list()

for(i in 1:2){
  fixed.intercept[[i]]=rep(lme4::fixef(lmod.LMMwRE[[i]])[1], times=15)
  ran.subject.intercept[[i]]=lme4::ranef(lmod.LMMwRE[[i]])$subj$'(Intercept)'
  fixed.intercept[[i]]=fixed.intercept[[i]]+ran.subject.intercept[[i]]
}

fixed.slope[[1]]=rep(lme4::fixef(lmod.LMMwRE[[1]])[2], times=15)
ran.subject.slope[[1]]=lme4::ranef(lmod.LMMwRE[[1]])$subj$cov
fixed.slope[[1]]=fixed.slope[[1]]+ran.subject.slope[[1]]

fixed.slope[[2]]=rep(lme4::fixef(lmod.LMMwRE[[2]])[2], times=15)
ran.subject.slope[[2]]=lme4::ranef(lmod.LMMwRE[[2]])$subj$logcov
fixed.slope[[2]]=fixed.slope[[2]]+ran.subject.slope[[2]]


plot(jitter(mala,2)~jitter(cd19,2), data=dat,
     xlab="X",
     ylab="Y",
     main="Model v Original Data")
for(j in 1:15)
{
  abline(a=fixed.intercept[[1]][j], b=fixed.slope[[1]][j], col=(20+j), lty=2, lwd=2)
}

dat$fitted=fitted(lmod.LMMwRE[[1]])
dat$resid=residuals(lmod.LMMwRE[[1]])

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_jitter(width = 0.1, height = 0.25)+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p

q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q

plot(jitter(logmala, 3)~jitter(logcd19, 3), data=logdat,
     xlab="log(X)",
     ylab="log(Y)",
     main="Model v Original Data")
for(j in 1:15)
{
  abline(a=fixed.intercept[[2]][j], b=fixed.slope[[2]][j], col=(20+j), lty=2, lwd=2)
}

logdat$fitted=fitted(lmod.LMMwRE[[2]])
logdat$resid=residuals(lmod.LMMwRE[[2]])

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_jitter(width = 0.1, height = 0.25)+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p

q=ggplot(logdat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q


####	POI	 ####
lmod.POI=POImodelFit(mala, cd19, subject)
summary(lmod.POI[[1]])
summary(lmod.POI[[2]])
AIC(lmod.POI[[1]])
AIC(lmod.POI[[2]])

range(cd19)
xrange=seq(0,5, by=0.01)
ypredict=predict(lmod.POI[[1]], list(cd19=xrange), type = "response")

plot(jitter(mala,2)~jitter(cd19,2), data=dat,
     xlab="X",
     ylab="Y",
     main="Model v Original Data")
lines(cd19, ypredict, col=(20), lty=2, lwd=2)


dat$fitted=fitted(lmod.POI[[1]])
dat$resid=residuals(lmod.POI[[1]])

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_jitter(width = 0.1, height = 0.25)+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p

q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q



logypredict=predict(lmod.POI[[2]], list(cd19=xrange), type = "response")

plot(jitter(logmala, 3)~jitter(logcd19, 3), data=logdat,
     xlab="log(X)",
     ylab="log(Y)",
     main="Model v Original Data")
lines(logcd19, logypredict, col=(20), lty=2, lwd=2)


logdat$fitted=fitted(lmod.POI[[2]])
logdat$resid=residuals(lmod.POI[[2]])

p=ggplot(logdat, aes(x=fitted, y=resid, color=subject))+
  geom_jitter(width = 0.1, height = 0.25)+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p

q=ggplot(logdat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q


####	POIql	 ####
lmod.POIql=POIqlmodelFit(mala, cd19, subject)
summary(lmod.POIql[[1]])
summary(lmod.POIql[[2]])


xrange=seq(0,5, by=0.1)
ypredict=predict(lmod.POIql[[1]], list(cd19=xrange), type = "response")

plot(jitter(mala,2)~jitter(cd19,2), data=dat,
     xlab="X",
     ylab="Y",
     main="Model v Original Data")
lines(cd19, ypredict, col=(20), lty=2, lwd=2)

dat$fitted=fitted(lmod.POIql[[1]])
dat$resid=residuals(lmod.POIql[[1]])

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_jitter(width = 0.1, height = 0.25)+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p

q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q


logypredict=predict(lmod.POIql[[2]], list(cd19=xrange), type = "response")

plot(jitter(logmala, 3)~jitter(logcd19, 3), data=logdat,
     xlab="log(X)",
     ylab="log(Y)",
     main="Model v Original Data")
lines(logcd19, logypredict, col=(20), lty=2, lwd=2)


logdat$fitted=fitted(lmod.POIql[[2]])
logdat$resid=residuals(lmod.POIql[[2]])

p=ggplot(logdat, aes(x=fitted, y=resid, color=subject))+
  geom_jitter(width = 0.1, height = 0.25)+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p

q=ggplot(logdat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q



####	POIqlLMM	 ####
#lmod.POIqlLMM=POIqlLMMmodelFit(mala, cd19, subject)

lmod.POIqlLMM=glmmPQL(mala~cd19,
                 random=list(~1|subject),
                 family = poisson,
                 data=df)

summary(lmod.POIqlLMM)



xrange=seq(0,5, by=0.1)
ypredict=predict(lmod.POIqlLMM, list(cd19=xrange), type = "response")

plot(mala~cd19, data=dat, main="Model v Original Data")
lines(cd19, ypredict, col=(20), lty=2, lwd=2)

dat$fitted=fitted(lmod.POIql[[1]])
dat$resid=residuals(lmod.POIql[[1]])

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p

q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q


logypredict=predict(lmod.POIql[[2]], list(cd19=xrange), type = "response")

plot(logmala~logcd19, data=logdat, main="Model v Original Data")
lines(logcd19, logypredict, col=(20), lty=2, lwd=2)


logdat$fitted=fitted(lmod.POIql[[2]])
logdat$resid=residuals(lmod.POIql[[2]])

p=ggplot(logdat, aes(x=fitted, y=resid, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p

q=ggplot(logdat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q


####	ZIP	 ####
lmod.ZIP=ZIPmodelFit(mala, cd19, subject)
summary(lmod.ZIP)
AIC(lmod.ZIP)

xrange=seq(0,1700, by=0.1)
ypredict=predict(lmod.ZIP, list(cd19=xrange), type = "response")

plot(jitter(mala,2)~jitter(cd19,2), data=dat,
     xlab="X",
     ylab="Y",
     main="Model v Original Data")
lines(cd19, ypredict, col=(20), lty=2, lwd=2)

dat$fitted=fitted(lmod.ZIP)
dat$resid=residuals(lmod.ZIP)

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_jitter(width = 0.1, height = 0.25)+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p

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
