####	Updated Initial Models	 ####
####	Script Name: UpdatedInitialModels.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will use the functions produced in ModelFunctions.R to produce basic analyses of know variables.


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


#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#

####	Pre-processing	 ####

# Define Response, covariate, and class variables to be used throughout the script
seqFilter=data.frame(t(seqFilter))

cd19=seqFilter[,which(colnames(seqFilter)=="CD19")]
logcd19=log(cd19+1, base = exp(1))

mala=seqFilter[,which(colnames(seqFilter)=="MALAT1")]
malaOld=mala
mala=mala-67
logmala=log(mala+1, base = exp(1))
subject=mdataFilter$subject.no

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


plot(mala~cd19, data=dat, main="Model v Original Data")
abline(lmod.LMwFE[[1]], col=(20+j), lty=2, lwd=2)


dat$fitted=fitted(lmod.LMwFE[[1]])
dat$resid=residuals(lmod.LMwFE[[1]])

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


plot(logmala~logcd19, data=logdat, main="Model v Original Data")
abline(lmod.LMwFE[[2]], b=slope[2], col=(20+j), lty=2, lwd=2)


logdat$fitted=fitted(lmod.LMwFE[[2]])
logdat$resid=residuals(lmod.LMwFE[[2]])

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
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


plot(mala~cd19, data=dat, main="Model v Original Data")
for(j in 1:15)
{
  abline(a=fixed.intercept[[1]][j], b=fixed.slope[[1]][j], col=(20+j), lty=2, lwd=2)
}

dat$fitted=fitted(lmod.LMMwRE[[1]])
dat$resid=residuals(lmod.LMMwRE[[1]])

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


plot(logmala~logcd19, data=logdat, main="Model v Original Data")
for(j in 1:15)
{
  abline(a=fixed.intercept[[2]][j], b=fixed.slope[[2]][j], col=(20+j), lty=2, lwd=2)
}

logdat$fitted=fitted(lmod.LMMwRE[[2]])
logdat$resid=residuals(lmod.LMMwRE[[2]])

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
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





####	POI	 ####
lmod.POI=POImodelFit(mala, cd19, subject)
summary(lmod.POI[[1]])
summary(lmod.POI[[2]])
AIC(lmod.POI[[1]])
AIC(lmod.POI[[2]])

# intercept=list()
# subject.intercept=list()
# slope=c()
#
# for(i in 1:2){
#   intercept[[i]]=rep(coefficients(lmod.POI[[i]])[1], times=15)
#   subject.intercept[[i]]=coefficients(lmod.POI[[i]])[2:15]
#   subject.intercept[[i]]=c(0,subject.intercept[[i]])
#   intercept[[i]]=intercept[[i]]+subject.intercept[[i]]
#
#   slope[i]=coefficients(lmod.POI[[i]])[16]
# }

range(cd19)
xrange=seq(0,1700, by=0.1)
ypredict=predict(lmod.POI[[1]], list(cd19=xrange), type = "response")

plot(mala~cd19, data=dat, main="Model v Original Data")
lines(cd19, ypredict, col=(20), lty=2, lwd=2)


dat$fitted=fitted(lmod.POI[[1]])
dat$resid=residuals(lmod.POI[[1]])

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



logypredict=predict(lmod.POI[[2]], list(cd19=xrange), type = "response")

plot(logmala~logcd19, data=logdat, main="Model v Original Data")
lines(logcd19, logypredict, col=(20), lty=2, lwd=2)


# plot(logmala~logcd19, data=logdat, main="Model v Original Data")
# for(j in 1:15)
# {
#   abline(a=intercept[[2]][j], b=slope[2], col=(20+j), lty=2, lwd=2)
# }

logdat$fitted=fitted(lmod.POI[[2]])
logdat$resid=residuals(lmod.POI[[2]])

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



####	POIql	 ####
lmod.POIql=POIqlmodelFit(mala, cd19, subject)
summary(lmod.POIql[[1]])
summary(lmod.POIql[[2]])


xrange=seq(0,1700, by=0.1)
ypredict=predict(lmod.POIql[[1]], list(cd19=xrange), type = "response")

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




####	POIqlLMM	 ####
lmod.POIqlLMM=POIqlLMMmodelFit(mala, cd19, subject)
summary(lmod.POIqlLMM[[1]])
summary(lmod.POIqlLMM[[2]])


xrange=seq(0,1700, by=0.1)
ypredict=predict(lmod.POIqlLMM[[1]], list(cd19=xrange), type = "response")

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

plot(mala~cd19, data=dat, main="Model v Original Data")
lines(cd19, ypredict, col=(20), lty=2, lwd=2)

dat$fitted=fitted(lmod.ZIP)
dat$resid=residuals(lmod.ZIP)

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



####	Intial Models	 ####
initialModels=list()
initialModels[[1]]=lmod.LMwFE
initialModels[[2]]=lmod.LMMwRE
initialModels[[3]]=lmod.POI
initialModels[[4]]=lmod.POIql
initialModels[[5]]=lmod.POIqlLMM
initialModels[[6]]=lmod.ZIP

# Initial Model Summaries

summary(initialModels[[1]][[1]])
summary(initialModels[[2]][[1]])
summary(initialModels[[3]][[1]])
summary(initialModels[[4]][[1]])
summary(initialModels[[5]][[1]])
summary(initialModels[[6]][[1]])


stargazer(initialModels[[1]][[1]],title="LMwFE Results", align=TRUE, no.space = TRUE)
stargazer(initialModels[[2]][[1]],title="LMMwRE Results", align=TRUE, no.space = TRUE)
stargazer(initialModels[[3]][[1]],title="POI Results", align=TRUE, no.space = TRUE)
stargazer(initialModels[[4]][[1]],title="POIql Results", align=TRUE, no.space = TRUE)
stargazer(initialModels[[5]][[1]],title="POIqlLMM Results", align=TRUE, no.space = TRUE)
stargazer(initialModels[[6]][[1]],title="ZIP Results", align=TRUE, no.space = TRUE)



stargazer(initialModels[[1]][[2]],title="logLMwFE Results", align=TRUE, no.space = TRUE)
stargazer(initialModels[[2]][[2]],title="logLMMwRE Results", align=TRUE, no.space = TRUE)
stargazer(initialModels[[3]][[2]],title="logPOI Results", align=TRUE, no.space = TRUE)
stargazer(initialModels[[4]][[2]],title="logPOIql Results", align=TRUE, no.space = TRUE)
stargazer(initialModels[[5]][[2]],title="logPOIqlLMM Results", align=TRUE, no.space = TRUE)


Summary Statistics
AIC(initialModels[[1]][[1]])
AIC(initialModels[[2]][[1]])
AIC(initialModels[[3]][[1]])

AIC(initialModels[[1]][[2]])
AIC(initialModels[[2]][[2]])
AIC(initialModels[[3]][[2]])

AIC(initialModels[[1]][[1]],
    initialModels[[2]][[1]],
    initialModels[[3]][[1]])

AIC(initialModels[[1]][[2]],
    initialModels[[2]][[2]],
    initialModels[[3]][[2]])

BIC(initialModels[[1]][[1]])
BIC(initialModels[[2]][[1]])
BIC(initialModels[[3]][[1]])

BIC(initialModels[[1]][[2]])
BIC(initialModels[[2]][[2]])
BIC(initialModels[[3]][[2]])

BIC(initialModels[[1]][[1]],
    initialModels[[2]][[1]],
    initialModels[[3]][[1]])

BIC(initialModels[[1]][[2]],
    initialModels[[2]][[2]],
    initialModels[[3]][[2]])


df.temp=data.frame(mala, cd19, subject)
pp=ggplot(data=df.temp, aes(x=cd19, y=mala, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Malat1 vs CD19")+
  xlab("CD19")+
  ylab("Malat1")
pp


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
