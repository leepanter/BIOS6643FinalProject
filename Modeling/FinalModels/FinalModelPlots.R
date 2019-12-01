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
loglmod.LMwFE=logLMwFEmodelFit(mala, cd19, subject )
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
loglmod.LMMwRE=LMMwREmodelFit(logmala, logcd19, subject)
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






####	POI	 ####
lmod.POI=POImodelFit(mala, cd19, subject)
(lmod.POIs=summary(lmod.POI))


####	Model vs Original Data -- logLMMwRE	 ####
range(cd19)
xrange=seq(0,1624, length.out = 1110)
ypredict=predict(lmod.POI, list(cd19=xrange), type = "response")

plot(mala~cd19, data=dat, main="Model v Original Data")
lines(cd19, ypredict, col=(20), lty=2, lwd=2)


dat$fitted=fitted(lmod.POI)
dat$resid=residuals(lmod.POI)


####	Fitted vs Residuals -- POI	 ####
p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p


####	Q-Q plot -- POI	 ####
q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q




####	logPOI	 ####
loglmod.POI=logPOImodelFit(mala, cd19, subject)
(loglmod.POIs=summary(loglmod.POI))


####	Model vs Original Data -- logPOI	 ####
range(logcd19)
xrange=seq(0,7.393263, length.out = 1110)
ypredict=predict(loglmod.POI, list(logcd19=xrange), type = "response")

plot(logmala~logcd19, data=logdat, main="Model v Original Data")
lines(logcd19, ypredict, col=(20), lty=2, lwd=2)


####	Fitted vs Residuals -- logPOI	 ####
dat$fitted=fitted(loglmod.POI)
dat$resid=residuals(loglmod.POI)

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p


####	Q-Q plot -- logPOI	 ####
q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q





####	POIql	 ####
lmod.POIql=POIqlmodelFit(mala, cd19, subject)
(lmod.POIqls=summary(lmod.POIql))


####	Model vs Original Data -- logLMMwRE	 ####
range(cd19)
xrange=seq(0,1624, length.out = 1110)
ypredict=predict(lmod.POIql, list(cd19=xrange), type = "response")

plot(mala~cd19, data=dat, main="Model v Original Data")
lines(cd19, ypredict, col=(20), lty=2, lwd=2)

####	Fitted vs Residuals -- POI	 ####
dat$fitted=fitted(lmod.POIql)
dat$resid=residuals(lmod.POIql)

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p


####	Q-Q plot -- POI	 ####
q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q




####	logPOIql	 ####
loglmod.POIql=logPOIqlmodelFit(mala, cd19, subject)
(loglmod.POIqls=summary(loglmod.POIql))


####	Model vs Original Data -- logPOIql	 ####
range(logcd19)
xrange=seq(0,7.393263, length.out = 1110)
ypredict=predict(loglmod.POIql, list(logcd19=xrange), type = "response")

plot(logmala~logcd19, data=logdat, main="Model v Original Data")
lines(logcd19, ypredict, col=(20), lty=2, lwd=2)

####	Fitted vs Residuals -- logPOI	 ####
dat$fitted=fitted(loglmod.POIql)
dat$resid=residuals(loglmod.POIql)

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p


####	Q-Q plot -- logPOI	 ####
q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q






####	POIqlLMM	 ####
lmod.POIqlLMM=POIqlLMMmodelFit(mala, cd19, subject)
(lmod.POIqlLMMs=summary(lmod.POIqlLMM))


####	Model vs Original Data -- POIqlLMM	 ####
pred=list()
for(i in 1:2)
{
  pred[[i]]=predict(lmod.POIqlLMM, level = i)
}

plot(mala~cd19, data=dat, main="Model v Original Data")
for(i in 1:2)
{
  lines(cd19, pred[[i]], col=(20), lty=2, lwd=2)
}

####	Fitted vs Residuals -- POIqlLMM	 ####
dat$fitted=fitted(lmod.POIqlLMM)
dat$resid=residuals(lmod.POIqlLMM)

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p


####	Q-Q plot -- POIqlLMM	 ####
q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q



####	logPOIqlLMM	 ####
loglmod.POIqlLMM=logPOIqlLMMmodelFit(mala, cd19, subject)
(loglmod.POIqlLMMs=summary(loglmod.POIqlLMM))


####	Model vs Original Data -- logPOIqlLMM	 ####
pred=list()
for(i in 1:2)
{
  pred[[i]]=predict(loglmod.POIqlLMM, level = i)
}

plot(logmala~logcd19, data=logdat, main="Model v Original Data")
for(i in 1:2)
{
  lines(logcd19, pred[[i]], col=(20), lty=2, lwd=2)
}

####	Fitted vs Residuals -- logPOIqlLMM	 ####
dat$fitted=fitted(loglmod.POIqlLMM)
dat$resid=residuals(loglmod.POIqlLMM)

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p


####	Q-Q plot -- logPOIqlLMM	 ####
q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q




####	ZIP	 ####
lmod.ZIP=mixed_model(mala~1,
                           random = ~1|subject,
                           data=dat,
                           family = zi.poisson(),
                           zi_fixed = ~ 1)
lmod.ZIP=update(lmod.ZIP, random= ~cd19|subject)
(lmod.ZIPs=summary(lmod.ZIP))


####	Model vs Original Data -- ZIP	 ####
nDF=dat

plot_data=effectPlotData(lmod.ZIP, nDF)

plot(plot_data$mala ~ plot_data$cd19)
lines(plot_data$cd19, plot_data$fitted)


####	Fitted vs Residuals -- ZIP	 ####
dat$fitted=plot_data$fitted
dat$resid=plot_data$resid

p=ggplot(dat, aes(x=fitted, y=resid, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p


####	Q-Q plot -- ZIP	 ####
q=ggplot(dat, aes(sample = resid))+
  stat_qq()+
  stat_qq_line()
q





####	logZIP	 ####
loglmod.ZIP=mixed_model(logmala~logcd19,
                     random = ~1|subject,
                     data=logdat,
                     family = zi.poisson(),
                     zi_fixed = ~ 1)
(loglmod.ZIPs=summary(loglmod.ZIP))


####	Model vs Original Data -- ZIP	 ####
nDF=logdat

plot_data=effectPlotData(loglmod.ZIP, nDF)

plot(plot_data$logmala ~ plot_data$logcd19)
lines(plot_data$logcd19, plot_data$fitted)


####	Fitted vs Residuals -- ZIP	 ####
logdat$fit=plot_data$pred
logdat$res=plot_data$logmala-plot_data$pred

p=ggplot(logdat, aes(x=fit, y=res, color=subject))+
  geom_point()+
  geom_hline(yintercept = 0)+
  ggtitle("Fitted Vs Residual")+
  xlab("Fitted Value")+
  ylab("Residual Value")
p


####	Q-Q plot -- ZIP	 ####
q=ggplot(logdat, aes(sample = res))+
  stat_qq()+
  stat_qq_line()
q








####	AIC Comparisons ####

#### Non-transformed Models
AIC(lmod.LMwFE)
AIC(lmod.LMMwRE)
AIC(lmod.POI)
AIC(lmod.POIql)
AIC(lmod.POIqlLMM)
AIC(lmod.ZIP)



#### Transformed Models
AIC(loglmod.LMwFE)
AIC(loglmod.LMMwRE)
AIC(loglmod.POI)
AIC(loglmod.POIql)
AIC(loglmod.POIqlLMM)
AIC(loglmod.ZIP)




####	Summary	 Comparisons ####

#### Non-transformed Models
summary(lmod.LMwFE)
summary(lmod.LMMwRE)
summary(lmod.POI)
summary(lmod.POIql)
summary(lmod.POIqlLMM)
summary(lmod.ZIP)



#### Transformed Models
summary(loglmod.LMwFE)
summary(loglmod.LMMwRE)
summary(loglmod.POI)
summary(loglmod.POIql)
summary(loglmod.POIqlLMM)
summary(loglmod.ZIP)
















