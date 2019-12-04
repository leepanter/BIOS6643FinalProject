####	ZIP Model Selection	 ####
####	Script Name: ZIPmodelSelection.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will compare the Zero Inflated Poisson models created using adaptive Gaussian Quadrature to each other to find the best "Zero Inflated Poisson Model"

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

# Define Response, covariate, and class variables to be used throughout the script
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


####	Occurence Model(s)	 ####
#### (O1) FE = ~ 1          RE = NONE
#### (O2) FE = ~ 1 + cd19   RE = NONE
#### (O3) FE = ~ 1          RE = ~ (1|subject)
#### (O4) FE = ~ 1          RE = ~ (1|subject) +(0+cd19|subject)
#### (O5) FE = ~ 1 + cd19   RE = ~ (1|subject)
#### (O6) FE = ~ 1 + cd19   RE = ~ (1|subject) +(0+cd19|subject)

####	Intensity Model(s)	 ####
#### (I1) FE = ~ 1          RE = ~ (1|subject)
#### (I2) FE = ~ 1          RE = ~ (1|subject) +(0+cd19|subject)
#### (I3) FE = ~ 1 + cd19   RE = ~ (1|subject)
#### (I4) FE = ~ 1 + cd19   RE = ~ (1|subject) +(0+cd19|subject)



####	O1 + __	 ####

#### I0
lmod.ZIP.O1.I0=zeroinfl(mala~cd19|1,data=dat)
(lmod.ZIP.O1.I0s=summary(lmod.ZIP.O1.I0))

#### I1
lmod.ZIP.O1.I1=mixed_model(mala~1,
                      random = ~1|subject,
                      data=dat,
                      family = zi.poisson(),
                      zi_fixed = ~ 1)
(lmod.ZIP.O1.I1s=summary(lmod.ZIP.O1.I1))

loglmod.ZIP.O1.I1=mixed_model(logmala~1,
                      random = ~1|subject,
                      data=logdat,
                      family = zi.poisson(),
                      zi_fixed = ~ 1)
(loglmod.ZIP.O1.I1s=summary(loglmod.ZIP.O1.I1))

#### I2
lmod.ZIP.O1.I2=update(lmod.ZIP.O1.I1, random= ~cd19|subject)
(lmod.ZIP.O1.I2s=summary(lmod.ZIP.O1.I2))

loglmod.ZIP.O1.I2=update(loglmod.ZIP.O1.I1, random= ~logcd19|subject)
(loglmod.ZIP.O1.I2s=summary(loglmod.ZIP.O1.I2))

#### I3
lmod.ZIP.O1.I3=mixed_model(mala~cd19,
                           random = ~1|subject,
                           data=dat,
                           family = zi.poisson(),
                           zi_fixed = ~ 1)
(lmod.ZIP.O1.I3s=summary(lmod.ZIP.O1.I3))

loglmod.ZIP.O1.I3=mixed_model(logmala~logcd19,
                              random = ~1|subject,
                              data=logdat,
                              family = zi.poisson(),
                              zi_fixed = ~ 1)
(loglmod.ZIP.O1.I3s=summary(loglmod.ZIP.O1.I3))

#### I4
lmod.ZIP.O1.I4=update(lmod.ZIP.O1.I1, random= ~cd19|subject)
(lmod.ZIP.O1.I4s=summary(lmod.ZIP.O1.I2))

loglmod.ZIP.O1.I4=update(loglmod.ZIP.O1.I3, random= ~logcd19|subject)
(loglmod.ZIP.O1.I4s=summary(loglmod.ZIP.O1.I4))




####	O2 + __	 ####

#### I1 # NOTE: Fit Algorithm did not Converge
# lmod.ZIP.O2.I1=mixed_model(mala~1,
#                            random = ~1|subject,
#                            data=dat,
#                            family = zi.poisson(),
#                            zi_fixed = ~cd19,
#                            max_coef_value=100)
# (lmod.ZIP.O2.I1s=summary(lmod.ZIP.O2.I1))

loglmod.ZIP.O2.I1=mixed_model(logmala~1,
                              random = ~1|subject,
                              data=logdat,
                              family = zi.poisson(),
                              zi_fixed = ~ logcd19,
                              max_coef_value=1000)
(loglmod.ZIP.O2.I1s=summary(loglmod.ZIP.O2.I1))

#### I2
# lmod.ZIP.O2.I2=update(lmod.ZIP.O2.I1, random= ~cd19|subject)
# (lmod.ZIP.O2.I2s=summary(lmod.ZIP.O2.I2))

loglmod.ZIP.O2.I2=update(loglmod.ZIP.O2.I1, random= ~logcd19|subject)
(loglmod.ZIP.O2.I2s=summary(loglmod.ZIP.O2.I2))

#### I3 # NOTE: Fit Algorithm DID NOT CONVERGE
# lmod.ZIP.O2.I3=mixed_model(mala~cd19,
#                            random = ~1|subject,
#                            data=dat,
#                            family = zi.poisson(),
#                            zi_fixed = ~ cd19,
#                            max_coef_value=1000)
# (lmod.ZIP.O2.I3s=summary(lmod.ZIP.O2.I3))

loglmod.ZIP.O2.I3=mixed_model(logmala~logcd19,
                              random = ~1|subject,
                              data=logdat,
                              family = zi.poisson(),
                              zi_fixed = ~ logcd19,
                              max_coef_value=1000)
(loglmod.ZIP.O2.I3s=summary(loglmod.ZIP.O2.I3))

#### I4
# lmod.ZIP.O2.I4=update(lmod.ZIP.O2.I1, random= ~cd19|subject)
# (lmod.ZIP.O2.I4s=summary(lmod.ZIP.O2.I2))

loglmod.ZIP.O2.I4=update(loglmod.ZIP.O2.I3, random= ~logcd19|subject)
(loglmod.ZIP.O2.I4s=summary(loglmod.ZIP.O2.I4))




####	O3 + __	 ####

#### I1
lmod.ZIP.O3.I1=mixed_model(mala~1,
                           random = ~1|subject,
                           data=dat,
                           family = zi.poisson(),
                           zi_fixed = ~ 1,
                           zi_random = ~1|subject)
(lmod.ZIP.O3.I1s=summary(lmod.ZIP.O3.I1))

loglmod.ZIP.O3.I1=mixed_model(logmala~1,
                              random = ~1|subject,
                              data=logdat,
                              family = zi.poisson(),
                              zi_fixed = ~1,
                              zi_random = ~1|subject)
(loglmod.ZIP.O3.I1s=summary(loglmod.ZIP.O3.I1))

#### I2
lmod.ZIP.O3.I2=update(lmod.ZIP.O3.I1, random= ~cd19|subject)
(lmod.ZIP.O3.I2s=summary(lmod.ZIP.O3.I2))

loglmod.ZIP.O3.I2=update(loglmod.ZIP.O3.I1, random= ~logcd19|subject)
(loglmod.ZIP.O3.I2s=summary(loglmod.ZIP.O3.I2))

#### I3
lmod.ZIP.O3.I3=mixed_model(mala~cd19,
                           random = ~1|subject,
                           data=dat,
                           family = zi.poisson(),
                           zi_fixed = ~1,
                           zi_random = ~1|subject)
(lmod.ZIP.O3.I3s=summary(lmod.ZIP.O3.I3))

loglmod.ZIP.O3.I3=mixed_model(logmala~logcd19,
                              random = ~1|subject,
                              data=logdat,
                              family = zi.poisson(),
                              zi_fixed = ~1,
                              zi_random = ~1|subject)
(loglmod.ZIP.O3.I3s=summary(loglmod.ZIP.O3.I3))

#### I4
lmod.ZIP.O3.I4=update(lmod.ZIP.O3.I1, random= ~cd19|subject)
(lmod.ZIP.O3.I4s=summary(lmod.ZIP.O3.I2))

loglmod.ZIP.O3.I4=update(loglmod.ZIP.O3.I3, random= ~logcd19|subject)
(loglmod.ZIP.O3.I4s=summary(loglmod.ZIP.O3.I4))




####	O4 + __	 ####

#### I1
lmod.ZIP.O4.I1=mixed_model(mala~1,
                           random = ~1|subject,
                           data=dat,
                           family = zi.poisson(),
                           zi_fixed = ~ 1,
                           zi_random = ~1|subject,
                           max_coef_value=1000)
lmod.ZIP.O4.I1=update(lmod.ZIP.O4.I1, zi_random= ~cd19|subject)
(lmod.ZIP.O4.I1s=summary(lmod.ZIP.O4.I1))

loglmod.ZIP.O4.I1=mixed_model(logmala~1,
                              random = ~1|subject,
                              data=logdat,
                              family = zi.poisson(),
                              zi_fixed = ~1,
                              zi_random = ~1|subject,
                              max_coef_value=1000)
loglmod.ZIP.O4.I1=update(loglmod.ZIP.O4.I1, zi_random= ~logcd19|subject)
(loglmod.ZIP.O4.I1s=summary(loglmod.ZIP.O4.I1))

#### I2
lmod.ZIP.O4.I2=update(lmod.ZIP.O4.I1, random= ~cd19|subject)
(lmod.ZIP.O4.I2s=summary(lmod.ZIP.O4.I2))

loglmod.ZIP.O4.I2=update(loglmod.ZIP.O4.I1, random= ~logcd19|subject)
(loglmod.ZIP.O4.I2s=summary(loglmod.ZIP.O4.I2))

#### I3
lmod.ZIP.O4.I3=mixed_model(mala~cd19,
                           random = ~1|subject,
                           data=dat,
                           family = zi.poisson(),
                           zi_fixed = ~1,
                           zi_random = ~1|subject,
                           max_coef_value=1000)
(lmod.ZIP.O4.I3s=summary(lmod.ZIP.O4.I3))

loglmod.ZIP.O4.I3=mixed_model(logmala~logcd19,
                              random = ~1|subject,
                              data=logdat,
                              family = zi.poisson(),
                              zi_fixed = ~1,
                              zi_random = ~1|subject,
                              max_coef_value=1000)
(loglmod.ZIP.O4.I3s=summary(loglmod.ZIP.O4.I3))

#### I4
lmod.ZIP.O4.I4=update(lmod.ZIP.O4.I1, random= ~cd19|subject)
(lmod.ZIP.O4.I4s=summary(lmod.ZIP.O4.I2))

loglmod.ZIP.O4.I4=update(loglmod.ZIP.O4.I3, random= ~logcd19|subject)
(loglmod.ZIP.O4.I4s=summary(loglmod.ZIP.O4.I4))



####	O5 + __	 ####

#### I1 # NOTE: DID NOT CONVERGE
# lmod.ZIP.O5.I1=mixed_model(mala~1,
#                            random = ~1|subject,
#                            data=dat,
#                            family = zi.poisson(),
#                            zi_fixed = ~cd19,
#                              zi_random= ~1|subject,
#                              max_coef_value=1000)
# (lmod.ZIP.O5.I1s=summary(lmod.ZIP.O5.I1))

loglmod.ZIP.O5.I1=mixed_model(logmala~1,
                              random = ~1|subject,
                              data=logdat,
                              family = zi.poisson(),
                              zi_fixed = ~logcd19,
                              zi_random= ~1|subject,
                              max_coef_value=1000)
(loglmod.ZIP.O5.I1s=summary(loglmod.ZIP.O5.I1))

#### I2
# lmod.ZIP.O5.I2=update(lmod.ZIP.O5.I1, random= ~cd19|subject)
# (lmod.ZIP.O5.I2s=summary(lmod.ZIP.O5.I2))

loglmod.ZIP.O5.I2=update(loglmod.ZIP.O5.I1, random= ~logcd19|subject)
(loglmod.ZIP.O5.I2s=summary(loglmod.ZIP.O5.I2))

#### I3 # NOTE: DID NOT CONVERGE
# lmod.ZIP.O5.I3=mixed_model(mala~cd19,
#                            random = ~1|subject,
#                            data=dat,
#                            family = zi.poisson(),
#                            zi_fixed = ~cd19,
#                              zi_random= ~1|subject,
#                              max_coef_value=1000)
# (lmod.ZIP.O5.I3s=summary(lmod.ZIP.O5.I3))

loglmod.ZIP.O5.I3=mixed_model(logmala~logcd19,
                              random = ~1|subject,
                              data=logdat,
                              family = zi.poisson(),
                              zi_fixed = ~logcd19,
                                zi_random= ~1|subject,
                                max_coef_value=1000)
(loglmod.ZIP.O5.I3s=summary(loglmod.ZIP.O5.I3))

#### I4
# lmod.ZIP.O5.I4=update(lmod.ZIP.O5.I1, random= ~cd19|subject)
# (lmod.ZIP.O5.I4s=summary(lmod.ZIP.O5.I2))

loglmod.ZIP.O5.I4=update(loglmod.ZIP.O5.I3, random= ~logcd19|subject)
(loglmod.ZIP.O5.I4s=summary(loglmod.ZIP.O5.I4))



####	O6 + __	 ####

#### I1 # NOTE: DID NOT CONVERGE
# lmod.ZIP.O6.I1=mixed_model(mala~1,
#                            random = ~1|subject,
#                            data=dat,
#                            family = zi.poisson(),
#                            zi_fixed = ~ 1 + cd19,
#                             zi_random = ~ 1|subject,
#                             max_coef_value=1000)
# lmod.ZIP.O6.I1=update(lmod.ZIP.O6.I1, zi_random = ~ cd19|subject)
# (lmod.ZIP.O6.I1s=summary(lmod.ZIP.O6.I1))

loglmod.ZIP.O6.I1=mixed_model(logmala~1,
                              random = ~1|subject,
                              data=logdat,
                              family = zi.poisson(),
                              zi_fixed = ~ 1 + logcd19,
                                zi_random = ~ 1|subject,
                                max_coef_value=1000)
loglmod.ZIP.O6.I1=update(loglmod.ZIP.O6.I1, zi_random = ~ logcd19|subject)
(loglmod.ZIP.O6.I1s=summary(loglmod.ZIP.O6.I1))

#### I2
# lmod.ZIP.O6.I2=update(lmod.ZIP.O6.I1, random= ~cd19|subject)
# (lmod.ZIP.O6.I2s=summary(lmod.ZIP.O6.I2))

loglmod.ZIP.O6.I2=update(loglmod.ZIP.O6.I1, random= ~logcd19|subject)
(loglmod.ZIP.O6.I2s=summary(loglmod.ZIP.O6.I2))

#### I3 # NOTE: DID NOT CONVERGE
# lmod.ZIP.O6.I3=mixed_model(mala~cd19,
#                            random = ~1|subject,
#                            data=dat,
#                            family = zi.poisson(),
#                            zi_fixed = ~ 1 + cd19,
#                              zi_random = ~ 1|subject,
#                              max_coef_value=1000)
# lmod.ZIP.O6.I3=update(lmod.ZIP.O6.I3, zi_random = ~ cd19|subject)
# (lmod.ZIP.O6.I3s=summary(lmod.ZIP.O6.I3))

loglmod.ZIP.O6.I3=mixed_model(logmala~logcd19,
                              random = ~1|subject,
                              data=logdat,
                              family = zi.poisson(),
                              zi_fixed = ~ 1 + logcd19,
                              zi_random = ~ 1|subject,
                              max_coef_value=1000)
loglmod.ZIP.O6.I3=update(loglmod.ZIP.O6.I3, zi_random = ~ logcd19|subject)
(loglmod.ZIP.O6.I3s=summary(loglmod.ZIP.O6.I3))

#### I4
# lmod.ZIP.O6.I4=update(lmod.ZIP.O6.I1, random= ~cd19|subject)
# (lmod.ZIP.O6.I4s=summary(lmod.ZIP.O6.I2))

loglmod.ZIP.O6.I4=update(loglmod.ZIP.O6.I3, random= ~logcd19|subject)
(loglmod.ZIP.O6.I4s=summary(loglmod.ZIP.O6.I4))




####	Comparison of AIC Values	 ####

AIClmodZIP=c()

 AIClmodZIP[1]=AIC(lmod.ZIP.O1.I1)
 AIClmodZIP[2]=AIC(lmod.ZIP.O1.I2)
 AIClmodZIP[3]=AIC(lmod.ZIP.O1.I3)
 AIClmodZIP[4]=AIC(lmod.ZIP.O1.I4)
 AIClmodZIP[5]=AIC(lmod.ZIP.O2.I1)
 AIClmodZIP[6]=AIC(lmod.ZIP.O2.I2)
 AIClmodZIP[7]=AIC(lmod.ZIP.O2.I3)
 AIClmodZIP[8]=AIC(lmod.ZIP.O2.I4)
 AIClmodZIP[9]=AIC(lmod.ZIP.O3.I1)
AIClmodZIP[10]=AIC(lmod.ZIP.O3.I2)
AIClmodZIP[11]=AIC(lmod.ZIP.O3.I3)
AIClmodZIP[12]=AIC(lmod.ZIP.O3.I4)
AIClmodZIP[13]=AIC(lmod.ZIP.O4.I1)
AIClmodZIP[14]=AIC(lmod.ZIP.O4.I2)
AIClmodZIP[15]=AIC(lmod.ZIP.O4.I3)
AIClmodZIP[16]=AIC(lmod.ZIP.O4.I4)
AIClmodZIP[17]=AIC(lmod.ZIP.O5.I1)
AIClmodZIP[18]=AIC(lmod.ZIP.O5.I2)
AIClmodZIP[19]=AIC(lmod.ZIP.O5.I3)
AIClmodZIP[20]=AIC(lmod.ZIP.O5.I4)
AIClmodZIP[21]=AIC(lmod.ZIP.O6.I1)
AIClmodZIP[22]=AIC(lmod.ZIP.O6.I2)
AIClmodZIP[23]=AIC(lmod.ZIP.O6.I3)
AIClmodZIP[24]=AIC(lmod.ZIP.O6.I4)

AICloglmodZIP=c()

 AICloglmodZIP[1]=AIC(loglmod.ZIP.O1.I1)
 AICloglmodZIP[2]=AIC(loglmod.ZIP.O1.I2)
 AICloglmodZIP[3]=AIC(loglmod.ZIP.O1.I3)
 AICloglmodZIP[4]=AIC(loglmod.ZIP.O1.I4)
 AICloglmodZIP[5]=AIC(loglmod.ZIP.O2.I1)
 AICloglmodZIP[6]=AIC(loglmod.ZIP.O2.I2)
 AICloglmodZIP[7]=AIC(loglmod.ZIP.O2.I3)
 AICloglmodZIP[8]=AIC(loglmod.ZIP.O2.I4)
 AICloglmodZIP[9]=AIC(loglmod.ZIP.O3.I1)
AICloglmodZIP[10]=AIC(loglmod.ZIP.O3.I2)
AICloglmodZIP[11]=AIC(loglmod.ZIP.O3.I3)
AICloglmodZIP[12]=AIC(loglmod.ZIP.O3.I4)
AICloglmodZIP[13]=AIC(loglmod.ZIP.O4.I1)
AICloglmodZIP[14]=AIC(loglmod.ZIP.O4.I2)
AICloglmodZIP[15]=AIC(loglmod.ZIP.O4.I3)
AICloglmodZIP[16]=AIC(loglmod.ZIP.O4.I4)
AICloglmodZIP[17]=AIC(loglmod.ZIP.O5.I1)
AICloglmodZIP[18]=AIC(loglmod.ZIP.O5.I2)
AICloglmodZIP[19]=AIC(loglmod.ZIP.O5.I3)
AICloglmodZIP[20]=AIC(loglmod.ZIP.O5.I4)
AICloglmodZIP[21]=AIC(loglmod.ZIP.O6.I1)
AICloglmodZIP[22]=AIC(loglmod.ZIP.O6.I2)
AICloglmodZIP[23]=AIC(loglmod.ZIP.O6.I3)
AICloglmodZIP[24]=AIC(loglmod.ZIP.O6.I4)

lmod=as.factor(c("lmod.ZIP.O1.I1",
       "lmod.ZIP.O1.I2",
       "lmod.ZIP.O1.I3",
       "lmod.ZIP.O1.I4",
       "lmod.ZIP.O2.I1",
       "lmod.ZIP.O2.I2",
       "lmod.ZIP.O2.I3",
       "lmod.ZIP.O2.I4",
       "lmod.ZIP.O3.I1",
       "lmod.ZIP.O3.I2",
       "lmod.ZIP.O3.I3",
       "lmod.ZIP.O3.I4",
       "lmod.ZIP.O4.I1",
       "lmod.ZIP.O4.I2",
       "lmod.ZIP.O4.I3",
       "lmod.ZIP.O4.I4",
       "lmod.ZIP.O5.I1",
       "lmod.ZIP.O5.I2",
       "lmod.ZIP.O5.I3",
       "lmod.ZIP.O5.I4",
       "lmod.ZIP.O6.I1",
       "lmod.ZIP.O6.I2",
       "lmod.ZIP.O6.I3",
       "lmod.ZIP.O6.I4"))
AIC.zip=data.frame(lmod, AIClmodZIP, AICloglmodZIP)

which.min(AIC.zip$AIClmodZIP)
# lmod.ZIP.O1.I2 and lmod.ZIP.O1.I4 have the same AIC, both of which are the lowest.

which.min(AIC.zip$AICloglmodZIP)
# loglmod.ZIP.O1.I3 has the lowest AIC for the log models


####	lmod.ZIP.O1.I2 VS lmod.ZIP.O1.I4 ####
anova(lmod.ZIP.O1.I2, lmod.ZIP.O1.I4)
# We will choose lmod.ZIP.O1.I2 as it is the simpler model



####	FINAL MODELS ####
lmod.ZIP=mixed_model(mala~1,
                     random = ~1|subject,
                     data=dat,
                     family = zi.poisson(),
                     zi_fixed = ~ 1)
lmod.ZIP=update(lmod.ZIP.O1.I1, random= ~cd19|subject)

loglmod.ZIP=mixed_model(logmala~logcd19,
                                         random = ~1|subject,
                                         data=logdat,
                                         family = zi.poisson(),
                                         zi_fixed = ~ 1)


