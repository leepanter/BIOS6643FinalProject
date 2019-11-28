####	Model Functions	 ####
####	Script Name: ModelFunctions.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This is a list of functions that will be used to create models for analysis


####	Script Dependencies	 ####

# Package Dependencies:
library(lme4)
library(ggplot2)
library(nlme)
library(MASS)
library(pscl)
library(GLMMadaptive)


# Set Working Directory

# Data Dependencies:

# Variable Dependencies:

#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#

####	LMwFE ####

#####	Model Fit	#####
# Creates a linear model with fixed effects only function
# Output:
#   Model type (lm variable)---LMwFE
# Input(s):
#   Intended Response Variable-(vector)---resp
#   Intended Covariate Variable-(Vector)---cov
#   Subject Grouping Variable-(vector)---subj

# LMwFEmodelFit=function(resp, cov, subj)
# {
#   DatTemp=data.frame(resp, cov, subj)
#   DatTemp=groupedData(resp~cov|subj, data = DatTemp)
#
#   logresp=log(resp+1, base = exp(1))
#   logcov=log(cov+1, base = exp(1))
#
#   logDatTemp=data.frame(logresp, logcov, subj)
#   logDatTemp=groupedData(logresp~logcov|subj, data = logDatTemp)
#
#   LMwFE=lm(resp~subj+cov, data = DatTemp)
#   logLMwFE=lm(logresp~subj+logcov, data=logDatTemp)
#   return(list(LMwFE, logLMwFE))
# }

LMwFEmodelFit=function(resp, cov, subj)
{
  DatTemp=data.frame(resp, cov, subj)
  DatTemp=groupedData(resp~cov|subj, data = DatTemp)

  LMwFE=lm(resp~subj+cov, data = DatTemp)
  return(LMwFE)
}

logLMwFEmodelFit=function(resp, cov, subj)
{
  logresp=log(resp+1, base = exp(1))
  logcov=log(cov+1, base = exp(1))

  logDatTemp=data.frame(logresp, logcov, subj)
  logDatTemp=groupedData(logresp~logcov|subj, data = logDatTemp)

  logLMwFE=lm(logresp~subj, data=logDatTemp)
  return(logLMwFE)
}


#####	LMwFE function eval	#####
# Evaluates LMwFEmodelFit for covariate (j) on a given response variable (i)
# Output:
#   LMwFEmodels_i- list of models with response variable i
#   i- index of response variable

# evalLMwFEmodelFit=function(i)
# {
#   LMwFEmodels_i=list()
#   resp.temp=seqFilter[,i]
#   for(j in 1:38354)
#   {
#     if(j != i){
#       cov.temp=seqFilter[,j]
#       df.temp=data.frame(resp.temp, cov.temp, subject)
#       LMwFEmodels_i[[j]]=LMwFEmodelFit(resp=resp.temp,
#                                         cov=cov.temp,
#                                         subj=subject)}
#     else{LMwFEmodels_i[[j]]=NA}
#   }
#   return(LMwFEmodels_i)
# }




####	LMMwRE ####

#####	Model Fit	#####
# Creates a linear mixed model with fixed effects for count-covariate, and random effects for subject-specific intercept, and subject-specific slopes
# Output:
#   Model type (lme4 variable)---LMMwRE
# Input(s):
#   Intended Response Variable-(vector)---resp
#   Intended Covariate Variable-(Vector)---cov
#   Subject Grouping Variable-(vector)---subj

LMMwREmodelFit=function(resp, cov, subj)
{
  DatTemp=data.frame(resp, cov, subj)
  DatTemp=groupedData(resp~cov|subj, data = DatTemp)

  LMMwRE=lme4::lmer(resp~cov+(1|subj), data=DatTemp)
  return(LMMwRE)
}

logLMMwREmodelFit=function(resp, cov, subj)
{
  logresp=log(resp+1, base = exp(1))
  logcov=log(cov+1, base = exp(1))

  logDatTemp=data.frame(logresp, logcov, subj)
  logDatTemp=groupedData(logresp~logcov|subj, data = logDatTemp)

  logLMMwRE=lme4::lmer(logresp~1+(1|subj)+(0+logcov|subj), data=logDatTemp)
  return(logLMMwRE)
}

####	POI ####

#####	Model Fit	#####
# Creates a poisson regression model with only fixed effects, and no over-dispersion parameter
# Output:
#   Model type (glm variable)---POI
# Input(s):
#   Intended Response Variable-(vector)---resp
#   Intended Covariate Variable-(Vector)---cov
#   Subject Grouping Variable-(vector)---subj

# POImodelFit=function(resp, cov, subj)
# {
#   DatTemp=data.frame(resp, cov, subj)
#   DatTemp=groupedData(resp~cov|subj, data = DatTemp)
#
#   logresp=log(resp+1, base = exp(1))
#   logcov=log(cov+1, base = exp(1))
#
#   logDatTemp=data.frame(logresp, logcov, subj)
#   logDatTemp=groupedData(logresp~logcov|subj, data = logDatTemp)
#
#   POI=glm(resp~subj+cov, family = poisson(link = "log"))
#   logPOI=glm(logresp~subj+logcov, family = poisson(link = "log"))
#   return(list(POI,logPOI))
# }

POImodelFit=function(resp, cov, subj)
{
  DatTemp=data.frame(resp, cov, subj)
  DatTemp=groupedData(resp~cov|subj, data = DatTemp)

  POI=glm(resp~subj+cov, family = poisson(link = "log"))
  return(POI)
}

logPOImodelFit=function(resp, cov, subj)
{
  logresp=log(resp+1, base = exp(1))
  logcov=log(cov+1, base = exp(1))

  logDatTemp=data.frame(logresp, logcov, subj)
  logDatTemp=groupedData(logresp~logcov|subj, data = logDatTemp)

  logPOI=glm(logresp~subj, family = poisson(link = "log"))
  return(logPOI)
}

####	POIql	 ####

#####	Model Fit	#####
# Creates a poisson regression model according to the quasi-likelihood framework
# Output:
#   Model type (glm variable)---POIql
# Input(s):
#   Intended Response Variable-(vector)---resp
#   Intended Covariate Variable-(Vector)---cov
#   Subject Grouping Variable-(vector)---subj

# POIqlmodelFit=function(resp, cov, subj)
# {
#   DatTemp=data.frame(resp, cov, subj)
#   DatTemp=groupedData(resp~cov|subj, data = DatTemp)
#
#   logresp=log(resp+1, base = exp(1))
#   logcov=log(cov+1, base = exp(1))
#
#   logDatTemp=data.frame(logresp, logcov, subj)
#   logDatTemp=groupedData(logresp~logcov|subj, data = logDatTemp)
#
#   POIql=glm(resp~subj+cov, family = quasipoisson(link = "log"), data=DatTemp)
#   logPOIql=glm(logresp~subj+logcov, family = quasipoisson(link = "log"),
#                data = logDatTemp)
#   return(list(POIql, logPOIql))
# }

POIqlmodelFit=function(resp, cov, subj)
{
  DatTemp=data.frame(resp, cov, subj)
  DatTemp=groupedData(resp~cov|subj, data = DatTemp)

  POIql=glm(resp~subj+cov, family = quasipoisson(link = "log"), data=DatTemp)
  return(POIql)
}

logPOIqlmodelFit=function(resp, cov, subj)
{
  logresp=log(resp+1, base = exp(1))
  logcov=log(cov+1, base = exp(1))

  logDatTemp=data.frame(logresp, logcov, subj)
  logDatTemp=groupedData(logresp~logcov|subj, data = logDatTemp)

  logPOIql=glm(logresp~subj+logcov, family = quasipoisson(link = "log"),
               data = logDatTemp)
  return(logPOIql)
}


#### POIqlLMM	 ####

#####	Model Fit	#####
# Generalized Linear Mixed Model with Random Intercept and Slope fit using linearization
# Output:
#   Model type (lme variable)---POIqlLMM
# Input(s):
#   Intended Response Variable-(vector)---resp
#   Intended Covariate Variable-(Vector)---cov
#   Subject Grouping Variable-(vector)---subj

POIqlLMMmodelFit=function(resp, cov, subj)
{
  DatTemp=data.frame(resp, cov, subj)
  DatTemp=groupedData(resp~cov|subj, data = DatTemp)

  POIqlLMM=glmmPQL(resp~cov,
                   random=list(~1|subj, ~cov|subj),
                   family = poisson,
                   data=DatTemp)
  return(POIqlLMM)
}


logPOIqlLMMmodelFit=function(resp, cov, subj)
{
  logresp=log(resp+1, base = exp(1))
  logcov=log(cov+1, base = exp(1))

  logDatTemp=data.frame(logresp, logcov, subj)
  logDatTemp=groupedData(logresp~logcov|subj, data = logDatTemp)

  logPOIqlLMM=glmmPQL(logresp~cov,
                      random=list(~1|subj, ~logcov|subj),
                      family = poisson,
                      data=logDatTemp)
  return(logPOIqlLMM)
}



#### ZIP	 ####

#####	Model Fit	#####
# Zero Inflated Poisson model with fixed effects only
# Output:
#   Model type (pscl variable)---ZIP
# Input(s):
#   Intended Response Variable-(vector)---resp
#   Intended Covariate Variable-(Vector)---cov
#   Subject Grouping Variable-(vector)---subj

# ZIPmodelFit=function(resp, cov, subj)
# {
#   DatTemp=data.frame(resp, cov, subj)
#   DatTemp=groupedData(resp~cov|subj, data = DatTemp)
#
#   ZIP=zeroinfl(formula=resp~cov+subj|1,data=DatTemp)
#   return(ZIP)
# }




#-------------------------------------------------------------------------#

ZIPmodelFit=function(resp, cov, subj)
{
  DatTemp=data.frame(resp, cov, subj)
  DatTemp=groupedData(resp~cov|subj, data = DatTemp)

  ZIP=mixed_model(resp~cov,
                  data=DatTemp,
                  random = ~1+cov,
                  family = zi.poisson(),
                  zi_fixed = ~1)
  return(ZIP)
}

logZIPmodelFit=function(resp, cov, subj)
{
  logresp=log(resp+1, base = exp(1))
  logcov=log(cov+1, base = exp(1))

  logDatTemp=data.frame(logresp, logcov, subj)
  logDatTemp=groupedData(logresp~logcov|subj, data = logDatTemp)

  logZIP=mixed_model(logresp~logcov,
                  data=logDatTemp,
                  random = ~1+logcov,
                  family = zi.poisson(),
                  zi_fixed = ~1)
  return(logZIP)
}

####	End Script	 ####
#-------------------------------------------------------------------------#





#-------------------------------------------------------------------------#
#####	Post-Script	#####

####  Notes:

####  Compilation Errors:

####  Execution Errors:

####  Next Scripts to Consider:

#-------------------------------------------------------------------------#
