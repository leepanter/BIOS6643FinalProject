####	Model Creations	 ####
####	Script Name: ModelCreations.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will use the functions produced in ModelFunctions.R to produce the same models as those produced in the InitialModels.R script, but for each possible variable combination

####	Script Dependencies	 ####

# Package Dependencies:
library(lme4)
library(ggplot2)
library(nlme)
library(MASS)
library(pscl)
library(foreach)
library(doParallel)

# File Dependencies
source(file = "/Users/lee/Documents/Lee/School/CU Denver/Fall 2019/BIOS 6643/FinalProject/Modeling/ModelFunctions/ModelFunctions.R")


# Set Working Directory
WD="/Users/lee/Documents/Lee/School/CU Denver/Fall 2019/BIOS 6643/FinalProject/Modeling/ExtendedModeling"
setwd(WD)

# Data Dependencies:


#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#

#####	Variable Initializations	#####

# Parallel Computing Architechture
c1=makeCluster(12)
registerDoParallel(12)

# Script Variables
subject=mdataFilter$subject.no
seqFilter=data.frame(t(seqFilter))
LMwFEmodels=list()

#####	Evaluate LMwFE models	#####
LMwFEmodels <- foreach(i=1:3) %dopar% evalLMwFEmodelFit(i)





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
