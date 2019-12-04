# BIOS6643FinalProject
Final Project for BIOS 6643 Fall 2019

## Author: Lee Panter
## Contact Information:
* email:
  * lee.panter@ucdenver.edu
* mailing address:
  * 3055 South Milwaukee Circle
  * Denver, CO 80210

## Disclaimer:
The information contained in this repository, including all documents, code, code-output and data are (and never were) intended for publishing for public record under the designation as original intellectual property of the named author.

## **NOTE**:
TO LOAD THE DATA REQUIRED FOR ALL THE SCRIPTS IN THIS REPOSITORY, PLEASE VISIT:

https://drive.google.com/file/d/1YoqM5L9dTS58q9bX4LmRrq8whuwWbs0l/view?usp=sharing

and download the associated Rdata file.

## Contents:

### Phase 1
Inital data description, and project inception.

### Phase 2
Project Proposal

### Modeling
* Unused
  * Scripts were not used in the final results of this Project
  * ExampleGraph.R, ExampleAnalysis.R, ExploratoryAnalysis.R, InitialModels.R
* ModeFunctions:
  *  .R File with extensive R-function breakdown.  These functions are employed in all subsequent scripts, and are therefore centralized and sourced at the start of new scripts.
* InitalModels:
  * UpdatedInitialModels.R -- Used to fit, plot and analyze some incipient models that were not related to the final results
* FinalModels
  * Same script as InitialModels, but with the models that had been selected from Hierarchical Selection
* LMwFE (Linear Model with Fixed Effects)
  * LMwFEmodelSelection.R Hierarchical Model Selection for optimal model
* LMMwRE (Linear Mixed Models with Random Effects)
  * LMMwREmodelSelection.R Hierarchical Model Selection for optimal model
* POI (Generalized Linear Model - Poisson Regression)
  * POImodelSelection.R Hierarchical Model Selection for optimal model
* POIql (Generalized Linear Model - Poisson Quasi-likelihood)
  * POIqlmodelSelection.R Hierarchical Model Selection for optimal model
* POIqlLMM (Generalized Linear Mixed Model - Poisson Quasi-likelihood)
  * POIqlLMMmodleSelection.R Hierarchical Model Selection for optimal model
* ZIP (Zero Inflated Poisson)
  * ZIPmodelSelection.R Hierarchical Model Selection for optimal model
  * LoopingTemplate.txr -- Copy and Paste template used for constructing ZIPmodelSelection.R

### Presentation
.tex, .pdf, .jpeg, files that were used in the creation of the Beamer presentation. (as well as the presentaion itself).

## Final Model Plots
Output of the FinalModels.R script.

## Data
Redundant storage and first attempt to locate data for external use.  These data files have been compressed and have lost their data-typing to an irrecoverable extent.  They have been left in the repository because several referrenced scripts will download them.

## WriteUp
Remote storage and access point for paper, and paper resources.
