# FR_Prediction

This repository contains the code and data for Coblentz et al. 'Simple, universal rules predict trophic interaction strengths.'

The files included within the repository and descriptions of their contents are below.

### Files and code to generate the data for analyses

AbundanceScaling.csv -- This csv file contains data from Hatton et al. (2019) on the abundances and masses of 5,985 organisms.

MassAbundance_BayesianReg.R -- This R file contains the code to perform Bayesian regression analyses to separately determine the mass-abundance scaling relationships for mammals, protists, invertebrate ectotherms, vertebrate ectotherms, birds, and prokaryotes. This file also produces the RData file 'MassAbundanceScaling.RData.' This code requires cmdstan to perform the Bayesian analyses.

MassAbundanceScaling.RData -- This RData file contains the mass-abundance regression results that are used to predict the abundances of prey in FoRAGE from their masses.

Metabolism.csv -- This csv file contains data from Hatton et al. (2019) on the metabolism and masses of 7,343 organisms.

Metabolism_BayesianReg.R -- This R file contains the code to perform Bayesian regression analyses to separately determine metabolic scaling relationships for mammals, protists, invertebrates, vertebrate ectotherms, birds, and prokaryotes. The file also produces the RData file 'Metabolism_RData.RData'. This code requires cmdstan to perform the Bayesian analyses.

Metabolism_RData.RData -- This RData file contains the metabolic scaling results that are used to the predict the metabolic rates of predators in FoRAGE from their masses.

FoRAGE_db_V4_Sept_27_2023_mod.csv -- This csv file contains the FoRAGE database of functional response parameters and salient covariates from the associated studies. This version has been modified to have an 'Include' column in which zeros have been placed for studies that are excluded from the analysis for having non-living prey or eggs as prey.

DataManipulation.R -- This R file takes the FoRAGE_db_V4_Sept_17_2023_mod.csv file and uses the metabolic scaling and mass-abundance regressions to predict predator metabolic rates and prey densities for each study in FoRAGE. The code also excludes studies as outlined in the methods of the corresponding paper. The final dataset that is used in most of the analyses is output as forage_modified.csv. 

### Files and code to perform analyses and make plots

forage_modified.csv -- This csv file is created by DataManipulation.R and contains the FoRAGE database along with the predicted abundances of prey and metabolic rates of predators.

Predicting_FR_Parameters.R -- This R file does the bulk of the analyses that are presented in the main text. That is, this file predicts the values of the space clearance rates and handling times of each study, examines the predicted relationship between the half-saturation constant and high prey abundances, and examines the predicted relationship between the space clearance rates and handling times. This file also produces SCR_Prediction_Plot.RData and h_Prediction_Plot.RData that are used by Figure1.R to create Figure 1 of the manuscript.

FieldPredatorPrey.csv -- This csv file contains system-specific estimates of the parameters necessary in the theory to predict space clearance rates and handling times for a subset of studies in FoRAGE that are field studies for which the required information could be found and which met the criteria for inclusion laid out in the methods section of the associated paper. 

FieldPredictions.R -- This R file uses the FieldPredatorPrey.csv file to make space clearance rate and handling time predictions for the field studies. This file produces FieldHandlingPlot.RData, FieldAttackRatePlot.RData, and FieldPlotLegend.RData which are used by Figure1.R to produce Figure 1

AllometryPredictions.R -- This R file performs the regressions necessary to assess the theory's predicted allometric scaling relationships of the functional response parameters and then assesses the correspondence between the predicted and observed allometric relationships.

Figure1.R -- This R file contains the code to produce Figure 1 of the main text of the associated manuscript using plots created in the Predicting_FR_Parameters.R and FieldPredictions.R files and saved as RData files.

SensitivityAnalysis.R -- This R file performs the sensitivity analysis of the space clearance rate and handling time predictions that are presented in Supplemental Material S2 of the associated manuscript

References

Hatton, I. et al. 2019. Linking scaling laws across eukaryotes. PNAS. 116:21616-21622.





