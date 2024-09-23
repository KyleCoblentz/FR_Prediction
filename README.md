# FR_Prediction

This repository contains the code and data for Coblentz et al. 'Simple, universal rules predict trophic interaction strengths.'

The files included within the repository and descriptions of their contents are below.

### Files and code to generate the data for analyses

AbundanceScaling.csv -- This csv file contains data from Hatton et al. (2019) on the abundances and masses of 5,985 organisms.

MassAbundance_BayesianReg.R -- This R file contains the code to perform Bayesian regression analyses to separately determine the mass-abundance scaling relationships for mammals, protists, invertebrate ectotherms, vertebrate ectotherms, birds, and prokaryotes. This file also produces the RData file 'MassAbundanceScaling.RData.' This code requires cmdstan to perform the Bayesian analyses.

MassAbundanceScaling.RData -- This RData file contains the mass-abundance regression results that are used to predict the abundances of prey in FoRAGE from their masses.

Metabolism.csv -- 


### Files and code to perform analyses and make plots




References

Hatton, I. et al. 2019. Linking scaling laws across eukaryotes. PNAS. 116:21616-21622.





