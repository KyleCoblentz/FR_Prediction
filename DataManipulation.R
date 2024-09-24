###########################################################################################
### Cleaning the data to predict functional response parameters
###########################################################################################

### load libraries

library(dplyr); library(ggplot2); library(cowplot); library(brms);

### load bayesian regressions for mass-abundance and metabolic scaling relationships

load('MassAbundanceScaling.RData')

load('Metabolism_RData.RData')

### need to connect the mass-abundance and metabolic scaling regressions to FoRAGE to get

### load FoRAGE

forage <- read.csv('FoRAGE_db_V4_Sept_27_2023_mod.csv', stringsAsFactors = TRUE)

### drop rows to not include

forage <- forage %>% filter(Include. == 1)

### drop levels of Prey Major groups that no longer exist

forage$Major.grouping.1.1 <- droplevels(forage$Major.grouping.1.1)

### drop studies without prey masses

forage <- forage %>% filter(Prey.mass..mg. > 0)

### make sure all of the predators have masses

which(is.na(forage$Predator.mass..mg.))

# drop studies that do not have predator masses

forage <- forage %>% filter(!is.na(Predator.mass..mg.))

### add abundance group column to match studies with the appropriate
### regression for the prey in that study

forage <- forage %>% mutate(PreyAbundanceGroup = ifelse(Vert.invert.1== 'Protozoan' | Vert.invert.1 == 'Algae', 'Protist',
                                                        ifelse(Major.grouping.1.1 == 'Mammal', 'Mammal',
                                                               ifelse(Major.grouping.1.1 == 'Bird', 'Bird',
                                                                      ifelse(Vert.invert.1 == 'Prokaryote', 'Prokaryote',
                                                                             ifelse(Major.grouping.1.1 == 'Fish'| Major.grouping.1.1 == 'Amphibian', 'ecto_vertebrate', 'invertebrate'))))))  
### add metabolism group column for the predators in each study

forage <- forage %>% mutate(PredMetabolismGroup = ifelse(Vert.invert == 'Protozoan' | Vert.invert == 'Unicell', 'Protist',
                                                        ifelse(Major.grouping.1 == 'Mammal', 'Mammal', 
                                                        ifelse(Major.grouping.1 == 'Bird', 'Bird', 
                                                               ifelse(Major.grouping.1 == 'Fish' | Major.grouping.1 == 'Amphibian' | Major.grouping.1 == 'Reptile', 'ecto-vertebrate', 'invertebrate')))))

### add columns for masses of prey and predators in grams

forage$Mass_g <- forage$Prey.mass..mg./1000

forage$PredMass_g <- forage$Predator.mass..mg./1000

### run a loop that uses the appropriate mass-abundance regression to predict
### prey densities given the prey masses

Abundance <- matrix(nrow = 2390, ncol = 11)

for(i in 1:2390) {
  
  if(forage$PreyAbundanceGroup[i] == 'Protist') {
    newdat <-  forage[i,]
     post_pred <-  posterior_predict(object = protist_fit, newdata = newdat, draws = 10000) 
     Abundance[i,1:11] <- quantile(post_pred, probs = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95))
  } else if(forage$PreyAbundanceGroup[i] == 'Mammal') {
    newdat <-  forage[i,]
    post_pred <-  posterior_predict(object = mammal_fit, newdata = newdat, draws = 10000) 
    Abundance[i,1:11] <- quantile(post_pred, probs = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95))
  } else if(forage$PreyAbundanceGroup[i] == 'Bird') {
    newdat <-  forage[i,]
    post_pred <-  posterior_predict(object = bird_fit, newdata = newdat, draws = 10000)  
    Abundance[i,1:11] <- quantile(post_pred, probs = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95))
  } else if(forage$PreyAbundanceGroup[i] == 'ecto_vertebrate'){
    newdat <-  forage[i,]
    post_pred <-  posterior_predict(object = ecto_vertebrate_fit, newdata = newdat, draws = 10000) 
    Abundance[i,1:11] <- quantile(post_pred, probs = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95))
  } else if(forage$PreyAbundanceGroup[i] == 'invertebrate'){
    newdat <-  forage[i,]
    post_pred <-  posterior_predict(object = invertebrate_fit, newdata = newdat, draws = 10000)  
    Abundance[i,1:11] <- quantile(post_pred, probs = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95))
  } else {
    newdat <-  forage[i,]
    post_pred <-  posterior_predict(object = prokaryote_fit, newdata = newdat, draws = 10000)  
      Abundance[i,1:11] <- quantile(post_pred, probs = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95))
  }
  
  print(i)
  
}

### predictions are on the log scale so take the exponential to get back to
### the natural scale

Abundance <- exp(Abundance)

### convert abundance to a data frame and then collate this data frame 
### with FoRAGE

Abundance <- as.data.frame(Abundance)

colnames(Abundance) <- c('PreyAbundance_5', 'PreyAbundance_10',
                         'PreyAbundance_20', 'PreyAbundance_30', 'PreyAbundance_40',
                         'PreyAbundance_50', 'PreyAbundance_60', 'PreyAbundance_70',
                         'PreyAbundance_80', 'PreyAbundance_90', 'PreyAbundance_95')

forage <- forage %>% bind_cols(Abundance)

### now need to add predator metabolic rates using a similar loop

Metabolism_pred <- matrix(nrow = 2390, ncol = 1)

for(i in 1:2390) {
  if(forage$PredMetabolismGroup[i] == 'Mammal'){
    newdat <- data.frame(Mass_g = forage$PredMass_g[i])
    Metabolism_pred[i,] <- posterior_predict(object = mammal_met_fit, newdata = newdat, draws = 1000) %>% 
      quantile(probs = c(0.5))
  } else if(forage$PredMetabolismGroup[i] == 'Bird') {
    newdat <- data.frame(Mass_g = forage$PredMass_g[i])
    Metabolism_pred[i,] <- posterior_predict(object = bird_met_fit, newdata = newdat, draws = 1000) %>% 
      quantile(probs = c(0.5))
  } else if(forage$PredMetabolismGroup[i] == 'Protist') {
    newdat <- data.frame(Mass_g = forage$PredMass_g[i])
    Metabolism_pred[i, ] <- posterior_predict(object = protist_met_fit, newdata = newdat, draws = 1000) %>% 
      quantile(probs = c(0.5))
  } else if(forage$PredMetabolismGroup[i] == 'ecto_vertebrate'){
    newdat <- data.frame(Mass_g = forage$PredMass_g[i])
   Metabolism_pred[i,] <-  posterior_predict(object = ecto_vertebrate_met_fit, newdata = newdat, draws = 1000) %>% 
      quantile(probs = c(0.5))
  } else {
    newdat <- data.frame(Mass_g = forage$PredMass_g[i])
    Metabolism_pred[i,] <-  posterior_predict(object = invertebrate_met_fit, newdata = newdat, draws = 1000) %>% 
      quantile(probs = c(0.5))
  }
  
  print(i)
  
}

### exponentiate the metabolic rate estimates to get them on the natural scale

Metabolism_pred <- exp(Metabolism_pred)

### combine the metabolic rate estimates with FoRAGE

Metabolism_pred <- as.data.frame(Metabolism_pred)

colnames(Metabolism_pred) <- c('PredMetabolism')

forage <- forage %>% bind_cols(Metabolism_pred)

# ### save this data set as a .csv

write.csv(forage, file = 'forage_modified.csv', row.names = FALSE)


