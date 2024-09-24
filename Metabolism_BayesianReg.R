################################################################################
### Bayesian analysis of the Metabolism data from Hatton et al. 2019
################################################################################

### load packages

library(tidyverse); library(cowplot); library(brms);

### load metabolism data

met_data <- read.csv('Metabolism.csv')

### metabolism in watts - want to change to kiloJoules per day
### a watt is joules/second
### we have 60 second/minute 60 minutes/hour and 24 hours/day
### so we need to multiply metabolism by 60*60*24/1000 = 86.4

met_data <- met_data %>% mutate(Metabolism_kJDay = Metabolism_W*86.4)

### take a look at a plot

ggplot(data = met_data, aes(x = log(Mass_g), y = log(Metabolism_kJDay), color = Major_taxa)) + 
  geom_point() + geom_smooth(method = 'lm')

### drop plants

met_data <- met_data %>% filter(Major_taxa != 'Plant')

### Now perform regressions for each of the major groups between log mass
### and log metabolism in kiloJoules/day

### start with mammals

mammal_met <- met_data %>% filter(Major_taxa == 'Mammal')

mammal_met_fit <- brm(log(Metabolism_kJDay) ~ 0 + Intercept + log(Mass_g), data = mammal_met, backend = 'cmdstanr')                                

summary(mammal_met_fit)

### Protists

Protist_met <- met_data %>% filter(Major_taxa == 'Protist')

protist_met_fit <- brm(log(Metabolism_kJDay) ~ 0 + Intercept + log(Mass_g), data = Protist_met, backend = 'cmdstanr')

summary(protist_met_fit)

### Ectotherms -- Invertebrates

invertebrate_met <- met_data %>% filter(Major_taxa == 'Invertebrate')

invertebrate_met_fit <- brm(log(Metabolism_kJDay) ~ 0 + Intercept + log(Mass_g), data = invertebrate_met, backend = 'cmdstanr')

summary(invertebrate_met_fit)

### Ectotherms -- Vertebrates

ecto_vertebrate_met <- met_data %>% filter(Major_taxa == 'Fish' | Major_taxa == 'Amphibia' | Major_taxa == 'Reptilia')

ecto_vertebrate_met_fit <- brm(log(Metabolism_kJDay) ~ 0 + Intercept + log(Mass_g), data = ecto_vertebrate_met, backend = 'cmdstanr')

summary(invertebrate_met_fit)

### Birds

bird_met <- met_data %>% filter(Major_taxa == 'Bird')

bird_met_fit <- brm(log(Metabolism_kJDay) ~ 0 + Intercept + log(Mass_g), data = bird_met)

summary(bird_met_fit)

### Prokaryotes

prokaryote_met <- met_data %>% filter(Major_taxa == 'Prokaryote')

prokaryote_met_fit <- brm(log(Metabolism_kJDay) ~ 0 + Intercept + log(Mass_g), data = prokaryote_met, backend = 'cmdstanr')

summary(prokaryote_met_fit)

### save RData

save.image(file = 'Metabolism_RData.RData')
