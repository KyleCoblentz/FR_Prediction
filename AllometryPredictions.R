################################################################################
### Allometry predictions
################################################################################

### load packages

library(ggplot2); library(dplyr); library(cowplot); 

### load data

forage <- read.csv('forage_modified.csv')

forage <- forage %>% filter(Fittted.h..day. > 1e-6)

### change some column names

colnames(forage)[c(43,46)] <- c('Obs_a', 'Obs_h')

### get empirical allometric scalings of parameters with prey and predator masses 

# Low prey density

lowdensfit <- lm(log(PreyAbundance_10) ~ log(Mass_g), data = forage)

summary(lowdensfit)

# High Prey Density

highdensfit <- lm(log(PreyAbundance_90) ~ log(Mass_g), data = forage)

summary(highdensfit)

# Predator Metabolism

demandfit <- lm(log(PredMetabolism) ~ log(PredMass_g), data = forage)

summary(demandfit)

# Prey and Predator Masses

predtopreyfit <- lm(log(Mass_g) ~ log(PredMass_g), data = forage) 

summary(predtopreyfit)

preytopredfit <- lm(log(PredMass_g) ~ log(Mass_g), data = forage)

summary(preytopredfit)

### Space Clearance rate

a_scale <- lm(log(Obs_a) ~ log(PredMass_g), data = forage)

summary(a_scale)

confint(a_scale)

ggplot(data = forage, aes(x = log(PredMass_g), y = log(Obs_a))) + geom_point( ) + 
  geom_smooth(method = 'lm')

### Handling Time         

ggplot(data = forage, aes(x = log(PredMass_g), y = log(Obs_h/Mass_g))) + geom_point()

h_scale <- lm(log(Obs_h/Mass_g) ~ log(PredMass_g), data = forage)

summary(h_scale)

confint(h_scale)

### scaling plots for manuscript

### calculate exponents and prefactors for the scaling

# for the space clearance rate we need 

delta_0 <- exp(demandfit$coefficients[1])

delta <- demandfit$coefficients[2]

eta_0_low <- exp(lowdensfit$coefficients[1])

eta_low <- lowdensfit$coefficients[2]

energydens <- 5.6

mu_0_P <- exp(predtopreyfit$coefficients[1])

mu_P <- predtopreyfit$coefficients[2]

# the prefactor for a is

a_pre <- (delta_0*mu_0_P^(-eta_low-1))/(eta_0_low*energydens)

log_a_pre <- log(a_pre)

# the slope of a with predator mass on the log scale is

a_slope <- delta+mu_P*(-eta_low-1)

### plot for space clearance rate

scr_lines <- data.frame(slopes = c(a_scale$coefficients[2], a_slope), intercepts = c(a_scale$coefficients[1], log_a_pre), lines = c('Observed', 'Predicted'))

scr_plot <- ggplot(data = forage, aes(x = log(PredMass_g), y = log(Obs_a))) + geom_point(alpha = 0.25) + 
  theme_cowplot() + xlab('log Predator Mass') + ylab('log Observed Space\nClearance Rate') + 
  geom_abline(aes(slope = slopes, intercept = intercepts, linetype = lines), data = scr_lines, linewidth = 1) + 
  labs(linetype = 'Line')
  
### handling time

### for handling time scalings, in addition to the parameters for a, we need

IS <- 0.9

eta_0_high <- exp(highdensfit$coefficients[1])

h_pre <- (IS*eta_0_low*energydens)/((1-IS)*delta_0*eta_0_high)

log_h_pre <- log(h_pre)

h_slope <- -delta

### plot for handling time

handling_lines <- data.frame(slopes = c(h_scale$coefficients[2], h_slope), intercepts = c(h_scale$coefficients[1], log_h_pre), lines = c('Observed', 'Predicted'))

handling_plot <- ggplot(data = forage, aes(x = log(PredMass_g), y = log(Obs_h/Mass_g))) + geom_point(alpha = 0.25) + 
  theme_cowplot() + xlab('log Predator Mass') + ylab('log Handling Time/Prey Mass') +
  geom_abline(aes(slope = slopes, intercept = intercepts, linetype = lines), data = handling_lines, linewidth = 1) + 
  labs(linetype = 'Line')

### put plots together

together <- plot_grid(scr_plot, handling_plot, nrow = 1, labels = 'AUTO', axis = 'tblr', align = 'hv')

# save_plot(filename = 'allometry_plot.png', plot = together, ncol = 2, nrow = 1, bg = 'white', base_asp = 1.4)





