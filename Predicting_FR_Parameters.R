####################################################################
### Predicting Functional Response Parameters
####################################################################

### load packages

library(ggplot2); library(cowplot); library(dplyr); library(lmodel2) 

### load data

forage <- read.csv('forage_modified.csv')

colnames(forage)[c(43,46)] <- c('Obs_a', 'Obs_h')

forage <- forage %>% filter(Obs_h > 1e-6)

### calculate predicted space clearance rate

#  we will assume an energy density of prey of 5.6 kJ/g

energy_density <- 5.6

################################################################################
### predicted space clearance rate
################################################################################

### calculate predicted space clearance rates and handling times

forage <- forage %>% mutate(Pred_a = PredMetabolism/(PreyAbundance_10*energy_density*Mass_g),
                            Pred_h = 0.9*PreyAbundance_10*energy_density*Mass_g/(PreyAbundance_90*PredMetabolism*0.1))

### major axis regression of predicted scr on observed

lmodel2(log(Pred_a) ~ log(Obs_a), data = forage)

cor(log(forage$Obs_a), log(forage$Pred_a))

### plot of SCR

SCR_plot <- ggplot(data = forage, aes(x = log(Obs_a), y = log(Pred_a))) + geom_point(alpha = 0.25) +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, linetype = 'dashed') + theme_cowplot() + geom_abline(slope = 0.845, intercept = 0.73, linewidth = 1) +
  xlab('log Observed Space\nClearance Rate') + ylab('log Predicted Space\nClearance Rate')

# save plot object to later put plots together

saveRDS(SCR_plot, file = 'SCR_Prediction_Plot.RData')

################################################################################
### Handling time
################################################################################

# major axis regression for handling time

lmodel2(log(Pred_h) ~ log(Obs_h), data = forage)

cor(log(forage$Pred_h), log(forage$Obs_h))

### plot of handling time predictions

h_plot <- ggplot(data = forage, aes(x = log(Obs_h), y = log(Pred_h))) + geom_point(alpha = 0.25) + 
  geom_abline(intercept = 0, slope = 1, linewidth = 1, linetype = 'dashed') + theme_cowplot() +
  geom_abline(intercept = 1.04, slope = 1.15, linewidth = 1) +
  xlab('log Observed Handling Time') + ylab('log Predicted Handling Time')

# save plot object to put together later into composite figure

saveRDS(h_plot, file = 'h_Prediction_Plot.RData')

################################################################################
### prediction of half saturation constant (1/ah) and prey densities
################################################################################

forage <- forage %>% mutate(half_sat = 1/(Obs_a*Obs_h), pop_pred = 0.1*PreyAbundance_90/0.9)

lmodel2(log(pop_pred) ~ log(half_sat), data = forage) 

pop_plot <- ggplot(data = forage, aes(x = log(1/(Obs_a*Obs_h)), y = log(0.1*PreyAbundance_90/0.9))) + geom_point(alpha = 0.25) + 
  geom_abline(intercept = 0, slope = 1, linewidth = 1, linetype = 'dashed') + theme_cowplot() + 
  geom_abline(intercept = -0.74, slope = 0.8815, size = 1) + 
  xlab(expression('log'~'1/ah')) + ylab(expression(log~"(1-"*I[S]*")"*N[high]*"/"*I[S]))

################################################################################
### prediction of relationship between a and h
################################################################################

avh_color <- ggplot(data = forage, aes(x = log(Obs_a), y = log(Obs_h), color = log(PreyAbundance_90))) + geom_point() + 
  theme_cowplot() + xlab('log Space Clearance Rate') + ylab('log Handling Time') + scale_color_viridis_c(name = expression('log N'['high'])) + geom_abline(slope = -1, intercept = log(9/exp(30)), color = '#fcf079', size = 1) + 
  geom_abline(slope = -1, intercept = log(9/exp(20)), color = '#72cc58', size = 1) + 
  geom_abline(slope = -1, intercept = log(9/exp(10)), color = '#299a87', size = 1) + 
  geom_abline(slope = -1, intercept = log(9), color = '#3a608b', size = 1) + 
  geom_abline(slope = -1, intercept = log(9/exp(-10)), color = '#461e65', size = 1)

### put population plots together

together_plot <- plot_grid(pop_plot, avh_color, nrow = 1, labels = 'AUTO')

save_plot(filename = 'Figure2_pop_avh.png', plot = together_plot, nrow = 1, ncol = 2, bg = 'white')

