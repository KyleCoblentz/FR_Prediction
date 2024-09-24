################################################################################
### FR Prediction Sensitivity
################################################################################

### load packages

library(ggplot2); library(dplyr); library(cowplot); library(RColorBrewer); library(forcats)

### load data

forage <- read.csv('forage_modified.csv')

forage <- forage %>% filter(Fittted.h..day. > 1e-6)

### assume an energy density of 5.6 kJ/g of prey

energy_density <- 5.6

### change some column names

colnames(forage)[c(43,46)] <- c('Obs_a', 'Obs_h')

# for each study in forage want to get predictions of a 
# and h assuming different quantiles for Nhigh and Nlow
# and different values of I_S

# for N high we will use the 70th, 90th, and 95th percentiles

# for Nlow we will use the 5th, 10th, and 30th percentiles

# for I_S we will use 0.7, 0.9, 0.95

# this should lead to 27 combinations

### make a new data frame to store these 

sens_data <- data.frame(Obs_a = rep(forage$Obs_a, times = 27),
           Obs_h = rep(forage$Obs_h, 27))

I_S <- c('0.7','0.9','0.95')

Nhigh <- c('70', '90', '95')

Nlow <- c('5', '10', '30')

name_vec <- vector(length = 27)

I_S <- rep(I_S, each = 9)

Nhigh <- rep(rep(Nhigh, times = 3), each = 3)

Nlow <- rep(Nlow, times = 9)

name_vec <- paste(rep('IS', times = 27),
                  I_S, 
                  rep('Nhigh', times = 27),
                  Nhigh,
                  rep('Nlow', times = 27),
                  Nlow, sep = '_')

name_vec

sens_data$Combination <- rep(name_vec, each = 2162)

### add columns for the values of IS, Nhigh, and Nlow for each combination and each study in FoRAGE

sens_data$I_S <- rep(rep(c(0.7, 0.9, 0.95), each = 9), each = 2162)

sens_data$Nhigh <- c(rep(forage$PreyAbundance_70, times = 3), 
                     rep(forage$PreyAbundance_90, times = 3), rep(forage$PreyAbundance_95, times = 3),
                     rep(forage$PreyAbundance_70, times = 3), 
                     rep(forage$PreyAbundance_90, times = 3), rep(forage$PreyAbundance_95, times = 3),
                     rep(forage$PreyAbundance_70, times = 3), 
                     rep(forage$PreyAbundance_90, times = 3), rep(forage$PreyAbundance_95, times = 3))

sens_data$Nlow <- rep_len(c(forage$PreyAbundance_5, forage$PreyAbundance_10,
                            forage$PreyAbundance_20), length.out = 58374)

### add metabolic demand and mass

sens_data$MetabolicDemand <- rep(forage$PredMetabolism, times = 27)

sens_data$Mass_g <- rep(forage$Mass_g, times = 27)

### calculate predictions for each study using the different values of Nhigh, Nlow, and IS

sens_data <- sens_data %>% mutate(Pred_a = MetabolicDemand/(Nlow*energy_density*Mass_g),
                                  Pred_h = I_S*Nlow*energy_density*Mass_g/(MetabolicDemand*Nhigh*(1-I_S)))

### for space clearance rate, only need to look at differences in N_low

sens_data_a <- sens_data %>% mutate(Nlow_factor = matrix(unlist(strsplit(sens_data$Combination, split = "_")), ncol = 6, byrow = TRUE)[,6])

sens_data_a <- sens_data_a %>% mutate(study = rep(1:2162, times = 27))

sens_data_a <- sens_data_a %>% group_by(study, Nlow_factor) %>% select(-c(Combination)) %>% summarise_all(.funs = mean)

### for each of the Nlow values want to calculate correlation coefficient, R2, 
### and major axis regressions

library(lmodel2)

### calculate correlation coefficients

sens_data_a_nlow5 <- filter(sens_data_a, Nlow_factor == 5)

cor(x = log(sens_data_a_nlow5$Obs_a), y = log(sens_data_a_nlow5$Pred_a))

sens_data_a_nlow10 <- filter(sens_data_a, Nlow_factor == 10)

cor(x = log(sens_data_a_nlow10$Obs_a), y = log(sens_data_a_nlow10$Pred_a))

sens_data_a_nlow30 <- filter(sens_data_a, Nlow_factor == 30)

cor(x = log(sens_data_a_nlow30$Pred_a), y = log(sens_data_a_nlow30$Obs_a))

### calcluate 1:1 line R2 values 

r2_nlow5 <- 1 - sum((log(sens_data_a_nlow5$Obs_a)-log(sens_data_a_nlow5$Pred_a))^2)/sum(log(sens_data_a_nlow5$Obs_a)^2)

r2_nlow10 <- 1 - sum((log(sens_data_a_nlow10$Obs_a)-log(sens_data_a_nlow10$Pred_a))^2)/sum(log(sens_data_a_nlow10$Obs_a)^2)

r2_nlow30 <- 1 - sum((log(sens_data_a_nlow30$Obs_a)-log(sens_data_a_nlow30$Pred_a))^2)/sum(log(sens_data_a_nlow30$Obs_a)^2)

### calculate major axis regressions

nlow5_mar <- lmodel2(log(Pred_a) ~ log(Obs_a), data = sens_data_a_nlow5)

nlow5_mar

nlow10_mar <- lmodel2(log(Pred_a) ~ log(Obs_a), data = sens_data_a_nlow10)

nlow10_mar

nlow30_mar <- lmodel2(log(Pred_a) ~ log(Obs_a), data = sens_data_a_nlow30)

nlow30_mar

### make plots for space clearance rates

nlow5_aplot <- ggplot(data = sens_data_a_nlow5, aes(x = log(Obs_a), y = log(Pred_a))) + geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype  = "dashed", size = 1) + 
  geom_abline(slope = 0.852, intercept = 1.657, size = 1) + 
  ggtitle(label = expression(N[low] ~ 5^th ~ "Percentile")) + 
  theme_cowplot() + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("log Observed Space Clearance Rate") + 
  ylab("log Predicted Space Clearance Rate")

nlow10_aplot <-ggplot(data = sens_data_a_nlow10, aes(x = log(Obs_a), y = log(Pred_a))) + geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype  = "dashed", size = 1) + 
  geom_abline(slope = 0.845, intercept = 0.73, size = 1) + 
  ggtitle(label = expression(N[low] ~ 10^th ~ "Percentile")) + 
  theme_cowplot() + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("log Observed Space Clearance Rate") + 
  ylab("log Predicted Space Clearance Rate")

nlow30_aplot <-ggplot(data = sens_data_a_nlow30, aes(x = log(Obs_a), y = log(Pred_a))) + geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype  = "dashed", size = 1) + 
  geom_abline(slope = 0.837, intercept = -0.388, size = 1) + 
  ggtitle(label = expression(N[low] ~ 30^th ~ "Percentile")) + 
  theme_cowplot() + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("log Observed Space Clearance Rate") + 
  ylab("log Predicted Space Clearance Rate")

aplots_together <- plot_grid(nlow5_aplot, nlow10_aplot, nlow30_aplot, nrow = 1, labels = "AUTO")

# save_plot(filename = 'Sensitivity_a.png', plot = aplots_together, nrow =1, ncol = 3, bg = 'white',
#          base_asp = 1.4)

### sensitivity analysis for handling time 

# here each of the combinations of I_S, Nlow, and Nhigh matter for the value of h.

### get correlation coefficients

sens_data <- sens_data %>% mutate(Nhigh_factor = matrix(unlist(strsplit(sens_data$Combination, split = "_")), ncol = 6, byrow = TRUE)[,4],
                                  Nlow_factor = matrix(unlist(strsplit(sens_data$Combination, split = "_")), ncol = 6, byrow = TRUE)[,6])

h_cor <- sens_data %>% group_by(Nlow_factor, Nhigh_factor, I_S) %>% dplyr::summarise(cor_coeff = cor(x = log(Obs_h), y = log(Pred_h)))

### get r2 values of the 1:1 line

h_r2 <- sens_data %>% group_by(Nlow_factor, Nhigh_factor, I_S) %>% dplyr::summarise(r2 = 1 - sum((log(Obs_h) - log(Pred_h))^2)/sum(log(Obs_h)^2))


### get values of ma regression

mareg_h <- sens_data %>% group_by(Nlow_factor, Nhigh_factor, I_S) %>% dplyr::summarise(Intercept = lmodel2(log(Pred_h) ~ log(Obs_h))$regression.results$Intercept[2],
                                                                                                                        Slope = lmodel2(log(Pred_h) ~ log(Obs_h))$regression.results$Slope[2],
                                                                                                                        Int_l95 = lmodel2(log(Pred_h) ~ log(Obs_h))$confidence.intervals$`2.5%-Intercept`[2],
                                                                                                                        Int_u95 = lmodel2(log(Pred_h) ~ log(Obs_h))$confidence.intervals$`97.5%-Intercept`[2],
                                                                                                                        Slope_l95 = lmodel2(log(Pred_h) ~ log(Obs_h))$confidence.intervals$`2.5%-Slope`[2],
                                                                                                                        Slope_u95 = lmodel2(log(Pred_h) ~ log(Obs_h))$confidence.intervals$`97.5%-Slope`[2])

### make plots for handling time


sens_data$Nlow_factor <- factor(sens_data$Nlow_factor, levels = unique(sens_data$Nlow_factor))

# names for facet labels

Nlow_names <- list(
  '5'=expression(N[low]*'='*5^th~percentile),
  '10'=expression(N[low]*'='*10^th~percentile),
  '30'=expression(N[low]*'='*30^th~percentile)
)

Nhigh_names = list(
  '70'=expression(N[high]*'='*70^th~percentile),
  '90'=expression(N[high]*'='*90^th~percentile),
  '95'=expression(N[high]*'='*95^th~percentile)
)

facet_labeller <- function(variable,value){
  if(variable == "Nlow_factor"){
    return(Nlow_names[value])
  } else {
    return(Nhigh_names[value])
  }
}



h_sens_plot <- ggplot(data = sens_data, aes(x = log(Obs_h), y = log(Pred_h), color = as.factor(I_S))) + 
  facet_grid(rows = Nlow_factor ~ Nhigh_factor, labeller = facet_labeller) + geom_point(alpha = 0.15) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) + 
  geom_abline(data = mareg_h, aes(slope = Slope, intercept = Intercept, color = as.factor(I_S)), size = 1) + 
  theme_cowplot() + scale_color_brewer(palette = "Dark2") + guides(color = guide_legend(title = expression(I[S]))) + 
  xlab('log Observed Handling Time') + ylab('log Predicted Handling Time')

#save_plot(filename = 'Sensitivity_h_plot.png', plot = h_sens_plot,
#          nrow = 1.5, ncol = 1.5, bg = 'white')






