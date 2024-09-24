##################################################################
### Predicting FR parameters for particular field systems
##################################################################

### load packages

library(dplyr); library(ggplot2); library(cowplot); library(lmodel2)

### load data

data <- read.csv('FieldPredatorPrey.csv')

### load forage to get measured a and h values

forage <- read.csv('forage_modified.csv')

colnames(forage)[1] <- 'Dataset'

### get only overlapping studies from forage

forage <- forage %>% filter(Dataset %in% data$Dataset)

### drop most variables from forage

### keep scr and h estimates

forage <- forage %>% select(Dataset, Fitted.a..median.of.BS.samples.,
                            CI.a.low, CI.a.hi, Fittted.h..day.,
                            CI.h.low, CI.h.hi)

### change names of variables in forage

colnames(forage)[2:7] <- c('Fitted_a', 'a_low', 'a_high',
                           'Fitted_h', 'h_low', 'h_high')

### combine forage and data

data <- left_join(data, forage, by = 'Dataset')

### now we need to get the predicted a's and h's from the theory

data <- data %>% mutate(Predicted_a = PredatorMetabolicRate/(PreySize_g * kJpergValue * LowPreyDensity),
                        Predicted_h = (0.9*LowPreyDensity* kJpergValue*PreySize_g)/(PredatorMetabolicRate*HighPreyDensity*(1-0.9)))

### Field predictions of SCR's and Handling times

Field_Attack_Rate <- ggplot(data = data, aes(x = log(Fitted_a), y = log(Predicted_a), color = System)) + 
  geom_point(size = 3) + xlab('log Observed Space\nClearance Rate') + ylab('log Predicted Space\nClearance Rate') + theme_cowplot() + geom_abline(slope = 1, intercept = 0, linetype = 'dashed', linewidth = 1) + 
  scale_color_brewer(palette = 'Dark2') 

legend <- get_legend(Field_Attack_Rate)

Field_Attack_Rate <- Field_Attack_Rate + theme(legend.position = 'none')


Field_Handling_Time <- ggplot(data = data, aes(x = log(Fitted_h), y = log(Predicted_h), color = System)) + 
  geom_point(size = 3) + xlab('log Observed Handling Time') + ylab('log Predicted Handling Time') + theme_cowplot() + geom_abline(slope = 1, intercept = 0, linetype = 'dashed', linewidth = 1) + 
  scale_color_brewer(palette = 'Dark2') 

Field_Handling_Time <- Field_Handling_Time + theme(legend.position = 'none')

### save plots and legend to put together for figure 1

saveRDS(Field_Attack_Rate, file = 'FieldAttackRatePlot.RData')

saveRDS(Field_Handling_Time, file = 'FieldHandlingPlot.RData')

saveRDS(legend, file = 'FieldPlotLegend.RData')
