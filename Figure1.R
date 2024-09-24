################################################################################
### Code  for Figure 1
################################################################################

### load packages 

library(dplyr); library(ggplot2); library(cowplot);

### load plots that we want to combine

h_pred_plot <- readRDS('h_Prediction_Plot.RData')

a_pred_plot <- readRDS('SCR_Prediction_Plot.RData')

field_a <- readRDS('FieldAttackRatePlot.RData')

field_h <- readRDS('FieldHandlingPlot.RData')

field_legend <- readRDS('FieldPlotLegend.RData')

### put plots together 

# a and h prediction on first row

a_h_predplots <- plot_grid(a_pred_plot, h_pred_plot, nrow = 1, labels = c('A', 'B'),
                           axis = 'tblr', align = 'hv')

field_a_h <- plot_grid(field_a, field_h, nrow = 1, axis = 'tblr', align = 'hv', labels = c('C', 'D'))

field_wlegend <- plot_grid(field_a_h, field_legend, nrow = 1, rel_widths = c(1,0.25))

together <- plot_grid(a_h_predplots, field_wlegend, nrow = 2, ncol = 1)

save_plot(filename = 'Figure_1_prediction.png', plot = together, nrow = 1.7, ncol = 1.6, bg = 'white')

