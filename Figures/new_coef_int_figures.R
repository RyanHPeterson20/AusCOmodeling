#coefficient/interaction plots

## (these plots are unique enough to use their own .R file)

#libraries
suppressMessages(library(grid)) #gridlines between plots
suppressMessages( library(scales)) #for adjusting opacity

#data import
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/base_RAMPmodels.rda") #"base" model (e.g., SEmodels)
load("Data/loyo_models.rda") #leave one year out models/refits
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals) provides: SErefit.new
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data

#load functions
setwd("~/CO_AUS/AusCOmodeling") 
source("Figures/coef_int_plot_new.R") 

#season years/weeks
season.weeks <- c(38:52, 1:14)
season.years <- unique(resp.df$year) #TODO: update response df

seasons <- c()
for (i in 1:(length(season.years)-1)) {
  temp_season <- paste0(season.years[i], "-", season.years[i+1])
  #print(temp_season)  
  seasons <- c(seasons, temp_season)
}
rm(i, temp_season)

#SE models
SE1.lm <- SEmodels[[1]]
SE2.lm <- SEmodels[[2]]
SE3.lm <- SEmodels[[3]]

#NE models
NE1.lm <- NEmodels[[1]]
NE2.lm <- NEmodels[[2]]
NE3.lm <- NEmodels[[3]]

#SE models leave one year out (loyo)
SE.const.LM <- SEmodels.loyo[[2]] #fixed-selection
SE.vary.LM <- SEmodels.loyo[[3]] #withheld-season

#NE models leave one year out (loyo)
NE.const.LM <- NEmodels.loyo[[2]] #fixed-selection
NE.vary.LM <- NEmodels.loyo[[3]] #withheld-season


#extract linear models from constant/varying
SE.const.early <- lapply(SE.const.LM, function(x) x[[1]])
SE.const.peak <- lapply(SE.const.LM, function(x) x[[2]])
SE.const.late <- lapply(SE.const.LM, function(x) x[[3]])

NE.const.early <- lapply(NE.const.LM, function(x) x[[1]])
NE.const.peak <- lapply(NE.const.LM, function(x) x[[2]])
NE.const.late <- lapply(NE.const.LM, function(x) x[[3]])

SE.vary.early <- lapply(SE.vary.LM, function(x) x[[1]])
SE.vary.peak <- lapply(SE.vary.LM, function(x) x[[2]])
SE.vary.late <- lapply(SE.vary.LM, function(x) x[[3]])

NE.vary.early <- lapply(NE.vary.LM, function(x) x[[1]])
NE.vary.peak <- lapply(NE.vary.LM, function(x) x[[2]])
NE.vary.late <- lapply(NE.vary.LM, function(x) x[[3]])



## constant/varying model set-up
const.early <- lapply( SErefit.new[[2]], function(x) x[[1]])
const.peak <- lapply( SErefit.new[[2]], function(x) x[[2]]) 
const.late <- lapply( SErefit.new[[2]], function(x) x[[3]]) 

vary.early <- lapply( SErefit.new[[3]], function(x) x[[1]])
vary.peak <- lapply( SErefit.new[[3]], function(x) x[[2]]) 
vary.late <- lapply( SErefit.new[[3]], function(x) x[[3]]) 

#varying models w/o 2019/2020 (double-withheld)
#TODO: figure out where SErefit.wo2019 comes from (that is, .rda file)
#vary.early.wo2019 <- lapply( SErefit.wo2019[[3]], function(x) x[[1]])
#vary.peak.wo2019 <- lapply( SErefit.wo2019[[3]], function(x) x[[2]]) 
#vary.late.wo2019 <- lapply( SErefit.wo2019[[3]], function(x) x[[3]]) 


## ---- Coeff/Interaction Figures ---- ##
## setup
SE1.coef <- coef(SE1.lm)
SE2.coef <- coef(SE2.lm)
SE3.coef <- coef(SE3.lm)

NE1.coef <- coef(NE1.lm)
NE2.coef <- coef(NE2.lm)
NE3.coef <- coef(NE3.lm)

#Figure 2 corrections 
#TODO:
## increase the size of all text and numbers
## temp notation:   if (is.null(oma)) oma <- c(1.05, 1.25, if (!is.null(main_title)) 0.75 else 0.5, 0.25)

#cex setup (font/num size)
cex.main <- 2.6 #figure title
cex.axis <- 2.25 #axis number
cex.var.label <- 2.13 #climate mode predictor labels
cex.lag.label <- 1.85
cex.y.label <- 1.97
cex.int.label <- 1.85

cex.main.pt <- 2.5
cex.int.pt <- 2.5


#Figure 2 output:
i <- 19 #2019/2020 Withheld

setwd("~/CO_AUS/AusCOmodeling/Figures")

#fig 2b early-season
coefs1 <- list(
  base  = SE1.coef,
  #const = coef(SE.const.early[[i]]), 
  vary  = coef(SE.vary.early[[i]])  
)

png(filename = paste0("Fig2b_SEcoefint_early_", season.years[i], ".png"),  width = 2050, height = 4150, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino", "etio", "wtio", "tsa", "aao", "olr"),  # include OLR panel
  oma = c(1.25, 1.80, 0.75, 0.25),
  layout_widths = c(1.95, 1.30),
  y_axis_at = c(-4, 0, 4),
  y_axis_las = 1,
  half_ticks_y = TRUE, 
  int_axis_at = c(-2, 0, 2),
  coef_range_int = c(-3, 3),
  half_ticks_int = TRUE, 
  cex_axis = cex.axis,
  cex_lab_lag = cex.lag.label,
  cex_lab_y = cex.y.label, 
  cex_lab_int = cex.int.label, 
  ylab_left = "Main Coefficients",
  xlab_coef = "Interaction",
  xlab_coef2 = "Coefficients",
  xlab_coef2_line_gap = 2.0, 
  cex_var_label = cex.var.label,
  var_label_pos = 0.05, 
  cex_pt = cex.main.pt,
  cex_pt_int = cex.int.pt,
  lwd = 2.5,
  lty_ref = 1, 
  lwd_ref = 0.5, 
  coef_range = c(-5, 5),
  main_title = paste0("(b) Early (2019/2020 Withheld)"),
  title_line = -1,
  cex_main = cex.main,
  quad_y_jitter = 0.004,
  int_y_jitter = 0.003,
  int_x_jitter = 0.003,
  auto_jitter = TRUE,
  auto_jitter_y = 0.05,
  auto_int_x_jitter = TRUE,
  auto_int_x_nudge = 0.10,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange3"),
  model_lty  = c(base=1, const=2, vary=3),
  add_legends = FALSE)
dev.off()



#fig 2c peak
coefs2 <- list(
  base  = SE2.coef,
  #const = coef(SE.const.peak[[i]]), 
  vary  = coef(SE.vary.peak[[i]])  
)

png(filename = paste0("Fig2c_SEcoefint_peak_", season.years[i], ".png"),  width = 2000, height = 4150, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino", "etio", "wtio", "tsa", "aao", "olr"),  # include OLR panel
  oma = c(1.25, 1.25, 0.75, 0.25),
  layout_widths = c(1.95, 1.30),
  y_axis_at = c(-4, 0, 4),
  y_axis_las = 1,
  half_ticks_y = TRUE, 
  int_axis_at = c(-2, 0, 2),
  coef_range_int = c(-3, 3),
  half_ticks_int = TRUE, 
  cex_axis = cex.axis,
  cex_lab_lag = cex.lag.label,
  cex_lab_y = cex.y.label, 
  cex_lab_int = cex.int.label, 
  ylab_left = "",
  xlab_coef = "Interaction",
  xlab_coef2 = "Coefficients",
  xlab_coef2_line_gap = 2.0, 
  cex_var_label = cex.var.label,
  var_label_pos = 0.05, 
  cex_pt = cex.main.pt,
  cex_pt_int = cex.int.pt,
  lwd = 2.5,
  lty_ref = 1, 
  lwd_ref = 0.5, 
  coef_range = c(-5, 5),
  main_title = paste0("(c) Peak (2019/2020 Withheld)"), 
  title_line = -1,
  cex_main = cex.main,
  quad_y_jitter = 0.004,
  int_y_jitter = 0.003,
  int_x_jitter = 0.003,
  auto_jitter = TRUE,
  auto_jitter_y = 0.05,
  auto_int_x_jitter = TRUE,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange3"),
  model_lty  = c(base=1, const=2, vary=3),
  add_legends = TRUE, 
  legend_terms_pt_cex   = c(2.75, 2.5, 2.5, 2.75, 2.75, 2.75, 2.5),
  legend_pos_terms = "bottomright",
  legend_pos_model = "bottomright",
  legend_inset_terms = c(0.000, 0.0),
  legend_inset_model = c(0.00, 0.215),
  legend_x_intersp_terms  = 2.65,
  legend_x_intersp_model = 1.55,
  legend_cex_terms = 2.33,
  legend_cex_model = 2.40,
  legend_models = c("All-Data", "Withheld-\nSeason"),
  legend_model_keys = c("base", "vary"))
dev.off()



#fig 2d late-season
coefs3 <- list(
  base  = SE3.coef,
  #const = coef(SE.const.late[[i]]), 
  vary  = coef(SE.vary.late[[i]])  
)

png(filename = paste0("Fig2d_SEcoefint_late_", season.years[i], ".png"),  width = 2000, height = 4150, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino", "etio", "wtio", "tsa", "aao", "olr"),  # include OLR panel
  oma = c(1.25, 1.25, 0.75, 0.25),
  layout_widths = c(1.95, 1.30),
  y_axis_at = c(-4, 0, 4),
  y_axis_las = 1,
  half_ticks_y = TRUE, 
  int_axis_at = c(-2, 0, 2),
  coef_range_int = c(-3, 3),
  half_ticks_int = TRUE, 
  cex_axis = cex.axis,
  cex_lab_lag = cex.lag.label,
  cex_lab_y = cex.y.label, 
  cex_lab_int = cex.int.label, 
  ylab_left = "",
  xlab_coef = "Interaction",
  xlab_coef2 = "Coefficients",
  xlab_coef2_line_gap = 2.0, 
  cex_var_label = cex.var.label,
  cex_pt = cex.main.pt,
  cex_pt_int = cex.int.pt,
  var_label_pos = 0.05, 
  lwd = 2.5,
  lty_ref = 1, 
  lwd_ref = 0.5, 
  coef_range = c(-5, 5),
  main_title = paste0("(d) Late (2019/2020 Withheld)"),   
  title_line = -1,
  cex_main = cex.main,
  quad_y_jitter = 0.004,
  int_y_jitter = 0.003,
  int_x_jitter = 0.003,
  auto_jitter = TRUE,
  auto_jitter_y = 0.05,
  auto_int_x_jitter = TRUE,
  auto_int_x_nudge = 0.10,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange3"),
  model_lty  = c(base=1, const=2, vary=3),
  add_legends = FALSE, 
  legend_inset_terms = c(0.000, 0.05),
  legend_inset_model = c(0.00, 0.00),
  legend_cex_terms = 1.35,
  legend_cex_model = 1.25,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()






#SI figure with all three model variants
i <- 19 #2019/2020 Withheld


setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures")

#fig 2b early-season
coefs1 <- list(
  base  = SE1.coef,
  const = coef(SE.const.early[[i]]), 
  vary  = coef(SE.vary.early[[i]])  
)

png(filename = paste0("Fig2_SEcoefint_early_", season.years[i], ".png"),  width = 2050, height = 4050, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino", "etio", "wtio", "tsa", "aao", "olr"),  # include OLR panel
  layout_widths = c(2.00, 1.25),
  y_axis_at = c(-4,-2, 0, 2, 4),
  int_axis_at = c(-2, 0, 2),
  coef_range_int = c(-3, 3),
  cex_axis = 1.40,
  cex_lab = 1.75,
  ylab_left = "Main Coefficients",
  xlab_coef = "Interaction Coefficients",
  cex_var_label = 1.45,
  var_label_pos = 1, 
  lty_ref = 1, 
  lwd_ref = 0.5, 
  coef_range = c(-5, 5),
  main_title = paste0("(b) Early (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  int_y_jitter = 0.003,
  int_x_jitter = 0.003,
  auto_jitter = TRUE,
  auto_jitter_y = 0.05,
  auto_int_x_jitter = TRUE,
  auto_int_x_nudge = 0.10,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange3"),
  model_lty  = c(base=2, const=2, vary=6),
  add_legends = FALSE)
dev.off()


#fig 2c peak
coefs2 <- list(
  base  = SE2.coef,
  const = coef(SE.const.peak[[i]]), 
  vary  = coef(SE.vary.peak[[i]])  
)

png(filename = paste0("Fig2_SEcoefint_peak_", season.years[i], ".png"),  width = 2050, height = 4050, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino", "etio", "wtio", "tsa", "aao", "olr"),  # include OLR panel
  layout_widths = c(2.00, 1.25),
  y_axis_at = c(-4,-2, 0, 2, 4),
  int_axis_at = c(-2, 0, 2),
  coef_range_int = c(-3, 3),
  cex_axis = 1.40,
  cex_lab = 1.75,
  ylab_left = "",
  xlab_coef = "Interaction Coefficients",
  cex_var_label = 1.45,
  var_label_pos = 1, 
  lty_ref = 1, 
  lwd_ref = 0.5, 
  coef_range = c(-5, 5),
  main_title = paste0("(c) Peak (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  int_y_jitter = 0.003,
  int_x_jitter = 0.003,
  auto_jitter = TRUE,
  auto_jitter_y = 0.05,
  auto_int_x_jitter = TRUE,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange3"),
  model_lty  = c(base=2, const=2, vary=6),
  add_legends = FALSE)
dev.off()

#fig 2d late-season
coefs3 <- list(
  base  = SE3.coef,
  const = coef(SE.const.late[[i]]), 
  vary  = coef(SE.vary.late[[i]])  
)

png(filename = paste0("Fig2_SEcoefint_late_", season.years[i], ".png"),  width = 2050, height = 4050, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino", "etio", "wtio", "tsa", "aao", "olr"),  # include OLR panel
  layout_widths = c(2.00, 1.25),
  y_axis_at = c(-4,-2, 0, 2, 4),
  int_axis_at = c(-2, 0, 2),
  coef_range_int = c(-3, 3),
  cex_axis = 1.40,
  cex_lab = 1.75,
  ylab_left = "",
  xlab_coef = "Interaction Coefficients",
  cex_var_label = 1.45,
  var_label_pos = 1, 
  lty_ref = 1, 
  lwd_ref = 0.5, 
  coef_range = c(-5, 5),
  main_title = paste0("(d) Late (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  int_y_jitter = 0.003,
  int_x_jitter = 0.003,
  auto_jitter = TRUE,
  auto_jitter_y = 0.05,
  auto_int_x_jitter = TRUE,
  auto_int_x_nudge = 0.10,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange3"),
  model_lty  = c(base=2, const=2, vary=6),
  add_legends = TRUE, 
  legend_inset_terms = c(0.000, 0.05),
  legend_inset_model = c(0.00, 0.00),
  legend_cex_terms = 1.35,
  legend_cex_model = 1.25,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()





#TODO: below is testing/older code that will be finalized above. 
#testing new functions for coef_interaction plot
#test output with SE aus 2019/2020
i <- 19

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/SEAus")

coefs1 <- list(
  base  = SE1.coef,
  #const = coef(SE.const.early[[i]]), 
  vary  = coef(SE.vary.early[[i]])  
)

png(filename = paste0("SI_SEcoefs_early_", season.years[i], "_new.png"),  width = 2200, height = 3950, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  coef_range_int = c(-3, 3),
  cex_lab = 1.5,
  cex_var_label = 1.25,
  var_label_pos = 1.5, 
  lty_ref = 1, 
  lwd_ref = 0.5, 
  vars_order = c("nino","etio", "wtio", "tsa","aao", "olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = paste0("(a) Early-Season (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  int_y_jitter = 0.003,
  int_x_jitter = 0.003,
  auto_jitter = TRUE,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
  model_lty  = c(base=2, const=2, vary=6),
  add_legends = TRUE,
  legend_inset_terms = c(0.000, 0.05),
  legend_inset_model = c(0.00, 0.00),
  legend_cex_terms = 1.35,
  legend_cex_model = 1.25,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()



setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/SEAus")

coefs2 <- list(
  base  = SE2.coef,
  #const = coef(SE.const.peak[[i]]), 
  vary  = coef(SE.vary.peak[[i]])  
)


png(filename = paste0("SI_SEcoefs_peak_", season.years[i], "_new.png"),  width = 2200, height = 3950, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  coef_range_int = c(-3, 3),
  cex_lab = 1.5,
  cex_var_label = 1.25,
  var_label_pos = 1.5, 
  lty_ref = 1, 
  lwd_ref = 0.5, 
  vars_order = c("nino","etio", "wtio", "tsa","aao", "olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = paste0("(b) Peak-Season (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  int_y_jitter = 0.003,
  int_x_jitter = 0.000,
  auto_jitter = TRUE,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
  model_lty  = c(base=2, const=2, vary=5),
  add_legends = TRUE,
  legend_inset_terms = c(0.000, 0.05),
  legend_inset_model = c(0.00, 0.00),
  legend_cex_terms = 1.35,
  legend_cex_model = 1.25,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()




setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/SEAus")

coefs3 <- list(
  base  = SE3.coef,
  #const = coef(SE.const.late[[i]]), 
  vary  = coef(SE.vary.late[[i]])  
)

png(filename = paste0("SI_SEcoefs_late_", season.years[i], "_new.png"),  width = 2250, height = 3850, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  coef_range_int = c(-3, 3),
  cex_lab = 1.5,
  cex_var_label = 1.25,
  var_label_pos = 1.5, 
  lty_ref = 1, 
  lwd_ref = 0.5, 
  vars_order = c("nino","etio", "wtio", "tsa","aao", "olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = paste0("(c) Late-Season (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  int_y_jitter = 0.003,
  int_x_jitter = 0.004,
  auto_jitter = TRUE,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
  model_lty  = c(base=2, const=6, vary=5),
  add_legends = TRUE,
  legend_inset_terms = c(0.000, 0.05),
  legend_inset_model = c(0.00, 0.00),
  legend_cex_terms = 1.35,
  legend_cex_model = 1.25,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()


# NE Aus plots
i <- 19

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/NEAus")

coefs1 <- list(
  base  = NE1.coef,
  const = coef(NE.const.early[[i]]), 
  vary  = coef(NE.vary.early[[i]])  
)

png(filename = paste0("SI_NEcoefs_early_", season.years[i], "_new.png"),  width = 2200, height = 3950, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  coef_range_int = c(-3, 3),
  cex_lab = 1.5,
  cex_var_label = 1.25,
  var_label_pos = 1.5, 
  lty_ref = 1, 
  lwd_ref = 0.5, 
  vars_order = c("nino","etio", "wtio", "tsa","aao", "olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = paste0("(a) Early-Season (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  int_y_jitter = 0.003,
  int_x_jitter = 0.003,
  auto_jitter = TRUE,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
  model_lty  = c(base=2, const=6, vary=5),
  add_legends = TRUE,
  legend_inset_terms = c(0.000, 0.05),
  legend_inset_model = c(0.00, 0.00),
  legend_cex_terms = 1.35,
  legend_cex_model = 1.25,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()


#NE peak
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/NEAus")

coefs2 <- list(
  base  = NE2.coef,
  const = coef(NE.const.peak[[i]]), 
  vary  = coef(NE.vary.peak[[i]])  
)


png(filename = paste0("SI_NEcoefs_peak_", season.years[i], "_new.png"),  width = 2200, height = 3950, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  coef_range_int = c(-3, 3),
  cex_lab = 1.5,
  cex_var_label = 1.25,
  var_label_pos = 1.5, 
  lty_ref = 1, 
  lwd_ref = 0.5, 
  vars_order = c("nino","etio", "wtio", "tsa","aao", "olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = paste0("(b) Peak-Season (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  int_y_jitter = 0.003,
  int_x_jitter = 0.000,
  auto_jitter = TRUE,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
  model_lty  = c(base=2, const=6, vary=5),
  add_legends = TRUE,
  legend_inset_terms = c(0.000, 0.05),
  legend_inset_model = c(0.00, 0.00),
  legend_cex_terms = 1.35,
  legend_cex_model = 1.25,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()



#NE late
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/NEAus")

coefs3 <- list(
  base  = NE3.coef,
  #const = coef(NE.const.late[[i]]), 
  vary  = coef(NE.vary.late[[i]])  
)

png(filename = paste0("SI_NEcoefs_late_", season.years[i], "_new.png"),  width = 2250, height = 3950, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  coef_range_int = c(-3, 3),
  cex_lab = 1.5,
  cex_var_label = 1.25,
  var_label_pos = 1.5, 
  lty_ref = 1, 
  lwd_ref = 0.5, 
  vars_order = c("nino","etio", "wtio", "tsa","aao", "olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = paste0("(c) Late-Season (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  int_y_jitter = 0.003,
  int_x_jitter = 0.004,
  auto_jitter = TRUE,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
  model_lty  = c(base=2, const=6, vary=5),
  add_legends = TRUE,
  legend_inset_terms = c(0.000, 0.05),
  legend_inset_model = c(0.00, 0.00),
  legend_cex_terms = 1.35,
  legend_cex_model = 1.25,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()



#TODO: update (or delete) everything below; potentially 

#early models
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/Coef_Int")
for (i in 1:20) {
  
  coefs1 <- list(
    base  = SE1.coef,
    #const = coef(const.early[[i]]), 
    vary  = coef(vary.early[[i]])  
  )
  
  png(filename = paste0("SI_SEcoefs_early_", season.years[i], ".png"),  width = 2750, height = 4750, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs1,
    cex_num = 1.5,
    cex_label = 1.75,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Early Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
    add_legends = FALSE,
    legend_inset_terms = c(0.000, 0.00),
    legend_inset_model = c(0.00, 0.16),
    legend_cex_terms = 1.80,
    legend_cex_model = 1.50,
    legend_models = c("All-Data", "Withheld-Season"),
    legend_model_keys = c("base", "vary"))
  dev.off()

}


#peak models
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/Coef_Int")
for (i in 1:20) {
  coefs2 <- list(
    base  = SE2.coef,
    #const = coef(vary.peak.wo2019[[i]]), 
    vary  = coef(vary.peak[[i]])  
  )
  
  png(filename = paste0("SI_SEcoefs_peak_", season.years[i], ".png"),  width = 2750, height = 4750, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs2,
    cex_num = 1.5,
    cex_label = 1.75,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Peak Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
    add_legends = FALSE,
    legend_inset_terms = c(0.000, 0.00),
    legend_inset_model = c(0.00, 0.16),
    legend_cex_terms = 1.80,
    legend_cex_model = 1.50,
    legend_models = c("All-Data", "Withheld-Season"),
    legend_model_keys = c("base", "vary"))
  dev.off()
}


#late models
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/Coef_Int")
for (i in 1:20) {
  
  coefs3 <- list(
    base  = SE3.coef,
    #const = coef(vary.late.wo2019[[i]]), 
    vary  = coef(vary.late[[i]])  
  )
  
  png(filename = paste0("SI_SEcoefs_late_", season.years[i], ".png"),  width = 2750, height = 4750, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs3,
    cex_num = 1.5,
    cex_label = 1.75,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Late Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
    add_legends = TRUE,
    legend_inset_terms = c(0.000, 0.05),
    legend_inset_model = c(0.00, 0.00),
    legend_cex_terms = 1.80,
    legend_cex_model = 1.44,
    legend_models = c("All-Data", "Withheld-Season"),
    legend_model_keys = c("base", "vary"))
  dev.off()
}



#full 2019/2020 output



i <- 19
coefs1 <- list(
  base  = SE1.coef,
  #const = coef(const.early[[i]]), 
  vary  = coef(vary.early[[i]])  
)

coefs2 <- list(
  base  = SE2.coef,
  #const = coef(vary.peak.wo2019[[i]]), 
  vary  = coef(vary.peak[[i]])  
)

coefs3 <- list(
  base  = SE3.coef,
  #const = coef(vary.late.wo2019[[i]]), 
  vary  = coef(vary.late[[i]])  
)


#early models
setwd("~/CO_AUS/AusCOmodeling/Figures")

png(filename = paste0("SI_SEcoefs_early_", season.years[i], ".png"),  width = 2200, height = 5100, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  cex_num = 1.65,
  cex_label = 2.0,
  cex_subtitle = 1.5,
  vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = paste0("(b) Early-Season (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
  add_legends = FALSE,
  legend_inset_terms = c(0.000, 0.00),
  legend_inset_model = c(0.00, 0.16),
  legend_cex_terms = 1.80,
  legend_cex_model = 1.50,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()

png(filename = paste0("SI_SEcoefs_peak_", season.years[i], ".png"),  width = 2200, height = 5100, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  cex_num = 1.65,
  cex_label = 2.0,
  cex_subtitle = 1.5,
  vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = paste0("(c) Peak-Season (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
  add_legends = FALSE,
  legend_inset_terms = c(0.000, 0.00),
  legend_inset_model = c(0.00, 0.16),
  legend_cex_terms = 1.30,
  legend_cex_model = 1.30,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()

png(filename = paste0("SI_SEcoefs_late_", season.years[i], ".png"),  width = 2200, height = 5100, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  cex_num = 1.65,
  cex_label = 2.0,
  cex_subtitle = 1.5,
  vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = paste0("(d) Late-Season (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
  add_legends = TRUE,
  legend_inset_terms = c(0.000, 0.05),
  legend_inset_model = c(0.00, 0.00),
  legend_cex_terms = 1.35,
  legend_cex_model = 1.25,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()



#NE Aus models - SI figures
#early
i <- 19
coefs1 <- list(
  base  = NE1.coef,
  #const = coef(const.early[[i]]), 
  vary  = coef(NE.vary.early[[i]])  
)

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/NEAus")

png(filename = paste0("SI_NEcoefs_early_", season.years[i], ".png"),  width = 2200, height = 4200, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  cex_num = 1.65,
  cex_label = 2.0,
  cex_subtitle = 1.5,
  vars_order = c("nino", "etio", "wtio", "tsa","aao", "olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = paste0("(b) Early-Season (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
  add_legends = FALSE,
  legend_inset_terms = c(0.000, 0.00),
  legend_inset_model = c(0.00, 0.16),
  legend_cex_terms = 1.80,
  legend_cex_model = 1.50,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()



#peak
i <- 19
coefs2 <- list(
  base  = NE2.coef,
  #const = coef(const.early[[i]]), 
  vary  = coef(NE.vary.peak[[i]])  
)

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/NEAus")

png(filename = paste0("SI_NEcoefs_peak_", season.years[i], ".png"),  width = 2200, height = 5100, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  cex_num = 1.65,
  cex_label = 2.0,
  cex_subtitle = 1.5,
  vars_order = c("nino","etio", "wtio", "tsa","aao", "olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = paste0("(c) Peak-Season (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
  add_legends = FALSE,
  legend_inset_terms = c(0.000, 0.00),
  legend_inset_model = c(0.00, 0.16),
  legend_cex_terms = 1.30,
  legend_cex_model = 1.30,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()



#late
i <- 19
coefs3 <- list(
  base  = NE3.coef,
  #const = coef(const.early[[i]]), 
  vary  = coef(NE.vary.late[[i]])  
)

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/NEAus")
png(filename = paste0("SI_NEcoefs_late_", season.years[i], ".png"),  width = 2200, height = 4100, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  coef_range_int = c(-1.5, 1.5),
  cex_num = 1.65,
  cex_label = 2.0,
  cex_subtitle = 1.5,
  vars_order = c("nino","etio", "wtio", "tsa","aao", "olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = paste0("(d) Late-Season (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  int_y_jitter = 0.002,
  int_x_jitter = 0.000,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
  model_lty  = c(base=2, const=2, vary=5),
  add_legends = TRUE,
  legend_inset_terms = c(0.000, 0.05),
  legend_inset_model = c(0.00, 0.00),
  legend_cex_terms = 1.35,
  legend_cex_model = 1.25,
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()







#varying models w.o 2019/2020
#early models
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp/New")
for (i in 1:20) {
  coefs1 <- list(
    base  = SE1.coef,
    const = coef(vary.early.wo2019[[i]]), 
    vary  = coef(vary.early[[i]])  
  )
  
  png(filename = paste0("new_SEcoefs_early_", season.years[i], ".png"), width = 2400, height = 4400, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs1,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Early Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="royalblue3", vary="darkorange2"))
  dev.off()
}


#peak models
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp/New")
for (i in 1:20) {
  coefs2 <- list(
    base  = SE2.coef,
    const = coef(vary.peak.wo2019[[i]]), 
    vary  = coef(vary.peak[[i]])  
  )
  
  png(filename = paste0("new_SEcoefs_peak_", season.years[i], ".png"), width = 2400, height = 4400, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs2,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Peak Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="royalblue3", vary="darkorange2"))
  dev.off()
}


#late models
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp/New")
for (i in 1:20) {
  coefs3 <- list(
    base  = SE3.coef,
    const = coef(vary.late.wo2019[[i]]), 
    vary  = coef(vary.late[[i]])  
  )
  
  png(filename = paste0("new_SEcoefs_late_", season.years[i], ".png"), width = 2400, height = 4400, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs3,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Late Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="royalblue3", vary="darkorange2"))
  dev.off()
}




#varying models 
#early models
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
for (i in 1:20) {
  coefs1 <- list(
    base  = SE1.coef,
    #const = coef(vary.early.wo2019[[i]]), 
    vary  = coef(vary.early[[i]])  
  )
  
  png(filename = paste0("SEcoefs_early_", season.years[i], ".png"), width = 2400, height = 4400, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs1,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Early Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"))
  dev.off()
}


#peak models
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
for (i in 1:20) {
  coefs2 <- list(
    base  = SE2.coef,
    #const = coef(vary.peak.wo2019[[i]]), 
    vary  = coef(vary.peak[[i]])  
  )
  
  png(filename = paste0("SEcoefs_peak_", season.years[i], ".png"), width = 2400, height = 4400, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs2,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Peak Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"))
  dev.off()
}


#late models
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
for (i in 1:20) {
  coefs3 <- list(
    base  = SE3.coef,
    #const = coef(vary.late.wo2019[[i]]), 
    vary  = coef(vary.late[[i]])  
  )
  
  png(filename = paste0("SEcoefs_late_", season.years[i], ".png"), width = 2400, height = 4400, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs3,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Late Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"))
  dev.off()
}





#2001/2002
#early 
SE1.constcoef <- coef(SE.const.LM$`2001-2002`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2001-2002`[[1]])

i <- 1

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2001.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2001/2002 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2001-2002`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2001-2002`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2001.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2001/2002 Withheld)",
  quad_y_jitter = 0.005)
dev.off()

#late
SE3.constcoef <- coef(SE.const.LM$`2001-2002`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2001-2002`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2001.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2001/2002 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2002/2003
#early 
SE1.constcoef <- coef(SE.const.LM$`2002-2003`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2002-2003`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2002.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2002/2003 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2002-2003`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2002-2003`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2002.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2002/2003 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2002-2003`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2002-2003`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2002.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2002/2003 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2003/2004
#early 
SE1.constcoef <- coef(SE.const.LM$`2003-2004`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2003-2004`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2003.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2003/2004 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2003-2004`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2003-2004`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2003.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2003/2004 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2003-2004`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2003-2004`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2003.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2003/2004 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2004/2005
#early 
SE1.constcoef <- coef(SE.const.LM$`2004-2005`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2004-2005`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2004.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2004/2005 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2004-2005`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2004-2005`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2004.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2004/2005 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2004-2005`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2004-2005`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2004.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2004/2005 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2005/2006
#early 
SE1.constcoef <- coef(SE.const.LM$`2005-2006`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2005-2006`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2005.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2005/2006 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2005-2006`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2005-2006`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2005.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2005/2006 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2005-2006`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2005-2006`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2005.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2005/2006 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2006/2007
#early 
SE1.constcoef <- coef(SE.const.LM$`2006-2007`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2006-2007`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2006.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2006/2007 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2006-2007`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2006-2007`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2006.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2006/2007 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2006-2007`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2006-2007`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2006.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2006/2007 Withheld)",
  quad_y_jitter = 0.005)
dev.off()



#2007/2008
#early 
SE1.constcoef <- coef(SE.const.LM$`2007-2008`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2007-2008`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2007.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2007/2008 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2007-2008`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2007-2008`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2007.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2007/2008 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2007-2008`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2007-2008`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2007.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2007/2008 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2008/2009
#early 
SE1.constcoef <- coef(SE.const.LM$`2008-2009`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2008-2009`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2008.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2008/2009 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2008-2009`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2008-2009`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2008.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2008/2009 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2008-2009`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2008-2009`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2008.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2008/2009 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2009/2010
#early 
SE1.constcoef <- coef(SE.const.LM$`2009-2010`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2009-2010`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2009.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2009/2010 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2009-2010`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2009-2010`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2009.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2009/2010 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2009-2010`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2009-2010`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2009.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2009/2010 Withheld)",
  quad_y_jitter = 0.005)
dev.off()



#2010/2011
#early 
SE1.constcoef <- coef(SE.const.LM$`2010-2011`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2010-2011`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2010.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2010/2011 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2010-2011`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2010-2011`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2010.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2010/2011 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2010-2011`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2010-2011`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2010.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2010/2011 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2011/2012
#early 
SE1.constcoef <- coef(SE.const.LM$`2011-2012`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2011-2012`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2011.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2011/2012 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2011-2012`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2011-2012`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2011.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2011/2012 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2011-2012`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2011-2012`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2011.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2011/2012 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2012/2013
#early 
SE1.constcoef <- coef(SE.const.LM$`2012-2013`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2012-2013`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2012.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2012/2013 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2012-2013`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2012-2013`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2012.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2012/2013 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2012-2013`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2012-2013`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2012.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2012/2013 Withheld)",
  quad_y_jitter = 0.005)
dev.off()

#2013/2014
#early 
SE1.constcoef <- coef(SE.const.LM$`2013-2014`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2013-2014`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2013.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2013/2014 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2013-2014`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2013-2014`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2013.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2013/2014 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2013-2014`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2013-2014`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2013.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2013/2014 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2014/2015
#early 
SE1.constcoef <- coef(SE.const.LM$`2014-2015`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2014-2015`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2014.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2014/2015 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2014-2015`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2014-2015`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2014.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2014/2015 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2014-2015`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2014-2015`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2014.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2014/2015 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2015/2016
#early 
SE1.constcoef <- coef(SE.const.LM$`2015-2016`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2015-2016`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2015.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2015/2016 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2015-2016`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2015-2016`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2015.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2015/2016 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2015-2016`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2015-2016`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2015.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2015/2016 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2016/2017
#early 
SE1.constcoef <- coef(SE.const.LM$`2016-2017`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2016-2017`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2016.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2016/2017 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2016-2017`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2016-2017`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2016.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2016/2017 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2016-2017`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2016-2017`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2016.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2016/2017 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2017/2018
#early 
SE1.constcoef <- coef(SE.const.LM$`2017-2018`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2017-2018`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2017.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2017/2018 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2017-2018`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2017-2018`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2017.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2017/2018 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2017-2018`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2017-2018`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2017.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2017/2018 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2018/2019
#early 
SE1.constcoef <- coef(SE.const.LM$`2018-2019`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2018-2019`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2018.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2018/2019 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2018-2019`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2018-2019`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2018.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2018/2019 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2018-2019`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2018-2019`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2018.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2018/2019 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2019/2020
#early 
SE1.constcoef <- coef(SE.const.LM$`2019-2020`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2019-2020`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2019.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2019/2020 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2019-2020`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2019-2020`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2019.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2019/2020 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2019-2020`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2019-2020`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2019.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2019/2020 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2020/2021
#early 
SE1.constcoef <- coef(SE.const.LM$`2020-2021`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2020-2021`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2020.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2020/2021 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2020-2021`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2020-2021`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2020.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2020/2021 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2020-2021`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2020-2021`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2020.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2020/2021 Withheld)",
  quad_y_jitter = 0.005)
dev.off()




###

