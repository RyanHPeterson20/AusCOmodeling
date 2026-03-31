
#SI figures for coef-int 


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


#SE models leave one year out (loyo)
SE.const.LM <- SEmodels.loyo[[2]] #fixed-selection
SE.vary.LM <- SEmodels.loyo[[3]] #withheld-season


#extract linear models from constant/varying
SE.const.early <- lapply(SE.const.LM, function(x) x[[1]])
SE.const.peak <- lapply(SE.const.LM, function(x) x[[2]])
SE.const.late <- lapply(SE.const.LM, function(x) x[[3]])

SE.vary.early <- lapply(SE.vary.LM, function(x) x[[1]])
SE.vary.peak <- lapply(SE.vary.LM, function(x) x[[2]])
SE.vary.late <- lapply(SE.vary.LM, function(x) x[[3]])


## constant/varying model set-up
const.early <- lapply( SErefit.new[[2]], function(x) x[[1]])
const.peak <- lapply( SErefit.new[[2]], function(x) x[[2]]) 
const.late <- lapply( SErefit.new[[2]], function(x) x[[3]]) 

vary.early <- lapply( SErefit.new[[3]], function(x) x[[1]])
vary.peak <- lapply( SErefit.new[[3]], function(x) x[[2]]) 
vary.late <- lapply( SErefit.new[[3]], function(x) x[[3]]) 


## ---- Coeff/Interaction Figures ---- ##
## setup
SE1.coef <- coef(SE1.lm)
SE2.coef <- coef(SE2.lm)
SE3.coef <- coef(SE3.lm)



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

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures")


#fig 2b early-season
coefs1 <- list(
  base  = SE1.coef,
  const = coef(SE.const.early[[i]]), 
  vary  = coef(SE.vary.early[[i]])  
)

png(filename = paste0("SI_Fig4a_SEcoefint_early_", season.years[i], ".png"),  width = 2050, height = 4150, res = 300)
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
  model_cols = c(base= "forestgreen", const="magenta3", vary= "darkorange3"),
  model_lty  = c(base=1, const=2, vary=3),
  add_legends = FALSE)
dev.off()




#fig 2c peak
coefs2 <- list(
  base  = SE2.coef,
  const = coef(SE.const.peak[[i]]), 
  vary  = coef(SE.vary.peak[[i]])  
)

png(filename = paste0("SI_Fig4b_SEcoefint_peak_", season.years[i], ".png"),  width = 2000, height = 4150, res = 300)
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
  model_cols = c(base="forestgreen", const="magenta3", vary="darkorange3"),
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
  legend_models = c("All-Data", "Fixed-\nSelection", "Withheld-\nSeason"),
  legend_model_keys = c("base", "const", "vary"))
dev.off()




#fig 2d late-season
coefs3 <- list(
  base  = SE3.coef,
  const = coef(SE.const.late[[i]]), 
  vary  = coef(SE.vary.late[[i]])  
)

png(filename = paste0("SI_Fig4c_SEcoefint_late_", season.years[i], ".png"),  width = 2000, height = 4150, res = 300)
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
  model_cols = c(base="forestgreen", const="magenta3", vary="darkorange3"),
  model_lty  = c(base=1, const=2, vary=3),
  add_legends = FALSE)
dev.off()




