
#time-series figures for figure 3 and SI.

#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot
suppressMessages( library(lubridate))

#data
#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/matrixdata.rda") #data as matrix (resp.matrix, etc.)
load("Data/modeldata.rda") #resp/pred data 
load("Data/lagdata.rda") #lagged data (SEAus.lag, etc.)
load("Data/base_RAMPmodels.rda") #"base" model (e.g., SEmodels)

#functions
source("Functions/modeling_functions.R")
source("Figures/pred_ts_plot_functions.R") #testing new figure automation,
source("Figures/subseason_comparison_functions.R")

#setup
season.weeks <- c(38:52, 1:14)
season.years <- unique(resp.df$year) #TODO: update response df

seasons <- c()
for (i in 1:(length(season.years)-1)) {
  temp_season <- paste0(season.years[i], "-", season.years[i+1])
  #print(temp_season)  
  seasons <- c(seasons, temp_season)
}
rm(i, temp_season)

#group weeks
SE.early <- 38:50
SE.mid <- c(51, 52, 1, 2)
SE.late <- 3:14

#SE models
SE1.lm <- SEmodels[[1]]
SE2.lm <- SEmodels[[2]]
SE3.lm <- SEmodels[[3]]

#model coefs
SE1.coef <- coef(SE1.lm)
SE2.coef <- coef(SE2.lm)
SE3.coef <- coef(SE3.lm)

#season setup
i <- 19  #2019 #loop through as many years as needed

#peak setup
peak_mats <- build_season_mats(SEAus.lag, season.weeks, SE.mid)
y_max_all <- ceiling(max(abs(unlist(peak_mats)), na.rm = TRUE) * 10) / 10
preds     <- extract_season_preds(i,peak_mats )
dates     <- build_season_dates(i, pred.df, season.years, season.weeks, SE.mid)

#output, all built-in the function 
out_dir <- "~/CO_AUS/AusCOmodeling/Figures"

plot_pred_ts_panels(
  season_i = i, 
  preds = preds, 
  dates = dates, 
  seasons = seasons,
  y_max    = y_max_all,
  preds_ord = c("nino", "etio", "wtio", "tsa", "sam", "olr"),
  model_coef = coef(SE2.lm),
  outfile  = file.path(out_dir, paste0("Test_fig3_SE", season.years[i], "pred_peak.png"))
)



#early setup
early_mats <- build_season_mats(SEAus.lag, season.weeks, SE.early)
y_max_all <- ceiling(max(abs(unlist(early_mats)), na.rm = TRUE) * 10) / 10
preds     <- extract_season_preds(i,early_mats )
dates     <- build_season_dates(i, pred.df, season.years, season.weeks, SE.early)

#output, all built-in the function 
out_dir <- "~/CO_AUS/AusCOmodeling/Figures"

plot_pred_ts_panels(
  season_i = i, 
  preds = preds, 
  dates = dates, 
  seasons = seasons,
  y_max    = y_max_all,
  preds_ord = c("nino", "etio", "wtio", "tsa", "sam", "olr"),
  model_coef = coef(SE1.lm),
  outfile  = file.path(out_dir, paste0("Test_fig3_SE", season.years[i], "pred_early.png"))
)


#late setup
late_mats <- build_season_mats(SEAus.lag, season.weeks, SE.late)
y_max_all <- ceiling(max(abs(unlist(late_mats)), na.rm = TRUE) * 10) / 10
preds     <- extract_season_preds(i, late_mats )
dates     <- build_season_dates(i, pred.df, season.years, season.weeks, SE.late)

#output, all built-in the function 
out_dir <- "~/CO_AUS/AusCOmodeling/Figures"

plot_pred_ts_panels(
  season_i = i, 
  preds = preds, 
  dates = dates, 
  seasons = seasons,
  y_max    = y_max_all,
  preds_ord = c("nino", "etio", "wtio", "tsa", "sam", "olr"),
  model_coef = coef(SE3.lm),
  outfile  = file.path(out_dir, paste0("Test_fig3_SE", season.years[i], "pred_late.png"))
)


#Current issues to address everything:
## generalize/automate absolutely everything (specifically the lag_list)
## get a bunch of parameters to tweak everything. so that I can adjust everything as needed.


#predictor sub-season comparison

groups <- list(
   Early = build_group_data(19, SEAus.lag, pred.df, season.years,
                             season.weeks, SE.early,
                             model_coef = coef(SE1.lm)),
    Peak  = build_group_data(19, SEAus.lag, pred.df, season.years,
                             season.weeks, SE.mid,
                             model_coef = coef(SE2.lm)),
    Late  = build_group_data(19, SEAus.lag, pred.df, season.years,
                             season.weeks, SE.late,
                             model_coef = coef(SE3.lm))
)



out_dir <- "~/CO_AUS/AusCOmodeling/Figures"

plot_mode_comparison_panels(
  season_i = 19,
  mode     = "nino",
  groups   = groups,
  main_title = "(a) Ni\u00f1o 3.4 Predictors for 2019-2020 Wildfire Season",
  ylim  = c(-3.2, 3.2),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5400, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0.020,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  lag_label_cex       = 2.0,
  spacer_height = 0,
  outfile  = file.path(out_dir, "fig3a_SE2019_nino_comparison.png")
)
  
  
plot_mode_comparison_panels(
  season_i = 19,
  mode     = "etio",
  groups   = groups,
  main_title = "(b) ETIO Predictors for 2019-2020 Wildfire Season",
  ylim  = c(-3.2, 3.2),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5400, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0.020,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  lag_label_cex       = 2.0,
  spacer_height = 0,
  outfile  = file.path(out_dir, "fig3b_SE2019_etio_comparison.png")
)


plot_mode_comparison_panels(
  season_i = 19,
  mode     = "wtio",
  groups   = groups,
  main_title = "(c) WTIO Predictors for 2019-2020 Wildfire Season",
  ylim  = c(-3.2, 3.2),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5400, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0.020,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  lag_label_cex       = 2.0,
  spacer_height = 0,
  outfile  = file.path(out_dir, "fig3c_SE2019_wtio_comparison.png")
)


out_dir <- "~/CO_AUS/AusCOmodeling/Supporting_Information"

plot_mode_comparison_panels(
  season_i = 19,
  mode     = "tsa",
  groups   = groups,
  main_title = "(a) TSA Predictors for 2019-2020 Wildfire Season",
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5200, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  spacer_height = 0,
  outfile  = file.path(out_dir, "SIfig_SE2019_tsa_comparison.png")
)


plot_mode_comparison_panels(
  season_i = 19,
  mode     = "sam",
  groups   = groups,
  main_title = "(b) SAM Predictors for 2019-2020 Wildfire Season",
  ylab_centered = TRUE,
  ylab     = "Anomaly",
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5200, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  spacer_height = 0,
  outfile  = file.path(out_dir, "SIfig_SE2019_aao_comparison.png")
)

plot_mode_comparison_panels(
  season_i = 19,
  mode     = "olr",
  groups   = groups,
  main_title = "(c) OLR Predictors for 2019-2020 Wildfire Season",
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5200, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  spacer_height = 0,
  outfile  = file.path(out_dir, "SIfig_SE2019_olr_comparison.png")
)




#other years
j <- 15 #2015-2016

#predictor sub-season comparison

groups <- list(
  Early = build_group_data(j, SEAus.lag, pred.df, season.years,
                           season.weeks, SE.early,
                           model_coef = coef(SE1.lm)),
  Peak  = build_group_data(j, SEAus.lag, pred.df, season.years,
                           season.weeks, SE.mid,
                           model_coef = coef(SE2.lm)),
  Late  = build_group_data(j, SEAus.lag, pred.df, season.years,
                           season.weeks, SE.late,
                           model_coef = coef(SE3.lm))
)

out_dir <- "~/CO_AUS/AusCOmodeling/Supporting_Information"

plot_mode_comparison_panels(
  season_i = j,
  mode     = "nino",
  groups   = groups,
  main_title = "(a) Ni\u00f1o 3.4 Predictors for 2015-2016 Wildfire Season",
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5200, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  spacer_height = 0,
  outfile  = file.path(out_dir, "fig3a_SE2015_nino_comparison.png")
)


plot_mode_comparison_panels(
  season_i = j,
  mode     = "etio",
  groups   = groups,
  main_title = "(b) ETIO Predictors for 2015-2016 Wildfire Season",
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5200, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  spacer_height = 0,
  outfile  = file.path(out_dir, "fig3b_SE2015_etio_comparison.png")
)


plot_mode_comparison_panels(
  season_i = j,
  mode     = "wtio",
  groups   = groups,
  main_title = "(c) WTIO Predictors for 2015-2016 Wildfire Season",
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5200, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  spacer_height = 0,
  outfile  = file.path(out_dir, "fig3c_SE2015_wtio_comparison.png")
)



j <- 2 #2002-2003

#predictor sub-season comparison

groups <- list(
  Early = build_group_data(j, SEAus.lag, pred.df, season.years,
                           season.weeks, SE.early,
                           model_coef = coef(SE1.lm)),
  Peak  = build_group_data(j, SEAus.lag, pred.df, season.years,
                           season.weeks, SE.mid,
                           model_coef = coef(SE2.lm)),
  Late  = build_group_data(j, SEAus.lag, pred.df, season.years,
                           season.weeks, SE.late,
                           model_coef = coef(SE3.lm))
)

out_dir <- "~/CO_AUS/AusCOmodeling/Supporting_Information"

plot_mode_comparison_panels(
  season_i = j,
  mode     = "nino",
  groups   = groups,
  main_title = "(a) Ni\u00f1o 3.4 Predictors for 2002-2003 Wildfire Season",
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5200, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  spacer_height = 0,
  outfile  = file.path(out_dir, "fig3a_SE2002_nino_comparison.png")
)


plot_mode_comparison_panels(
  season_i = j,
  mode     = "etio",
  groups   = groups,
  main_title = "(b) ETIO Predictors for 2002-2003 Wildfire Season",
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5200, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  spacer_height = 0,
  outfile  = file.path(out_dir, "fig3b_SE2002_etio_comparison.png")
)


plot_mode_comparison_panels(
  season_i = j,
  mode     = "wtio",
  groups   = groups,
  main_title = "(c) WTIO Predictors for 2002-2003 Wildfire Season",
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5200, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  spacer_height = 0,
  outfile  = file.path(out_dir, "fig3c_SE2002_wtio_comparison.png")
)


