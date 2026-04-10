
#pred term times series for a given season


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
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)

#functions
source("Functions/modeling_functions.R")
source("Figures/subseason_comparison_functions.R")

#setup
season.weeks <- c(38:52, 1:14)
season.years <- unique(resp.df$year) #TODO: update response df

seasons <- c()
for (i in 1:(length(season.years)-1)) {
  temp_season <- paste0(season.years[i], "/", season.years[i+1])
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

#begin with group 1: 2006/2007, 2015/2016, 2019/2020

#combined peak season plot
#plot_pred_ts_panels
out_dir <- "~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures"

#2006/2007
plot_pred_ts_panels(
  season_i = 6,
  aus.lag = SEAus.lag,
  pred_df = pred.df,
  season_years = season.years,
  season.weeks = season.weeks,
  sub_season = SE.mid,
  seasons = seasons,
  cex_main = 3.5,
  preds_ord = c("nino", "etio", "wtio", 
                "tsa", "sam", "olr"),
  pred_label_x_offset = 5,
  pred_label_cex = 3.5,
  model_coef = SE2.coef,
  png_dims = list(width = 3600, height = 5600, res = 275),
  outfile = file.path(out_dir, "SI_SE2006_pred_ts.png"),
  lag_label_cex =3.0,
  lag_offsets = list(
    nino = list("40" = c(-3, 0.1)),
    etio = list("7" = c(-3, 0.1),
                "33" = c(-3, 0.1)),
    wtio = list("14" = c(-3, 0.1),
                "46" = c(0, 0.1)),
    tsa = list("29" = c(-3, 0.25)),
    sam = list("9" = c(-3, 0.1))
  ),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_cex = 1.85,
  spacer_height = 0
)



#2015/2016
plot_pred_ts_panels(
  season_i = 15,
  aus.lag = SEAus.lag,
  pred_df = pred.df,
  season_years = season.years,
  season.weeks = season.weeks,
  sub_season = SE.mid,
  seasons = seasons,
  cex_main = 3.5,
  preds_ord = c("nino", "etio", "wtio", 
                "tsa", "sam", "olr"),
  pred_label_x_offset = 9,
  pred_label_cex = 3.5,
  model_coef = SE2.coef,
  png_dims = list(width = 3600, height = 5600, res = 275),
  outfile = file.path(out_dir, "SI_SE2015_pred_ts.png"),
  lag_label_cex =3.0,
  lag_offsets = list(
    nino = list("40" = c(-3, 0.1)),
    etio = list("7" = c(-3, 0.1),
                "33" = c(-3, 0.1)),
    wtio = list("14" = c(-3, 0.1),
                "46" = c(0, 0.1)),
    tsa = list("29" = c(-3, 0.25)),
    sam = list("9" = c(-3, 0.1))
  ),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_cex = 1.85,
  spacer_height = 0
)



#2019/2020
plot_pred_ts_panels(
  season_i = 19,
  aus.lag = SEAus.lag,
  pred_df = pred.df,
  season_years = season.years,
  season.weeks = season.weeks,
  sub_season = SE.mid,
  seasons = seasons,
  cex_main = 3.5,
  preds_ord = c("nino", "etio", "wtio", 
                "tsa", "sam", "olr"),
  pred_label_x_offset = 9,
  pred_label_cex = 3.5,
  model_coef = SE2.coef,
  png_dims = list(width = 3600, height = 5600, res = 275),
  outfile = file.path(out_dir, "SI_SE2019_pred_ts.png"),
  lag_label_cex =3.0,
  lag_offsets = list( 
    nino = list("40" = c(-3, 0.1)),
    etio = list("7" = c(-3, 0.1),
                "33" = c(-3, 0.1)),
    wtio = list("14" = c(-6, 0.40),
                "46" = c(0, 0.1)),
    tsa = list("29" = c(-3, 0.25)),
    sam = list("9" = c(-3, 0.1))
  ),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_cex = 1.85,
  spacer_height = 0
)



#2002/2003
plot_pred_ts_panels(
  season_i = 2,
  aus.lag = SEAus.lag,
  pred_df = pred.df,
  season_years = season.years,
  season.weeks = season.weeks,
  sub_season = SE.mid,
  seasons = seasons,
  cex_main = 3.5,
  preds_ord = c("nino", "etio", "wtio", 
                "tsa", "sam", "olr"),
  pred_label_x_offset = 9,
  pred_label_cex = 3.5,
  model_coef = SE2.coef,
  png_dims = list(width = 3600, height = 5600, res = 275),
  outfile = file.path(out_dir, "SI_SE2002_pred_ts.png"),
  lag_label_cex =3.0,
  lag_offsets = list(
    nino = list("40" = c(-3, 0.1)),
    etio = list("7" = c(-3, 0.1),
                "33" = c(-3, 0.75)),
    wtio = list("14" = c(-3, 0.1),
                "46" = c(-6, 0.1)),
    tsa = list("29" = c(-3, 0.25)),
    sam = list("9" = c(-3, 0.1))
  ),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_cex = 1.85,
  spacer_height = 0
)



#2003/2004
plot_pred_ts_panels(
  season_i = 3,
  aus.lag = SEAus.lag,
  pred_df = pred.df,
  season_years = season.years,
  season.weeks = season.weeks,
  sub_season = SE.mid,
  seasons = seasons,
  cex_main = 3.5,
  preds_ord = c("nino", "etio", "wtio", 
                "tsa", "sam", "olr"),
  pred_label_x_offset = 9,
  pred_label_cex = 3.5,
  model_coef = SE2.coef,
  png_dims = list(width = 3600, height = 5600, res = 275),
  outfile = file.path(out_dir, "SI_SE2003_pred_ts.png"),
  lag_label_cex =3.0,
  lag_offsets = list( 
    nino = list("40" = c(-3, 0.1)),
    etio = list("7" = c(-3, 0.1),
                "33" = c(-3, 0.1)),
    wtio = list("14" = c(-3, 0.1),
                "46" = c(0, 0.1)),
    tsa = list("29" = c(-3, 0.25)),
    sam = list("9" = c(-3, 0.1))
  ),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_cex = 1.85,
  spacer_height = 0
)


#2005/2006
plot_pred_ts_panels(
  season_i = 5,
  aus.lag = SEAus.lag,
  pred_df = pred.df,
  season_years = season.years,
  season.weeks = season.weeks,
  sub_season = SE.mid,
  seasons = seasons,
  cex_main = 3.5,
  preds_ord = c("nino", "etio", "wtio", 
                "tsa", "sam", "olr"),
  pred_label_x_offset = 9,
  pred_label_cex = 3.5,
  model_coef = SE2.coef,
  png_dims = list(width = 3600, height = 5600, res = 275),
  outfile = file.path(out_dir, "SI_SE2005_pred_ts.png"),
  lag_label_cex =3.0,
  lag_offsets = list( 
    nino = list("40" = c(-3, 0.1)),
    etio = list("7" = c(-3, 0.1),
                "33" = c(-3, 0.1)),
    wtio = list("14" = c(-3, 0.1),
                "46" = c(0, 0.1)),
    tsa = list("29" = c(-3, 0.25)),
    sam = list("9" = c(-3, 0.1))
  ),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_cex = 1.85,
  spacer_height = 0
)





#comparison plots
out_dir <- "~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures"

i <- 19

groups <- list(
  Early = build_group_data(i, SEAus.lag, pred.df, season.years,
                           season.weeks, SE.early,
                           model_coef = coef(SE1.lm)),
  Peak  = build_group_data(i, SEAus.lag, pred.df, season.years,
                           season.weeks, SE.mid,
                           model_coef = coef(SE2.lm)),
  Late  = build_group_data(i, SEAus.lag, pred.df, season.years,
                           season.weeks, SE.late,
                           model_coef = coef(SE3.lm))
)

#tsa
plot_mode_comparison_panels(
  season_i = i,
  mode     = "tsa",
  groups   = groups,
  main_title = "(a) TSA Predictors for 2019/2020 Wildfire Season",
  ylim  = c(-3.2, 3.2),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_centered = TRUE,
  ylab_cex = 1.75,
  seasons  = seasons,
  png_dims = list(width = 5400, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0.020,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.65,
  lag_label_cex       = 2.2,
  lag_offsets = list(
    early = list("12" = c(7.45, -0.475),
                 "14" = c(-7.45, 0.475)),
    peak  = list("29" = c(-2.0, 0.125)),
    late = list("22" =  c(0.0 , 0.1))
  ),
  spacer_height = 0,
  outfile  = file.path(out_dir, "SI_SE2019_tsa_comparison.png")
)


#aao (sam)
plot_mode_comparison_panels(
  season_i = i,
  mode     = "sam",
  groups   = groups,
  main_title = "(b) SAM Predictors for 2019/2020 Wildfire Season",
  ylim  = c(-3.2, 3.2),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_centered = TRUE,
  ylab_cex = 1.75,
  seasons  = seasons,
  png_dims = list(width = 5400, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0.020,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.65,
  lag_label_cex       = 2.2,
  lag_offsets = list(
    early = list("24" = c(1.75, 0.85),
                 "28" = c(2.15, -0.15),
                 "29" = c(-1.55, -0.25),
                 "33" = c(-2.5, 0.1),
                 "41" = c(-3.0, 0.20)),
    peak  = list("9" = c(-3.0, 0.15),
                 "21" = c(-1.0, 0.25)),
    late = list("1" =  c(-3.0 , 0.1),
                "50" = c(-0.0, 0.0))
  ),
  spacer_height = 0,
  outfile  = file.path(out_dir, "SI_SE2019_sam_comparison.png")
)


#olr
plot_mode_comparison_panels(
  season_i = i,
  mode     = "olr",
  groups   = groups,
  main_title = "(c) OLR Predictors for 2019/2020 Wildfire Season",
  ylim  = c(-3.2, 3.2),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_centered = TRUE,
  ylab_cex = 1.75,
  seasons  = seasons,
  png_dims = list(width = 5400, height = 2100, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0.020,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.65,
  lag_label_cex       = 2.2,
  lag_offsets = list(
    early = list("2" = c(-2.0,-0.1),
                 "14" = c(0.0, -0.0)),
    late = list("6" =  c(-0.0 , 0.0))
  ),
  spacer_height = 0,
  outfile  = file.path(out_dir, "SI_SE2019_olr_comparison.png")
)





#comparison plots
i <- 6  #2006 #loop through as many years as needed

groups <- list(
  Early = build_group_data(i, SEAus.lag, pred.df, season.years,
                           season.weeks, SE.early,
                           model_coef = coef(SE1.lm)),
  Peak  = build_group_data(i, SEAus.lag, pred.df, season.years,
                           season.weeks, SE.mid,
                           model_coef = coef(SE2.lm)),
  Late  = build_group_data(i, SEAus.lag, pred.df, season.years,
                           season.weeks, SE.late,
                           model_coef = coef(SE3.lm))
)


#output, all built-in the function 
out_dir <- "~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures"

#Nino 3.4
plot_mode_comparison_panels(
  season_i = i,
  mode     = "nino",
  groups   = groups,
  main_title = "Ni\u00f1o 3.4 Predictor Terms",
  ylim  = c(-3.2, 3.2),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5400, height = 1500, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0.020,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  lag_label_cex       = 2.2,
  lag_offsets = list(
    early = list("33" = c(0, 0)),
    peak  = list("40" = c(-3, 0.15)),
    late  = list("47" = c(0, 0),
                 "25" = c(0, 0.25))
  ),
  spacer_height = 0,
  outfile  = file.path(out_dir, "SI_SE2006_nino_comparison.png")
)

#ETIO
plot_mode_comparison_panels(
  season_i = i,
  mode     = "etio",
  groups   = groups,
  main_title = "ETIO Predictor Terms",
  ylim  = c(-3.2, 3.2),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5400, height = 1500, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0.020,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  lag_label_cex       = 2.2,
  lag_offsets = list(
    peak  = list("33" = c(-2, 0.25),
                 "7" =  c(-2, 0.25)),
    late  = list("33" = c(0, 0.25),
                 "16" = c(0, 0.25))
  ),
  spacer_height = 0,
  outfile  = file.path(out_dir, "SI_SE2006_etio_comparison.png")
)


#wtio
plot_mode_comparison_panels(
  season_i = i,
  mode     = "wtio",
  groups   = groups,
  main_title = "WTIO Predictor Terms",
  ylim  = c(-3.2, 3.2),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5400, height = 1500, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0.020,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  lag_label_cex       = 2.2,
  lag_offsets = list(
    early = list("5" = c(0, -0.65)),
    peak  = list("46" = c(-2, 0.275),
                 "14" =  c(-3 , 0.275))
  ),
  spacer_height = 0,
  outfile  = file.path(out_dir, "SI_SE2006_wtio_comparison.png")
)


#tsa
plot_mode_comparison_panels(
  season_i = i,
  mode     = "tsa",
  groups   = groups,
  main_title = "TSA Predictor Terms",
  ylim  = c(-3.2, 3.2),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5400, height = 1500, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0.020,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  lag_label_cex       = 2.2,
  lag_offsets = list(
    early = list("12" = c(7.25, -0.475),
                 "14" = c(-7.25, 0.475)),
    peak  = list("29" = c(-1.0, 0.125)),
    late = list("22" =  c(0.0 , 0.1))
  ),
  spacer_height = 0,
  outfile  = file.path(out_dir, "SI_SE2006_tsa_comparison.png")
)


#aao (sam)
plot_mode_comparison_panels(
  season_i = i,
  mode     = "sam",
  groups   = groups,
  main_title = "SAM Predictor Terms",
  ylim  = c(-3.2, 3.2),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5400, height = 1500, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0.020,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  lag_label_cex       = 2.2,
  lag_offsets = list(
    early = list("24" = c(2.0, 0.75),
                 "28" = c(2.15, -0.25),
                 "29" = c(-1.45, -0.4),
                 "33" = c(-2.5, 0.1),
                 "41" = c(-3.0, 0.20)),
    peak  = list("9" = c(-0.0, 0.0),
                 "21" = c(-1.0, 0.25)),
    late = list("1" =  c(-3.0 , 0.1),
                "50" = c(-0.0, 0.0))
  ),
  spacer_height = 0,
  outfile  = file.path(out_dir, "SI_SE2006_sam_comparison.png")
)


#olr
plot_mode_comparison_panels(
  season_i = i,
  mode     = "olr",
  groups   = groups,
  main_title = "OLR Predictor Terms",
  ylim  = c(-3.2, 3.2),
  y_axis_at = c(-1.5, 0, 1.5),
  ylab_centered = TRUE,
  ylab_cex = 1.5,
  seasons  = seasons,
  png_dims = list(width = 5400, height = 1500, res = 300), #was res = 275
  show_pred_label = FALSE,
  group_label_x_frac  = 0.020,
  group_label_y_frac  = 0.1,
  group_label_cex     = 2.5,
  lag_label_cex       = 2.2,
  lag_offsets = list(
    early = list("2" = c(-2.0,-0.1),
                 "14" = c(0.0, -0.0)),
    late = list("6" =  c(-0.0 , 0.0))
  ),
  spacer_height = 0,
  outfile  = file.path(out_dir, "SI_SE2006_olr_comparison.png")
)



i <- 15







i <- 6  #2006 #loop through as many years as needed

#peak setup
peak_mats <- build_season_mats(SEAus.lag, season.weeks, SE.mid)
y_max_all <- ceiling(max(abs(unlist(peak_mats)), na.rm = TRUE) * 10) / 10
preds     <- extract_season_preds(i,peak_mats )
dates     <- build_season_dates(i, pred.df, season.years, season.weeks, SE.mid)



plot_pred_ts_panels(
  season_i = i, 
  preds = preds, 
  dates = dates, 
  seasons = seasons,
  y_max    = y_max_all,
  preds_ord = c("nino", "etio", "wtio", "tsa", "sam", "olr"),
  model_coef = coef(SE2.lm),
  png_dims            = list(width = 4600, height = 5200, res = 275L),
  outfile  = file.path(out_dir, paste0("Test_SI_SE", season.years[i], "pred_peak.png"))
)




