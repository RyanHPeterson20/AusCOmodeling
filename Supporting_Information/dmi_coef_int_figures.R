#coefficient/interaction plots for dmi instead of WTIO/ETIO


#libraries
suppressMessages(library(grid)) #gridlines between plots
suppressMessages( library(scales)) #for adjusting opacity

#data import
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/dmi_models.rda") #pred models with dmi

#load functions
source("Functions/coef_int_functions.R")

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


#SEmodels
SE.base.dmi <- SEmodels.dmi[[1]]
SE.const.dmi <- SEmodels.dmi[[2]]
SE.vary.dmi <- SEmodels.dmi[[3]]

SE1dmi.lm <- SE.base.dmi[[1]]
SE2dmi.lm <- SE.base.dmi[[2]]
SE3dmi.lm <- SE.base.dmi[[3]]

vary.early <- lapply( SEmodels.dmi[[3]], function(x) x[[1]])
vary.peak <- lapply( SEmodels.dmi[[3]], function(x) x[[2]]) 
vary.late <- lapply( SEmodels.dmi[[3]], function(x) x[[3]]) 

## setup
SE1.coef <- coef(SE1dmi.lm)
SE2.coef <- coef(SE2dmi.lm)
SE3.coef <- coef(SE3dmi.lm)


#early models
coefs1 <- list(
  base  = SE1.coef,
  #const = coef(const.early[[i]]), 
  vary  = coef(vary.early[[1]])  
)

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/Coef_Int")
png(filename = paste0("SI_dmi_early_2019.png"),  width = 2750, height = 4750, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  cex_num = 1.5,
  cex_label = 1.75,
  cex_subtitle = 1.75,
  vars_order = c("nino", "dmi", "tsa", "aao", "olr"),  # include OLR panel
  pch_map = c(nino=21, dmi=24, tsa=22, aao=23, olr=10),
  coef_range = c(-5, 5),
  main_title = paste0("Early Fire Season (2019/2020 Withheld)"),   
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




#peak models
coefs2 <- list(
  base  = SE2.coef,
  #const = coef(const.early[[i]]), 
  vary  = coef(vary.peak[[1]])  
)

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/Coef_Int")
png(filename = paste0("SI_dmi_peak_2019.png"),  width = 2750, height = 4750, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  cex_num = 1.5,
  cex_label = 1.75,
  cex_subtitle = 1.75,
  vars_order = c("nino", "dmi", "tsa", "aao", "olr"),  # include OLR panel
  pch_map = c(nino=21, dmi=24, tsa=22, aao=23, olr=10),
  coef_range = c(-5, 5),
  main_title = paste0("Peak Fire Season (2019/2020 Withheld)"),   
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



#peak models
coefs3 <- list(
  base  = SE3.coef,
  #const = coef(const.early[[i]]), 
  vary  = coef(vary.late[[1]])  
)

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/Coef_Int")
png(filename = paste0("SI_dmi_late_2019.png"),  width = 2750, height = 4750, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  cex_num = 1.5,
  cex_label = 1.75,
  cex_subtitle = 1.75,
  vars_order = c("nino", "dmi", "tsa", "aao", "olr"),  # include OLR panel
  pch_map = c(nino=21, dmi=24, tsa=22, aao=23, olr=10),
  coef_range = c(-5, 5),
  main_title = paste0("Late Fire Season (2019/2020 Withheld)"),   
  quad_y_jitter = 0.004,
  model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
  add_legends = TRUE,
  legend_inset_terms = c(0.000, 0.06),
  legend_inset_model = c(0.00, 0.00),
  legend_cex_terms = 1.80,
  legend_cex_model = 1.50,
  legend_terms = c("Ni\u00f1o 3.4", "DMI", "TSA", "SAM", "OLR", "Interaction"),
  legend_terms_pch = c(21, 24, 22, 23, 10, 11),
  legend_terms_pt_cex = c(2.25, 1.8, 2.25, 2.25, 2.25, 1.8),
  legend_models = c("All-Data", "Withheld-Season"),
  legend_model_keys = c("base", "vary"))
dev.off()
