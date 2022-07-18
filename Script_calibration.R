source("SI5_calibration_functions.R")

parameters <- c("klight","phi","g1","fallocwood","falloccanopy","m","vC","Cseedrain","log10nbs0","Hmaxcor","CR_a","CR_b","m1","DBHmaxcor","ahCorr")
lower <- c(0.1,1E-2,2,1E-2,1E-2,1E-2,1E-2,1E2,0,0.5,1.5,0.4,1,0.5,0.5)
upper = c(1,0.2-1E-2,5,1,1,0.05,0.15,1E5,3,1.5,3,0.8,1.2,1.5,1.5)
# lower <- c(0.1,NA,NA,NA,NA,1E-2,NA,NA,NA,NA,1.5,0.4,1,NA,NA)
# upper = c(1,NA,NA,NA,NA,0.05,NA,NA,NA,NA,3,0.8,1.2,NA,NA)
global_parameters_boundaries <- data.frame("parameter" = parameters,"lower" = lower,
                                           "upper" = upper)

library(rcontroll)
library(tidyverse)
library(raster)
library(doSNOW)
library(sensitivity)

LHS_design <- Generate_LHS_Autocalib(nsim = 100,nreplicat = 5,nparam = 5,paramLHS = global_parameters_boundaries,Nyears = 600,Nsampling = 100)

Generated_parameters <- Generate_parameters_autocalib(LHS_design = LHS_design)

calib_dataset <- autocalibGP(Generated_parameters = Generated_parameters,
                             PATH = getwd(),
                             ncores_sim = 100,
                             ncores = 25,
                             NiterHetGP = NULL,
                             initj = 1,Jrefresh = 25)



morrisOut <- morris(
  model = NULL,
  factors = calib_dataset$params,
  r = 500,
  design = list(type = "oat", levels = 20, grid.jump = 3),
  binf = 0,
  bsup = 1,
  scale = FALSE)

Y <- matrix(c(predict(calib_dataset$GPmodels$RateDBH$mod.RateDBH,morrisOut[['X']])$mean,
              predict(calib_dataset$GPmodels$MeanAgb$mod.MeanAgb,morrisOut[['X']])$mean,
              predict(calib_dataset$GPmodels$MeanSum10$mod.MeanSum10,morrisOut[['X']])$mean,
              predict(calib_dataset$GPmodels$MeanSum30$mod.MeanSum30,morrisOut[['X']])$mean,
              predict(calib_dataset$GPmodels$MeanGpp$mod.MeanGpp,morrisOut[['X']])$mean,
              predict(calib_dataset$GPmodels$MeanNpp$mod.MeanNpp,morrisOut[['X']])$mean,
              predict(calib_dataset$GPmodels$MeanBa10$mod.MeanBa10,morrisOut[['X']])$mean,
              predict(calib_dataset$GPmodels$MeanHill$mod.MeanHill,morrisOut[['X']])$mean,
              predict(calib_dataset$GPmodels$MeanRao$mod.MeanRao,morrisOut[['X']])$mean,
              predict(calib_dataset$GPmodels$Hmean$mod.Hmean,morrisOut[['X']])$mean,
              predict(calib_dataset$GPmodels$Lambda1ter$mod.Lambda1ter,morrisOut[['X']])$mean,
              predict(calib_dataset$GPmodels$VolECMP$mod.VolECMP,morrisOut[['X']])$mean,
              predict(calib_dataset$GPmodels$VolECMPS$mod.VolECMPS,morrisOut[['X']])$mean,
              predict(calib_dataset$GPmodels$GmDBH$mod.GmDBH,morrisOut[['X']])$mean), ncol = 14, byrow = FALSE)

tell(morrisOut,Y)

# summarise the moris output
morrisOut.df <- data.frame(
  parameter = calib_dataset$params,
  mu.star = apply(abs(morrisOut$ee), 2, mean, na.rm = T),
  sigma = apply(morrisOut$ee, 2, sd, na.rm = T)
) %>%
  arrange( mu.star )

save(calib_dataset,
     Generated_parameters,
     LHS_design,
     global_parameters_boundaries,
     morrisOut,
     morrisOut.df,
     file = "~/Nextcloud/Model/TROLL/datasets/rcontrollTROLL_calib_500x10LHS_1100y.rda")