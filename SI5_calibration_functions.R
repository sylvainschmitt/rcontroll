# 2022-19-05 SI5_calibration_functions.R
# Author : Guillaume SALZET

###################################Generate_LHS_Autocalib################################################
Generate_LHS_Autocalib <- function(nsim = 100, 
                                   nreplicat = 10,
                                   Nyears = 600,
                                   Nsampling = 100,
                                   nparam = NULL,
                                   paramLHS){
  if (is.null(nparam)) {
    stop("nparam have to specified")
  }
  
  default_values <- generate_parameters(iterperyear = 12, nbiter = 1)
  
  X <- lhs::maximinLHS(n = nsim, k = nparam +1,debug = FALSE)
  X <- rbind(X, matrix(rep(t(X), nreplicat -1), ncol= nparam+1 , byrow=TRUE))
  Xparam <- matrix(nrow = dim(X)[1],ncol = 15)
  
  params <- c()
  
  i <- 1
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "klight"]) | is.na(paramLHS$upper[paramLHS$parameter == "klight"])) {
    Xparam[,1] <- default_values$value[default_values$param == "klight"]
  }else{
    Xparam[,1] <-  qunif(X[,i],paramLHS$lower[paramLHS$parameter == "klight"],paramLHS$upper[paramLHS$parameter == "klight"]) #klight
    params <- c(params,"klight")
    i <- i+1
  }
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "phi"]) | is.na(paramLHS$upper[paramLHS$parameter == "phi"])) {
    Xparam[,2] <- default_values$value[default_values$param == "phi"]
  }else{
    Xparam[,2] <-  qunif(X[,i],paramLHS$lower[paramLHS$parameter == "phi"],paramLHS$upper[paramLHS$parameter == "phi"]) #phi
    params <- c(params,"phi")
    i <- i+1
  }
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "g1"]) | is.na(paramLHS$upper[paramLHS$parameter == "g1"])) {
    Xparam[,3] <- default_values$value[default_values$param == "g1"]
  }else{
    Xparam[,3] <-  qunif(X[,i],paramLHS$lower[paramLHS$parameter == "g1"],paramLHS$upper[paramLHS$parameter == "g1"]) #g1
    params <- c(params,"g1")
    i <- i+1
  }
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "fallocwood"]) | is.na(paramLHS$upper[paramLHS$parameter == "fallocwood"]) |
      is.na(paramLHS$lower[paramLHS$parameter == "falloccanopy"]) | is.na(paramLHS$upper[paramLHS$parameter == "falloccanopy"])) {
    Xparam[,4] <- default_values$value[default_values$param == "fallocwood"] #fallocwood
    Xparam[,5] <- default_values$value[default_values$param == "falloccanopy"] #falloccanopy
  }else{
    Xparam[,4] <- X[,i]/rowSums(X[,c(4,5,(nparam + 1))]) #fallocwood
    Xparam[,5] <- X[,i+1]/rowSums(X[,c(4,5,(nparam + 1))]) #falloccanopy
    params <- c(params,"fallocwood","falloccanopy")
    i <- i+2
  }
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "m"]) | is.na(paramLHS$upper[paramLHS$parameter == "m"])) {
    Xparam[,6] <- default_values$value[default_values$param == "m"]
  }else{
    Xparam[,6] <-  qunif(X[,i],paramLHS$lower[paramLHS$parameter == "m"],paramLHS$upper[paramLHS$parameter == "m"]) #m
    params <- c(params,"m")
    i <- i+1
  }
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "vC"]) | is.na(paramLHS$upper[paramLHS$parameter == "vC"])) {
    Xparam[,7] <- default_values$value[default_values$param == "vC"]
  }else{
    Xparam[,7] <-  qunif(X[,i],paramLHS$lower[paramLHS$parameter == "vC"],paramLHS$upper[paramLHS$parameter == "vC"]) #vC
    params <- c(params,"vC")
    i <- i+1
  }
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "Cseedrain"]) | is.na(paramLHS$upper[paramLHS$parameter == "Cseedrain"])) {
    Xparam[,8] <- default_values$value[default_values$param == "Cseedrain"]
  }else{
    Xparam[,8] <-  qunif(X[,i],paramLHS$lower[paramLHS$parameter == "Cseedrain"],paramLHS$upper[paramLHS$parameter == "Cseedrain"]) #Cseedrain
    params <- c(params,"Cseedrain")
    i <- i+1
  }
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "log10nbs0"]) | is.na(paramLHS$upper[paramLHS$parameter == "log10nbs0"])) {
    Xparam[,9] <- default_values$value[default_values$param == "nbs0"]
  }else{
    Xparam[,9] <-  qunif(X[,i],paramLHS$lower[paramLHS$parameter == "log10nbs0"],paramLHS$upper[paramLHS$parameter == "log10nbs0"]) #nbs0
    params <- c(params,"log10nbs0")
    i <- i+1
  }
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "Hmaxcor"]) | is.na(paramLHS$upper[paramLHS$parameter == "Hmaxcor"])) {
    Xparam[,10] <- 1
  }else{
    Xparam[,10] <-  qunif(X[,i],paramLHS$lower[paramLHS$parameter == "Hmaxcor"],paramLHS$upper[paramLHS$parameter == "Hmaxcor"]) #Hmax modifier
    params <- c(params,"Hmaxcor")
    i <- i+1
  }
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "CR_a"]) | is.na(paramLHS$upper[paramLHS$parameter == "CR_a"])) {
    Xparam[,11] <- 1
  }else{
    Xparam[,11] <-  qunif(X[,i],paramLHS$lower[paramLHS$parameter == "CR_a"],paramLHS$upper[paramLHS$parameter == "CR_a"]) #CR_a modifier
    params <- c(params,"CR_a")
    i <- i+1
  }
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "CR_b"]) | is.na(paramLHS$upper[paramLHS$parameter == "CR_b"])) {
    Xparam[,12] <- 1
  }else{
    Xparam[,12] <-  qunif(X[,i],paramLHS$lower[paramLHS$parameter == "CR_b"],paramLHS$upper[paramLHS$parameter == "CR_b"]) #CR_b modifier
    params <- c(params,"CR_b")
    i <- i+1
  }
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "m1"]) | is.na(paramLHS$upper[paramLHS$parameter == "m1"])) {
    Xparam[,13] <- default_values$value[default_values$param == "m1"]
  }else{
    Xparam[,13] <-  Xparam[,6]/qunif(X[,i],paramLHS$lower[paramLHS$parameter == "m1"],paramLHS$upper[paramLHS$parameter == "m1"]) #m1
    params <- c(params,"m1")
    i <- i+1
  }
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "DBHmaxcor"]) | is.na(paramLHS$upper[paramLHS$parameter == "DBHmaxcor"])) {
    Xparam[,14] <- 1
  }else{
    Xparam[,14] <-  qunif(X[,i],paramLHS$lower[paramLHS$parameter == "DBHmaxcor"],paramLHS$upper[paramLHS$parameter == "DBHmaxcor"]) #DBHmax modifier
    params <- c(params,"DBHmaxcor")
    i <- i+1
  }
  
  if (is.na(paramLHS$lower[paramLHS$parameter == "ahCorr"]) | is.na(paramLHS$upper[paramLHS$parameter == "ahCorr"])) {
    Xparam[,15] <- 1
  }else{
    Xparam[,15] <-  qunif(X[,i],paramLHS$lower[paramLHS$parameter == "ahCorr"],paramLHS$upper[paramLHS$parameter == "ahCorr"]) #DBHmax modifier
    params <- c(params,"ahCorr")
    i <- i+1
  }
  
  if (length(params) != nparam){
    stop("nparam variable is not adapted to provided paramLHS table")
  }
  
  XRd <- X[,1:nparam]
  XGP <- X[,1:nparam]
  
  colnames(Xparam) <- c("klight","phi","g1","fallocwood","falloccanopy","m","vC","Cseedrain","nbs0","Hmaxcor","CR_a","CR_b","m1","DBHmaxcor","ahCorr")
  colnames(XGP) <- params
  colnames(XRd) <- params
  
  
  return(list("X" = as_tibble(Xparam), 
              "XGP" = XGP, 
              "XRd" = XRd,
              "params" = params,
              "paramLHS" = paramLHS,
              "Nyears" = Nyears, 
              "Nsampling" = Nsampling,
              "nreplicat" = nreplicat))
}


Generate_parameters_autocalib <- function(LHS_design,
                                          data_species = rcontroll::TROLLv3_species,
                                          dataclim12mths = rcontroll::TROLLv3_climatedaytime12,
                                          dataclimdayvar = rcontroll::TROLLv3_daytimevar){
  X <- LHS_design$X
  
  
  Traits_table <- data_species[,2:8]
  rownames(Traits_table) <- data_species$s_name
  Trait_phylo <- hclust(dist(Traits_table))
  
  for (sim in 1:dim(X)[1]) {
    
    if (sim ==1) {
      TROLL_species_data <- data_species %>%
        mutate(simulation = paste0("sim_",sim)) %>%
        mutate(s_hmax = s_hmax * X$Hmaxcor[sim]) %>% 
        mutate(s_dbhmax = s_dbhmax * X$DBHmaxcor[sim]) %>% 
        mutate(s_ah = s_ah * X$ahCorr[sim])
      
      TROLL_global_params <- generate_parameters(cols = 250, 
                                                 rows = 250,
                                                 iterperyear = 12,
                                                 nbiter = LHS_design$Nyears*12,
                                                 klight = X$klight[sim],
                                                 phi = X$phi[sim],
                                                 g1 = X$g1[sim],
                                                 fallocwood = X$fallocwood[sim],
                                                 falloccanopy = X$falloccanopy[sim],
                                                 m=X$m[sim],
                                                 vC = X$vC[sim],
                                                 Cseedrain = X$Cseedrain[sim],
                                                 nbs0 = X$nbs0[sim],
                                                 CR_a = X$CR_a[sim],
                                                 CR_b = X$CR_b[sim], 
                                                 m1 = X$m1[sim],
                                                 NONRANDOM = 0,
                                                 sigma_height = 0,
                                                 sigma_CR = 0,
                                                 sigma_P = 0,
                                                 sigma_N = 0,
                                                 sigma_LMA = 0,
                                                 sigma_wsg = 0,
                                                 corr_CR_height = 0,
                                                 corr_N_LMA = 0,
                                                 corr_N_P = 0,
                                                 corr_P_LMA = 0,
                                                 OUTPUT_extended = 1) %>%
        mutate(simulation = paste0("sim_",sim))
    }else{
      
      
      TROLL_species_data_tmp <- data_species %>%
        mutate(simulation = paste0("sim_",sim)) %>%
        mutate(s_hmax = s_hmax * X$Hmaxcor[sim]) %>% 
        mutate(s_dbhmax = s_dbhmax * X$DBHmaxcor[sim]) %>% 
        mutate(s_ah = s_ah * X$ahCorr[sim])
      TROLL_species_data <- rbind(TROLL_species_data,TROLL_species_data_tmp)
      
      TROLL_global_params_tmp <- generate_parameters(cols = 250, rows = 250,iterperyear = 12,
                                                     nbiter = LHS_design$Nyears*12,
                                                     klight = X$klight[sim],
                                                     phi = X$phi[sim],
                                                     g1 = X$g1[sim],
                                                     fallocwood = X$fallocwood[sim],
                                                     falloccanopy = X$falloccanopy[sim],
                                                     m=X$m[sim],
                                                     vC = X$vC[sim],
                                                     Cseedrain = X$Cseedrain[sim],
                                                     nbs0 = X$nbs0[sim],
                                                     CR_a = X$CR_a[sim],
                                                     CR_b = X$CR_b[sim],
                                                     m1 = X$m1[sim],
                                                     NONRANDOM = 0,
                                                     sigma_height = 0,
                                                     sigma_CR = 0,
                                                     sigma_P = 0,
                                                     sigma_N = 0,
                                                     sigma_LMA = 0,
                                                     sigma_wsg = 0,
                                                     corr_CR_height = 0,
                                                     corr_N_LMA = 0,
                                                     corr_N_P = 0,
                                                     corr_P_LMA = 0,
                                                     OUTPUT_extended = 1) %>%
        mutate(simulation = paste0("sim_",sim))
      
      TROLL_global_params <- rbind(TROLL_global_params,TROLL_global_params_tmp)
      
    }
    
    
    
  }
  
  
  return(list("params" = LHS_design$params, 
              "paramLHS" = LHS_design$paramLHS,
              "nreplicat" = LHS_design$nreplicat,
              "X" = LHS_design$X, 
              "XGP" = LHS_design$XGP, 
              "XRd" = LHS_design$XRd, 
              "Nyears" = LHS_design$Nyears,
              "Nsampling" = LHS_design$Nsampling,
              "TROLL_clim_mth_params" = dataclim12mths,
              "TROLL_clim_dayvar_params" = dataclimdayvar,
              "TROLL_global_params" = TROLL_global_params,
              "TROLL_species_data" = TROLL_species_data,
              "Trait_phylo" = Trait_phylo,
              "initk" = 0))
  
}

###################################dtexp################################################
dtexp <- function(x, rate, low, upp)
{
  PU <- pexp(upp, rate=rate)
  PL <- pexp(low, rate=rate)
  dexp(x, rate) / (PU-PL) * (x >= low) * (x <= upp) 
}

###################################ptexp################################################
ptexp <- function(q, rate, low, upp)
{
  PU <- pexp(upp, rate=rate)
  PL <- pexp(low, rate=rate)
  (pexp(q, rate)-PL) / (PU-PL) * (q >= low) * (q <= upp) + 1 * (q > upp)
}

###################################get_lambda################################################

get_lambda <- function(CHM,sumCHM) {

  gaps_stats <-ForestGapR::GapStats(gap_layer = ForestGapR::getForestGaps(CHM,threshold = sumCHM[4]*1/3),chm_layer = CHM)
  
  if (dim(gaps_stats)[1] != 0) {
    a <- poweRlaw::conpl$new(gaps_stats$gap_area)
    lambda_poweRlaw <- as.numeric(poweRlaw::estimate_pars(a)[1])
  }else{
    lambda_poweRlaw <- 0
  }
  
  return(lambda_poweRlaw)
  
}


###################################SumVar################################################

SumVar <- function(i,
                   WIP_folder_PATH,
                   MCtotal,
                   MCSpecies,
                   FinalPattern,
                   Nyears,
                   Nsampling,Trait_phylo,bbox,precision,SpeciesCriteria){
  
  EstParam <- c()
  
  DBHData <-as.numeric(t(FinalPattern %>% dplyr::filter(simulation == i) %>% dplyr::select(dbh) %>% dplyr::filter(dbh > 0.1)))
  if (length(DBHData) > 0) {
    DBHParam <- try(fitdistrplus::fitdist(DBHData, distr = "texp", method="mle", start=list(rate=5), fix.arg=list(low=0.01, upp=max(DBHData))),silent = TRUE)
    if (!inherits(DBHParam,'try-error')) {
      EstParam <- c(EstParam,as.numeric(DBHParam$estimate))
      
    }else{
      
      EstParam <- c(EstParam,NA)
    }
  }else{
    EstParam <- c(EstParam,0)
  }
  
  
  
  
  LSAgb <- MCtotal %>% dplyr::filter(simulation == i,iter >= 12*Nyears-12*Nsampling, sum1 != "NaN")%>% mutate(LogAgb = log(sum1+1)) %>% dplyr::select(LogAgb)
  LSAgb[LSAgb == Inf] <- NA
  LSAgb[LSAgb == NaN] <- NA
  LSAgb[LSAgb == -Inf] <- NA
  if (length(na.exclude(LSAgb)) != 0 & sum(na.exclude(LSAgb)) != 0) {
    TSAgb <- ts(LSAgb,frequency = 12,start = c(Nyears - Nsampling,1))
    TDAgb <- decompose(TSAgb)
    MCAgb <- coda::mcmc(na.exclude(TDAgb$x - TDAgb$seasonal))
    
    EstParam <- c(EstParam,as.numeric(summary(MCAgb)$statistics[1]))
    EstParam <- c(EstParam,as.numeric(summary(MCAgb)$statistics[2]))
    
  }else{EstParam <- c(EstParam,0)
  EstParam <- c(EstParam,0)}
  
  LSAbu10 <- MCtotal %>% dplyr::filter(simulation == i,iter >= 12*Nyears-12*Nsampling, sum10 != "NaN")%>% mutate(LogAbu10 = log(sum10+1)) %>% dplyr::select(LogAbu10)
  LSAbu10[LSAbu10 == Inf] <- NA
  LSAbu10[LSAbu10 == NaN] <- NA
  LSAbu10[LSAbu10 == -Inf] <- NA
  if (length(na.exclude(LSAbu10)) != 0 & sum(na.exclude(LSAbu10)) != 0 ) {
    TSAbu10 <- ts(LSAbu10,frequency = 12,start = c(Nyears - Nsampling,1))
    TDAbu10 <- decompose(TSAbu10)
    MCAbu10 <- coda::mcmc(na.exclude(TDAbu10$x - TDAbu10$seasonal))
    
    EstParam <- c(EstParam,as.numeric(summary(MCAbu10)$statistics[1]))
    EstParam <- c(EstParam,as.numeric(summary(MCAbu10)$statistics[2]))
    
  }else{EstParam <- c(EstParam,0)
  EstParam <- c(EstParam,0)}
  
  LSAbu30 <- MCtotal %>% dplyr::filter(simulation == i,iter >= 12*Nyears-12*Nsampling, sum30 != "NaN")%>% mutate(LogAbu30 = log(sum30+1)) %>% dplyr::select(LogAbu30)
  LSAbu30[LSAbu30 == Inf] <- NA
  LSAbu30[LSAbu30 == NaN] <- NA
  LSAbu30[LSAbu30 == -Inf] <- NA
  if (length(na.exclude(LSAbu30)) != 0 & sum(na.exclude(LSAbu30)) != 0 ) {
    TSAbu30 <- ts(LSAbu30,frequency = 12,start = c(Nyears - Nsampling,1))
    TDAbu30 <- decompose(TSAbu30)
    MCAbu30 <- coda::mcmc(na.exclude(TDAbu30$x - TDAbu30$seasonal))
    
    EstParam <- c(EstParam,as.numeric(summary(MCAbu30)$statistics[1]))
    EstParam <- c(EstParam,as.numeric(summary(MCAbu30)$statistics[2]))
    
  }else{EstParam <- c(EstParam,0)
  EstParam <- c(EstParam,0)}
  
  
  LSGpp <- MCtotal %>% dplyr::filter(simulation == i,iter >= 12*Nyears-12*Nsampling, gpp != "NaN")%>% dplyr::select(gpp)
  LSGpp[LSGpp == Inf] <- NA
  LSGpp[LSGpp == NaN] <- NA
  LSGpp[LSGpp == -Inf] <- NA
  if (length(na.exclude(LSGpp)) != 0 & sum(na.exclude(LSGpp)) != 0) {
    TSGpp <- ts(LSGpp,frequency = 12,start = c(Nyears - Nsampling,1))
    TDGpp <- decompose(TSGpp)
    MCGpp <- coda::mcmc(na.exclude(TDGpp$x - TDGpp$seasonal))
    
    EstParam <- c(EstParam,as.numeric(summary(MCGpp)$statistics[1]))
    EstParam <- c(EstParam,as.numeric(summary(MCGpp)$statistics[2]))
    
  }else{EstParam <- c(EstParam,0)
  EstParam <- c(EstParam,0)}
  
  LSNpp <- MCtotal %>% dplyr::filter(simulation == i,iter >= 12*Nyears-12*Nsampling, npp != "NaN")%>% dplyr::select(npp)
  LSNpp[LSNpp == Inf] <- NA
  LSNpp[LSNpp == NaN] <- NA
  LSNpp[LSNpp == -Inf] <- NA
  if (length(na.exclude(LSNpp)) != 0 & sum(na.exclude(LSNpp)) != 0) {
    TSNpp <- ts(LSNpp,frequency = 12,start = c(Nyears - Nsampling,1))
    TDNpp <- decompose(TSNpp)
    MCNpp <- coda::mcmc(na.exclude(TDNpp$x - TDNpp$seasonal))
    
    EstParam <- c(EstParam,as.numeric(summary(MCNpp)$statistics[1]))
    EstParam <- c(EstParam,as.numeric(summary(MCNpp)$statistics[2]))
    
  }else{EstParam <- c(EstParam,0)
  EstParam <- c(EstParam,0)}
  
  LSBa10 <- MCtotal %>% dplyr::filter(simulation == i,iter >= 12*Nyears-12*Nsampling, sum1 != "NaN") %>% dplyr::select(ba10)
  LSBa10[LSBa10 == Inf] <- NA
  LSBa10[LSBa10 == NaN] <- NA
  LSBa10[LSBa10 == -Inf] <- NA
  if (length(na.exclude(LSBa10)) != 0 & sum(na.exclude(LSBa10)) != 0 ) {
    TSBa10 <- ts(LSBa10,frequency = 12,start = c(Nyears - Nsampling,1))
    TDBa10 <- decompose(TSBa10)
    MCBa10 <- coda::mcmc(na.exclude(TDBa10$x - TDBa10$seasonal))
    
    EstParam <- c(EstParam,as.numeric(summary(MCBa10)$statistics[1]))
    EstParam <- c(EstParam,as.numeric(summary(MCBa10)$statistics[2]))
    
  }else{EstParam <- c(EstParam,0)
  EstParam <- c(EstParam,0)}
  
  
  
  NB_SP <- MCSpecies %>% dplyr::filter(iter >= round(12*Nyears-12*Nsampling) & species != "total",simulation == i)%>% 
    dplyr::select(iter)
  if (length(NB_SP) != 0) {
    DiversityVec <- MCSpecies %>% dplyr::filter(iter >= round(12*Nyears-12*Nsampling) & species != "total",simulation == i)%>% 
      dplyr::select(iter,species,sum1)
    DivVec <- c()
    
    for (iterk in (Nyears - Nsampling +1 ):(Nyears-1)) {
      
      indexk <- floor(runif(1,1,12)) + 12*(iterk)
      
      DiversityVecTmp <- DiversityVec %>% dplyr::filter(iter == indexk)
      
      DiversityList <- DiversityVecTmp$sum1
      if (sum(DiversityVecTmp$sum1) == 0) {
        DivVec <- rbind(DivVec,c(iterk,0,0))
      }else{
        names(DiversityList) <- as.character(DiversityVecTmp$species)
        
        Hilltmp <- as.numeric (entropart::Diversity(Ns = entropart::as.AbdVector(DiversityList,CheckArguments=FALSE),q=1,CheckArguments=FALSE))
        Raotmp <- as.numeric (entropart::PhyloDiversity(Ns = entropart::as.AbdVector(DiversityList,CheckArguments=FALSE),q = 1,Tree = Trait_phylo,CheckArguments=FALSE)$Total)
        DivVec <- rbind(DivVec,c(iterk,Hilltmp,Raotmp))
      }
      
      
    }
    
    HillVec <- as.numeric(DivVec[,2])
    RaoVec <- as.numeric(DivVec[,3])
    
    MCHill <- coda::mcmc(na.exclude(HillVec))
    MCRao <- coda::mcmc(na.exclude(RaoVec))
    EstParam <- c(EstParam,as.numeric(summary(MCHill)$statistics[1]))
    EstParam <- c(EstParam,as.numeric(summary(MCHill)$statistics[2]))
    EstParam <- c(EstParam,as.numeric(summary(MCRao)$statistics[1]))
    EstParam <- c(EstParam,as.numeric(summary(MCRao)$statistics[2]))
  }else{
    EstParam <- c(EstParam,NA,NA,NA,NA)
  }
  
  DB_CHM <- read.delim(paste0(WIP_folder_PATH,"/sim_stack/",i,"/",i,"_0_CHM.txt"))
  
  
  CHM <- raster::rasterFromXYZ(cbind(DB_CHM$row,DB_CHM$col,DB_CHM$height_spikefree))
  
  sumCHM <- raster::summary(CHM)
  
  EstParam <- c(EstParam,mean(na.exclude(CHM@data@values)))
  
  lambda <- try(get_lambda(CHM,sumCHM),silent = TRUE)
    
  EstParam <- c(EstParam,lambda)
    
  FinalPatterni <- FinalPattern %>% dplyr::filter(simulation == i)
  if (length(FinalPatterni$dbh) > 0) {
    splited_species <- do.call(rbind, strsplit(FinalPatterni$s_name, split = "_"))
    Inventory <- FinalPatterni %>% mutate(Genus = splited_species[,1], Species = splited_species[,2]) %>% filter(dbh *100 > 10)
    CommercialInventory <- na.exclude(LoggingLab::commercialcriteriajoin(Inventory,LoggingLab::SpeciesCriteria))  %>% mutate(Vol = pi * dbh*(height - CD)) %>% filter(dbh * 100 > MinFD & dbh * 100 < MaxFD)
    ECMP <- CommercialInventory %>% filter(CommercialLevel == "1")
    ECMS <- CommercialInventory %>% filter(CommercialLevel == "1"| CommercialLevel == "2")
    ECMPVol <- sum(c( ECMP$Vol,0))/((bbox[1]*bbox[2])*1E-4)
    ECMSVol <- sum(c( ECMS$Vol,0))/((bbox[1]*bbox[2])*1E-4)
    EstParam <- c(EstParam,as.numeric(ECMPVol),as.numeric(ECMSVol))
    
    
    EstParam <- c(EstParam,exp(mean(log(FinalPatterni$dbh))))
  }else{
    EstParam <- c(EstParam,NA,NA,NA)
  }
  
  return(EstParam)
}

###################################extractVar################################################

extractVar <- function(stacktmp,
                       WIP_folder_PATH,
                       Nyears,
                       Nsampling,
                       Trait_phylo,
                       ncores = 1){
  
  MCtotal <- stacktmp@ecosystem
  MCSpecies <- stacktmp@species %>% 
    dplyr::select(iter,species,sum1,simulation)
  
  
  FinalPattern <- stacktmp@forest
  
  bbox <- c(stacktmp@parameters[1],stacktmp@parameters[2])
  
  L <- length(unique(MCtotal$simulation))
  
  # Create cluster with desired number of cores, leave one open for the machine
  # core processes
  cl <- parallel::makeCluster(ncores, outfile = "")
  #Register cluster
  registerDoSNOW(cl)
  clusterExport(cl, c("dtexp","ptexp","SumVar","get_lambda"))
  pb <- txtProgressBar(max = length(unique(MCtotal$simulation)), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  stack_res <- foreach(i=unique(MCtotal$simulation),
                       .packages = c("dplyr","coda","tidyr","entropart","fitdistrplus","sf","sp","raster","ForestGapR","LoggingLab"),
                       .combine = rbind,
                       .inorder = TRUE,.options.snow = opts) %dopar% {
                         SumVar(i = i,
                                WIP_folder_PATH = WIP_folder_PATH,
                                MCtotal = MCtotal,
                                MCSpecies = MCSpecies,
                                FinalPattern = FinalPattern,
                                Nyears = Nyears,
                                Nsampling = Nsampling,
                                Trait_phylo = Trait_phylo,
                                bbox = bbox,
                                precision = 1)
                         
                       }
  
  
  close(pb)
  stopCluster(cl)
  results <- matrix(data = stack_res,ncol = 22,byrow = FALSE)
  colnames(results) <- c("lambda_dbh10",
                         "agb_mean",
                         "agb_sigma",
                         "abu10_mean",
                         "abu10_sigma",
                         "abu30_mean",
                         "abu30_sigma",
                         "gpp_mean",
                         "gpp_sigma",
                         "npp_mean",
                         "npp_sigma",
                         "ba10_mean",
                         "ba10_sigma",
                         "hill_mean",
                         "hill_sigma",
                         "rao_mean",
                         "rao_sigma",
                         "h_mean",
                         "lambda_1ter",
                         "Vol_ECMP",
                         "Vol_ECMP_ECMS",
                         "GM_DBH")
  
  return(results)
  
  
}

###################################autocalibGP################################################

autocalibGP <- function(Generated_parameters,
                        PATH,
                        FILEsave = NULL,
                        initj = 1,
                        NiterHetGP = NULL,
                        Jrefresh = 25,
                        ncores_sim = 1,
                        ncores = 1){
  
  
  WIP_folder_PATH = tempdir()
  
  if (!is.null(FILEsave)) {
    load(paste0(PATH,FILEsave))
  }
  
  nrep <- tibble(Sim_ID = unique(Generated_parameters$TROLL_global_params$simulation)) %>% 
    mutate(ID_iter = ceiling(row_number()/ncores_sim))
  initk <- Generated_parameters$initk
  
  for (blocki in (initk/ncores_sim +1):max(nrep$ID_iter)) {
    
    simulations <- as.character(unlist(nrep %>%
                                         filter(ID_iter == ( blocki)) %>% 
                                         dplyr::select(Sim_ID)))
    
    
    cat(paste0("Computing TROLL initial simulation # ", blocki , " / ", max(nrep$ID_iter), " ",
               gsub(":", "-",
                    gsub(
                      " ", "_",
                      timestamp(
                        prefix = "",
                        suffix = "",
                        quiet = T
                      )
                    )),"\n"))
    
    gc()
    
    sim_stack <- rcontroll::stack(path = WIP_folder_PATH,
                                  name = "sim_stack",
                                  simulations = simulations,
                                  global = Generated_parameters$TROLL_global_params %>% 
                                    filter(simulation %in% simulations),
                                  species = Generated_parameters$TROLL_species_data  %>% 
                                    filter(simulation %in% simulations),
                                  climate = Generated_parameters$TROLL_clim_mth_params,
                                  daily = Generated_parameters$TROLL_clim_dayvar_params,
                                  verbose = FALSE,
                                  thin = (12*(Generated_parameters$Nyears - Generated_parameters$Nsampling +1 )):(12*Generated_parameters$Nyears),
                                  cores = ncores_sim)
    
    
    cat(paste0("Extract TROLL simulations results # ", blocki , " / ", max(nrep$ID_iter), " ",
               gsub(":", "-",
                    gsub(
                      " ", "_",
                      timestamp(
                        prefix = "",
                        suffix = "",
                        quiet = T
                      )
                    )),"\n"))
    
    
    
    Yi <- extractVar(stacktmp = sim_stack,
                     WIP_folder_PATH = WIP_folder_PATH,
                     Nyears = Generated_parameters$Nyears,
                     Nsampling = Generated_parameters$Nsampling,
                     Trait_phylo = Generated_parameters$Trait_phylo,
                     ncores = ncores)
    
    
    if (Generated_parameters$initk == 0) {
      Y <- Yi
    }else{
      Y <- rbind(Y,Yi)
    }
    
    Generated_parameters$initk <- blocki * ncores_sim
    
    if (blocki %% 10 == 0) {
      save(Generated_parameters,
           Y,
           file = paste0(PATH,
                         "tmp_env_save_iter","_autocalibGP_", Generated_parameters$initk , "-", dim(Generated_parameters$X)[1], "_", 
                         gsub(":", "-",
                              gsub(
                                " ", "_",
                                timestamp(
                                  prefix = "",
                                  suffix = "",
                                  quiet = T
                                )
                              ))
                         ,".Rdata"))
    }
    
  }
  
  
  if (!is.null(NiterHetGP)) {
    
    NA_values_index <- Y %>%  na.exclude() %>% attr(which = "na.action") %>%  as.integer()
    
    XGPCor <- Generated_parameters$XGP %>% as_tibble() %>% filter(!(row_number() %in% NA_values_index)) %>%  as.matrix()
    
    Ycor <- Y %>% as_tibble() %>% filter(!(row_number() %in% NA_values_index))  %>%  as.matrix()
    
    prdataRateDBH <- hetGP::find_reps(XGPCor, Ycor[,1], rescale = FALSE, normalize = FALSE)
    prdataMeanAgb <- hetGP::find_reps(XGPCor, Ycor[,2], rescale = FALSE, normalize = FALSE)
    prdatSdAgb <- hetGP::find_reps(XGPCor, Ycor[,3], rescale = FALSE, normalize = FALSE)
    prdataMeanAbu10 <- hetGP::find_reps(XGPCor, Ycor[,4], rescale = FALSE, normalize = FALSE)
    prdataSdAbu10 <- hetGP::find_reps(XGPCor, Ycor[,5], rescale = FALSE, normalize = FALSE)
    prdataMeanAbu30 <- hetGP::find_reps(XGPCor, Ycor[,6], rescale = FALSE, normalize = FALSE)
    prdataSdAbu30 <- hetGP::find_reps(XGPCor, Ycor[,7], rescale = FALSE, normalize = FALSE)
    prdataMeanGpp <- hetGP::find_reps(XGPCor, Ycor[,8], rescale = FALSE, normalize = FALSE)
    prdataSdGpp <- hetGP::find_reps(XGPCor, Ycor[,9], rescale = FALSE, normalize = FALSE)
    prdataMeanNpp <- hetGP::find_reps(XGPCor, Ycor[,10], rescale = FALSE, normalize = FALSE)
    prdataSdNpp <- hetGP::find_reps(XGPCor, Ycor[,11], rescale = FALSE, normalize = FALSE)
    prdataMeanBa10 <- hetGP::find_reps(XGPCor, Ycor[,12], rescale = FALSE, normalize = FALSE)
    prdataSdBa10 <- hetGP::find_reps(XGPCor, Ycor[,13], rescale = FALSE, normalize = FALSE)
    prdataMeanHill <- hetGP::find_reps(XGPCor, Ycor[,14], rescale = FALSE, normalize = FALSE)
    prdataSdHill <- hetGP::find_reps(XGPCor, Ycor[,15], rescale = FALSE, normalize = FALSE)
    prdataMeanRao <- hetGP::find_reps(XGPCor, Ycor[,16], rescale = FALSE, normalize = FALSE)
    prdataSdRao <- hetGP::find_reps(XGPCor, Ycor[,17], rescale = FALSE, normalize = FALSE)
    prdataHmean <- hetGP::find_reps(XGPCor, Ycor[,18], rescale = FALSE, normalize = FALSE)
    prdataLambda1ter <- hetGP::find_reps(XGPCor, Ycor[,19], rescale = FALSE, normalize = FALSE)
    prdataVolECMP <- hetGP::find_reps(XGPCor, Ycor[,20], rescale = FALSE, normalize = FALSE)
    prdataVolECMPS <- hetGP::find_reps(XGPCor, Ycor[,21], rescale = FALSE, normalize = FALSE)
    prdataGmDBH <- hetGP::find_reps(XGPCor, Ycor[,22], rescale = FALSE, normalize = FALSE)
    
    
    mod.RateDBH <- hetGP::mleHetGP(X = list(X0 = prdataRateDBH$X0,Z0 = prdataRateDBH$Z0,mult=prdataRateDBH$mult), Z = prdataRateDBH$Z,covtype = "Gaussian")
    mod.MeanAgb <- hetGP::mleHetGP(X = list(X0 = prdataMeanAgb$X0,Z0 = prdataMeanAgb$Z0,mult=prdataMeanAgb$mult), Z = prdataMeanAgb$Z,covtype = "Gaussian")
    mod.SdAgb <- hetGP::mleHetGP(X = list(X0 = prdatSdAgb$X0,Z0 = prdatSdAgb$Z0,mult=prdatSdAgb$mult), Z = prdatSdAgb$Z,covtype = "Gaussian")
    mod.MeanAbu10 <- hetGP::mleHetGP(X = list(X0 = prdataMeanAbu10$X0,Z0 = prdataMeanAbu10$Z0,mult=prdataMeanAbu10$mult), Z = prdataMeanAbu10$Z,covtype = "Gaussian")
    mod.SdAbu10 <- hetGP::mleHetGP(X = list(X0 = prdataSdAbu10$X0,Z0 = prdataSdAbu10$Z0,mult=prdataSdAbu10$mult), Z = prdataSdAbu10$Z,covtype = "Gaussian")
    mod.MeanAbu30 <- hetGP::mleHetGP(X = list(X0 = prdataMeanAbu30$X0,Z0 = prdataMeanAbu30$Z0,mult=prdataMeanAbu30$mult), Z = prdataMeanAbu30$Z,covtype = "Gaussian")
    mod.SdAbu30 <- hetGP::mleHetGP(X = list(X0 = prdataSdAbu30$X0,Z0 = prdataSdAbu30$Z0,mult=prdataSdAbu30$mult), Z = prdataSdAbu30$Z,covtype = "Gaussian")
    mod.MeanGpp <- hetGP::mleHetGP(X = list(X0 = prdataMeanGpp$X0,Z0 = prdataMeanGpp$Z0,mult=prdataMeanGpp$mult), Z = prdataMeanGpp$Z,covtype = "Gaussian")
    mod.SdGpp <- hetGP::mleHetGP(X = list(X0 = prdataSdGpp$X0,Z0 = prdataSdGpp$Z0,mult=prdataSdGpp$mult), Z = prdataSdGpp$Z,covtype = "Gaussian")
    mod.MeanNpp <- hetGP::mleHetGP(X = list(X0 = prdataMeanNpp$X0,Z0 = prdataMeanNpp$Z0,mult=prdataMeanNpp$mult), Z = prdataMeanNpp$Z,covtype = "Gaussian")
    mod.SdNpp <- hetGP::mleHetGP(X = list(X0 = prdataSdNpp$X0,Z0 = prdataSdNpp$Z0,mult=prdataSdNpp$mult), Z = prdataSdNpp$Z,covtype = "Gaussian")
    mod.MeanBa10 <- hetGP::mleHetGP(X = list(X0 = prdataMeanBa10$X0,Z0 = prdataMeanBa10$Z0,mult=prdataMeanBa10$mult), Z = prdataMeanBa10$Z,covtype = "Gaussian")
    mod.SdBa10 <- hetGP::mleHetGP(X = list(X0 = prdataSdBa10$X0,Z0 = prdataSdBa10$Z0,mult=prdataSdBa10$mult), Z = prdataSdBa10$Z,covtype = "Gaussian")
    mod.MeanHill <- hetGP::mleHetGP(X = list(X0 = prdataMeanHill$X0,Z0 = prdataMeanHill$Z0,mult=prdataMeanHill$mult), Z = prdataMeanHill$Z,covtype = "Gaussian")
    mod.SdHill <- hetGP::mleHetGP(X = list(X0 = prdataSdHill$X0,Z0 = prdataSdHill$Z0,mult=prdataSdHill$mult), Z = prdataSdHill$Z,covtype = "Gaussian")
    mod.MeanRao <- hetGP::mleHetGP(X = list(X0 = prdataMeanRao$X0,Z0 = prdataMeanRao$Z0,mult=prdataMeanRao$mult), Z = prdataMeanRao$Z,covtype = "Gaussian")
    mod.SdRao <- hetGP::mleHetGP(X = list(X0 = prdataSdRao$X0,Z0 = prdataSdRao$Z0,mult=prdataSdRao$mult), Z = prdataSdRao$Z,covtype = "Gaussian")
    mod.Hmean <- hetGP::mleHetGP(X = list(X0 = prdataHmean$X0,Z0 = prdataHmean$Z0,mult=prdataHmean$mult), Z = prdataHmean$Z,covtype = "Gaussian")
    mod.Lambda1ter <- hetGP::mleHetGP(X = list(X0 = prdataLambda1ter$X0,Z0 = prdataLambda1ter$Z0,mult=prdataLambda1ter$mult), Z = prdataLambda1ter$Z,covtype = "Gaussian")
    mod.VolECMP <- hetGP::mleHetGP(X = list(X0 = prdataVolECMP$X0,Z0 = prdataVolECMP$Z0,mult=prdataVolECMP$mult), Z = prdataVolECMP$Z,covtype = "Gaussian")
    mod.VolECMPS <- hetGP::mleHetGP(X = list(X0 = prdataVolECMPS$X0,Z0 = prdataVolECMPS$Z0,mult=prdataVolECMPS$mult), Z = prdataVolECMPS$Z,covtype = "Gaussian")
    mod.GmDBH <- hetGP::mleHetGP(X = list(X0 = prdataGmDBH$X0,Z0 = prdataGmDBH $Z0,mult=prdataGmDBH$mult), Z = prdataGmDBH$Z,covtype = "Gaussian")
    
    
    h.RateDBH <- rep(0, NiterHetGP)
    h.MeanAgb <- rep(0, NiterHetGP)
    h.SdAgb <- rep(0, NiterHetGP)
    h.MeanAbu10 <- rep(0, NiterHetGP)
    h.SdAbu10 <- rep(0, NiterHetGP)
    h.MeanAbu30 <- rep(0, NiterHetGP)
    h.SdAbu30 <- rep(0, NiterHetGP)
    h.MeanGpp <- rep(0, NiterHetGP)
    h.SdGpp <- rep(0, NiterHetGP)
    h.MeanNpp <- rep(0, NiterHetGP)
    h.SdNpp <- rep(0, NiterHetGP)
    h.MeanBa10 <- rep(0, NiterHetGP)
    h.SdBa10 <- rep(0, NiterHetGP)
    h.MeanHill <- rep(0, NiterHetGP)
    h.SdHill <- rep(0, NiterHetGP)
    h.MeanRao <- rep(0, NiterHetGP)
    h.SdRao <- rep(0, NiterHetGP)
    h.Hmean <- rep(0, NiterHetGP)
    h.Lambda1ter <- rep(0, NiterHetGP)
    h.VolECMP <- rep(0, NiterHetGP)
    h.VolECMPS <- rep(0, NiterHetGP)
    h.GmDBH <- rep(0, NiterHetGP)
    
    for (j in initj:NiterHetGP) {
      
      cat(paste0("--------ITER ", j,"---------------","",
                 gsub(":", "-",
                      gsub(
                        " ", "_",
                        timestamp(
                          prefix = "",
                          suffix = "",
                          quiet = T
                        )
                      )),"\n"))
      
    h.RateDBH[j] <- hetGP::horizon(mod.RateDBH)
    h.MeanAgb[j] <- hetGP::horizon(mod.MeanAgb)
    h.SdAgb[j] <- hetGP::horizon(mod.SdAgb)
    h.MeanAbu10[j] <- hetGP::horizon(mod.MeanAbu10)
    h.SdAbu10[j] <- hetGP::horizon(mod.SdAbu10)
    h.MeanAbu30[j] <- hetGP::horizon(mod.MeanAbu30)
    h.SdAbu30[j] <- hetGP::horizon(mod.SdAbu30)
    h.MeanGpp[j] <- hetGP::horizon(mod.MeanGpp)
    h.SdGpp[j] <- hetGP::horizon(mod.SdGpp)
    h.MeanNpp[j] <- hetGP::horizon(mod.MeanNpp)
    h.SdNpp[j] <- hetGP::horizon(mod.SdNpp)
    h.MeanBa10[j] <- hetGP::horizon(mod.MeanBa10)
    h.SdBa10[j] <- hetGP::horizon(mod.SdBa10)
    h.MeanHill[j] <- hetGP::horizon(mod.MeanHill)
    h.SdHill[j] <- hetGP::horizon(mod.SdHill)
    h.MeanRao[j] <- hetGP::horizon(mod.MeanRao)
    h.SdRao[j] <- hetGP::horizon(mod.SdRao)
    h.Hmean[j] <- hetGP::horizon(mod.Hmean)
    h.Lambda1ter[j] <- hetGP::horizon(mod.Lambda1ter)
    h.VolECMP[j] <- hetGP::horizon(mod.VolECMP)
    h.VolECMPS[j] <- hetGP::horizon(mod.VolECMPS)
    h.GmDBH[j] <- hetGP::horizon(mod.GmDBH)
      
      
      cat(paste0("IMSPE optimisation iter ", j,"/",NiterHetGP,"_",
                 gsub(":", "-",
                      gsub(
                        " ", "_",
                        timestamp(
                          prefix = "",
                          suffix = "",
                          quiet = T
                        )
                      )),"\n"))
      
      opt.RateDBH <- hetGP::IMSPE_optim(mod.RateDBH, h = h.RateDBH[j],ncores = ncores)
      opt.MeanAgb <- hetGP::IMSPE_optim(mod.MeanAgb, h = h.MeanAgb[j],ncores = ncores)
      opt.SdAgb <- hetGP::IMSPE_optim(mod.SdAgb, h = h.SdAgb[j],ncores = ncores)
      opt.MeanAbu10 <- hetGP::IMSPE_optim(mod.MeanAbu10, h = h.MeanAbu10[j],ncores = ncores)
      opt.SdAbu10 <- hetGP::IMSPE_optim(mod.SdAbu10, h = h.SdAbu10[j], ncores = ncores)
      opt.MeanAbu30 <- hetGP::IMSPE_optim(mod.MeanAbu30, h = h.MeanAbu30[j],ncores = ncores)
      opt.SdAbu30 <- hetGP::IMSPE_optim(mod.SdAbu30, h = h.SdAbu30[j], ncores = ncores)
      opt.MeanGpp <- hetGP::IMSPE_optim(mod.MeanGpp, h = h.MeanGpp[j], ncores = ncores)
      opt.SdGpp<- hetGP::IMSPE_optim(mod.SdGpp, h = h.SdGpp[j], ncores = ncores)
      opt.MeanNpp <- hetGP::IMSPE_optim(mod.MeanNpp, h = h.MeanNpp[j], ncores = ncores)
      opt.SdNpp<- hetGP::IMSPE_optim(mod.SdNpp, h = h.SdNpp[j], ncores = ncores)
      opt.MeanBa10 <- hetGP::IMSPE_optim(mod.MeanBa10, h = h.MeanBa10[j], ncores = ncores)
      opt.SdBa10<- hetGP::IMSPE_optim(mod.SdBa10, h = h.NdBa10[j], ncores = ncores)
      opt.MeanHill <- hetGP::IMSPE_optim(mod.MeanHill, h = h.MeanHill[j], ncores = ncores,
                                  control = list(tol_dist = 1e-06, tol_diff = 1e-06, multi.start = 20, maxit = 100))
      opt.SdHill<- hetGP::IMSPE_optim(mod.SdHill, h = h.SdHill[j], ncores = ncores)
      opt.MeanRao <- hetGP::IMSPE_optim(mod.MeanRao, h = h.MeanRao[j], ncores = ncores)
      opt.SdRao<- hetGP::IMSPE_optim(mod.SdRao, h = h.SdRao[j], ncores = ncores)
      opt.Hmean <- hetGP::IMSPE_optim(mod.Hmean, h = h.Hmean[j], ncores = ncores)
      opt.Lambda1ter<- hetGP::IMSPE_optim(mod.Lambda1ter, h = h.Lambda1ter[j], ncores = ncores)
      opt.VolECMP<- hetGP::IMSPE_optim(mod.VolECMP, h = h.VolECMP[j], ncores = ncores)
      opt.VolECMPS<- hetGP::IMSPE_optim(mod.VolECMPS, h = h.VolECMPS[j], ncores = ncores)
      opt.GmDBH<- hetGP::IMSPE_optim(mod.GmDBH, h = h.GmDBH[j], ncores = ncores)
      
      Xnew <- rbind(opt.RateDBH$par,
                    opt.MeanAgb$par,opt.SdAgb$par,
                    opt.MeanAbu10$par,opt.SdAbu10$par,
                    opt.MeanAbu30$par,opt.SdAbu30$par,
                    opt.MeanGpp$par,opt.SdGpp$par,
                    opt.MeanNpp$par,opt.SdNpp$par,
                    opt.MeanBa10$par,opt.SdBa10$par,
                    opt.MeanHill$par,opt.SdHill$par,
                    opt.MeanRao$par,opt.SdRao$par,
                    opt.Hmean$par, opt.Lambda1ter$par,
                    opt.VolECMP$par, opt.VolECMPS$par,
                    opt.GmDBH$par)
      if (ncores_sim < 22) {
        X <- rbind(Xnew)
        XGP <- rbind(XGP, X)
      }else{
        XRd<- lhs::augmentLHS(XRd, ncores - 22)
      XRdNew <- XRd[(dim(XRd)[1]-(ncores - 23)):(dim(XRd)[1]),]
      
      
      X <- rbind(Xnew,XRdNew)
      XGP <- rbind(XGP, X)
      }
      
      
      
      default_values <- generate_parameters(iterperyear = 12, nbiter = 1)
      Xparam <- matrix(nrow = dim(XRdNew)[1],ncol = 15)
      i<- 1
      
      if ("klight" %in% Generated_parameters$params) {
        Xparam[,1] <- qunif(X[,i],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "klight"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "klight"]) #klight
        i <- i+1
      }else{
        Xparam[,1] <- default_values$value[default_values$param == "klight"]
      }
      
      if ("phi" %in% Generated_parameters$params) {
        Xparam[,2] <- qunif(X[,i],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "phi"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "phi"]) #phi
        i <- i+1
      }else{
        Xparam[,2] <- default_values$value[default_values$param == "phi"]
      }
      
      if ("g1" %in% Generated_parameters$params) {
        Xparam[,3] <- qunif(X[,i],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "g1"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "g1"]) #g1
        i <- i+1
      }else{
        Xparam[,3] <- default_values$value[default_values$param == "g1"]
      }
      
      if ("fallocwood" %in% Generated_parameters$params & "falloccanopy" %in% Generated_parameters$params) {
        Xparam[,4] <- qunif(X[,i],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "fallocwood"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "fallocwood"]) #fallocwood
        Xparam[,5] <- qunif(X[,i+1],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "falloccanopy"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "falloccanopy"]) #falloccanopy
        i <- i+2
      }else{
        Xparam[,4] <- default_values$value[default_values$param == "fallocwood"]
        Xparam[,5] <- default_values$value[default_values$param == "falloccanopy"]
      }
      
      if ("m" %in% Generated_parameters$params) {
        Xparam[,6] <- qunif(X[,i],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "m"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "m"]) #m
        i <- i+1
      }else{
        Xparam[,6] <- default_values$value[default_values$param == "m"]
      }
      
      if ("vC" %in% Generated_parameters$params) {
        Xparam[,7] <- qunif(X[,i],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "vC"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "vC"]) #vC
        i <- i+1
      }else{
        Xparam[,7] <- default_values$value[default_values$param == "vC"]
      }
      
      if ("Cseedrain" %in% Generated_parameters$params) {
        Xparam[,8] <- qunif(X[,i],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "Cseedrain"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "Cseedrain"]) #Cseedrain
        i <- i+1
      }else{
        Xparam[,8] <- default_values$value[default_values$param == "Cseedrain"]
      }
      
      if ("nbs0" %in% Generated_parameters$params) {
        Xparam[,9] <- qunif(X[,i],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "nbs0"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "nbs0"]) #nbs0
        i <- i+1
      }else{
        Xparam[,9] <- default_values$value[default_values$param == "nbs0"]
      }
      
      if ("Hmaxcor" %in% Generated_parameters$params) {
        Xparam[,10] <- qunif(X[,i],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "Hmaxcor"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "Hmaxcor"]) #Hmaxcor
        i <- i+1
      }else{
        Xparam[,10] <- 1
      }
      
      if ("CR_a" %in% Generated_parameters$params) {
        Xparam[,11] <- qunif(X[,i],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "CR_a"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "CR_a"]) #CR_a
        i <- i+1
      }else{
        Xparam[,11] <- default_values$value[default_values$param == "CR_a"]
      }
      
      if ("CR_b" %in% Generated_parameters$params) {
        Xparam[,12] <- qunif(X[,i],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "CR_b"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "CR_b"]) #CR_b
        i <- i+1
      }else{
        Xparam[,12] <- default_values$value[default_values$param == "CR_b"]
      }
      
      if ("m1" %in% Generated_parameters$params) {
        Xparam[,13] <- Xparam[,6]/qunif(X[,i],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "m1"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "m1"]) #m1
        i <- i+1
      }else{
        Xparam[,13] <- default_values$value[default_values$param == "m1"]
      }
      
      if ("DBHmaxcor" %in% Generated_parameters$params) {
        Xparam[,14] <- qunif(X[,i],Generated_parameters$ParamLHS$lower[Generated_parameters$ParamLHS$parameter == "DBHmaxcor"],Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "DBHmaxcor"]) #DBHmaxcor
        i <- i+1
      }else{
        Xparam[,14] <- 1
      }
      
      if ("ahCorr" %in% Generated_parameters$params) {
        Xparam[,15] <- qunif(X[,i],Generated_parameters$Generated_parameters$ParamLHS$lower[Generated_parameters$Generated_parameters$ParamLHS$parameter == "ahCorr"],Generated_parameters$Generated_parameters$ParamLHS$upper[Generated_parameters$ParamLHS$parameter == "ahCorr"]) #ahCorr
        i <- i+1
      }else{
        Xparam[,15] <- 1
      }
      
      New_LHS <- list("X" = as_tibble(Xparam), 
           "XGP" = XGP, 
           "XRd" = Generated_parameters$XRd,
           "params" = Generated_parameters$params,
           "paramLHS" = Generated_parameters$paramLHS,
           "Nyears" = Generated_parameters$Nyears, 
           "Nsampling" = Generated_parameters$Nsampling)
      
      New_generated_parameters <- Generate_parameters_autocalib(LHS_design = New_LHS,
                                    data_species = Generated_parameters$data_species,
                                    dataclim12mths = Generated_parameters$dataclim12mths,
                                    dataclimdayvar = Generated_parameters$dataclimdayvar)
      
      nrep <- tibble(Sim_ID = unique(New_generated_parameters$TROLL_global_params$simulation)) %>% 
        mutate(ID_iter = ceiling(row_number()/ncores_sim))
      
      
      for (blocki in (New_generated_parameters$initk/ncores_sim +1):max(nrep$ID_iter)) {
        
        simulations <- as.character(unlist(nrep %>%
                                             filter(ID_iter == ( blocki)) %>% 
                                             dplyr::select(Sim_ID)))
        
        
        cat(paste0("Computing TROLL initial simulation # ", blocki , " / ", max(nrep$ID_iter), " ",
                   gsub(":", "-",
                        gsub(
                          " ", "_",
                          timestamp(
                            prefix = "",
                            suffix = "",
                            quiet = T
                          )
                        )),"\n"))
        
        sim_stack <- rcontroll::stack(path = WIP_folder_PATH,
                                      name = paste0("sim_stack_",j),
                                      simulations = simulations,
                                      global = New_generated_parameters$TROLL_global_params %>% 
                                        filter(simulation %in% simulations),
                                      species = New_generated_parameters$TROLL_species_data  %>% 
                                        filter(simulation %in% simulations),
                                      climate = New_generated_parameters$TROLL_clim_mth_params,
                                      daily = New_generated_parameters$TROLL_clim_dayvar_params,
                                      verbose = FALSE,
                                      thin = (12*(New_generated_parameters$Nyears - New_generated_parameters$Nsampling +1 )):(12*New_generated_parameters$Nyears),
                                      cores = ncores_sim)
        
        cat(paste0("Extract TROLL simulations results # ", blocki , " / ", max(nrep$ID_iter), " ",
                   gsub(":", "-",
                        gsub(
                          " ", "_",
                          timestamp(
                            prefix = "",
                            suffix = "",
                            quiet = T
                          )
                        )),"\n"))
        
        Ynew <- extractVar(stacktmp = sim_stack,
                         WIP_folder_PATH = WIP_folder_PATH,
                         Nyears = New_generated_parameters$Nyears,
                         Nsampling = New_generated_parameters$Nsampling,
                         Trait_phylo = Generated_parameters$Trait_phylo,
                         ncores = ncores)
        
        Y <- rbind(Y,Ynew)
        
        if (blocki %% 10 == 0) {
          save(New_generated_parameters,
               Y,
               file = paste0(PATH,
                             "tmp_env_save_iter","_autocalibGP_", New_generated_parameters$initk , "-", dim(New_generated_parameters$X)[1], "_", 
                             gsub(":", "-",
                                  gsub(
                                    " ", "_",
                                    timestamp(
                                      prefix = "",
                                      suffix = "",
                                      quiet = T
                                    )
                                  ))
                             ,".Rdata"))
        }
        
      }
      
      cat(paste0("Update GP models iter ", j,"/",NiterHetGP,"_",
                 gsub(":", "-",
                      gsub(
                        " ", "_",
                        timestamp(
                          prefix = "",
                          suffix = "",
                          quiet = T
                        )
                      )),"\n"))
      
      NA_values_index <- Ynew %>%  na.exclude() %>% attr(which = "na.action") %>%  as.integer()
      
      XnewCor <- Xnew %>% as_tibble() %>% filter(!(row_number() %in% NA_values_index)) %>%  as.matrix()
      
      Ynewcor <- Ynew %>% as_tibble() %>% filter(!(row_number() %in% NA_values_index))  %>%  as.matrix()
      
      mod.RateDBH <- hetGP::update(mod.RateDBH,Xnew = XnewCor,Znew  = Ynewcor[,1],ginit =mod.RateDBH$g*1.01 )
      mod.MeanAgb <- hetGP::update(mod.MeanAgb,Xnew = XnewCor,Znew = Ynewcor[,2],ginit =mod.MeanAgb$g*1.01)
      mod.SdAgb <- hetGP::update(mod.SdAgb,Xnew = XnewCor,Znew = Ynewcor[,3],ginit =mod.SdAgb$g*1.01)
      mod.MeanAbu10 <- hetGP::update(mod.MeanAbu10,Xnew = XnewCor,Znew = Ynewcor[,4],ginit =mod.MeanAbu10$g*1.01)
      mod.SdAbu10 <-hetGP::update(mod.SdAbu10,Xnew = XnewCor,Znew = Ynewcor[,5],ginit =mod.SdAbu10$g*1.01)
      mod.MeanAbu30 <- hetGP::update(mod.MeanAbu30,Xnew = XnewCor,Znew = Ynewcor[,6],ginit =mod.MeanAbu30$g*1.01)
      mod.SdAbu30 <-hetGP::update(mod.SdAbu30,Xnew = XnewCor,Znew = Ynewcor[,7],ginit =mod.SdAbu30$g*1.01)
      mod.MeanGpp <- hetGP::update(mod.MeanGpp,Xnew = XnewCor,Znew = Ynewcor[,8],ginit =mod.MeanGpp$g*1.01)
      mod.SdGpp <- hetGP::update(mod.SdGpp,Xnew = XnewCor,Znew = Ynewcor[,9],ginit =mod.SdGpp$g*1.01)
      mod.MeanNpp <- hetGP::update(mod.MeanNpp,Xnew = Xnew,Znew = Ynewcor[,10],ginit =mod.MeanNpp$g*1.01)
      mod.SdNpp <- hetGP::update(mod.SdNpp,Xnew = XnewCor,Znew = Ynewcor[,11],ginit =mod.SdNpp$g*1.01)
      mod.MeanBa10 <- hetGP::update(mod.MeanBa10,Xnew = XnewCor,Znew = Ynewcor[,12],ginit =mod.MeanBa10$g*1.01)
      mod.SdBa10 <- hetGP::update(mod.SdBa10,Xnew = XnewCor,Znew = Ynewcor[,13],ginit =mod.SdBa10$g*1.01)
      mod.MeanHill <- hetGP::update(mod.MeanHill,Xnew = XnewCor,Znew = Ynewcor[,14],ginit =mod.MeanHill$g*1.01)
      mod.SdHill <- hetGP::update(mod.SdHill,Xnew = XnewCor,Znew = Ynewcor[,15],ginit =mod.SdHill$g*1.01)
      mod.MeanRao <- hetGP::update(mod.MeanRao,Xnew = XnewCor,Znew = Ynewcor[,16],ginit =mod.MeanRao$g*1.01)
      mod.SdRao <- hetGP::update(mod.SdRao,Xnew = XnewCor,Znew = Ynewcor[,17],ginit =mod.SdRao$g*1.01)
      mod.Hmean <- hetGP::update(mod.Hmean,Xnew = XnewCor,Znew = Ynewcor[,18],ginit =mod.Hmean$g*1.01)
      mod.Lambda1ter <- hetGP::update(mod.Lambda1ter,Xnew = XnewCor,Znew = Ynewcor[,19],ginit =mod.Lambda1ter$g*1.01)
      mod.VolECMP <- hetGP::update(mod.VolECMP,Xnew = XnewCor,Znew = Ynewcor[,20],ginit =mod.VolECMP$g*1.01)
      mod.VolECMPS <- hetGP::update(mod.VolECMPS,Xnew = XnewCor,Znew = Ynewcor[,21],ginit =mod.VolECMPS$g*1.01)
      mod.GmDBH <- hetGP::update(mod.GmDBH,Xnew = XnewCor,Znew = Ynewcor[,22],ginit =mod.GmDBH$g*1.01)
      
      if(j %% Jrefresh == 0) {
        cat(paste0("Recompute GP models iter ", j,"/",NiterHetGP,"_",
                   gsub(":", "-",
                        gsub(
                          " ", "_",
                          timestamp(
                            prefix = "",
                            suffix = "",
                            quiet = T
                          )
                        ))))
        mod2.RateDBH <- hetGP::mleHetGP(X = list(X0 = mod.RateDBH$X0, Z0 = mod.RateDBH$Z0,
                                          mult = mod.RateDBH$mult), Z = mod.RateDBH$Z,covtype = "Gaussian")
        mod2.MeanAgb <- hetGP::mleHetGP(X = list(X0 = mod.MeanAgb$X0, Z0 = mod.MeanAgb$Z0,
                                          mult = mod.MeanAgb$mult), Z = mod.MeanAgb$Z,covtype = "Gaussian")
        mod2.SdAgb <- hetGP::mleHetGP(X = list(X0 = mod.SdAgb$X0, Z0 = mod.SdAgb$Z0,
                                        mult = mod.SdAgb$mult), Z = mod.SdAgb$Z,covtype = "Gaussian")
        mod2.MeanAbu10 <- hetGP::mleHetGP(X = list(X0 = mod.MeanAbu10$X0, Z0 = mod.MeanAbu10$Z0,
                                          mult = mod.MeanAbu10$mult), Z = mod.MeanAbu10$Z,covtype = "Gaussian")
        mod2.SdAbu10 <- hetGP::mleHetGP(X = list(X0 = mod.SdAbu10$X0, Z0 = mod.SdAbu10$Z0,
                                        mult = mod.SdAbu10$mult), Z = mod.SdAbu10$Z,covtype = "Gaussian")
        mod2.MeanAbu30 <- hetGP::mleHetGP(X = list(X0 = mod.MeanAbu30$X0, Z0 = mod.MeanAbu30$Z0,
                                                   mult = mod.MeanAbu30$mult), Z = mod.MeanAbu30$Z,covtype = "Gaussian")
        mod2.SdAbu30 <- hetGP::mleHetGP(X = list(X0 = mod.SdAbu30$X0, Z0 = mod.SdAbu30$Z0,
                                                 mult = mod.SdAbu30$mult), Z = mod.SdAbu30$Z,covtype = "Gaussian")
        mod2.MeanGpp <- hetGP::mleHetGP(X = list(X0 = mod.MeanGpp$X0, Z0 = mod.MeanGpp$Z0,
                                                 mult = mod.MeanGpp$mult), Z = mod.MeanGpp$Z,covtype = "Gaussian")
        mod2.SdGpp <- hetGP::mleHetGP(X = list(X0 = mod.SdGpp$X0, Z0 = mod.SdGpp$Z0,
                                               mult = mod.SdGpp$mult), Z = mod.SdGpp$Z,covtype = "Gaussian")
        mod2.MeanNpp <- hetGP::mleHetGP(X = list(X0 = mod.MeanNpp$X0, Z0 = mod.MeanNpp$Z0,
                                                 mult = mod.MeanNpp$mult), Z = mod.MeanNpp$Z,covtype = "Gaussian")
        mod2.SdNpp <- hetGP::mleHetGP(X = list(X0 = mod.SdNpp$X0, Z0 = mod.SdNpp$Z0,
                                               mult = mod.SdNpp$mult), Z = mod.SdNpp$Z,covtype = "Gaussian")
        mod2.MeanBa10 <- hetGP::mleHetGP(X = list(X0 = mod.MeanBa10$X0, Z0 = mod.MeanBa10$Z0,
                                                 mult = mod.MeanBa10$mult), Z = mod.MeanBa10$Z,covtype = "Gaussian")
        mod2.SdBa10 <- hetGP::mleHetGP(X = list(X0 = mod.SdBa10$X0, Z0 = mod.SdBa10$Z0,
                                               mult = mod.SdBa10$mult), Z = mod.SdBa10$Z,covtype = "Gaussian")
        mod2.MeanHill <- hetGP::mleHetGP(X = list(X0 = mod.MeanHill$X0, Z0 = mod.MeanHill$Z0,
                                           mult = mod.MeanHill$mult), Z = mod.MeanHill$Z,covtype = "Gaussian")
        mod2.SdHill <- hetGP::mleHetGP(X = list(X0 = mod.SdHill$X0, Z0 = mod.SdHill$Z0,
                                         mult = mod.SdHill$mult), Z = mod.SdHill$Z,covtype = "Gaussian")
        mod2.MeanRao <- hetGP::mleHetGP(X = list(X0 = mod.MeanRao$X0, Z0 = mod.MeanRao$Z0,
                                          mult = mod.MeanRao$mult), Z = mod.MeanRao$Z,covtype = "Gaussian")
        mod2.SdRao <- hetGP::mleHetGP(X = list(X0 = mod.SdRao$X0, Z0 = mod.SdRao$Z0,
                                        mult = mod.SdRao$mult), Z = mod.SdRao$Z,covtype = "Gaussian")
        mod2.Hmean <- hetGP::mleHetGP(X = list(X0 = mod.Hmean$X0, Z0 = mod.Hmean$Z0,
                                        mult = mod.Hmean$mult), Z = mod.Hmean$Z,covtype = "Gaussian")
        mod2.Lambda1ter <- hetGP::mleHetGP(X = list(X0 = mod.Lambda1ter$X0, Z0 = mod.Lambda1ter$Z0,
                                             mult = mod.Lambda1ter$mult), Z = mod.Lambda1ter$Z,covtype = "Gaussian")
        mod2.VolECMP <- hetGP::mleHetGP(X = list(X0 = mod.VolECMP$X0, Z0 = mod.VolECMP$Z0,
                                                    mult = mod.VolECMP$mult), Z = mod.VolECMP$Z,covtype = "Gaussian")
        mod2.VolECMPS <- hetGP::mleHetGP(X = list(X0 = mod.VolECMPS$X0, Z0 = mod.VolECMPS$Z0,
                                                    mult = mod.VolECMPS$mult), Z = mod.VolECMPS$Z,covtype = "Gaussian")
        mod2.GmDBH <- hetGP::mleHetGP(X = list(X0 = mod.GmDBH$X0, Z0 = mod.GmDBH$Z0,
                                                    mult = mod.GmDBH$mult), Z = mod.GmDBH$Z,covtype = "Gaussian")
        
        
        
        if(mod2.RateDBH$ll > mod.RateDBH$ll) {mod.RateDBH <- mod2.RateDBH
        }
        
        if(mod2.MeanAgb$ll > mod.MeanAgb$ll) {mod.MeanAgb <- mod2.MeanAgb
        }
        
        if(mod2.SdAgb$ll > mod.SdAgb$ll) {mod.SdAgb <- mod2.SdAgb
        }
        
        if(mod2.MeanAbu10$ll > mod.MeanAbu10$ll) {mod.MeanAbu10 <- mod2.MeanAbu10
        }
        
        if(mod2.SdAbu10$ll > mod.SdAbu10$ll) {mod.SdAbu10 <- mod2.SdAbu10
        }
        
        if(mod2.MeanAbu30$ll > mod.MeanAbu30$ll) {mod.MeanAbu30 <- mod2.MeanAbu30
        }
        
        if(mod2.SdAbu30$ll > mod.SdAbu30$ll) {mod.SdAbu30 <- mod2.SdAbu30
        }
        
        if(mod2.MeanGpp$ll > mod.MeanGpp$ll) {mod.MeanGpp <- mod2.MeanGpp
        }
        
        if(mod2.SdGpp$ll > mod.SdGpp$ll) {mod.SdGpp <- mod2.SdGpp
        }
        if(mod2.MeanNpp$ll > mod.MeanNpp$ll) {mod.MeanNpp <- mod2.MeanNpp
        }
        
        if(mod2.SdNpp$ll > mod.SdNpp$ll) {mod.SdNpp <- mod2.SdNpp
        }
        
        if(mod2.MeanBa10$ll > mod.MeanBa10$ll) {mod.MeanBa10 <- mod2.MeanBa10
        }
        
        if(mod2.SdBa10$ll > mod.SdBa10$ll) {mod.SdBa10 <- mod2.SdBa10
        }
        
        if(mod2.MeanHill$ll > mod.MeanHill$ll) {mod.MeanHill <- mod2.MeanHill
        }
        
        if(mod2.SdHill$ll > mod.SdHill$ll) {mod.SdHill <- mod2.SdHill
        }

        if(mod2.MeanRao$ll > mod.MeanRao$ll) {mod.MeanRao <- mod2.MeanRao
        }
        
        if(mod2.SdRao$ll > mod.SdRao$ll) {mod.SdRao <- mod2.SdRao
        }
        if(mod2.Hmean$ll > mod.Hmean$ll) {mod.Hmean <- mod2.Hmean
        }
        if(mod2.Lambda1ter$ll > mod.Lambda1ter$ll) {mod.Lambda1ter <- mod2.Lambda1ter
        }
        
        if(mod2.VolECMP$ll > mod.VolECMP$ll) {mod.VolECMP <- mod2.VolECMP
        }
        
        if(mod2.VolECMPS$ll > mod.VolECMPS$ll) {mod.VolECMPS <- mod2.VolECMPS
        }
        
        if(mod2.GmDBH$ll > mod.GmDBH$ll) {mod.GmDBH <- mod2.GmDBH
        }
      
        
        cat(paste0("Save temp R env iter ", j,"/",NiterHetGP))
        
        save(Generated_parameters,
             New_generated_parameters,
             Jrefresh,
             NiterHetGP,
             XGP,
             XRd,
             h.RateDBH,
             h.MeanAgb,
             h.SdAgb,
             h.MeanAbu10,
             h.SdAbu10,
             h.MeanAbu30,
             h.SdAbu30,
             h.MeanGpp,
             h.SdGpp,
             h.MeanNpp,
             h.SdNpp,
             h.MeanBa10,
             h.SdBa10,
             h.MeanHill,
             h.SdHill,
             h.MeanRao,
             h.SdRao,
             h.Hmean,
             h.Lambda1ter,
             h.VolECMP,
             h.VolECMPS,
             h.GmDBH,
             mod.RateDBH,
             mod.MeanAgb,
             mod.SdAgb,
             mod.MeanAbu10,
             mod.SdAbu10,
             mod.MeanAbu30,
             mod.SdAbu30,
             mod.MeanGpp,
             mod.SdGpp,
             mod.MeanNpp,
             mod.SdNpp,
             mod.MeanBa10,
             mod.SdBa10,
             mod.MeanHill,
             mod.SdHill,
             mod.MeanRao,
             mod.SdRao,
             mod.Hmean,
             mod.Lambda1ter,
             mod.VolECMP,
             mod.VolECMPS,
             mod.GmDBH,
             Y,
             file = paste0(PATH,
                           "tmp_env_save_iter",round(j/NiterHetGP,digits = 5),"_",
                           gsub(":", "-",
                                gsub(
                                  " ", "_",
                                  timestamp(
                                    prefix = "",
                                    suffix = "",
                                    quiet = T
                                  )
                                ))
                           ,".Rdata"))
        
      }
      
    }
    
    return(list("params" = Generated_parameters$params, 
                "paramLHS" = Generated_parameters$paramLHS,
                "nreplicat" = Generated_parameters$nreplicat,
                "X" = X, 
                "Y" = Y,
                "XGP" = XGP, 
                "XRd" = XRd, 
                "Nyears" = Generated_parameters$Nyears,
                "Nsampling" = Generated_parameters$Nsampling,
                "TROLL_clim_mth_params" = Generated_parameters$dataclim12mths,
                "TROLL_clim_dayvar_params" = Generated_parameters$dataclimdayvar,
                "TROLL_global_params" = Generated_parameters$TROLL_global_params,
                "TROLL_species_data" = Generated_parameters$TROLL_species_data,
                "Trait_phylo" = Generated_parameters$Trait_phylo,
                "initk" = Generated_parameters$initk,
                "NiterHetGP" = NiterHetGP,
                "GPmodels" = list("RateDBH" = list("h.RateDBH" = h.RateDBH,
                                                                                                 "mod.RateDBH" = mod.RateDBH),
                                                                                "MeanAgb" = list("h.MeanAgb" = h.MeanAgb,
                                                                                                 "mod.MeanAgb" = mod.MeanAgb),
                                                                                "SdAgb" = list("h.SdAgb" = h.SdAgb,
                                                                                                 "mod.SdAgb" = mod.SdAgb),
                                                                                "MeanSum10" = list("h.MeanSum10" = h.MeanAbu10,
                                                                                               "mod.MeanSum10" = mod.MeanAbu10),
                                                                                "SdSum10" = list("h.SdSum10" = h.SdAbu10,
                                                                                               "mod.SdAgb" = mod.SdAbu10),
                                                                                "MeanSum30" = list("h.MeanSum30" = h.MeanAbu30,
                                                                                                   "mod.MeanSum30" = mod.MeanAbu30),
                                                                                "SdSum30" = list("h.SdSum10" = h.SdAbu30,
                                                                                                 "mod.SdAgb" = mod.SdAbu30),
                                                                                "MeanGpp" = list("h.MeanGpp" = h.MeanGpp,
                                                                                                   "mod.MeanGpp" = mod.MeanGpp),
                                                                                "SdGpp" = list("h.SdSpp" = h.SdGpp,
                                                                                                 "mod.Spp" = mod.SdGpp),
                                                                                "MeanNpp" = list("h.MeanNpp" = h.MeanNpp,
                                                                                                 "mod.MeanNpp" = mod.MeanNpp),
                                                                                "SdNpp" = list("h.SdNpp" = h.SdNpp,
                                                                                               "mod.SdNpp" = mod.SdNpp),
                                                                                "MeanBa10" = list("h.MeanBa10" = h.MeanBa10,
                                                                                                 "mod.MeanBa10" = mod.MeanBa10),
                                                                                "SdBa10" = list("h.SdBa10" = h.SdBa10,
                                                                                               "mod.SdBa10" = mod.SdBa10),
                                                                                "MeanHill" = list("h.MeanHill" = h.MeanHill,
                                                                                                  "mod.MeanHill" = mod.MeanHill),
                                                                                "SdHill" = list("h.SdHill" = h.SdHill,
                                                                                                "mod.SdHill" = mod.SdHill),
                                                                                "MeanRao" = list("h.MeanRao" = h.MeanRao,
                                                                                                  "mod.MeanRao" = mod.MeanRao),
                                                                                "SdHill" = list("h.SdRao" = h.SdRao,
                                                                                                "mod.SdRao" = mod.SdRao),
                                                                                "Hmean" = list("h.Hmean" = h.Hmean,
                                                                                                 "mod.Hmean" = mod.Hmean),
                                                                                "Lambda1ter" = list("h.Lambda1ter" = h.Lambda1ter,
                                                                                               "mod.Lambda1ter" = mod.Lambda1ter),
                                                                                "VolECMP" = list("h.VolECMP" = h.VolECMP,
                                                                                                    "mod.VolECMP" = mod.VolECMP),
                                                                                "VolECMPS" = list("h.VolECMPS" = h.VolECMPS,
                                                                                                 "mod.VolECMPS" = mod.VolECMPS),
                                                                                "GmDBH" = list("h.GmDBH" = h.GmDBH,
                                                                                                  "mod.GmDBH" = mod.GmDBH)
                                                                                )))
    
    
  }else{
    
    
    XGP <- Generated_parameters$XGP
    
    prdataRateDBH <- hetGP::find_reps(XGP, Y[,1], rescale = FALSE, normalize = FALSE)
    prdataMeanAgb <- hetGP::find_reps(XGP, Y[,2], rescale = FALSE, normalize = FALSE)
    prdatSdAgb <- hetGP::find_reps(XGP, Y[,3], rescale = FALSE, normalize = FALSE)
    prdataMeanAbu10 <- hetGP::find_reps(XGP, Y[,4], rescale = FALSE, normalize = FALSE)
    prdataSdAbu10 <- hetGP::find_reps(XGP, Y[,5], rescale = FALSE, normalize = FALSE)
    prdataMeanAbu30 <- hetGP::find_reps(XGP, Y[,6], rescale = FALSE, normalize = FALSE)
    prdataSdAbu30 <- hetGP::find_reps(XGP, Y[,7], rescale = FALSE, normalize = FALSE)
    prdataMeanGpp <- hetGP::find_reps(XGP, Y[,8], rescale = FALSE, normalize = FALSE)
    prdataSdGpp <- hetGP::find_reps(XGP, Y[,9], rescale = FALSE, normalize = FALSE)
    prdataMeanNpp <- hetGP::find_reps(XGP, Y[,10], rescale = FALSE, normalize = FALSE)
    prdataSdNpp <- hetGP::find_reps(XGP, Y[,11], rescale = FALSE, normalize = FALSE)
    prdataMeanBa10 <- hetGP::find_reps(XGP, Y[,12], rescale = FALSE, normalize = FALSE)
    prdataSdBa10 <- hetGP::find_reps(XGP, Y[,13], rescale = FALSE, normalize = FALSE)
    prdataMeanHill <- hetGP::find_reps(XGP, Y[,14], rescale = FALSE, normalize = FALSE)
    prdataSdHill <- hetGP::find_reps(XGP, Y[,15], rescale = FALSE, normalize = FALSE)
    prdataMeanRao <- hetGP::find_reps(XGP, Y[,16], rescale = FALSE, normalize = FALSE)
    prdataSdRao <- hetGP::find_reps(XGP, Y[,17], rescale = FALSE, normalize = FALSE)
    prdataHmean <- hetGP::find_reps(XGP, Y[,18], rescale = FALSE, normalize = FALSE)
    prdataLambda1ter <- hetGP::find_reps(XGP, Y[,19], rescale = FALSE, normalize = FALSE)
    prdataVolECMP <- hetGP::find_reps(XGP, Y[,20], rescale = FALSE, normalize = FALSE)
    prdataVolECMPS <- hetGP::find_reps(XGP, Y[,21], rescale = FALSE, normalize = FALSE)
    prdataGmDBH <- hetGP::find_reps(XGP, Y[,22], rescale = FALSE, normalize = FALSE)
    
    
    mod.RateDBH <- hetGP::mleHetGP(X = list(X0 = prdataRateDBH$X0,Z0 = prdataRateDBH$Z0,mult=prdataRateDBH$mult), Z = prdataRateDBH$Z,covtype = "Gaussian")
    mod.MeanAgb <- hetGP::mleHetGP(X = list(X0 = prdataMeanAgb$X0,Z0 = prdataMeanAgb$Z0,mult=prdataMeanAgb$mult), Z = prdataMeanAgb$Z,covtype = "Gaussian")
    mod.SdAgb <- hetGP::mleHetGP(X = list(X0 = prdatSdAgb$X0,Z0 = prdatSdAgb$Z0,mult=prdatSdAgb$mult), Z = prdatSdAgb$Z,covtype = "Gaussian")
    mod.MeanAbu10 <- hetGP::mleHetGP(X = list(X0 = prdataMeanAbu10$X0,Z0 = prdataMeanAbu10$Z0,mult=prdataMeanAbu10$mult), Z = prdataMeanAbu10$Z,covtype = "Gaussian")
    mod.SdAbu10 <- hetGP::mleHetGP(X = list(X0 = prdataSdAbu10$X0,Z0 = prdataSdAbu10$Z0,mult=prdataSdAbu10$mult), Z = prdataSdAbu10$Z,covtype = "Gaussian")
    mod.MeanAbu30 <- hetGP::mleHetGP(X = list(X0 = prdataMeanAbu30$X0,Z0 = prdataMeanAbu30$Z0,mult=prdataMeanAbu30$mult), Z = prdataMeanAbu30$Z,covtype = "Gaussian")
    mod.SdAbu30 <- hetGP::mleHetGP(X = list(X0 = prdataSdAbu30$X0,Z0 = prdataSdAbu30$Z0,mult=prdataSdAbu30$mult), Z = prdataSdAbu30$Z,covtype = "Gaussian")
    mod.MeanGpp <- hetGP::mleHetGP(X = list(X0 = prdataMeanGpp$X0,Z0 = prdataMeanGpp$Z0,mult=prdataMeanGpp$mult), Z = prdataMeanGpp$Z,covtype = "Gaussian")
    mod.SdGpp <- hetGP::mleHetGP(X = list(X0 = prdataSdGpp$X0,Z0 = prdataSdGpp$Z0,mult=prdataSdGpp$mult), Z = prdataSdGpp$Z,covtype = "Gaussian")
    mod.MeanNpp <- hetGP::mleHetGP(X = list(X0 = prdataMeanNpp$X0,Z0 = prdataMeanNpp$Z0,mult=prdataMeanNpp$mult), Z = prdataMeanNpp$Z,covtype = "Gaussian")
    mod.SdNpp <- hetGP::mleHetGP(X = list(X0 = prdataSdNpp$X0,Z0 = prdataSdNpp$Z0,mult=prdataSdNpp$mult), Z = prdataSdNpp$Z,covtype = "Gaussian")
    mod.MeanBa10 <- hetGP::mleHetGP(X = list(X0 = prdataMeanBa10$X0,Z0 = prdataMeanBa10$Z0,mult=prdataMeanBa10$mult), Z = prdataMeanBa10$Z,covtype = "Gaussian")
    mod.SdBa10 <- hetGP::mleHetGP(X = list(X0 = prdataSdBa10$X0,Z0 = prdataSdBa10$Z0,mult=prdataSdBa10$mult), Z = prdataSdBa10$Z,covtype = "Gaussian")
    mod.MeanHill <- hetGP::mleHetGP(X = list(X0 = prdataMeanHill$X0,Z0 = prdataMeanHill$Z0,mult=prdataMeanHill$mult), Z = prdataMeanHill$Z,covtype = "Gaussian")
    mod.SdHill <- hetGP::mleHetGP(X = list(X0 = prdataSdHill$X0,Z0 = prdataSdHill$Z0,mult=prdataSdHill$mult), Z = prdataSdHill$Z,covtype = "Gaussian")
    mod.MeanRao <- hetGP::mleHetGP(X = list(X0 = prdataMeanRao$X0,Z0 = prdataMeanRao$Z0,mult=prdataMeanRao$mult), Z = prdataMeanRao$Z,covtype = "Gaussian")
    mod.SdRao <- hetGP::mleHetGP(X = list(X0 = prdataSdRao$X0,Z0 = prdataSdRao$Z0,mult=prdataSdRao$mult), Z = prdataSdRao$Z,covtype = "Gaussian")
    mod.Hmean <- hetGP::mleHetGP(X = list(X0 = prdataHmean$X0,Z0 = prdataHmean$Z0,mult=prdataHmean$mult), Z = prdataHmean$Z,covtype = "Gaussian")
    mod.Lambda1ter <- hetGP::mleHetGP(X = list(X0 = prdataLambda1ter$X0,Z0 = prdataLambda1ter$Z0,mult=prdataLambda1ter$mult), Z = prdataLambda1ter$Z,covtype = "Gaussian")
    mod.VolECMP <- hetGP::mleHetGP(X = list(X0 = prdataVolECMP$X0,Z0 = prdataVolECMP$Z0,mult=prdataVolECMP$mult), Z = prdataVolECMP$Z,covtype = "Gaussian")
    mod.VolECMPS <- hetGP::mleHetGP(X = list(X0 = prdataVolECMPS$X0,Z0 = prdataVolECMPS$Z0,mult=prdataVolECMPS$mult), Z = prdataVolECMPS$Z,covtype = "Gaussian")
    mod.GmDBH <- hetGP::mleHetGP(X = list(X0 = prdataGmDBH$X0,Z0 = prdataGmDBH $Z0,mult=prdataGmDBH$mult), Z = prdataGmDBH$Z,covtype = "Gaussian")
    
    
    return(list("params" = Generated_parameters$params, 
                "paramLHS" = Generated_parameters$paramLHS,
                "nreplicat" = Generated_parameters$nreplicat,
                "X" = Generated_parameters$X, 
                "XGP" = Generated_parameters$XGP, 
                "XRd" = Generated_parameters$XRd,  
                "Y" = Y,
                "Nyears" = Generated_parameters$Nyears,
                "Nsampling" = Generated_parameters$Nsampling,
                "TROLL_clim_mth_params" = Generated_parameters$dataclim12mths,
                "TROLL_clim_dayvar_params" = Generated_parameters$dataclimdayvar,
                "TROLL_global_params" = Generated_parameters$TROLL_global_params,
                "TROLL_species_data" = Generated_parameters$TROLL_species_data,
                "Trait_phylo" = Generated_parameters$Trait_phylo,
                "initk" = Generated_parameters$initk,
                "NiterHetGP" = NiterHetGP,
                "GPmodels" = list("RateDBH" = list("h.RateDBH" = NULL,
                                                   "mod.RateDBH" = mod.RateDBH),
                                  "MeanAgb" = list("h.MeanAgb" = NULL,
                                                   "mod.MeanAgb" = mod.MeanAgb),
                                  "SdAgb" = list("h.SdAgb" = NULL,
                                                 "mod.SdAgb" = mod.SdAgb),
                                  "MeanSum10" = list("h.MeanSum10" = NULL,
                                                     "mod.MeanSum10" = mod.MeanAbu10),
                                  "SdSum10" = list("h.SdSum10" = h.SdAbu10,
                                                   "mod.SdAgb" = mod.SdAbu10),
                                  "MeanSum30" = list("h.MeanSum30" = NULL,
                                                     "mod.MeanSum30" = mod.MeanAbu30),
                                  "SdSum30" = list("h.SdSum10" = NULL,
                                                   "mod.SdAgb" = mod.SdAbu30),
                                  "MeanGpp" = list("h.MeanGpp" = NULL,
                                                   "mod.MeanGpp" = mod.MeanGpp),
                                  "SdGpp" = list("h.SdSpp" = NULL,
                                                 "mod.Spp" = mod.SdGpp),
                                  "MeanNpp" = list("h.MeanNpp" = NULL,
                                                   "mod.MeanNpp" = mod.MeanNpp),
                                  "SdNpp" = list("h.SdNpp" = NULL,
                                                 "mod.SdNpp" = mod.SdNpp),
                                  "MeanBa10" = list("h.MeanBa10" = NULL,
                                                    "mod.MeanBa10" = mod.MeanBa10),
                                  "SdBa10" = list("h.SdBa10" = NULL,
                                                  "mod.SdBa10" = mod.SdBa10),
                                  "MeanHill" = list("h.MeanHill" = NULL,
                                                    "mod.MeanHill" = mod.MeanHill),
                                  "SdHill" = list("h.SdHill" = NULL,
                                                  "mod.SdHill" = mod.SdHill),
                                  "MeanRao" = list("h.MeanRao" = NULL,
                                                   "mod.MeanRao" = mod.MeanRao),
                                  "SdHill" = list("h.SdRao" = NULL,
                                                  "mod.SdRao" = mod.SdRao),
                                  "Hmean" = list("h.Hmean" = NULL,
                                                 "mod.Hmean" = mod.Hmean),
                                  "Lambda1ter" = list("h.Lambda1ter" = NULL,
                                                      "mod.Lambda1ter" = mod.Lambda1ter),
                                  "VolECMP" = list("h.VolECMP" = NULL,
                                                   "mod.VolECMP" = mod.VolECMP),
                                  "VolECMPS" = list("h.VolECMPS" = NULL,
                                                    "mod.VolECMPS" = mod.VolECMPS),
                                  "GmDBH" = list("h.GmDBH" = NULL,
                                                 "mod.GmDBH" = mod.GmDBH)
                )))
  }
  
  
  
  
}