Generate_LHS_Autocalib <- function(nsim = 100, 
                                   Nyears = 600,
                                   Nsampling = 100,
                                   nparam = NULL,
                                   paramLHS){
  if (is.null(nparam)) {
    stop("nparam have to specified")
  }
  
  default_values <- generate_parameters(iterperyear = 12, nbiter = 1)
  
  X <- lhs::maximinLHS(n = nsim, k = nparam +1,debug = FALSE)
  Xparam <- matrix(nrow = nsim,ncol = 15)
  
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
              "Nsampling" = Nsampling))
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


dtexp <- function(x, rate, low, upp)
{
  PU <- pexp(upp, rate=rate)
  PL <- pexp(low, rate=rate)
  dexp(x, rate) / (PU-PL) * (x >= low) * (x <= upp) 
}
ptexp <- function(q, rate, low, upp)
{
  PU <- pexp(upp, rate=rate)
  PL <- pexp(low, rate=rate)
  (pexp(q, rate)-PL) / (PU-PL) * (q >= low) * (q <= upp) + 1 * (q > upp)
}

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
    DBHParam <- try(fitdist(DBHData, distr = "texp", method="mle", start=list(rate=5), fix.arg=list(low=0.01, upp=max(DBHData))))
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
        
        Hilltmp <- as.numeric (entropart::Diversity(Ns = entropart::as.AbdVector(DiversityList),q=1))
        Raotmp <- as.numeric (entropart::PhyloDiversity(Ns = entropart::as.AbdVector(DiversityList),q = 1,Tree = Trait_phylo)$Total)
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
  
  lambda <- try(get_lambda(CHM,sumCHM))
    
  EstParam <- c(EstParam,lambda)
    
  
  if (length(FinalPattern$dbh) > 0) {
    splited_species <- do.call(rbind, strsplit(FinalPattern$s_name, split = "_"))
    Inventory <- FinalPattern %>% mutate(Genus = splited_species[,1], Species = splited_species[,2]) %>% filter(dbh *100 > 10)
    CommercialInventory <- na.exclude(LoggingLab::commercialcriteriajoin(Inventory,LoggingLab::SpeciesCriteria))  %>% mutate(Vol = pi * dbh*(height - CD)) %>% filter(dbh * 100 > MinFD & dbh * 100 < MaxFD)
    CommercialInventory %>% filter(CommercialLevel == "1") 
    ECMP <- CommercialInventory %>% filter(CommercialLevel == "1")
    ECMS <- CommercialInventory %>% filter(CommercialLevel == "1"| CommercialLevel == "2")
    ECMPVol <- sum(c( ECMP$Vol,0))/((bbox[1]*bbox[2])*1E-4)
    ECMSVol <- sum(c( ECMS$Vol,0))/((bbox[1]*bbox[2])*1E-4)
    EstParam <- c(EstParam,ECMPVol,ECMSVol)
    
    
    EstParam <- c(EstParam,exp(mean(log(FinalPattern$dbh))))
  }else{
    EstParam <- c(EstParam,NA,NA,NA)
  }
  
  return(EstParam)
}

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

autocalibGP <- function(Generated_parameters,
                        PATH,
                        FILEsave = NULL,
                        initj = 1,
                        NiterHetGP = NULL,
                        Jrefresh = 25,
                        ncores = 2){
  
  
  WIP_folder_PATH = tempdir()
  
  if (!is.null(FILEsave)) {
    load(paste0(PATH,FILEsave))
  }
  
  nrep <- tibble(Sim_ID = unique(Generated_parameters$TROLL_global_params$simulation)) %>% 
    mutate(ID_iter = ceiling(row_number()/ncores))
  
  
  for (blocki in (Generated_parameters$initk/ncores +1):max(nrep$ID_iter)) {
    
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
                                  cores = ncores)
    
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
    
    if (initk == 0) {
      Y <- Yi
    }else{
      Y <- rbind(Y,Yi)
    }
    
    Generated_parameters$initk <- blocki * ncores
    
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
    
    XGP <- Generated_parameters$XGP
    
    prdataRateDBH <- find_reps(XGP, Y[,1], rescale = FALSE, normalizoe = FALSE)
    prdataMeanAgb <- find_reps(XGP, Y[,1], rescale = FALSE, normalize = FALSE)
    prdatSdAgb <- find_reps(XGP, Y[,2], rescale = FALSE, normalize = FALSE)
    prdataMeanAbu <- find_reps(XGP, Y[,3], rescale = FALSE, normalize = FALSE)
    prdataSdAbu <- find_reps(XGP, Y[,4], rescale = FALSE, normalize = FALSE)
    prdataMeanHill <- find_reps(XGP, Y[,5], rescale = FALSE, normalize = FALSE)
    prdataSdHill <- find_reps(XGP, Y[,6], rescale = FALSE, normalize = FALSE)
    prdataMeanGpp <- find_reps(XGP, Y[,7], rescale = FALSE, normalize = FALSE)
    prdataSdGpp <- find_reps(XGP, Y[,8], rescale = FALSE, normalize = FALSE)
    prdataMeanRao <- find_reps(XGP, Y[,9], rescale = FALSE, normalize = FALSE)
    prdataSdRao <- find_reps(XGP, Y[,10], rescale = FALSE, normalize = FALSE)
    prdataHmean <- find_reps(XGP, Y[,11], rescale = FALSE, normalize = FALSE)
    prdataLambda1ter <- find_reps(XGP, Y[,13], rescale = FALSE, normalize = FALSE)
    
    mod.RateDBH <- mleHetGP(X = list(X0 = prdataRateDBH$X0,Z0 = prdataRateDBH$Z0,mult=prdataRateDBH$mult), Z = prdataRateDBH$Z,covtype = "Gaussian")
    mod.MeanAgb <- mleHetGP(X = list(X0 = prdataMeanAgb$X0,Z0 = prdataMeanAgb$Z0,mult=prdataMeanAgb$mult), Z = prdataMeanAgb$Z,covtype = "Gaussian")
    mod.SdAgb <- mleHetGP(X = list(X0 = prdatSdAgb$X0,Z0 = prdatSdAgb$Z0,mult=prdatSdAgb$mult), Z = prdatSdAgb$Z,covtype = "Gaussian")
    mod.MeanAbu <- mleHetGP(X = list(X0 = prdataMeanAbu$X0,Z0 = prdataMeanAbu$Z0,mult=prdataMeanAbu$mult), Z = prdataMeanAbu$Z,covtype = "Gaussian")
    mod.SdAbu <- mleHetGP(X = list(X0 = prdataSdAbu$X0,Z0 = prdataSdAbu$Z0,mult=prdataSdAbu$mult), Z = prdataSdAbu$Z,covtype = "Gaussian")
    mod.MeanHill <- mleHetGP(X = list(X0 = prdataMeanHill$X0,Z0 = prdataMeanHill$Z0,mult=prdataMeanHill$mult), Z = prdataMeanHill$Z,covtype = "Gaussian")
    mod.SdHill <- mleHetGP(X = list(X0 = prdataSdHill$X0,Z0 = prdataSdHill$Z0,mult=prdataSdHill$mult), Z = prdataSdHill$Z,covtype = "Gaussian")
    mod.MeanGpp <- mleHetGP(X = list(X0 = prdataMeanGpp$X0,Z0 = prdataMeanGpp$Z0,mult=prdataMeanGpp$mult), Z = prdataMeanGpp$Z,covtype = "Gaussian")
    mod.SdGpp <- mleHetGP(X = list(X0 = prdataSdGpp$X0,Z0 = prdataSdGpp$Z0,mult=prdataSdGpp$mult), Z = prdataSdGpp$Z,covtype = "Gaussian")
    mod.MeanRao <- mleHetGP(X = list(X0 = prdataMeanRao$X0,Z0 = prdataMeanRao$Z0,mult=prdataMeanRao$mult), Z = prdataMeanRao$Z,covtype = "Gaussian")
    mod.SdRao <- mleHetGP(X = list(X0 = prdataSdRao$X0,Z0 = prdataSdRao$Z0,mult=prdataSdRao$mult), Z = prdataSdRao$Z,covtype = "Gaussian")
    mod.Hmean <- mleHetGP(X = list(X0 = prdataHmean$X0,Z0 = prdataHmean$Z0,mult=prdataHmean$mult), Z = prdataHmean$Z,covtype = "Gaussian")
    mod.Lambda1ter <- mleHetGP(X = list(X0 = prdataLambda1ter$X0,Z0 = prdataLambda1ter$Z0,mult=prdataLambda1ter$mult), Z = prdataLambda1ter$Z,covtype = "Gaussian")
    
    h.RateDBH <- rep(0, NiterHetGP)
    h.MeanAgb <- rep(0, NiterHetGP)
    h.SdAgb <- rep(0, NiterHetGP)
    h.MeanAbu <- rep(0, NiterHetGP)
    h.SdAbu <- rep(0, NiterHetGP)
    h.MeanHill <- rep(0, NiterHetGP)
    h.SdHill <- rep(0, NiterHetGP)
    h.MeanGpp <- rep(0, NiterHetGP)
    h.SdGpp <- rep(0, NiterHetGP)
    h.MeanRao <- rep(0, NiterHetGP)
    h.SdRao <- rep(0, NiterHetGP)
    h.Hmean <- rep(0, NiterHetGP)
    h.Lambda1ter <- rep(0, NiterHetGP)
    
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
                      ))))
      
      h.RateDBH[j] <- horizon(mod.RateDBH)
      h.MeanAgb[j] <- horizon(mod.MeanAgb)
      h.SdAgb[j] <- horizon(mod.SdAgb)
      h.MeanAbu[j] <- horizon(mod.MeanAbu)
      h.SdAbu[j] <- horizon(mod.SdAbu)
      h.MeanHill[j] <- horizon(mod.MeanHill)
      h.SdHill[j] <- horizon(mod.SdHill)
      h.MeanGpp[j] <- horizon(mod.MeanGpp)
      h.SdGpp[j] <- horizon(mod.SdGpp)
      h.MeanRao[j] <- horizon(mod.MeanRao)
      h.SdRao[j] <- horizon(mod.SdRao)
      h.Hmean[j] <- horizon(mod.Hmean)
      h.Lambda1ter[j] <- horizon(mod.Lambda1ter)
      
      cat(paste0("IMSPE optimisation iter ", j,"/",NiterHetGP,"_",
                 gsub(":", "-",
                      gsub(
                        " ", "_",
                        timestamp(
                          prefix = "",
                          suffix = "",
                          quiet = T
                        )
                      ))))
      
      opt.RateDBH <- IMSPE_optim(mod.RateDBH, h = h.RateDBH[j],ncores = ncores)
      opt.MeanAgb <- IMSPE_optim(mod.MeanAgb, h = h.MeanAgb[j],ncores = ncores)
      opt.SdAgb <- IMSPE_optim(mod.SdAgb, h = h.SdAgb[j],ncores = ncores)
      opt.MeanAbu <- IMSPE_optim(mod.MeanAbu, h = h.MeanAbu[j],ncores = ncores)
      opt.SdAbu <- IMSPE_optim(mod.SdAbu, h = h.SdAbu[j], ncores = ncores)
      opt.MeanHill <- IMSPE_optim(mod.MeanHill, h = h.MeanHill[j], ncores = ncores,
                                  control = list(tol_dist = 1e-06, tol_diff = 1e-06, multi.start = 20, maxit = 100))
      opt.SdHill<- IMSPE_optim(mod.SdHill, h = h.SdHill[j], ncores = ncores)
      opt.MeanGpp <- IMSPE_optim(mod.MeanGpp, h = h.MeanGpp[j], ncores = ncores)
      opt.SdGpp<- IMSPE_optim(mod.SdGpp, h = h.SdGpp[j], ncores = ncores)
      opt.MeanRao <- IMSPE_optim(mod.MeanRao, h = h.MeanRao[j], ncores = ncores)
      opt.SdRao<- IMSPE_optim(mod.SdRao, h = h.SdRao[j], ncores = ncores)
      opt.Hmean <- IMSPE_optim(mod.Hmean, h = h.Hmean[j], ncores = ncores)
      opt.Lambda1ter<- IMSPE_optim(mod.Lambda1ter, h = h.Lambda1ter[j], ncores = ncores)
      
      Xnew <- rbind(opt.MeanAgb$par,opt.SdAgb$par,opt.RateDBH$par,
                    opt.MeanAbu$par,opt.SdAbu$par,opt.MeanHill$par,opt.SdHill$par,
                    opt.MeanGpp$par,opt.SdGpp$par,opt.MeanRao$par,opt.SdRao$par,
                    opt.Hmean$par, opt.Lambda1ter$par)
      
      XRd<- lhs::augmentLHS(XRd, 7)
      XRdNew <- XRd[(dim(XRd)[1]-6):(dim(XRd)[1]),]
      
      
      X <- rbind(Xnew,XRdNew)
      XGP <- rbind(XGP, X)
      
      default_values <- generate_parameters(iterperyear = 12, nbiter = 1)
      Xparam <- matrix(nrow = 20,ncol = 15)
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
        mutate(ID_iter = ceiling(row_number()/ncores))
      
      
      for (blocki in (New_generated_parameters$initk/ncores +1):max(nrep$ID_iter)) {
        
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
                                      name = "sim_stack",
                                      simulations = simulations,
                                      global = New_generated_parameters$TROLL_global_params %>% 
                                        filter(simulation %in% simulations),
                                      species = New_generated_parameters$TROLL_species_data  %>% 
                                        filter(simulation %in% simulations),
                                      climate = New_generated_parameters$TROLL_clim_mth_params,
                                      daily = New_generated_parameters$TROLL_clim_dayvar_params,
                                      verbose = FALSE,
                                      thin = (12*(New_generated_parameters$Nyears - New_generated_parameters$Nsampling +1 )):(12*New_generated_parameters$Nyears),
                                      cores = ncores)
        
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
                      ))))
      
      mod.RateDBH <- update(mod.RateDBH,Xnew = Xnew,Znew  = Ynew[,1],ginit =mod.RateDBH$g*1.01 )
      mod.MeanAgb <- update(mod.MeanAgb,Xnew = Xnew,Znew = Ynew[,1],ginit =mod.MeanAgb$g*1.01)
      mod.SdAgb <- update(mod.SdAgb,Xnew = Xnew,Znew = Ynew[,2],ginit =mod.SdAgb$g*1.01)
      mod.MeanAbu <- update(mod.MeanAbu,Xnew = Xnew,Znew = Ynew[,3],ginit =mod.MeanAbu$g*1.01)
      mod.SdAbu <-update(mod.SdAbu,Xnew = Xnew,Znew = Ynew[,4],ginit =mod.SdAbu$g*1.01)
      mod.MeanHill <- update(mod.MeanHill,Xnew = Xnew,Znew = Ynew[,5],ginit =mod.MeanHill$g*1.01)
      mod.SdHill <- update(mod.SdHill,Xnew = Xnew,Znew = Ynew[,6],ginit =mod.SdHill$g*1.01)
      mod.MeanGpp <- update(mod.MeanGpp,Xnew = Xnew,Znew = Ynew[,7],ginit =mod.MeanGpp$g*1.01)
      mod.SdGpp <- update(mod.SdGpp,Xnew = Xnew,Znew = Ynew[,8],ginit =mod.SdGpp$g*1.01)
      mod.MeanRao <- update(mod.MeanRao,Xnew = Xnew,Znew = Ynew[,9],ginit =mod.MeanRao$g*1.01)
      mod.SdRao <- update(mod.SdRao,Xnew = Xnew,Znew = Ynew[,10],ginit =mod.SdRao$g*1.01)
      mod.Hmean <- update(mod.Hmean,Xnew = Xnew,Znew = Ynew[,11],ginit =mod.Hmean$g*1.01)
      mod.Lambda1ter <- update(mod.Lambda1ter,Xnew = Xnew,Znew = Ynew[,13],ginit =mod.Lambda1ter$g*1.01)
      
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
        mod2.RateDBH <- mleHetGP(X = list(X0 = mod.RateDBH$X0, Z0 = mod.RateDBH$Z0,
                                          mult = mod.RateDBH$mult), Z = mod.RateDBH$Z,covtype = "Gaussian")
        mod2.MeanAgb <- mleHetGP(X = list(X0 = mod.MeanAgb$X0, Z0 = mod.MeanAgb$Z0,
                                          mult = mod.MeanAgb$mult), Z = mod.MeanAgb$Z,covtype = "Gaussian")
        mod2.SdAgb <- mleHetGP(X = list(X0 = mod.SdAgb$X0, Z0 = mod.SdAgb$Z0,
                                        mult = mod.SdAgb$mult), Z = mod.SdAgb$Z,covtype = "Gaussian")
        mod2.MeanAbu <- mleHetGP(X = list(X0 = mod.MeanAbu$X0, Z0 = mod.MeanAbu$Z0,
                                          mult = mod.MeanAbu$mult), Z = mod.MeanAbu$Z,covtype = "Gaussian")
        mod2.SdAbu <- mleHetGP(X = list(X0 = mod.SdAbu$X0, Z0 = mod.SdAbu$Z0,
                                        mult = mod.SdAbu$mult), Z = mod.SdAbu$Z,covtype = "Gaussian")
        mod2.MeanHill <- mleHetGP(X = list(X0 = mod.MeanHill$X0, Z0 = mod.MeanHill$Z0,
                                           mult = mod.MeanHill$mult), Z = mod.MeanHill$Z,covtype = "Gaussian")
        mod2.SdHill <- mleHetGP(X = list(X0 = mod.SdHill$X0, Z0 = mod.SdHill$Z0,
                                         mult = mod.SdHill$mult), Z = mod.SdHill$Z,covtype = "Gaussian")
        mod2.MeanGpp <- mleHetGP(X = list(X0 = mod.MeanGpp$X0, Z0 = mod.MeanGpp$Z0,
                                          mult = mod.MeanGpp$mult), Z = mod.MeanGpp$Z,covtype = "Gaussian")
        mod2.SdGpp <- mleHetGP(X = list(X0 = mod.SdGpp$X0, Z0 = mod.SdGpp$Z0,
                                        mult = mod.SdGpp$mult), Z = mod.SdGpp$Z,covtype = "Gaussian")
        mod2.MeanRao <- mleHetGP(X = list(X0 = mod.MeanRao$X0, Z0 = mod.MeanRao$Z0,
                                          mult = mod.MeanRao$mult), Z = mod.MeanRao$Z,covtype = "Gaussian")
        mod2.SdRao <- mleHetGP(X = list(X0 = mod.SdRao$X0, Z0 = mod.SdRao$Z0,
                                        mult = mod.SdRao$mult), Z = mod.SdRao$Z,covtype = "Gaussian")
        mod2.Hmean <- mleHetGP(X = list(X0 = mod.Hmean$X0, Z0 = mod.Hmean$Z0,
                                        mult = mod.Hmean$mult), Z = mod.Hmean$Z,covtype = "Gaussian")
        mod2.Lambda1ter <- mleHetGP(X = list(X0 = mod.Lambda1ter$X0, Z0 = mod.Lambda1ter$Z0,
                                             mult = mod.Lambda1ter$mult), Z = mod.Lambda1ter$Z,covtype = "Gaussian")
        
        
        
        if(mod2.RateDBH$ll > mod.RateDBH$ll) {mod.RateDBH <- mod2.RateDBH
        }
        
        if(mod2.MeanAgb$ll > mod.MeanAgb$ll) {mod.MeanAgb <- mod2.MeanAgb
        }
        
        if(mod2.SdAgb$ll > mod.SdAgb$ll) {mod.SdAgb <- mod2.SdAgb
        }
        
        if(mod2.MeanAbu$ll > mod.MeanAbu$ll) {mod.MeanAbu <- mod2.MeanAbu
        }
        
        if(mod2.SdAbu$ll > mod.SdAbu$ll) {mod.SdAbu <- mod2.SdAbu
        }
        
        if(mod2.MeanHill$ll > mod.MeanHill$ll) {mod.MeanHill <- mod2.MeanHill
        }
        
        if(mod2.SdHill$ll > mod.SdHill$ll) {mod.SdHill <- mod2.SdHill
        }
        
        if(mod2.MeanGpp$ll > mod.MeanGpp$ll) {mod.MeanGpp <- mod2.MeanGpp
        }
        
        if(mod2.SdGpp$ll > mod.SdGpp$ll) {mod.SdGpp <- mod2.SdGpp
        }
        if(mod2.MeanRao$ll > mod.MeanRao$ll) {mod.MeanRao <- mod2.MeanRao
        }
        
        if(mod2.SdRao$ll > mod.SdRao$ll) {mod.SdRao <- mod2.SdRao
        }
        if(mod2.Hmean$ll > mod.Hmean$ll) {mod.Hmean <- mod2.Hmean
        }
        if(mod2.Lambda1ter$ll > mod.Lambda1ter$ll) {mod.Lambda1ter <- mod2.Lambda1ter
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
             h.MeanAbu,
             h.SdAbu,
             h.MeanGpp,
             h.SdGpp,
             h.MeanHill,
             h.SdHill,
             h.MeanRao,
             h.SdRao,
             h.Hmean,
             h.Lambda1ter,
             mod.RateDBH,
             mod.MeanAgb,
             mod.SdAgb,
             mod.MeanAbu,
             mod.SdAbu,
             mod.MeanGpp,
             mod.SdGpp,
             mod.MeanHill,
             mod.SdHill,
             mod.MeanRao,
             mod.SdRao,
             mod.Hmean,
             mod.Lambda1ter,
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
                "X" = X,
                "Y" = Y,
                "Generated_parameters" = Generated_parameters,"GPmodels" = list("RateDBH" = list("h.RateDBH" = h.RateDBH,
                                                                                                 "mod.RateDBH" = mod.RateDBH),
                                                                                "MeanAgb" = list("h.MeanAgb" = h.MeanAgb,
                                                                                                 "mod.MeanAgb" = mod.MeanAgb),
                                                                                "SdAgb" = list("h.SdAgb" = h.SdAgb,
                                                                                                 "mod.SdAgb" = mod.SdAgb),
                                                                                "MeanSum1" = list("h.MeanSum1" = h.MeanAbu,
                                                                                               "mod.MeanSum1" = mod.MeanAbu))))
    
    
  }else{
    return(list("params" = Generated_parameters$params, 
                "X" = Generated_parameters$X, 
                "XGP" = Generated_parameters$XGP, 
                "XRd" = Generated_parameters$XRd, 
                "Nyears" = Generated_parameters$Nyears,
                "Nsampling" = Generated_parameters$Nsampling,
                "TROLL_clim_mth_params" = Generated_parameters$dataclim12mths,
                "TROLL_clim_dayvar_params" = Generated_parameters$dataclimdayvar,
                "TROLL_global_params" = Generated_parameters$TROLL_global_params,
                "TROLL_species_data" = Generated_parameters$TROLL_species_data,
                "Trait_phylo" = Generated_parameters$Trait_phylo,
                "initk" = Generated_parameters$initk,
                "Y" = Y))
  }
  
  
  
  
}