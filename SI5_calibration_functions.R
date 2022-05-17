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
                        NiterHetGP = 100,
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
      
      
    }
    
    
    
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