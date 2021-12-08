#' Generate parameters
#'
#' @param cols num. 
#' @param rows num. 
#' @param HEIGHT num. 
#' @param length_dcell num. 
#' @param nbiter num. 
#' @param iterperyear num. 
#' @param NV num. 
#' @param NH num. 
#' @param nbout num. 
#' @param nbspp num. 
#' @param SWtoPPFD num. 
#' @param p_nonvert num. 
#' @param klight num. 
#' @param phi num. 
#' @param absorptance_leaves num. 
#' @param theta num. 
#' @param g1 num. 
#' @param vC num. 
#' @param DBH0 num. 
#' @param H0 num. 
#' @param CR_min num. 
#' @param CR_a num. 
#' @param CR_b num. 
#' @param CD_a num. 
#' @param CD_b num. 
#' @param CD0 num. 
#' @param shape_crown num. 
#' @param dens num. 
#' @param fallocwood num. 
#' @param falloccanopy num. 
#' @param Cseedrain num. 
#' @param nbs0 num. 
#' @param sigma_height num. 
#' @param sigma_CR num. 
#' @param sigma_CD num. 
#' @param sigma_P num. 
#' @param sigma_N num. 
#' @param sigma_LMA num. 
#' @param sigma_wsg num. 
#' @param sigma_dbhmax num. 
#' @param corr_CR_height num. 
#' @param corr_N_P num. 
#' @param corr_N_LMA num. 
#' @param corr_P_LMA num. 
#' @param leafdem_resolution num. 
#' @param p_tfsecondary num. 
#' @param hurt_decay num. 
#' @param crown_gap_fraction num. 
#' @param m num. 
#' @param m1 num. 
#' @param Cair num. 
#' @param LL_parameterization num. 
#' @param LA_regulation num. 
#' @param sapwood num. 
#' @param seedsadditional num. 
#' 
#' @return a data frame of global parameters
#'
#' @export
#'
#' @examples
#' NA
#' 
generate_parameters <- function(
  cols = 200,
  rows = 200,
  HEIGHT = 70,
  length_dcell = 25,
  nbiter = 10,
  iterperyear = 365,
  NV = 1,
  NH = 1,
  nbout = 4,
  nbspp = 45,
  SWtoPPFD = 2.27,
  p_nonvert = 0.05,
  klight = 0.63,
  phi = 0.093,
  absorptance_leaves = 0.9,
  theta = 0.7,
  g1 = 3.77,
  vC = 0.021,
  DBH0 = 0.005,
  H0 = 0.950,
  CR_min = 0.3,
  CR_a = 2.13,
  CR_b = 0.63,
  CD_a = 0,
  CD_b = 0.2,
  CD0 = 0.3,
  shape_crown = 0.72,
  dens = 1,
  fallocwood = 0.35,
  falloccanopy = 0.25,
  Cseedrain = 50000,
  nbs0 = 10,
  sigma_height = 0.19,
  sigma_CR = 0.29,
  sigma_CD = 0,
  sigma_P = 0.24,
  sigma_N = 0.12,
  sigma_LMA = 0.24,
  sigma_wsg = 0.06,
  sigma_dbhmax = 0.05,
  corr_CR_height = 0,
  corr_N_P = 0.65,
  corr_N_LMA = -0.43,
  corr_P_LMA = -0.39,
  leafdem_resolution = 30,
  p_tfsecondary =  1,
  hurt_decay = 0,
  crown_gap_fraction = 0.15,
  m = 0.013,
  m1 = 0.013,
  Cair = 400,
  LL_parameterization = 1,
  LA_regulation = 2,
  sapwood = 1,
  seedsadditional = 0
){
  # check args
  if(!all(unlist(lapply(
    list(cols, rows, HEIGHT, length_dcell, nbiter, iterperyear,
         NV, NH, nbout, nbspp, SWtoPPFD, p_nonvert, klight, phi, 
         absorptance_leaves, theta, g1, vC, DBH0, H0, CR_min, 
         CR_a, CR_b, CD_a, CD_b, CD0, shape_crown, dens, fallocwood, 
         falloccanopy, Cseedrain, nbs0, sigma_height, sigma_CR,
         sigma_CD, sigma_P, sigma_N, sigma_LMA, sigma_wsg, sigma_dbhmax,
         corr_CR_height, corr_N_P, corr_N_LMA, corr_P_LMA,
         leafdem_resolution, p_tfsecondary, hurt_decay, crown_gap_fraction, 
         m, m1, Cair, LL_parameterization, LA_regulation, 
         sapwood, seedsadditional), class)) == "numeric"))
    stop("parameters should be numeric.")
  
  data.frame(
    param = c("cols", "rows", "HEIGHT", "length_dcell",
              "nbiter", "iterperyear", "NV", "NH", "nbout",
              "nbspp", "SWtoPPFD", "p_nonvert", "klight", "phi",
              "absorptance_leaves", "theta", "g1", "vC", "DBH0",
              "H0", "CR_min", "CR_a", "CR_b", "CD_a", "CD_b",
              "CD0", "shape_crown", "dens", "fallocwood", 
              "falloccanopy", "Cseedrain", "nbs0", "sigma_height",
              "sigma_CR", "sigma_CD", "sigma_P", "sigma_N", 
              "sigma_LMA", "sigma_wsg", "sigma_dbhmax", "corr_CR_height", 
              "corr_N_P", "corr_N_LMA", "corr_P_LMA", "leafdem_resolution", 
              "p_tfsecondary", "hurt_decay", "crown_gap_fraction", 
              "m", "m1", "Cair", "_LL_parameterization", 
              "_LA_regulation", "_sapwood", "_seedsadditional"),
    value = c(cols, rows, HEIGHT, length_dcell, nbiter, iterperyear,
             NV, NH, nbout, nbspp, SWtoPPFD, p_nonvert, klight, phi, 
             absorptance_leaves, theta, g1, vC, DBH0, H0, CR_min, 
             CR_a, CR_b, CD_a, CD_b, CD0, shape_crown, dens, fallocwood, 
             falloccanopy, Cseedrain, nbs0, sigma_height, sigma_CR,
             sigma_CD, sigma_P, sigma_N, sigma_LMA, sigma_wsg, sigma_dbhmax,
             corr_CR_height, corr_N_P, corr_N_LMA, corr_P_LMA,
             leafdem_resolution, p_tfsecondary, hurt_decay, crown_gap_fraction, 
             m, m1, Cair, LL_parameterization, LA_regulation, 
             sapwood, seedsadditional),
    description = c(
      "/* nb of columns */",
      "/* nb of rows  */",
      "/* vertical extent of simulation */",
      "/* linear size of a dcell */",
      "/* total nb of timesteps */",
      "/* number of iteration per year */",
      "/* vertical nb of cells (nb per m) */",
      "/* horizontal nb of cells (nb per m) */",
      "/* Number of outputs */",
      "/* Number of species */",
      "/* convert short wave irradiance to PAR photons (cf. code) */",
      "/* light incidence param (diff through turbid medium) */",
      "/* light attenuation in the canopy Beer-Lambert */",
      "/*  quantum yield (in micromol C/micromol photon) */",
      "/* absorptance of individual leaves */",
      "/* parameter of the Farquhar model */",
      "/* parameter g1 of Medlyn et al s stomatal conductance model */",
      "/* variance of the flexion moment */",
      "/* initial dbh (m) */",
      "/* initial height (m) */",
      "/* minimum crown radius (in m) */",
      "/* CR log intercept or Michaelis Menten initial growth */",
      "/* CR log slope or Michaelis Menten asymptotic CR */",
      "/* CD intercept (absolute value) */",
      "/* CD slope (as fraction of tree height) */",
      "/* initial crown depth(in m) */",
      "/* crown shape parameter */",
      "/* initial leaf density (m^2/m^2) */",
      "/* fraction of biomass allocated to above ground wood (branch turnover+stem) */",
      "/* fraction of biomass allocated to canopy (leaves + reproductive organs + twigs) */",
      "/* constant used to scale total seed rain per hectare across species (in next computation) */",
      "/* nb of seeds produced and dispersed by each mature tree when SEEDTRADEOFF is not defined */",
      "/* intraspecific variation in tree height (lognormal) */",
      "/* intraspecific variation in crown radius (lognormal) */",
      "/* intraspecific variation in crown depth (lognormal) */",
      "/* intraspecific variation in leaf phosphorus (lognormal) */",
      "/* intraspecific variation in leaf nitrogen (lognormal) */",
      "/* intraspecific variation in LMA (lognormal) */",
      "/* intraspecific variation in wood specific gravity */",
      "/* intraspecific variation in maximum diameter */",
      "/* correlation coefficient between crown radius and tree height */",
      "/* correlation coefficient between leaf nitrogen and leaf phosphorus */",
      "/* correlation coefficient between leaf nitrogen and LMA */",
      "/* correlation coefficient between leaf phosphorus and LMA */",
      "/* resolution of leaf demography model */",
      "/* probability of secondary treefall */",
      "/*  parameter determining how tree damages are repaired */",
      "/* fraction of gaps in the crown */",
      "/* minimal death rate */",
      "/* m1 (slope of death rate) */",
      "/* atmospheric CO2 concentration in micromol/mol */",
      "/* LL parameterizations: Reich empirical, Kikuzawa model, and Kikuzawa model with leaf plasticity (0,1,2) */",
      "/* dynamic LA regulation: off, 1.0, 0.75, or 0.5 (0,1,2,3) */",
      "/* sapwood parameterizations: constant thickness (0.04), Fyllas percentage, Fyllas lower limit (0,1,2) */",
      "/* excess biomass into seeds after maturation (0,1) */"
    )
  )
  
}
