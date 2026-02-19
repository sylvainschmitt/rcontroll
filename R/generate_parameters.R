#' Generate global parameters
#'
#' `generate_parameters()` generate the global parameters used in the `TROLL`
#' simulation. All parameters have a default value used in French Guiana
#' simulations.
#'
#' @param cols num. Number of columns.
#' @param rows num. Number of rows.
#' @param HEIGHT num. Vertical extent of simulation.
#' @param length_dcell num. Linear size of a dcell.
#' @param nbiter num. Total number of timesteps.
#' @param iterperyear num. Number of iterations per year.
#' @param NV num. Vertical number of cells (per m).
#' @param NH num. Horizontal number of cells (per m).
#' @param nbout num. Number of outputs.
#' @param nbspp num. Number of species
#' @param SWtoPPFD num. Convert shortwave irradiance to PAR photons.
#' @param p_nonvert num. Light incidence parameter (difference through turbid
#'   medium).
#' @param klight num. Light attenuation in the canopy following a Beer-Lambert
#'   law.
#' @param phi num. Quantum yield (in micromol C/micromol photon).
#' @param absorptance_leaves num. Absorptance of individual leaves.
#' @param theta num. Parameter of the Farquhar model.
#' @param g1 num. Parameter g1 of Medlyn et al stomatal conductance model.
#' @param vC num. Variance of the flexion moment.
#' @param DBH0 num. Initial diameter at breast height (m).
#' @param H0 num. Initial height (m).
#' @param CR_min num. Minimum crown radius (in m).
#' @param CR_a num. Crown radius log intercept or Michaelis Menten initial
#'   growth.
#' @param CR_b num. Crown radius log slope or Michaelis Menten asymptotic CR.
#' @param CD_a num. Crown depth intercept (absolute value).
#' @param CD_b num. Crown depth slope (as fraction of tree height).
#' @param CD0 num. Initial crown depth (in m).
#' @param shape_crown num. Crown shape parameter.
#' @param dens num. Initial leaf density (m^2/m^2).
#' @param fallocwood num. Fraction of biomass allocated to above ground wood
#'   (branch turnover+stem).
#' @param falloccanopy num. Fraction of biomass allocated to canopy (leaves +
#'   reproductive organs + twigs).
#' @param Cseedrain num. Constant used to scale total seed rain per hectare
#'   across species.
#' @param nbs0 num. Number of seeds produced and dispersed by each mature tree
#'   when SEEDTRADEOFF is not defined.
#' @param sigma_height num. Intraspecific variation in tree height (lognormal).
#' @param sigma_CR num. Intraspecific variation in crown radius (lognormal).
#' @param sigma_CD num. Intraspecific variation in crown depth (lognormal).
#' @param sigma_P num. Intraspecific variation in leaf phosphorus (lognormal).
#' @param sigma_N num. Intraspecific variation in leaf nitrogen (lognormal).
#' @param sigma_LMA num. Intraspecific variation in leaf mass per area
#'   (lognormal).
#' @param sigma_wsg num. Intraspecific variation in wood specific gravity.
#' @param sigma_dbhmax num. Intraspecific variation in maximum diameter.
#' @param corr_CR_height num. Correlation coefficient between crown radius and
#'   tree height.
#' @param corr_N_P num. Correlation coefficient between leaf nitrogen and leaf
#'   phosphorus.
#' @param corr_N_LMA num. Correlation coefficient between leaf nitrogen and leaf
#'   mass per area
#' @param corr_P_LMA num. Correlation coefficient between leaf phosphorus and
#'   leaf mass per area
#' @param leafdem_resolution num. Resolution of leaf demography model.
#' @param p_tfsecondary num. Probability of secondary treefall.
#' @param hurt_decay num. Parameter determining how tree damages are repaired.
#' @param crown_gap_fraction num. Fraction of gaps in the crown.
#' @param m num. Minimal death rate.
#' @param m1 num. Slope of death rate m1.
#' @param Cair num. Atmospheric CO2 concentration in micromol/mol.
#' @param LL_parameterization num. Leaf lifespan parameterizations: Reich
#'   empirical, Kikuzawa model, and Kikuzawa model with leaf plasticity (0,1,2).
#' @param LA_regulation num. Dynamic LA regulation: off, 1.0, 0.75, or 0.5
#'   (0,1,2,3).
#' @param sapwood num. Sapwood parameterizations: constant thickness (0.04),
#'   Fyllas percentage, Fyllas lower limit (0,1,2).
#' @param seedsadditional num. Excess biomass into seeds after maturation (0,1).
#' @param NONRANDOM num. If _NONRANDOM >= 1, the seeds for the random number
#'   generators will be set using fixed seed in R, default for bug fixing (0,1).
#' @param GPPcrown num. This defines an option to compute only GPP from the
#'   topmost value of PPFD and GPP, instead of looping within the crown (0,1).
#' @param BASICTREEFALL num. If defined: treefall is a source of tree death
#'   (0,1).
#' @param SEEDTRADEOFF num. if defined: the number of seeds produced is
#'   determined by NPP allocated to reproduction and seed mass, otherwise the
#'   number of seeds is fixed (0,1).
#' @param CROWN_MM num. Michaelis Menten allometry for crowns instead of power
#'   law, parameters have to be changed in other input sheets accordingly (0,1).
#' @param OUTPUT_extended num. extended set of ouput files (0,1).
#' @param extent_visual num. extent for visualization output. Unactivated when
#'   equal 0.
#' @param fecundity num. active le module fecundity (0;1) simulating tree species fecundity following trait-based models (Visser et al. 2016) (developped in v3.1.5 by Bruno)
#' @param Rrecruit num. active le module Rrecruit (0;1) simulating establishment rate following trait-based models (Visser et al. 2016) (developped in v3.1.5 by Bruno)
#' @param distdisperse num. active le module distdisperse (0;1) simulating dispersal distance controlling the dispersion maximum by seed mass and tree height (Tamme et al. 2014) (developped in v3.1.5 by Bruno)
#' @param torus num. active le module torus (0;1) implementing a torus
#' @param MinLAImax num. active le module MinLAImax (0;1) le filtrage par la lumière se fait avant de tirer le graine, version la plus contraignante
#' @param MaxLAImax num. active le module MaxLAImax (0;1) le filtrage par la lumière se fait avant de tirer le graine, version la moins contraignante
#'
#' @return A data frame of global parameters.
#'
#' @seealso [troll()], [stack()], [update_parameters()]
#'
#' @examples
#'
#' generate_parameters(nbiter = 12)
#'
#' @export
generate_parameters <- function(cols = 200,
                                rows = 200,
                                HEIGHT = 70, # nolint
                                length_dcell = 25,
                                nbiter,
                                iterperyear = 12,
                                NV = 1, # nolint
                                NH = 1, # nolint
                                nbout = 4,
                                nbspp = 45,
                                SWtoPPFD = 2.27, # nolint
                                p_nonvert = 0.05,
                                klight = 0.63,
                                phi = 0.093,
                                absorptance_leaves = 0.9,
                                theta = 0.7,
                                g1 = 3.77,
                                vC = 0.021, # nolint
                                DBH0 = 0.005, # nolint
                                H0 = 0.950, # nolint
                                CR_min = 0.3, # nolint
                                CR_a = 2.13, # nolint
                                CR_b = 0.63, # nolint
                                CD_a = 0, # nolint
                                CD_b = 0.2, # nolint
                                CD0 = 0.3, # nolint
                                shape_crown = 0.72,
                                dens = 1,
                                fallocwood = 0.35,
                                falloccanopy = 0.25,
                                Cseedrain = 50000, # nolint
                                nbs0 = 2250, # nolint
                                sigma_height = 0,
                                sigma_CR = 0, # nolint
                                sigma_CD = 0, # nolint
                                sigma_P = 0, # nolint
                                sigma_N = 0, # nolint
                                sigma_LMA = 0, # nolint
                                sigma_wsg = 0,
                                sigma_dbhmax = 0,
                                corr_CR_height = 0, # nolint
                                corr_N_P = 0, # nolint
                                corr_N_LMA = 0, # nolint
                                corr_P_LMA = 0, # nolint
                                leafdem_resolution = 30,
                                p_tfsecondary = 1,
                                hurt_decay = 0,
                                crown_gap_fraction = 0.15,
                                m = 0.013,
                                m1 = 0.013,
                                Cair = 400, # nolint
                                LL_parameterization = 1, # nolint
                                LA_regulation = 2, # nolint
                                sapwood = 1,
                                seedsadditional = 0,
                                NONRANDOM = 1, # nolint
                                GPPcrown = 0, # nolint
                                BASICTREEFALL = 1, # nolint
                                SEEDTRADEOFF = 0, # nolint
                                CROWN_MM = 0, # nolint
                                OUTPUT_extended = 1, # nolint
                                extent_visual = 0, 
                                fecundity = 1, 
                                Rrecruit = 1, 
                                distdisperse = 1, 
                                torus = 1,
                                MinLAImax = 1, 
                                MaxLAImax = 1) {
  # check args
  if (!all(unlist(lapply(
    list(
      cols, rows, HEIGHT, length_dcell, nbiter, iterperyear,
      NV, NH, nbout, nbspp, SWtoPPFD, p_nonvert, klight, phi,
      absorptance_leaves, theta, g1, vC, DBH0, H0, CR_min,
      CR_a, CR_b, CD_a, CD_b, CD0, shape_crown, dens, fallocwood,
      falloccanopy, Cseedrain, nbs0, sigma_height, sigma_CR,
      sigma_CD, sigma_P, sigma_N, sigma_LMA, sigma_wsg, sigma_dbhmax,
      corr_CR_height, corr_N_P, corr_N_LMA, corr_P_LMA,
      leafdem_resolution, p_tfsecondary, hurt_decay, crown_gap_fraction,
      m, m1, Cair, LL_parameterization, LA_regulation,
      sapwood, seedsadditional,
      NONRANDOM, GPPcrown, BASICTREEFALL, SEEDTRADEOFF,
      CROWN_MM, OUTPUT_extended, extent_visual, fecundity, Rrecruit, distdisperse, 
      torus, MinLAImax, MaxLAImax
    ),
    class
  )) == "numeric")) {
    stop("parameters should be numeric.")
  }

  if (NONRANDOM > 1) {
    Rseed <- sample.int(.Machine$integer.max, 1) # nolint
    NONRANDOM <- 1 # nolint
  } else {
    Rseed <- 1 # nolint
  }

  data.frame(
    param = c(
      "cols", "rows", "HEIGHT", "length_dcell",
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
      "_LA_regulation", "_sapwood", "_seedsadditional",
      "_NONRANDOM", "Rseed", "_GPPcrown", "_BASICTREEFALL", "_SEEDTRADEOFF",
      "_CROWN_MM", "_OUTPUT_extended", "extent_visual", 
      "_fecundity", "_Rrecruit", "_distdisperse", "_torus", "_MinLAImax", "_MaxLAImax"
    ),
    value = c(
      cols, rows, HEIGHT, length_dcell, nbiter, iterperyear,
      NV, NH, nbout, nbspp, SWtoPPFD, p_nonvert, klight, phi,
      absorptance_leaves, theta, g1, vC, DBH0, H0, CR_min,
      CR_a, CR_b, CD_a, CD_b, CD0, shape_crown, dens, fallocwood,
      falloccanopy, Cseedrain, nbs0, sigma_height, sigma_CR,
      sigma_CD, sigma_P, sigma_N, sigma_LMA, sigma_wsg, sigma_dbhmax,
      corr_CR_height, corr_N_P, corr_N_LMA, corr_P_LMA,
      leafdem_resolution, p_tfsecondary, hurt_decay, crown_gap_fraction,
      m, m1, Cair, LL_parameterization, LA_regulation,
      sapwood, seedsadditional,
      NONRANDOM, Rseed, GPPcrown, BASICTREEFALL, SEEDTRADEOFF,
      CROWN_MM, OUTPUT_extended, extent_visual, fecundity, Rrecruit, distdisperse, torus, 
      MinLAImax, MaxLAImax
    ),
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
      "/* fraction of biomass allocated to above ground wood (branch turnover+stem) */", # nolint
      "/* fraction of biomass allocated to canopy (leaves + reproductive organs + twigs) */", # nolint
      "/* constant used to scale total seed rain per hectare across species (in next computation) */", # nolint
      "/* nb of seeds produced and dispersed by each mature tree when SEEDTRADEOFF is not defined */", # nolint
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
      "/* LL parameterizations: Reich empirical, Kikuzawa model, and Kikuzawa model with leaf plasticity (0,1,2) */", # nolint
      "/* dynamic LA regulation: off, 1.0, 0.75, or 0.5 (0,1,2,3) */",
      "/* sapwood parameterizations: constant thickness (0.04), Fyllas percentage, Fyllas lower limit (0,1,2) */", # nolint
      "/* excess biomass into seeds after maturation (0,1) */",
      "/* If _NONRANDOM == 1, the seeds for the random number generators will be kept fixed at 1, default for bug fixing */", # nolint
      "/* selected seed according to _NONRANDOM and R fixed seed */",
      "/* This defines an option to compute only GPP from the topmost value of PPFD and GPP, instead of looping within the crown. */", # nolint
      "/* if defined: treefall is a source of tree death */",
      "/* if defined: the number of seeds produced is determined by NPP allocated to reproduction and seed mass, otherwise the number of seeds is fixed */", # nolint
      "/* Michaelis Menten allometry for crowns instead of power law, parameters have to be changed in other input sheets accordingly */", # nolint
      "/* extended set of ouput files */",
      "/* extent for visualization output */",
      "/* active le module fecundity (0;1) simulating tree species fecundity following trait-based models (Visser et al. 2016) (developped in v3.1.5 by Bruno)*/",
      "/* active le module Rrecruit (0;1) simulating species establishment rate following trait-based models (Visser et al. 2016) (developped in v3.1.5 by Bruno)*/",
      "/* active le module distdisperse (0;1) simulating dispersal distance following seed mass, dispersal syndrom and tree height (Visser et al. 2016) (developped in v3.1.5 by Bruno)*/", 
      "/* active le module torus (0;1) implementing a torus */", 
      "/* active le module MinLAImax (0;1) le filtrage pour la lumière se fait avant de tirer la graine, le + contraignant */",
      "/* active le module MaxLAImax (0;1) le filtrage pour la lumière se fait avant de tirer la graine, le - contraignant */"
    )
  )
}
