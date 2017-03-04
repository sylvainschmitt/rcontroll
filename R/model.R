#' @include build.R
#' @include run.R
#' @include load.R
NULL

#' Model
#'
#' Create and open a TROLL model
#'
#' @param name char. name of the model
#' @param path char. working directory
#' @param src char. path to src file
#' @param optimisation char. optimisation level to build cpp with g++ (default NULL)
#' @param app char. path to troll app (e.g. TROLL.out)
#' @param input char. input file to create
#' @param build logical. do you want to (re)-build the model
#' @param overwrite logical. allow to overwrite existing input and outputs files
#' @param verbose logical. allow output in console
#' @param nbcol int. number of columns
#' @param nbrows int. number of rows
#' @param nbiter int. total number of time step
#' @param iter int. number of iteration per year
#' @param NV int. vertical number of cells (nb per m)
#' @param NH int. horizontal number of cells (nb per m)
#' @param nbout int. number of outputs
#' @param numesp int. number of species
#' @param p num. light incidence parameter (diff through turbid medium)
#' @param disturb_iter num. iteration step where the disturbation occure
#' @param disturb_intensity num. intensity of disturbance in percent of BA
#' @param daylight num. normalized daily light course (from 7am to 7 pm, with a half-hour time step)
#' @param dayT num. normalized daily VPD course (from 7am to 7 pm, with a half-hour time step)
#' @param dayVPD num. normalized daily T course (from 7am to 7 pm, with a half-hour time step)
#' @param klight num. light attenuation in the canopy Beer-Lambert
#' @param phi quantum yield (in micromol C/micromol photon)
#' @param vC num. variance of the flexion moment
#' @param DBH0 num. initial dbh (m)
#' @param H0 num. initial height (m)
#' @param ra0 num. intial crown radius (m)
#' @param ra1 num. crown radius - dbh slope (m/m)
#' @param de0 num. initial crown depth (m)
#' @param de1 num Crown_Depth/height slope (m/m)
#' @param dens num. leaf density (m^2/m^2)
#' @param fbranchstem num. fraction of biomass allocated to above ground wood (branches+stem)
#' @param fcanopy num. fraction of biomass allocated to canopy (leaves + reproductive organs + twigs)
#' @param seedrain int. constant used to scale total seed rain per hectare across species (in next computation)
#' @param nbseeds int. nb of seeds produced and dispersed by each mature tree when SEEDTRADEOFF is not defined
#' @param mindeathrate num. inimal death rate
#' @param m1 num. slope of death rate
#' @param CO2 num. atmospheric CO2 concentration in micromol/mol
#' @param species df. species trait values (can be open from external file)
#' @param Tyear num. Temperature in degree C
#' @param maxT num. mean daily max temperature in degree C
#' @param nightmeanT num. Night mean temperature in degree C
#' @param rainfall num. Rainfall in mm
#' @param wind num. Wind speed in m.s-1
#' @param maxIrradiance num. Daily max irradiance mean in W.m-2
#' @param irradiance num. Irradiance mean in W.m-2
#' @param e_s num. e_s in kPa
#' @param e_a num. e_a in kPa
#' @param VPD num. VPD in kPa
#' @param dailymeanVPD num. Daily mean VPD
#' @param dailymaxVPD num. daily max VPD in kPa
#'
#' @return an S4 \linkS4class{TROLLsim} class object
#'
#' @export
#'
#' @examples
#'
model <- function(
  # function options
  name = getOption("RconTroll.name"),
  path = getOption("RconTroll.path"),
  src = getOption("RconTroll.src"),
  optimisation = NULL,
  app = getOption("RconTroll.app"),
  input = getOption("RconTroll.init"),
  output = getOption("RconTroll.output"),
  build = TRUE,
  overwrite = TRUE,
  verbose = TRUE,
  # general parameters
  nbcol = 400,
  nbrows = 400,
  nbiter = 12,
  iter = 12,
  NV = 1,
  NH = 1,
  nbout = 4,
  numesp = 163,
  p = 0.05,
  disturb_iter = 4,
  disturb_intensity = 0.4,
  daylight = c(0.030,	0.108, 0.208,	0.304,	0.473,	0.617,	0.723,	0.823,	0.925,
               0.965,	0.995,	0.988,	1.000,	0.942,	0.854,	0.766,	0.646,	0.493,
               0.360,	0.277,	0.183,	0.102,	0.058,	0.022),
  dayT = c(0.012,	0.018,	0.042,	0.087,	0.175,	0.304,	0.445,	0.569,	0.672,
           0.767,	0.856,	0.915,	0.961,	0.986,	1.000,	0.985,	0.952,	0.908,
           0.863,	0.820,	0.767,	0.699,	0.611,	0.498),
  dayVPD = c(0.768,	0.776,	0.795,	0.820,	0.851,	0.884,	0.912,	0.934,	0.950,
             0.966,	0.980,	0.989,	0.997,	0.999,	1.000,	0.9996,	0.990,	0.982,
             0.974,	0.966,	0.957,	0.945,	0.929,	0.906),
  # species parameters
  klight = 0.9,
  phi = 0.06,
  vC = 0.1,
  DBH0 = 0.01,
  H0 = 1,
  ra0 = 0.5,
  ra1 = 10,
  de0 = 0.3,
  de1 = 0.12,
  dens = 0.8,
  fbranchstem = 0.35,
  fcanopy = 0.30,
  seedrain = 200,
  nbseeds = 10,
  mindeathrate = 0.035,
  m1 = 0.035,
  CO2 = 360,
  # species data
  species = read.table(getOption("RconTroll.path"), header=TRUE, dec=".", sep=""),
  # climate data
  Tyear = c(24.64982014,	24.60624211,	24.49933474,	24.96279395,	24.88365139,
            24.87594184,	24.98313802,	25.68910135,	26.58033805,	26.98619405,
            25.87259696,	25.16530008),
  maxT = c(29.08402,	28.86072,	28.51707,	29.56514,	29.00680,	29.83177,	30.19183,
           31.34605,	32.93941,	33.14729,	31.72165,	30.32959),
  nightmeanT = c(23.22247,	23.15235,	23.21073,	23.35492,	23.58619,	23.29334,
                 23.35780,	23.66990,	24.09092,	24.55815,	23.71159,	23.34976),
  rainfall = c(296.726,	253.528,	454.302,	320.562,	425.2,	277.966,	163.426,
               102.012,	43.834,	26.466,	76,	157.1),
  wind = c(0.901671724,	0.864544643,	0.836516801,	0.851327994,	0.787419637,
           0.774398649,	0.831167015,	0.91152957,	1.021909722,	1.053285523,
           1.041843217,	0.921361559),
  maxIrradiance = c(719.7763,	765.6474679,	769.9658806,	794.82337,	717.8013194,
                    795.2024929,	791.62932,	797.9236677,	874.5701367,
                    857.4397219,	858.60655323,	779.2854645),
  irradiance = c(133.0579717,	158.3033498,	148.2984963,	164.1783546,
                 143.7356799,	167.5139082,	172.4494762,	190.3358903,
                 229.3920961,	225.1016445,	196.6685388,	170.289151),
  e_s = c(3.126835644,	3.122050934,	3.097285304,	3.194593888,	3.167241554,
          3.173096221,	3.197669097,	3.351896181,	3.552115706,	3.634188446,
          3.388384875,	3.239834054),
  e_a = c(2.820456547,	2.781722195,	2.794117369,	2.81884079,	2.918981525,
          2.889089767,	2.862294539,	2.823627394,	2.730098975,	2.774015541,
          2.794606935,	2.801060958),
  VPD = c(0.306379098,	0.340328739,	0.303167936,	0.375753098,	0.24826003,
          0.284006455,	0.335374559,	0.528268788,	0.822016731,	0.860172905,
          0.59377794,	0.438773096),
  dailymeanVPD = c(0.5873121,	0.6411128,	0.5661197,	0.7254200,	0.4913662,
                   0.5839890,	0.7444557,	1.0074374,	1.4987548,	1.5640231,
                   1.1442166,	0.8511218),
  dailymaxVPD = c(1.183852,	1.178975,	1.074576,	1.305361,	1.034367,	1.280059,
                  1.439207,	1.787562,	2.418552,	2.471909,	2.020966,	1.567758)
){
  init(path = path, 
       input = input, 
       overwrite = overwrite, 
       nbcol = nbcol, 
       nbrows = nbrows,
    nbiter = nbiter, 
    iter = iter, 
    NV = NV, 
    NH = NH, 
    nbout = nbout,
    numesp = numesp, 
    p = p, 
    disturb_iter = disturb_iter,
    disturb_intensity = disturb_intensity,
    daylight = daylight, 
    dayT = dayT, 
    dayVPD = dayVPD,
    klight = klight, 
    phi = phi, 
    vC = vC, 
    DBH0 = DBH0, 
    H0 = H0, 
    ra0 = ra0,
    ra1 = ra1, 
    de0 = de0, 
    de1 = de1, 
    dens = dens, 
    fbranchstem = fbranchstem,
    fcanopy = fcanopy, 
    seedrain = seedrain, 
    nbseeds = nbseeds,
    mindeathrate = mindeathrate, 
    m1 = m1, 
    CO2 = CO2, 
    species = species,
    Tyear = Tyear, 
    maxT = maxT, 
    nightmeanT = nightmeanT,
    rainfall = rainfall, 
    wind = wind, 
    maxIrradiance = maxIrradiance,
    irradiance = irradiance, 
    e_s = e_s, 
    e_a = e_a, 
    VPD = VPD,
    dailymeanVPD = dailymeanVPD, 
    dailymaxVPD = dailymaxVPD)
  if(build)
    build(src = src,
      app = app,
      path = path,
      optimisation = optimisation,
      verbose = verbose)
  run(name = name,
      path = path,
      app = app,
      input = input,
      overwrite = overwrite,
      verbose = verbose)
  path <- file.path(path, name)
  model <- load(name = name, path = path)

  return(model)
}
