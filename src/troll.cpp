#include <Rcpp.h>
using namespace Rcpp;

//////////////////////////////////////////////////////////////////////////////////
/*! \mainpage
 *  \section TROLL
 *
 *  Individual-based forest dynamics simulator
 *
 * History
 * -------
 *    - Version 1: Jerome Chave (Sept 1997 to Sept 2001; Chave 1999, 2001)
 *    - Version 2.0: Jerome Chave (March 2011)
 *    - Version 2.1 & 2.2: Isabelle Marechaux & Jerome Chave (Oct 2013 to May 2016)
 *    - Version 2.3: Isabelle Marechaux, Fabian Fischer, Jerome Chave (Oct 2016 to March 2017; Marechaux & Chave 2017)
 *    - Version 2.4 & 2.5: Fabian Fischer (Feb 2018 to May 2020)
 *    - Version 3.0: Fabian Fischer, Isabelle Marechaux, Jerome Chave (Jan 2021)
 * 
 * Compiling options
 * -----------------
 *    - Compilation test on osx and linux plateforms
 *    - Compilation command: _g++ main.cpp -O3 -o TROLL.out -lgsl -lgslcblas -Wall_
 *    - Code profiling: _g++ -O3 -Wall -o troll main.cpp -lgsl -lgslcblas -lm -g -pg_
 *
 * Bibliography
 * -----------
 *    - Chave, J. (1999). Study of structural, successional and spatial patterns in tropical rain forests using TROLL, a spatially explicit forest model. Ecological modelling, 124(2-3), 233-254.
 *    - Chave, J. (2001). Spatial patterns and persistence of woody plant species in ecological communities. The American Naturalist, 157(1), 51-65.
 *    - Maréchaux, I., & Chave, J. (2017). An individual‐based forest model to jointly simulate carbon and tree diversity in Amazonia: description and applications. Ecological Monographs, 87(4), 632-664.
 *
 * Comments
 * -------
 *     - TODO: include iomanip, typedef, be careful with parameter_names initialisation
 *     - As of v.2.4, GNU scientific library (gsl) is needed (current version is 2.6)
 *        -# to work, the gsl_linalg.h header is needed, so versions above 1.7 are the minimum 
 *        -# on osx, type "brew install gsl"
 *     - Basic uniform sampling functions (for more information: https://www.gnu.org/software/gsl/doc/html/rng.html):
 *        -# gsl_rng_uniform(gsl_rand) which samples range [0,1)
 *        -# gsl_rng_uniform_pos(gsl_rand) which samples range (0,1) to avoid 0 for log transformations
 *        -# gls_rng_uniform_int(gsl_rand, unsigned long int n) samples long integers from 0 to n-1
 */
//////////////////////////////////////////////////////////////////////////////////

#define FF_todiscuss     //!< small additions and quick fixes that need to be checked/discussed very carefully 
#undef MPI               //!< MPI = Message Passing Interface. Software for sharing information across processors in parallel computers. If global variable MPI is not defined, TROLL functions on one processor only. if flag MPI defined, parallel routines (MPI software) are switched on. WARNING!!!: MPI has not been maintained since v.2.2, several functions need updating 
#undef WATER             //!< new in v.3.0: If defined, an explicit water cycle is added, with an explicit belowground space. The horizontal resolution of the soil field is currently set by DCELL 
#define CROWN_UMBRELLA   //!< new in v.2.4.1, modified in v.2.5: If activated, crowns are assumed to grow cylindrical until they reach 3m in depth, then the center of the crown will experience quicker height growth than the outer parts of the crown, thus leading to an umbrella like crown shape (with slope depending on crown depth and width). Depending on the slope parameter, trees will look more like cylinders or more like cones. Contrary to previous versions, however, the crown will not fill its shape underneath the first three layers. The crown will simply become umbrella-like, with a dense layer that spans out from its stem, and empty space underneath. This allows for a very simple computation of just three crown layers, while retaining their spread across the crown depth, enough realism for LiDAR derived CHMs and for eventual non vertical light penetration. New in v.2.5: Entirely modelled through one template that simulates loops across a crown layer/shell and can be applied to any calculation that requires to calculate crown properties (CalcLAI, Fluxh, etc.). Furthermore, enhanced flexibility for future incorporation of other crown shape functions (e.g. spherical, etc.) 
#undef LAI_gradient      //!< new in v.2.4.1, modified in v.2.5: a tree's leaf density changes from top to bottom. Broadly based on Kitajima 2004 (Ann Bot), we assume that trees allocate 50% of their leaves to the first meter of their crown, 25% to the second meter layer, and the rest evenly spread across the crown. In case of the umbrella-like crowns, layers correspond to depth from canopy top. In case of a gap in the top layers, there is no shift of leaf density to a lower layer. This ensures that the gaps are real gaps. New in v.2.5: activated within a single function (LAI2dens) as part of the crown template scheme. Small effect in this case, however, since all leaves are concentrated in three shells anyways, with a shift from (0.33,0.33,0.33) to (0.5, 0.25,0.25) likely no strong effect. If implemented for cylindric crowns with full depth, this might, however, have a strong effect, as upper leaves are less impacted by crown overlap than lower leaves. 
#define Output_ABC       //!< new in v.2.4.1, refined in v.2.5: PARAMETERIZATION/OUTPUT TOOL, inclusion of ABC routines for comprehensive parameter inference with TROLL 
#define CHM_SPIKEFREE    //!< new in v.2.5: PARAMETERIZATION/OUTPUT TOOL. Since our empirical CHMs are typically derived with a spike-free/pit-free algorithm (e.g. from LAStools), we should also remove spikes/pits from the TROLL-generated CHM. Here, this is simply implemented as considering all crown gaps as filled. A more sophisticated version could use a simulated lidar point cloud and then apply the spike-free/pit-free algorithms or compare the raw CHMs of simulated lidar and actual lidar. Since we are, however, unlikely to capture the microstructure of crowns well (i.e. crown gaps are likely to be smaller, but more frequent than the simulated gaps), the current approach is probably a good compromise. 
#undef TRACK_INDIVIDUALS //!< new in v.2.5: DIAGNOSTIC TOOL, individual-based tracking of trees for PBA (process-based analytics, an accounting-like monitoring of model behavior through individual states and processes). PLEASE BE AWARE: onset of tracking is currently hardcoded to comprise trees born in year 501 in forest regeneration. Also: Filesizes can get VERY VERY LARGE if more trees are included. 
#undef CHECK_CARBON      //!< new in v.2.5: DIAGNOSTIC TOOL, checking of carbon budgets, could potentially be extended for nutrient budget checking in the future. The idea is to keep track of carbon stocks and carbon fluxes every timestep to see whether there are any deviations from expectations - to do so, differences between stocks are computed at each timestep, and can be compared to the gross and net assimilation of carbon 

// LIBRAIRIES 
#include <cstdio>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <cstdlib>
#include <string>
#include <limits>
#include <ctime>
#include <cmath>
#include <vector>
#include <sstream>
#include <algorithm>

#ifdef MPI
#include "mpi.h"
#endif

#include <gsl/gsl_math.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_errno.h>

using namespace std;

// Global constants (e.g. PI and various derivatives...) 
# define PI 3.141592654  //!< Global constant: Pi
# define twoPi 6.2831853071 //!< Global constant: 2.0*Pi
# define Pis2 1.570796327 //!< Global constant: Pi/2.0
# define iPi 0.3183099 //!< Global constant: 1.0/Pi

char buffer[256], inputfile[256], inputfile_daytimevar[256], inputfile_climate[256], inputfile_soil[256], outputinfo[256], inputfile_data[256], *bufi(0), *bufi_daytimevar(0), *bufi_climate(0), *bufi_soil(0), *buf(0), *bufi_data(0); //!< Global variable: character strings used to read file names, or other features
char inputfile_species[256], *bufi_species(0); //!< Global variable: vector of input files 

// file output streams 
fstream out,out2; //!< Global variable: output files 
fstream output[40];//!< Global variable: output files 

// USER CONTROLS. Options that can be turned on (1) or off (0). This comes at computational cost: where routines have to be called frequently, if-conditioning should be done as far outside the loop as possible (e.g. for DAYTIMELIGHT outside voxel loops) .Options are set below, but inclusion in parameter sheet needed (for control from R) 
bool _GPPcrown=0;      //!< User control: This defines an option to compute only GPP from the topmost value of PPFD and GPP, instead of looping within the crown. 
bool _BASICTREEFALL=1; //!< User control: if defined: treefall is a source of tree death (and if TREEFALL not defined, this is modeled through simple comparison between tree height and a threshold t_Ct, if not defined, treefall is not represented as a separated and independent source of death, but instead, all tree death are due to the deathrate value) 
bool _DAYTIMELIGHT=1;  //!< User control: if defined: the rate of carbon assimilation integrates an average daily fluctuation of light (thanks to GPPDaily). Should be defined to ensure an appropriate use of Farquhar model; deprecated, but renamed as DAYTIMELIGHT for clarity's sake since v.2.5 
bool _SEEDTRADEOFF=0;  //!< User control: if defined: the number of seeds produced by each tree is determined by the tree NPP allocated to reproduction and the species seed mass, otherwise the number of seeds is fixed; besides, seedling recruitment in one site is not made by randomly and 'equiprobably' picking one species among the seeds present at that site but the probability of recruitment among the present seeds is proportional to the number of seeds (in s_Seed[site]) time the seed mass of each species 
bool _NDD=0;           //!< User control: if defined, negative density dependant processes affect both the probability of seedling recruitment and the local tree death rate. The term of density-dependance is computed as the sum of conspecific tree basal area divided by their distance to the focal tree within a neighbourhood (circle of radius 15m) 
bool _CROWN_MM=0;      //!< User control: new in v.2.4.1: Michaelis Menten allometry for crowns instead of power law, since v.2.5: not a macro anymore, but set at runtime (little overhead since queried only once at birth and once per timestep), !!!: power law is the default. If Michaelis Menten type allometry is activated, the parameters have to be changed in input sheet accordingly 
bool _sapwood;         //!< User control: two ways of parameterising sapwood density: constant thickness (0), Fyllas, but with lower limit (1) 
bool _seedsadditional; //!< User control: excess carbon into seeds? no/yes=(0/1) 
bool _LL_parameterization;   //!< User control: two ways for parameterising leaf lifespan: empirical (derived by Sylvain Schmitt, TODO: from which data?), Kikuzawa model (0,1) 
int _LA_regulation;    //!< User control: three ways of parameterising leaf dynamic allocation: no regulation (0), never exceed LAImax, i.e. the maximum LAI under full sunlight (1), adjust LAI to the current light environment (2). To switch between option 1 and 2, only one line is necessary in leafarea_max 
bool _OUTPUT_reduced;  //!< User control: reduced set of output files 
bool _FromData;        //!< User control: if defined, an additional input file can be provided to start simulations from an existing data set or a simulated data set (5 parameters are needed: x and y coordinates, dbh, species_label, species 
bool _NONRANDOM;       //!< User control: If _NONRANDOM == 1, the seeds for the random number generators will be kept fixed at 1, for bug fixing 

// GLOBAL PARAMETERS OF THE SIMULATION 
int sites;      //!< Global variable: number of pixels in the scene (cols*rows) 
int cols;       //!< Global variable: number of columns in the scene 
int rows;       //!< Global variable: number of rows in the scene
int nbspp;      //!< Global variable: number of species 
int iterperyear;//!< Global variable: number of iterations per year (=12 if monthly timestep, =365 if daily timestep) 
int nbiter;     //!< Global variable: total number of timesteps 
int iter;       //!< Global variable: current timestep 
int nbout;      //!< Global variable: number of outputs 
int freqout;    //!< Global variable: frequency HDF outputs 

// Random number generator for trait covariance calculation
gsl_rng *gslrand;   //!< Global variable: random number generator
gsl_matrix *mcov_N_P_LMA;   //!< Global variable: covariance matrix for leaf_properties 
gsl_vector *mu_N_P_LMA, *variation_N_P_LMA; //!< Global variable: mean values of the distributions and the output vector for the multivariate draw 
int covariance_status;      //!< Global variable: covariance status: if one of N, P, or LMA has zero variation, the Cholesky decomposition fails, we then use no correlation at all 

// DCELL: coarser grids (typically, one site is 1 m^2, while one dcell would be 20*20 m^2)
int length_dcell;   //!< Global variable: linear size of a dcell (coarser grid of the simulated scene); default since v.3.0 standard 
int linear_nb_dcells;    //!< Global variable: linear number of dcells
int sites_per_dcell;    //!< Global variable: number of sites per dcell 
int nbdcells;           //!< Global variable: total number of dcells; note that nbdcells = linear_nb_dcells * linear_nb_dcells 

#ifdef WATER
int *site_DCELL(0); //!< Global variable: site_DCELL[site] attributes a site to its dcell
float i_sites_per_dcell; //!< Global variable: 1.0/sites_per_dcell, where sites_per_dcell is number of sites per dcell
#endif

int HEIGHT; //!< Global variable: maximum height (m) 
int dbhmaxincm; //!< Global variable: maximum diameter at breast height (DBH) in cm (the convention in forestry; !!! departure from usual SI units)
int RMAX; //!< Global variable: maximum crown radius (m)
int SBORD; //!< Global variable: boundary condition, defined as RMAX*cols 
int leafdem_resolution; //!< Global variable: resolution for leaf demography model !!!MORE_COMMENT
float NV; //!< Global variable: number of cells per m (vertical) 
float NH; //!< Global variable: number of cells per m (horizontal) 
float LV; //!< Global variable: LV = 1.0/NV; NV is vertical number of cells per m
float LH; //!< Global variable: LH = 1.0/NH; NH is horizontal number of cells per m 
float timestep; //!< Global variable: duration of one timestep (in years)=1/iterperyear 

float p_nonvert; //!< Global variable: ratio of non-vertical incident light 
float Cseedrain; //!< Global variable: constant used to scale total seed rain per hectare across species 
float nbs0; //!< Global variable: number of seeds produced and dispersed by each mature tree when SEEDTRADEOFF is not defined 
float Cair; //!< Global variable: atmospheric CO_2 concentration (in ppm). If in the future CO_2 is allowed to vary, Cair should have the same status as other climatic variables  
float iCair; //!< Global variable: inverse of Cair 

float crown_gap_fraction; //!< Global variable: fraction of gaps in a crown, new in v.2.4.1: adds gaps according to a predetermined gap_fraction in tree crown. Gaps inserted regularly (i.e. every 4 pixels for a gap fraction of 0.25). In v.2.5:  gap fraction is now given as a target fraction and, in filling up a tree's crown, gaps are inserted when fraction_filled is too high: more accurate approximation of gap fraction (e.g., gap fractions > 0.5 are now possible). Also in v.2.5: intraspecific variation in crown radius affects gap fraction: trees with larger crowns also have more gaps, so large crowns get more open and smaller crowns denser, while keeping the same leaf area. !!!IMPROVE_IN_FUTURE
float shape_crown; //!< Global variable: percentage of crown radius that reaches the crown top and defines how steep the crown slope is; only required if CROWN_UMBRELLA is activated, but added to main code so that input sheet does not have to be modified.

// NEGATIVE DENSITY DEPENDENCE (NDD)
float Rndd; //!< Global variable: distance beyond which negative density dependence (NDD) effect is not accounted anymore
float deltaR; //!< Global variable: negative density dependence (NDD) strength parameter in recruitment rate
float deltaD; //!< Global variable: negative density dependence (NDD) strength parameter in death rate
float BAtot; //!< Global variable: !!!UPDATE


//! ENVIRONMENTAL VARIABLES

// Daily variation of environmental variables. Defined since version 2.2, updated 2.5 and 3.0: now parameterised as variation around daily mean value, with arbitrary length of step. Users should ensure that climate file provides the correct average (i.e. if they provide only daytime variation, then the climate file needs to provide daytime averages, if they provide a 24h cycle, then the climate file needs to provide 24h averages) 
vector<float> varday_light;     //!< Global vector: light irradiance variation during an average day, since v.2.5: standardized with respect to the mean and summing to 0 (adimensional)
vector<float> varday_vpd;       //!< Global vector: Vapour pressure deficit (VPD) variation during an average day, since v.2.5:  standardized with respect to the mean and summing to 0 (adimensional)
vector<float> varday_T;         //!< Global vector: Temperature variation during an average day, since v.2.5:  standardized with respect to the mean and summing to 0 (adimensional)
int nbsteps_varday;             //!< Global variable: number of steps for environmental variables
float inv_nbsteps_varday;       //!< Global variable: the inverse of the step number for environmental variables
float nbhours_covered;          //!< Global variable: number of hours of environmental variables in the environmental input file 

#ifdef WATER
float totday_light;           //!< Global variable: total amount of daytime light; needed to compute water evaporation from the top soil layer on a daily basis.
#endif

// Climate input data; as provided in the input file. File structure depends on the timestep and scenario used for the simulation 
// new version 2.2, updated in 2.5: use mean temperatures instead of maxima as reference point. This corrects an overestimation of all environmental variables (daily cycle in input file has been adjusted accordingly)
vector<float> Temperature;                      //!< Global vector: temperature in degree Celsius 
vector<float> DailyMeanTemperature;             //!< Global vector: daily mean temperature in degree Celsius 
vector<float> DailyMeanIrradiance;              //!< Global vector: daily mean irradiance (W/m^2) 
vector<float> DailyMeanVapourPressureDeficit;   //!< Global vector: daily mean vapour pressure deficit (kPa) 
vector<float> NightTemperature;                 //!< Global vector: night temperature in degree Celsius 
vector<float> Rainfall;                         //!< Global vector: rainfall (mm)
vector<float> WindSpeed;                        //!< Global vector: wind speed (m/s)
vector<float> MeanIrradiance;                   //!< Global vector: mean irradiance (W/m2) !!!UPDATE
vector<float> SaturatedVapourPressure;          //!< Global vector: saturated vapour pressure (kPa)
vector<float> VapourPressure;                   //!< Global vector: vapour pressure (kPa)
vector<float> VapourPressureDeficit;            //!< Global vector: vapour pressure deficit (kPa) 
vector<float> DailyVapourPressureDeficit;       //!< Global vector: daily vapour pressure deficit (kPa) 

// LOOKUP TABLES
// Complex temperature-dependent functions used in the Farquhar model are computed once at 'Taccuracy' resolution. Leaf temperature must be comprised between 0°C and 60°C, Values are stored every 0.5°C step in Tleaf, so 120 values in total 
int nbTbins;                            //!< Global variable: number of bins for the temperature lookup tables 
float iTaccuracy;                       //!< Global variable: inverse of accuracy of a temperature bin (e.g. if Taccuracy is 0.1 or 0.5 °C, then iTaccuracy is 10.0 or 2.0, respectively) 
float *LookUp_KmT(0);                   //!< Global vector: lookup table for Km(T) in Farquhar model
float *LookUp_GammaT(0);                //!< Global vector: lookup table for Gamma(T) in Farquhar model
float *LookUp_VcmaxT(0);                //!< Global vector: lookup table for Vcmax(T) in Farquhar model
float *LookUp_JmaxT(0);                 //!< Global vector: lookup table for Jmax(T) in Farquhar model
float *LookUp_Rday(0);                  //!< Global vector: lookup table for Rday(T) in Farquhar model
float *LookUp_flux_absorption(0);       //!< Global vector: lookup table for faster computation of PPFD. New in v.2.4: absorption flux 
float *LookUp_flux(0);                  //!< Global vector: lookup table for faster computation of PPFD. New in v.2.4: averaging instead of top value, largely replaced by LookUp_flux_absorption, but still needed, for example to compute light above crown without explicit computation of absorption 
float *LookUp_VPD(0);                   //!< Global vector: lookup table for faster computation of vapour pressure deficit (VPD). New in v.2.4:averaging instead of top value 
float *LookUp_T(0);                     //!< Global vector: lookup table for faster computation of temperature. New in v.2.4: averaging instead of top value 
float *LookUp_Rstem(0);                 //!< Global vector: lookup table for faster computation of Rstem 
float *LookUp_Rnight(0);                //!< Global vector: lookup table for faster computation of Rstem 
int LookUp_Crown_site[2601];            //!< Global vector: new in v.2.4: lookup table to fill crown cylinder sequentially from inside to outside, allowing for smooth crown growth 

// ENVIRONMENTAL VARIABLES UPDATED EACH TIME STEP
float temp; //!< Global variable: Temperature (degree C)
float tnight; //!< Global variable: Night mean temperature (degree C) 
float precip; //!< Global variable: Rainfall  (mm) 
float WS; //!< Global variable: WindSpeed (m/s)
float Wmean; //!< Global variable: mean irradiance (W/m2)
float e_s; //!< Global variable: SaturatedVapourPressure (kPa)
float e_a; //!< Global variable: VapourPressure (kPa)
float VPDbasic; //!< Global variable: VapourPressureDeficit (kPa) 
float VPDday; //!< Global variable: DailyVapourPressureDeficit (kPa)
float WDailyMean; //!< Global variable: Daily mean irradiance (average for timestep) (micromol PAR photon/m^2/s), used in the photosynthesis part. !!!UPDATE: perhaps have value in the right unit in the input file, yet W/m2 is the common unit of meteorological stations 
float tDailyMean; //!< Global variable: Daily mean temperature (degree C)
float VPDDailyMean; //!< Global variable: Daily mean VapourPressureDeficit (kPa)
float WDailyMean_year; //!< Global variable: average WDailyMean per year 
float tDailyMean_year; //!< Global variable: average tDailyMean per year 
float VPDDailyMean_year; //!< Global variable: average VPDDailyMean per year 
float Tnight_year; //!< Global variable: average tnight per year 
float temp_year; //!< Global variable: average temperature per year 

#ifdef WATER
float PET;             //!< Global variable: Potential evapotranspiration !!!UNITS
#endif

// GLOBAL VARIABLES ACROSS SPECIES
float SWtoPPFD; //!< Global variable: conversion factor for shortwave irradiance measured in W/m2 to PPFD in micromol of PAR (micromol/s/m^2, as used in the Farquhar model). Around 2.0-2.5 in the tropics. Data at Nouragues (comparing photon count and irradiance) give a value: 2.27. Depends on cloudiness (in non-cloudy areas, the fraction of PAR in irradiance arriving on the ground may be much lower)
float klight; //!< Global variable: light absorption rate or extinction coefficient used in Beer-Lambert law to compute light within the canopy 
float kpar; //!< Global variable: new in v.2.5: effective light absorption rate or extinction coefficient used in Beer-Lambert law to compute light within the canopy; kpar = klight * absorptance_leaves 
float phi; //!< Global variable: true quantum yield; previously was apparent quantum yield (in micromol C/micromol photon): quantum yield multiplied by leaf absorptance. Quantum yield is often provided per absorbed light flux, so one should multiply incident PPFD by leaf absorptance (Poorter et al American Journal of Botany). For some authors, it should be species-dependent or environment dependent, but these options are not implemented here (see eg Domingues et al 2014 Plant Ecology & Diversity). As of v2.5: phi is the quantum yield, without multiplication with absorptance (see absorptance_leaves)
float theta; //!< Global variable: parameter of the Farquhar model set to 0.7 in this version. For some authors, it should be species-dependent or environment dependent, but these options are not implemented here 
float absorptance_leaves; //!< Global variable: absorptance of leaves (close to 0.91 for tropical tree species)
float g1; //!< Global variable: g1 parameter of Medlyn et al's model of stomatal conductance. v230: defined as a global parameter shared by species, instead of a Species class variable. !!!UPDATE A species-specific value of g1 (cf Lin et al. 2015 NCC or Wu et al. 2019 GCB) is needed to simulate functional shift through a regeneration. Different values of g1 across PFT are also used by Xu et al. 2016 New Phytologist using ED2+SPA applied on tropical dry forests
float alpha; //!< Global variable: apparent quantum yield to electron transport in mol e-/mol photons, equal to the true quantum yield multiplied by leaf absorbance
float vC; //!< Global variable: variance of treefall threshold 
float H0; //!< Global variable: initial tree height (m) 
float DBH0; //!< Global variable: initial tree DBH (m) 
float CD0; //!< Global variable: initial tree crown depth (m) 
float fallocwood; //!< Global variable: fraction of biomass allocated to above ground wood (branch turnover+stem) 
float falloccanopy; //!< Global variable: fraction of biomass allocated to canopy (leaves + reproductive organs + twigs) 
float dens; //!< Global variable: initial crown leaf density (in m^2/m^3) 
float CD_a; //!< Global variable: allometric parameter crown depth intercept 
float CD_b; //!< Global variable: allometric parameter crown depth slope 
float CR_a; //!< Global variable: allometric parameter crown radius log scale intercept (translates into factor on regular scale) 
float CR_b; //!< Global variable: allometric parameter crown radius log scale slope (translates into exponent on regular scale) 
float CR_min; //!< Global variable: allometric parameter minimum crown radius (in m) 
float p_tfsecondary; //!< Global variable: probability that a death due to a treefall is a treefall itself (v.2.4.0)
float hurt_decay; //!< Global variable: "healing factor" rate at which t_hurt, the tree-level negative impact of a treefall on a tree, declines each timestep (v.2.4.0)
float m; //!< Global variable: basal death rate 
float m1; //!< Global variable: slope of the dependence between death rate and wood density (wsg); usually a negative constant (v.2.2)

// sigmas for intraspecific variation, currently assumed to be the same for all species 
float sigma_height; //!< Global variable: standard deviation of intraspecific variation in maximal height
float sigma_CR; //!< Global variable: standard deviation of intraspecific variation in crown radius
float sigma_CD; //!< Global variable: standard deviation of intraspecific variation in crown depth
float sigma_P; //!< Global variable: standard deviation of intraspecific variation in leaf phosphorus concentration
float sigma_N; //!< Global variable: standard deviation of intraspecific variation in leaf nitrogen concentration
float sigma_LMA; //!< Global variable: standard deviation of intraspecific variation in leaf mass per area (LMA)
float sigma_wsg; //!< Global variable: standard deviation of intraspecific variation in wood specific gravity (wsg)
float sigma_dbhmax; //!< Global variable: standard deviation of intraspecific variation in maximal trunk diameter
float corr_CR_height; //!< Global variable: intraspecific correlation between crown radius and maximal height
float corr_N_P; //!< Global variable: intraspecific correlation between leaf nitrogen and leaf phosphorus
float corr_N_LMA; //!< Global variable: intraspecific correlation between leaf nitrogen and LMA
float corr_P_LMA; //!< Global variable: intraspecific correlation between leaf phosphorus and LMA
float cov_N_P; //!< Global variable: intraspecific covariance between leaf nitrogen and leaf phosphorus
float cov_N_LMA; //!< Global variable: intraspecific covariance between leaf nitrogen and LMA
float cov_P_LMA; //!< Global variable: intraspecific covariance between leaf phosphorus and LMA

//LookUp_tables for intraspecific variation (TODO: maybe drawing from gsl distribution is equally quick or quicker?) 
float d_intraspecific_height[100000]; //!< Global vector: distribution of intraspecific values for maximal tree height
float d_intraspecific_CR[100000]; //!< Global vector: distribution of intraspecific values for maximal crown radius
float d_intraspecific_CD[100000]; //!< Global vector: distribution of intraspecific values for maximal crown depth
float d_intraspecific_P[100000]; //!< Global vector: distribution of intraspecific values for leaf phosphorus
float d_intraspecific_N[100000]; //!< Global vector: distribution of intraspecific values for leaf nitrogen
float d_intraspecific_LMA[100000]; //!< Global vector: distribution of intraspecific values for leaf mass per area (LMA)
float d_intraspecific_wsg[100000]; //!< Global vector: distribution of intraspecific values for wood density (wsg)
float d_intraspecific_dbhmax[100000];//!< Global vector: distribution of intraspecific values for maximal trunk diameter

// THREE DIMENSIONAL LAI FIELD
float **LAI3D(0);   //!< Global 3D field: leaf density (per volume unit) 
// TREEFALL IMPACT ON TREE HEALTH
unsigned short *Thurt[3];  //!<  Global vector:Treefall field 

#ifdef WATER
int nblayers_soil; //!< Global variable: number of soil layers (for water module)
//float *layer_thickness(0); ###FF: since layer_thickness is used once in the main code and contains the same information as layer depth, it is removed as a global variable. During the reading in of the soil layers, it is replaced by a vector, and otherwise layer_depth will be used (v.3.0).
float *layer_depth(0); //!< Global vector: depth of each layer (m) !!!UPDATE

// soil parameters (Sat_SWC, Res_SWC) are computed from soil texture data (%clay, %silt, %sand) provided in input. If additional information is available from the field (soil pH, organic content, dry bulk density, cation exchange capacity), this should be also provided in input and used to refine the computation of these soil parameters (see Table 2 in Marthews et al. 2014 Geoscientific Model Development and Hodnett & Tomasella 2002 Geoderma -- for tropical soils, and comments in the code). Alternatively, if no local field soil data is available, these soil parameters (Sat_SWC, Res_SWC) should be drawn from global maps and databases --see Marthews et al. 2014, and directly provided in input. ==> ccl: to standardize the input file, the soil parameters (Sat_SWC, Res_SWC) should probably be provided in input, and the computation of those properties from the available local data made using a new function of RconTROLL, if unearthed.
// since soil layers (silt, clay, sand) are only needed locally, they are now coded as vectors 
//float *proportion_Silt(0);             //!soil layer silt fraction
//float *proportion_Clay(0);             //!soil layer clay fraction
//float *proportion_Sand(0);             //!soil layer sand fraction
float *Sat_SWC(0);          //!< Global vector: soil layer saturated water content, in m3/m3 -- this is often assumed similar to porosity, even though it is usually 5-10% lower than total porosity due to entrapped or dissolved air -- see comment Table 1 in Marthews et al. 2014 
float *Max_SWC(0);          //!< Global vector: soil layer maximum absolute water content (m^3)
float *Res_SWC(0);          //!< Global vector: soil layer residual water content (m^3/m^3)
float *Min_SWC(0);          //!< Global vector: soil layer minimum absolute water content (m^3)
float *Ksat(0);             //!< Global vector: soil layer saturated conductivity (m^3)
float *phi_e(0);            //!< Global vector: parameter for the Campbell-Mualem soil water retention curves (possible update: replace with a Genuchten parameter)
float *b(0);                //!< Global vector: parameter for the Campbell-Mualem soil water retention curves (possible update: replace with a Genuchten parameter)
float **SWC3D(0);           //!< Global 3D field: soil water content in each soil voxel (layer * DCELL) 
float **soil_phi3D(0);      //!< Global 3D field: soil water potential (in MPa) in each soil voxel (layer * DCELL) 
float **Ks(0);              //!< Global 3D field: soil hydraulic conductivity in each soil voxel (layer * DCELL) 
float **KsPhi(0);           //!< Global vector: soil hydraulic conductivity * soil water potential for each soil voxel (layer * DCELL), useful to ease computation 
float *LAI_DCELL(0);        //!< Global vector: total leaf area index (LAI), averaged per DCELL at ground level 
float *Interception(0);     //!< Global vector: water interception by the canopy, per DCELL !!!UNITS
float *Throughfall(0);      //!< Global vector: throughfall, per DCELL !!!UNITS
float *Runoff(0);           //!< Global vector: water run-off, per DCELL !!!UNITS
float *Leakage(0);          //!< Global vector: water leakage, per DCELL !!!UNITS
float *Evaporation(0);      //!< Global vector: water evaporation (physical process), per DCELL 
float **Transpiration(0);   //!< Global vector: water uptake by trees, in each soil voxel (layer * DCELL) 
#endif

//SEED DENSITIES
int **SPECIES_SEEDS (0);    //!< Global 3D field: seed density per site 
double *p_seed(0);          //!< Global vector: probability to draw at a particular site (1/sites) 
unsigned int *n_seed(0);    //!< Global vector: number of seeds distributed on each site 
double *p_species(0);       //!< Global vector: relative frequency of species (not normalised, as it is normalised by the gsl multinomial function) 
unsigned int *n_species(0); //!< Global vector: number of seeds assigned to each species 

int    *SPECIES_GERM (0); //!< Global vector: !!!TO_DOCUMENT
float  *PROB_S (0); //!< Global vector: !!!TO_DOCUMENT _SEEDTRADEOFF 

#ifdef Output_ABC
//Output creation for Approximate Bayesian Computation and Summary Statistics calculation. ABC produces large files, so metrics are pre-calculated within TROLL 
int chmchange_iter = 43;          //!< Global variable: ad-hoc variable. Output at step 43 iterations before the final output, to simulate a suite of successive LiDAR measurements at Nouragues, from March 2012 to October 2015, so 3 * 12 + 7 = 43 !!!UPDATE
//the margins of the ABC analysis, can be smaller than the actually simulated plot to deal with edge effects

int margin; //!< Global variable: for ABC -- margin used to deal with edge effects
int row_start; //!< Global variable: for ABC -- row start after margin clipping
int row_end; //!< Global variable: for ABC -- row end after margin clipping
int col_start; //!< Global variable: for ABC -- col start after margin clipping
int col_end; //!< Global variable: for ABC -- col end after margin clipping
int sites_abc; //!< Global variable: for ABC -- number of sites after margin clipping
int nbvisited; //!< Global variable: for ABC -- number of visited sites !!!UPDATE
int patch_size;   //!< Global variable: for ABC -- patch size !!!UPDATE
float isites_abc;//!< Global variable: for ABC -- inverse of number of sites after margin clipping

int *chm_field_previous(0); //!< Global vector: for ABC -- canopy height model (chm) calculated at previous step !!!UPDATE
int *chm_field_current(0); //!< Global vector: for ABC -- canopy height model (chm) calculated at current step !!!UPDATE
int *chm_field_previous_ALS(0); //!< Global vector: for ABC -- canopy height model (chm) inferred from Airborne Lidar Scanning (ALS) at previous step
int *chm_field_current_ALS(0); //!< Global vector: for ABC -- canopy height model (chm) inferred from Airborne Lidar Scanning (ALS) at current step
int *chm_field_changes(0); //!< Global vector: for ABC -- change in canopy height as calculated 
int *chm_field_changes_ALS(0); //!< Global vector: for ABC -- change in canopy height as observed 
float **transmittance_simulatedALS(0);
float **transmittance_direct(0);
int **transmittance_simulatedALS_sampling(0);
float GPP_MA[120] = {0.0};      //!< Global variable: moving average of GPP across 120 iterations (originally 120 months, could also be made dependent on iterperyear) 
float Litterfall_MA[120] = {0.0};      //!< Global variable: moving average of litterfall across 120 iterations (originally 120 months, could also be made dependent on iterperyear) 
float Mortality_MA[120] = {0.0};      //!< Global variable: moving average of mortality across 120 iterations (originally 120 months, could also be made dependent on iterperyear) 
float Treefall_MA[120] = {0.0};     //!< Global variable: moving average of treefall across 120 iterations (originally 120 months, could also be made dependent on iterperyear) 
#endif

// DIAGNOSTICS OUTPUT STATISTICS
int nblivetrees;  //!< Global variable: for output -- number of live trees at each timestep
int nbtrees_n10;  //!< Global variable: for output -- number of trees dbh > 10 cm, computed at beginning of each timestep 
int nbtrees_n30;  //!< Global variable: for output -- number of trees dbh > 30 cm, computed at beginning of each timestep 
int nbdead_n1;    //!< Global variable: for output -- number of deaths dbh > 1 cm, computed at each timestep 
int nbdead_n10;   //!< Global variable: for output -- number of deaths dbh > 10 cm, computed at each timestep 
int nbdead_n30;   //!< Global variable: for output -- number of deaths dbh > 30 cm, computed at each timestep 
int nbTreefall1;  //!< Global variable: for output -- number of treefalls at each timestep (dbh > 1cm), _BASICTREEFALL 
int nbTreefall10; //!< Global variable: for output -- number of treefalls at each timestep (dbh > 10 cm), _BASICTREEFALL 
int nbTreefall30; //!< Global variable: for output -- number of treefalls at each timestep (dbh > 30 cm), _BASICTREEFALL 

#ifdef Output_ABC
int nbdead_n10_abc;  //!< Global variable: for ABC output -- number of trees dbh > 10 cm at each timestep
int nbTreefall10_abc;//!< Global variable: for ABC output -- number of treefalls at each timestep (dbh > 10 cm) at each timestep
#endif

// Carbon starvation statistics 
int nbtrees_carbstarv_n1; //!< Global variable: for output -- number of trees > 1 cm with carbon starvation
int nbtrees_carbstarv_n10;//!< Global variable: for output -- number of trees > 10 cm with carbon starvation
int nbtrees_carbstarv_n30;//!< Global variable: for output -- number of trees > 30 cm with carbon starvation

// Further output statistics 
//long int *persist;    //!< Global vector: for output --  persistence histogram 
int *nbdbh(0);          //!< Global vector: for output --  dbh size distribution 
float *layer(0);        //!< Global vector: for output --  vertical LAI histogram 

#ifdef Output_ABC
int *abundances_species(0);  //!< Global vector: for output --  !!!UPDATE
int *abundances_species10(0);//!< Global vector: for output --  !!!UPDATE
float *biomass_species(0);   //!< Global vector: for output --  !!!UPDATE
float **traits_species(0);   //!< Global field: for output --  !!!UPDATE
float **traits_species10(0); //!< Global field: for output --  !!!UPDATE
#endif

#ifdef CHECK_CARBON
float carbon_assimilated_total; //!< Global variable: --  !!!UPDATE
float carbon_net_total;         //!< Global variable: --  !!!UPDATE
float carbon_stored_leaves;     //!< Global variable: --  !!!UPDATE
float carbon_stored_trunk;      //!< Global variable: --  !!!UPDATE
float carbon_stored_free;       //!< Global variable: --  !!!UPDATE
#endif

// PROCESSORS FOR MPI OPTION
int mpi_rank; //!< Global variable: processor rank (Message Passing Interface)
int mpi_size; //!< Global variable: processor number (Message Passing Interface)
int easympi_rank;//!< Global variable: processor rank (easy Message Passing Interface)

// MPI PROCEDURES 
#ifdef MPI
unsigned short **LAIc[2]; //!< Global 3D field: sharing LAI across boundaries 
void MPI_ShareSeed(unsigned char **,int); //!< Global MPI function: Communication of border fields in the parallel version of the code
void MPI_ShareField(unsigned short **,unsigned short ***,int); //!< Global MPI function: Communication of fields
void MPI_ShareTreefall(unsigned short **,int); //!< Global MPI function: Communication of treefalls
#endif

#ifdef CROWN_UMBRELLA
// TREESHAPE CALCULATIONS
// All these calculations are currently defined as non-member functions, but could potentially be converted to class Tree 
// See function description for more details
void GetPPFDabove(int height, int site, float noinput, float (&PPFD)[2]); //!< Global function: PPFD retrieval for function leafarea_max()
void GetCanopyEnvironment(int height, int site, float dens, float (&canopy_environment_cumulated)[4]); //!< Global function: calculates the canopy environment
void AddCrownVolumeLayer(int row_center, int col_center, float height, float CR, float CD, int crownvolume[70]); //!< Global function: calculates packing densities 
void UpdateLAI3D(int height, int site, float dens, float &LA_cumulated); //!< Global function: update of LAI3D field, called by CalcLAI()
#ifdef CHM_SPIKEFREE
void UpdateCHMvector(int height, int site, float noinput, vector<int> &chm); //!< Global function: remove outliers in canopy height model (CHM); vector option
void UpdateCHM(int height, int site, float noinput, int *chm); //!< Global function: remove outliers in canopy height model (CHM)
#endif
void ModifyNoinput(float noinput, float &dens_layer, float CD, float height, int layer_fromtop); //!< Global function: dummy function when no modification is needed 
void LAI2dens(float LAI, float &dens_layer, float CD, float height, int layer_fromtop); //!< Global function: a modifying function that converts LAI to the density of a specific layer, using the GetDensity functions
void GetDensitiesGradient(float LAI, float CD, float &dens_top, float &dens_belowtop, float &dens); //!< Global function: deduces within-crown densities from LAI with a gradient from 50% in top layer to 25% in belowtop and 25% in all shells underneath (1 layer for umbrella-like shape) 
void GetDensityUniform(float LAI, float CD, float &dens); //!< Global function: deduces within-crown density from LAI, assuming uniform leaf distribution 
int GetCrownIntarea(float radius); //!< Global function: converts floating point crown area into integer value, imposing lower and upper limits 
float GetRadiusSlope(float CR, float crown_extent, float crown_position); //!< Global function: linear decrease of crown radius 
float GetRadiusCylinder(float CR, float crown_extent, float crown_position); //!< Global function: not currently used, but returns the input radius 
template <typename I, typename O, typename M, typename F>
void LoopLayerUpdateCrownStatistic_template(int row_center, int col_center, float height, float CR, float CD, float fraction_filled_target, int shell_fromtop, float GetRadiusLayer(float, float, float), I CrownStatistic_input, O &CrownStatistic_output, M ModifyCrownStatistic_input, F UpdateCrownStatistic_output);
//!< Global function: Template function to loop across crown layer and update 
template <typename I, typename O, typename F>
void CircleAreaUpdateCrownStatistic_template(int row_center, int col_center, int pos_start, int pos_end, float fraction_filled_target, float &fraction_filled_actual, int height_layer, I CrownStatistic_input, O &CrownStatistic_output, F UpdateCrownStatistic);
//!< Global function: Template function called by LoopLayerUpdateCrownStatistic_template
#endif

// GLOBAL FUNCTIONS 
void Initialise(void); //!< Global function: initialisation with bare ground conditions
void InitialiseFromData(void); //!< Global function: initialisation from inventories
void AllocMem(void); //!< Global function: Field dynamic memory allocation
void BirthInit(void); //!< Global function: Initial non-local germination
void Evolution(void); //!< Global function: Evolution at each timestep
void UpdateSeeds(void); //!< Global function: Compute field Seed
void UpdateField(void); //!< Global function: Update all fields
void TriggerTreefall(void); //!< Global function: Treefall gap formation; v.2.4
void TriggerTreefallSecondary(void); //!< Global function: Secondary treefall gap formation
void FillSeed(int col, int row, int spp); //!< Global function: update SPECIES_SEEDS field; v.2.5
void RecruitTree(void); //!< Global function: tree germination module; v.2.5
void Average(void);  //!< Global function: output of the global averages every timestep 
void OutputField(void); //!< Global function: output of the field variables every timestep 
void OutputSnapshot(fstream& output, bool header, float dbh_limit); //!< Global function: output snapshots of the scene at one point in time
void OutputLAI(fstream& output_transmLAI3D); //!< Global function: writes the whole 3D LAI voxel field to file 
void FreeMem(void);

// HELPER FUNCTIONS
int CalcIntabsorb(float absorb_prev, float absorb_delta); //!< Helper function
int CalcIntabsorb(float absorb_prev); //!< Helper function

#ifdef WATER
//! Global structure: FOR GPPleaf FUNCTION 
struct leafFluxes {
  float carbon_flux;
  float water_flux;
};
# endif

#ifdef Output_ABC
// these are functions needed to create ABC output 
void InitialiseABC(); //!< Global ABC function: initialise ABC conditions
void UpdateMovingAveragesABC(); //!< Global ABC function: yearly statistics
void UpdateDBHtrackingABC(); //!< Global ABC function: update DBH function for ABC routines
void UpdateTransmittanceCHM_ABC(int mean_beam, float sd_beam, float klaser,float transmittance_laser); //!< Global function: calculating the TROLL transmittance field from simulated LiDAR
void OutputABCWriteHeaders(fstream& output_traitconservation, fstream& output_field, fstream& output_CHM, fstream& output_CHM_ALS, fstream& output_transmittance, fstream& output_transmittance_ALS, fstream& output_LAIfield, fstream& output_LAIfield_ALS, fstream& output_chmpotential, fstream& output_species, fstream& output_species10, fstream& output_traits, fstream& output_traits10, fstream& output_biomass); //!< Global function: write headers for ABC outputs 
void OutputABCConservationTraits(fstream& output_traitconservation); //!< Global ABC function: assesses the conservation of traits between input and output in TROLL 
void OutputABC_ground(fstream& output_field); //!< Global ABC function: returns ABC ground outputs
void OutputABC_species(fstream& output_species, fstream& output_species10,fstream& output_traits, fstream& output_traits10, fstream& output_biomass); //!< Global ABC function: returns ABC outputs for species
void OutputABC_CHM(fstream& output_CHM, fstream& output_CHM_ALS, fstream& output_chmpotential); //!< Global function: returns ABC outputs for canopy height model (CHM)
void OutputABC_transmittance(fstream& output_transmittance, fstream& output_transmittance_ALS); //!< Global ABC function: ABC outputs
void OutputABC(); //!< Global ABC function: output general ABC statistics
void OutputCHM(fstream& output_CHM); //!< Global ABC function: creates a Canopy Height Model and LAD profile
#endif

#ifdef TRACK_INDIVIDUALS
void TrackingData_andOutput(); //!< Global function: tree level tracking of key variables 
#endif

//###########################################
//! Species class 
//############################################

class Species {
  
public:
  int s_nbind;            //!< Number of individuals per species 
  int s_nbind10;          //!< Number of individuals > 10cm per species 
  int s_nbind30;          //!< Number of individuals > 30cm per species 
  int s_nbext;            //!< Total number of incoming seeds in the simulated plot at each timestep (seed rain) -- v.2.2 
  string s_name;          //!< Species name, updated to string in v.3.0 (higher flexibility) 
  
  float s_LCP;            //!< Light compensation point  (micromol photon/m^2/s) 
  float s_Rdark;          //!< Dark respiration rate at PPFD = 0 (micromol C/m^2/s) 
  float s_ds;             //!< Mean seed dispersal distance (m)
  float s_dbhmax;         //!< Maximal dbh (m) !! data usually report dbh in cm
  float s_hmax;           //!< Maximal height (m) 
  float s_LMA;            //!< Leaf mass per area (g/m^2) 
  float s_Nmass;          //!< Leaf nitrogen concentration (g/g) v.2.01 
  float s_Pmass;          //!< Leaf phosphorous concentration (g/g) v.2.01 
  float s_wsg;            //!< Wood specific gravity (g/cm^3) 
  float s_ah;             //!< Parameter for allometric height-dbh equation 
  float s_regionalfreq;   //!< Regional frequency; v.3.0 !!!UPDATE
  float s_drymass;        //!< Drymass; v.3.0  !!!UPDATE
  float s_seedmass;       //!< Seed mass (g); See Baraloto & Forget 2007 dataset v.2.3; deprecated in v.2.2, but still necessary for SEEDTRADEOFF 
  float s_iseedmass;      //!< Inverse of seed mass (1/g), v.2.3
  float s_output_field[12];     //!< Scalar output fields per species, in v.2.5: removed 12 fields that were not used 
  
  float s_tlp;            //!< Leaf water potential at turgor loss point (MPa); defined for consistency when WATER is deactivated 
#ifdef WATER // Some of these parameters may include intraspecific variability, as in v.2.4.1.
  //float s_g1;                  // parameter of stomatal conductance model. I went back to a species-specific value of g1 using Lin et al. 2015 relationship or Wu et al. 2019 GCB. It seems needed to well simulate functional shift through a regeneration. Different values of g1 across PFT are also used by Xu et al. 2016 New Phytologist using ED2+SPA on tropical dry forest. See also Domingues, Martinelli, & Ehleringer (2014) and Franks et al. (2018) for values of g1 for an Amazonian forest or potential alternative parameterization of g1 respectively
  //float s_dWSF;               // this is the denominator of WSF, when WSF is a linear normalization of phi_root (ie. s_dWSF= phi_sc - s_tlp, where phi_sc corresponds to the onset of water stress (inducing stomatal and non-stomatal responses), ie. leaf predawn water potential at which WSF starts decreasing <1) 
  float s_phi_lethal,           //!< plant water potential at drought-induced death (MPa)
  s_itlp,         //!< inverse of s_tlp
  s_b;            //!< parameter used to compute the water stress factor (WSF) for stomatal limitation 
#endif
  
#ifdef Output_ABC
  float s_dbhmax_realized;  //!< Maximum dbh found across individuals from this species 
#endif
  
#ifdef MPI
  unsigned char *s_Gc[4]; //!< MPI: seeds on neighboring procs 
#endif
  
  //! Constructor of the Species class
  Species() {
    s_nbind=0;
    s_nbind10=0;
    s_nbind30=0;
  };
  
  void Init();
};

vector<Species> S; //!< Definition of a vector of the Species class

//######################################
//! Species class function:
//! Initialise Species vector
//######################################
void Species::Init() {
  
  // !!!: WARNING! seed volume is provided instead of seed mass
  // We assume a conversion factor of 1.0 from wet volume to wet mass (~density of water)
  // We assume a conversion factor of 0.4 from wet mass to dry mass (~40% of the seed are water)
  s_seedmass *= 0.4;
  s_iseedmass=1.0/s_seedmass;
  s_ds=40.0; // !!!UPDATE
  
  if(_SEEDTRADEOFF) s_nbext = (int(s_regionalfreq*Cseedrain*s_iseedmass)+1);
  else s_nbext = int(s_regionalfreq*Cseedrain*(sites*LH*LH/10000));
  
  s_nbind=0;
  
  //Computation of the light compensation point from dark respiration and the quantum yield phi
  //By definition, Rdark is in micromolC/m^2/s and it is used in the Species::NPP() routine 
  s_LCP = s_Rdark/phi;    
  
#ifdef WATER
  //s_g1=-3.97*s_wsg+6.53;                      // from Lin et al. 2015 Nature Climate Change 
  //s_dWSF=1.0/(-0.00395145-0.3626778*s_tlp);         // this is the denominator of WSF= phi_sc - s_tlp, where phi_sc (for 'stomatal closure') corresponds to the onset of water stress, derived from the relationship between leaf phi at 50%loss of stomatal closure (ie. WSF~0.5 considering only stomatal responses to water stress) and turgor loss point (drawn from Bartlett et al. 2016 PNAS), and given the shape of WSF factor here assumed
  s_phi_lethal= -0.9842 + 3.1795*s_tlp;       // Inferred from data provided in Bartlett et al. 2016 PNAS
  s_itlp=1/s_tlp;
  s_b=-2.23*s_itlp; // This relationship should be justified !!!UPDATE
#endif
}

//###########################################
//! Tree class 
//############################################

class Tree {
public:
  int t_site;           //!< Geolocation of the tree 
  int t_NPPneg;         //!< Diagnostic variable: number of consecutive timesteps with NPP<0; v.2.2 
  int t_CrownDisplacement; //!< Displacement of the crown center with respect to the stem. Currently not used in TROLL, but required for initialization via the Canopy Constructor algorithm. Its rationale is the same as for t_site, i.e. t_Crown_Displacement = col_displacement + row_displacement * cols, so it can be added to t_site to obtain the geolocation of the crown center; v.2.5
  
  float t_age;         //!< Tree age, also indicates whether tree is alive (live trees are such that t_age > 0.0) 
  float t_hmax;        //!< Allometric parameter, not real maximum 
  float t_ah;          //!< Allometric parameter, for consistency with t_hmax also an individual parameter; v.2.4
  float t_dbh;         //!< Diameter at breast height (m) beware: this scales with NH, the horizontal size of voxels
  float t_dbhmature;   //!< Reproductive size threshold; v.2.3
  float t_dbhmax;      //!< Maximum diameter at breast height (dbh), as estimated from field data 
  float t_Tree_Height; //!< Total tree height (m) beware: this scales with NV, the vertical size of voxels
  float t_Crown_Depth; //!< crown depth (m) beware: this scales with NV, the vertical size of voxels
  float t_Crown_Radius;//!< crown radius (m) beware: this scales with NH, the horizontal size of voxels
  float t_Ct;          //!< flexural force threshold, _BASICTREEFALL 
  float t_GPP;         //!< Gross primary productivity of the tree (gC/timestep)
  float t_NPP;         //!< Net primary productivity of the tree (gC/timestep)
  float t_Rday;        //!< Daytime leaf respiration of the tree (gC/timestep)
  float t_Rnight;      //!< Nighttime leaf respiration of the tree (gC/timestep)
  float t_Rstem;       //!< Stem respiration (gC/timestep)
  float t_leafarea;    //!< Total crown leaf area (m^2); v.2.2
  float t_youngLA;     //!< Total young leaf area (m^2); v.2.2
  float t_matureLA;    //!< Total mature leaf area (m^2); v.2.2
  float t_oldLA;       //!< Total old leaf area (m^2); v.2.2
  float t_LAI;         //!< Total leaf area index (m^2/m^2), t_LAI replaces t_dens and average crown leaf density. LAI can be converted into densities; LAI is more relevant given the new dynamic leaf module, and also more informative as output variable; v.2.5
  float t_litter;      //!< Tree litterfall at each timestep, in dry mass (g); v.2.2
  
  vector<float> t_NDDfield;   //!< Tree field useful when option _NDD is activated
  
#ifdef WATER
  // !!!: suggestion, maybe redefine some quantities via a field (i.e. tree root biomass...) or locally where they are needed (especially if they need to be recomputed every timestep)
  float t_root_depth;     //!< Tree rooting depth (m)
  float t_phi_root;       //!< Soil water potential in the root zone (MPa)
  vector<float> t_root_biomass;   //!< Tree root biomass in each soil layer !!!UPDATE: which unit?
  vector<float> t_soil_layer_weight;  //!< Soil layer weight (to compute t_phi_root), if different from root biomass 
  float t_WSF;            //!< Tree water stress factor for stomatal conductance, unitless, between 0 and 1
  float t_WSF_A;          //!< Tree water stress factor for photosynthetic capacities, unitless, between 0 and 1
  float t_transpiration;  //!< Amount of water taken up from the soil and transpired at each timestep !!!UPDATE: which unit?
#endif
  
  // new in v.2.4.0: traits defined at  individual level 
  float t_Pmass;        //!< Phosphorus content, defined at tree scale !!!UPDATE: which unit?
  float t_Nmass;        //!< Nitrogen content, defined at tree scale !!!UPDATE: which unit?
  float t_LMA;          //!< Leaf mass per area (LMA), defined at tree scale (g/m^2)
  float t_wsg;          //!< Wood specific gravity, defined at tree scale (g/cm^3)
  float t_Rdark;        //!< Dark respiration rate at PPFD = 0 (micromol C/m^2/s)
  float t_Vcmax;        //!< Maximal rate of carboxylation, on an area basis (micromolC/m^2/s)
  float t_Jmax;         //!< Maximal rate of electron transport, on an area basis (micromol/m^2/s)
  //t_Vcmaxm, //maximal rate of carboxylation, on an mass basis (in micromolC/g-1/s), since v.2.4.1 calculated locally 
  //t_Jmaxm,  //maximal rate of electron transport, on a mass basis (in micromol/g-1/s), since v.2.4.1 calculated locally 
  //t_fci,    //fraction of CO2 partial pressure in intercellular spaces divided by ambiant CO2 partial pressure (both in microbar, or ppm = micromol/mol) -- deprecated in v.2.4.1 
  // t_Gamma, //compensation point for the carboxylation rate, here NORMALIZED by atm CO2 concentration (Cair) -- not used anymore, removed in v.2.4.1 
  //t_Km,     //apparent kinetic constant for the rubiscco = Kc*(1+[O]/Ko), here normalized by atm CO2 concentration (Cair) -- deprecated in v.2.4.1 
  float t_leaflifespan;  //!< Average leaf lifespan (months) !!!UPDATE: should be in SI units?
  float t_lambda_young;  //!< Residence time in the young leaf class, inverse of young leaf lifespan; v.2.5 !!!UPDATE: which unit?
  float t_lambda_mature; //!< Residence time in the mature leaf class, inverse of mature leaf lifespan; v.2.5  !!!UPDATE: which unit?
  float t_lambda_old;    //!< Residence time in the old leaf class, inverse of mature leaf lifespan; v.2.5 !!!UPDATE: which unit? 
  
  float t_intraspecific_multiplier_height;  //!< Intraspecific multiplier for height (due to intraspp lognormal variation); v.2.4.0
  float t_intraspecific_multiplier_CR;      //!< Intraspecific multiplier for crown radius (due to intraspp lognormal variation); v.2.4.0
  float t_intraspecific_multiplier_CD;      //!< Intraspecific multiplier for crown depth (due to intraspp lognormal variation); v.2.4.0
  float t_intraspecific_multiplier_P;       //!< Intraspecific multiplier for leaf phosphorus concentration (due to intraspp lognormal variation); v.2.4.0
  float t_intraspecific_multiplier_N;       //!< Intraspecific multiplier for leaf nitrogen concentration (due to intraspp lognormal variation); v.2.4.0
  float t_intraspecific_multiplier_LMA;     //!< Intraspecific multiplier for leaf mass per area (due to intraspp lognormal variation); v.2.4.0
  float t_intraspecific_multiplier_dbhmax;  //!< Intraspecific multiplier for dbh maximum (due to intraspp lognormal variation); v.2.4.0
  float t_intraspecific_deviation_wsg;      //!< Intraspecific absolution deviation for wood specific gravity (due to intraspp normal variation); v.2.4.0
  
  float t_LAImax;           //!< Maximal LAI; Dynamic adjustment of leaf allocation, based on light environment !!!UPDATE
  float t_leafarea_max;     //!< Maximal leaf area; Dynamic adjustment of leaf allocation, based on light environment  !!!UPDATE
  float t_carbon_storage;   //!< Leaf C storage pool. If leaf area is optimal, surplus carbon is allocated to a storage pool
  float t_carbon_biometry;  //!< Biometry C storage pool. In case a tree is suffering from carbon stress, it will tap this pool to renew its leaves and slow down growth 
  float t_fraction_filled;  //!< Filled fraction of tree crown (1-gap fraction). Differs from the global fraction: trees with larger variation in CR also have a lower fraction of filled crown area; so intraspp variation in crown radius is decoupled from leaf area and only describes crown size; v.2.5.0
  
  int t_multiplier_seed;    //!< Not documented  !!!UPDATE
  float t_sapwood_area;     //!< Sapwood area !!!UPDATE units, description
  
#ifdef CROWN_UMBRELLA
  float t_Crown_Slope;      //!< Not documented  !!!UPDATE
#endif
  
  unsigned short
    t_from_Data,            //!< Indicator: tree was born through initialisation (1) or through simulation routine (0)
    t_sp_lab,               //!< Stores the species label. Can be defined even if the site is empty (cf. persistence function defined in Chave, Am Nat. 2001) 
    t_hurt;                 //!< Stories the treefall index: tree is either killed or harmed. In the latter case, t_hurt records the harm level
  
#ifdef Output_ABC
  float t_dbh_previous;  //!< Not documented  !!!UPDATE
#endif
  
#ifdef TRACK_INDIVIDUALS
  int t_month_born;     //!< Not documented  !!!UPDATE
  
  int t_seedsproduced;            //!< Number of seeds produced in total 
  int t_seedsproduced_sumyear;    //!< Number of seed produced in a given year 
  int t_time_carbonstarvation;    //!< Total number of iterations in carbon starvation status
  int t_time_carbonstarvation_year;//!< Number of iterations in carbon starvation status in a given year 
  
  float t_GPP_sumyear;            //!< Total GPP for a specific year (gC/yr)
  float t_NPP_sumyear;            //!< Total NPP for a specific year (gC/yr)
  float t_GPPsquared_sumyear;     //!< Squared GPP for a specific year, to calculate standard deviation !!!UPDATE: is that really needed in the class??
  float t_NPPsquared_sumyear;     //!< Squared NPP for a specific year, to calculate standard deviation !!!UPDATE: is that really needed in the class??
  float t_Rday_sumyear;           //!< Total day leaf respiration for a specific year (gC/yr)
  float t_Rnight_sumyear;         //!< Total night leaf respiration for a specific year (gC/yr)
  float t_Rstem_sumyear;          //!< Total stem respiration for a specific year (gC/yr)
  float t_LAIabove_effavgyear;    //!< LAI effectively experienced for each tree, effective, because LAI is not simply averaged, but calculated from average PPFD experienced (maybe in future versions, PPFD could be directly reported) 
  float t_carbon_storage_avgyear; //!< Total carbon storage per year 
  
  float t_dbh_tracked;            //!< Stores previous dbh to compute dbh increment
  float t_Tree_Height_tracked;    //!< Stores previous height to compute height increment 
  float t_Crown_Radius_tracked;   //!< Stores previous CR (crown radius) to compute CR increment 
  float t_agb_tracked;            //!< Stores previous agb (aboveground biomass) to compute agb increment 
  
  //These variables are used for calculating lifetime means and standard deviations 
  float t_LAIcum;             //!< Lifetime LAI (leaf area index), cumulated 
  float t_LAIeffcum;          //!< Lifetime effective LAI (leaf area index, computed from inversion of PPFD), cumulated 
  float t_GPPcum;             //!< Lifetime GPP (gross primary productivity), cumulated 
  float t_NPPcum;             //!< Lifetime NPP (net primary productivity), cumulated 
  float t_LAIsquared_cum;     //!< Squared lifetime LAI, to compute standard deviation 
  float t_LAIeffsquared_cum;  //!< Squared lifetime LAI (effective, i.e. from reversed PPFD), to compute standard deviation 
  float t_GPPsquared_cum;     //!< Squared lifetime GPP, to compute standard deviation 
  float t_NPPsquared_cum;     //!< Squared lifetime NPP, to compute standard deviation 
  
#endif
  //! Function constructor Tree()
  Tree(){
    t_from_Data = 0;
    t_sp_lab = 0;
    t_age = 0;
    t_hurt = 0;
    t_NPP=t_GPP=t_Rday=t_Rnight=t_Rstem=0.0; // new v.2.2 
    t_dbh = t_Tree_Height = t_Crown_Radius = t_Crown_Depth= 0.0;
    t_CrownDisplacement = 0;
    
    if(_NDD){
      t_NDDfield.reserve(nbspp+1);
      for(int sp=0;sp<(nbspp+1);sp++) t_NDDfield.push_back(0.0);
    }
#if defined(WATER)
    t_transpiration = 0.0;
    t_root_biomass.reserve(nblayers_soil);
    for(int l=0;l<nblayers_soil;l++) {
      t_root_biomass.push_back(0.0);
    }
    t_soil_layer_weight.reserve(nblayers_soil);
    for(int l=0;l<nblayers_soil;l++){
      t_soil_layer_weight.push_back(0.0);
    }
#endif
    
#ifdef Output_ABC
    t_dbh_previous = 0.0;
#endif
    if(_BASICTREEFALL) t_Ct = 0.0;
  };
  
  void Birth(int,int);    //!< Tree birth 
  void BirthFromInput(int species_label, int site0, float dbh, float height, float CR, float CD, float var_height, float var_CR, float var_CD, float CrownDisplacement, float Nmass, float Pmass, float LMA, float wsg, float leafarea); //!< Tree initialisation from field data 
  void Death();                   //!< Tree death, called by Tree::Update
  void Growth();                  //!< Tree growth
  void Fluxh(int h, float &PPFD, float &VPD, float &Tmp, float &leafarea_layer); //!< Computation of PPFD right above the tree -- called by Tree::Birth and Tree::Growth
  
#ifdef WATER
  void Water_availability();      //!< Computation of the tree water availability in the root zone
  //compute root depth, root biomass in each layer, soil water potential in the root zone, and water stress factor 
  //see comments at Tree::Water_availability
  //void UpdateRootDistribution();  //compute root depth, root biomass in each layer, soil water potential in the root zone, and water stress factor 
  void Water_uptake();            //!< Contribution of trees to the stand Transpiration field -- called by UpdateField
#endif
  
  void CalcRespGPP();             //!< GPP and respiration calculation, called by Tree::Growth
  void UpdateLeafDynamics();      //!< Leaf dynamics and C allocation, called by Tree::Growth
  void UpdateTreeBiometry();      //!< Compute biometric relations, including allometry
  void UpdateVolumeDensity();     //!< Follows updates to leaf dynamics and biometry and computes the new leaf density; v.2.4.1
  //!< - For plastic and shy crowns, moved inside allocation step, otherwise computed straight after other update functions 
  
  void DisperseSeed();            //!< Seed dispersal, called by UpdateField
  void Treefall(float angle);     //!< Tree falling function, called by TriggerTreefall
  void Update();                  //!< Tree death and growth 
  void Average();                 //!< Local computation of the averages 
  void CalcLAI();                 //!< Update of the LAI3D field 
  void histdbh();                 //!< Computation of dbh histograms 
  
  //! empirical functions for trait calculation and tree level variables (Calc functions return the specific parameter, Update functions update a specific variable at tree level)  
  float CalcVcmaxm();                         //!< Returns Vcmax for Farquhar model 
  float CalcJmaxm();                          //!< Returns Jmax for for Farquhar model 
  float CalcRdark();                          //!< Returns Rdark for for Farquhar model 
  float CalcAGB();                            //!< Calculation of above ground biomass (in kg) !!!: if updated, also update CalcIncrementDBH, cf. below 
  float CalcIncrementDBH(float delta_agb);    //!< Calculation of the increment of dbh from assimilated carbon/biomass (in m) 
  float CalcCarbonStorageMax();               //!< Calculation of the maximum amount of carbon stored in a tree 
  float CalcCt();                             //!< Calculation of the treefall threshold, if _BASICTREEFALL is activated 
  
  void UpdateLeafLifespan();                  //!< Determines leaf life span, either from empirical function or from Kikuzawa model 
  void UpdateSapwoodArea(float ddbh);         //!< Determine sapwood area, limited by increase in dbh (ddbh) (in m2) 
  void UpdateHeight();                        //!< Updates t_Tree_Height, based on t_dbh 
  void UpdateCR();                            //!< Updates t_Crown_Radius, based on t_dbh 
  void UpdateCD();                            //!< Updates t_Crown_Depth based on t_Tree_Height 
  
#ifdef CROWN_UMBRELLA
  void UpdateCrownSlope();                    //!< Updates t_Crown_Slope based on t_Crown_Radius and t_Tree_Height
#endif
  
  // GPP functions are now calculated at tree level 
  float DeathRateNDD(float, int, float); //!< Death rate function, including negative density dependence mortality, called when option _NDD is on.
#ifdef WATER
  float DeathRate(float, int, float);      //!< Death rate function, including drought-induced mortality
  leafFluxes FluxesLeaf(float, float, float); //!< Farquhar von Caemmerer Berry model -- computation of the light-limited leaf-level average C assimilation rate per m^2 (micromol/m^2/s); AND leaf-level evapotranspiration per m^2 (micromol/m^2/s)-- depends on daily variation in light (PPFD), vapor pressure deficit (VPD) and temperature (T)
  leafFluxes dailyFluxesLeaf(float, float, float); //!< Computation of average C assimilation rate and water evapotranspiration per leaf area across daily variation in light (PPFD), vapor pressure deficit (VPD) and temperature (T); valid with option _DAYTIMELIGHT 
  void OutputTreeStandard(fstream& output); //!< Standard outputs during the simulation -- written to file
  void OutputTreeStandard(); //!< Standard outputs during the simulation -- written to screen in real time
  // !!!UPDATE: question, why are these two functions needed? the first function with Rcout as an argument is the same as the second function, no? question2: why are these functions only available in WATER mode?
#else
  float DeathRate(float, int);  //!< Death rate function
  float GPPleaf(float, float, float);    //!< Farquhar von Caemmerer Berry model -- computation of the light-limited leaf-level average C assimilation rate per m^2 (micromol/m^2/s) -- depends on daily variation in light (PPFD), vapor pressure deficit (VPD) and temperature (T)
  float dailyGPPleaf(float, float, float);    //!< Computation of average C assimilation rate per leaf area across daily variation in light (PPFD), vapor pressure deficit (VPD) and temperature (T); valid with option _DAYTIMELIGHT 
  float dailyGPPcrown(float, float, float, float);  //!< Farquhar von Caemmerer Berry model -- proposition of reinsertion of fastdailyGPPleaf() as dailyGPPcrown(), main reason: despite sharing some code with dailyGPPleaf, structurally very different, updated in v.2.5, replaced dens * CD by LAI 
#endif
  float Rdayleaf(float T); //!< !!!UPDATE
  float dailyRdayleaf(float T); //!< !!!UPDATE
  
  void treeLAImax();             //!< Determines the maximum LAI that the tree should reach, given the Farquhar model, at a theoretical average day, up to which point leaves can be allocated and until which the costs (self-shading) are lower than the benefits (additional assimilation) 
  void leafarea_max(float &LAIexperienced_eff, float &leafarea_max);  //!< Determines the maximum LAI that the tree should reach, similar to treeLAImax, but it directly calculates the maximum leafarea for the current light environment the tree experiences rather than for theoretical day
  float predLeafLifespanKikuzawa();                                   //!< Kikuzawa model for leaf lifespan 
  float GetCrownAreaFilled(float crown_area);                         //!< Calculate the crown area filled by leaves (only relevant for crown gap fractions > 0.0) 
  
#ifdef TRACK_INDIVIDUALS
  float StartTracking(); //!< Diagnostic function to track trees born at a reference year
#endif
  
};

vector<Tree> T; //!< Definition of a vector of the Tree class

//#############################################
// Tree birth
//#############################################
//! Actions needed at birth for a tree (precisely, at time when a stem enters the > 1 cm trunk diameter class)
void Tree::Birth(int nume, int site0) {
  nblivetrees++;
  t_site = site0;
  t_sp_lab = nume; 
  S[t_sp_lab].s_nbind++;
  t_multiplier_seed = 1;
  t_age = 1;
  t_hurt = 0;
  t_NPPneg=0;
  
  //######################
  //# intraspecific var ##
  //######################
  
  int dev_rand = int(gsl_rng_uniform_int(gslrand,100000));    //Updated to 100000 from 10000 in v.3.0
  t_intraspecific_multiplier_height = d_intraspecific_height[dev_rand];
  t_intraspecific_multiplier_CR = d_intraspecific_CR[dev_rand];
  t_intraspecific_multiplier_N = d_intraspecific_N[dev_rand];
  t_intraspecific_multiplier_P = d_intraspecific_P[dev_rand];
  t_intraspecific_multiplier_LMA = d_intraspecific_LMA[dev_rand];
  t_intraspecific_multiplier_CD = d_intraspecific_CD[dev_rand];
  t_intraspecific_deviation_wsg = d_intraspecific_wsg[dev_rand];
  t_intraspecific_multiplier_dbhmax = d_intraspecific_dbhmax[dev_rand];
  
  //#####################
  //# leaf/wood traits ##
  //#####################
  t_Pmass = S[t_sp_lab].s_Pmass * t_intraspecific_multiplier_P;
  t_Nmass = S[t_sp_lab].s_Nmass * t_intraspecific_multiplier_N;
  t_LMA = S[t_sp_lab].s_LMA * t_intraspecific_multiplier_LMA;
  t_wsg = fmaxf(S[t_sp_lab].s_wsg + t_intraspecific_deviation_wsg,0.1); //Cutoff for normal distribution
  
  t_Vcmax =  CalcVcmaxm() * t_LMA;
  t_Jmax = CalcJmaxm() * t_LMA;
  t_Rdark = CalcRdark();
  
  //#################$####
  //# biometry/allometry ##
  //##################$###
  
  t_hmax = S[t_sp_lab].s_hmax;
  t_ah = S[t_sp_lab].s_ah;
  
  t_dbh = DBH0;
  t_dbhmax = S[t_sp_lab].s_dbhmax;
  t_dbhmax *= t_intraspecific_multiplier_dbhmax;
  t_dbhmax = fmaxf(t_dbhmax,DBH0 * 2.0);
  t_dbhmature = t_dbhmax*0.5; //Mean threshold of tree size to maturity - see Visser et al. 2016 Functional Ecology (suited to both understory and top-canopy species). NOTE that if we decide to keep it as a fixed species-specific value, this could be defined as a Species calss variable, and computed once in Species::Init. -- v230
  
  UpdateHeight();
  UpdateCR();
  UpdateCD();
  
  t_CrownDisplacement = 0;
  
#ifdef CROWN_UMBRELLA
  UpdateCrownSlope();
#endif
  
  if(_BASICTREEFALL) {
    t_Ct = CalcCt();
  }
  
  //##################
  //# leaf dynamics ##
  //##################
  float crown_area = PI*t_Crown_Radius*t_Crown_Radius;
  float fraction_filled_general = 1.0 - crown_gap_fraction;
  t_fraction_filled = fminf(fraction_filled_general/(t_intraspecific_multiplier_CR * t_intraspecific_multiplier_CR),1.0);
  float crown_area_nogaps = GetCrownAreaFilled(crown_area);
  
  if(_LA_regulation > 0){
    //In this case, we determine both maximum LAI (theoretical if fully exposed to sunlight) and maximum leaf area (actual, i.e. also considering shading by neighboring trees) 
    treeLAImax();
    float LAIexperienced_eff;
    leafarea_max(LAIexperienced_eff, t_leafarea_max);
    t_leafarea = 0.25 * t_leafarea_max;      //Initially trees are set to have a quarter of their maximum leaf area 
    t_LAI = t_leafarea/crown_area_nogaps;
    t_carbon_storage = CalcCarbonStorageMax() * 0.5; //Initial value with half of the maximum storage
    t_carbon_biometry = 0.0;
  } else {
#ifdef CROWN_UMBRELLA
    t_LAI = dens * fminf(t_Crown_Depth, 3.0);
#else
    t_LAI = dens * t_Crown_Depth;
#endif
    t_leafarea = t_LAI * crown_area_nogaps;
  }
  UpdateLeafLifespan();
  
  //############
  //# sapwood ##
  //############
  float ddbh = t_dbh;     //Initially each stem can be treated as juvenile and stem cross section is mostly sapwood
  t_sapwood_area = 0.0;   // the sapwood area is initialized to zero and then updated
  UpdateSapwoodArea(ddbh);
  
#ifdef WATER
  //##########
  //# water ##
  //##########
  //Water_availability();
  t_transpiration = 0.0;
  t_WSF = t_WSF_A = 1.0; // Tree leaf lifespan and LAImax are computed under no water stress and with average climatic conditions (radiation, temperature). 
  //This could/should be updated later, so that these two quantities change under water stress, and also seasonally. 
  //t_WSF and t_WSF_A are then updated later in Tree::Birth to account for the real water conditions at birth.
  Water_availability(); //Roots are not set here, but at the beginning of Tree::Update (however see comments within function Tree::Water_availability)
  //UpdateRootDistribution();
#endif
  
  //###############
  //# for output ##
  //###############
#ifdef Output_ABC
  t_dbh_previous = t_dbh;
#endif
  
#ifdef TRACK_INDIVIDUALS
  StartTracking();
#endif
}


//#############################################
// Tree initialisation from field data
//##############################################
//! - Modelled for compatibility with Tree::Birth. For comments cf. Tree::Birth
//! - For comments regarding allometries and t_leafarea cf. Tree::Growth.
void Tree::BirthFromInput(int species_label, int site0, float dbh, float height, float CR, float CD, float var_height, float var_CR, float var_CD, float CrownDisplacement, float Nmass, float Pmass, float LMA, float wsg, float leafarea) {
  
  t_age = 1;          //value not correct, but generally not problematic, used mainly as diagnostic variable and as indicator of whether tree is alive or not (death itself is independent of age), BUT: use as diagnostic variable cannot be ensured anymore and be careful if conditioning on t_age (e.g. for maturation)
  t_from_Data = 1;    //indicates that tree stems from data (and therefore t_age could not be used, etc.)
  
  t_site = site0;
  t_sp_lab = species_label;
  t_hurt = 0;
  t_hmax = S[t_sp_lab].s_hmax;
  t_ah = S[t_sp_lab].s_ah;
  
  t_dbh = dbh;
  t_Tree_Height = height;
  t_Crown_Radius = CR;
  t_Crown_Depth = CD;
  t_intraspecific_multiplier_height = exp(var_height);
  t_intraspecific_multiplier_CR = exp(var_CR);
  t_intraspecific_multiplier_CD = exp(var_CD);
  t_CrownDisplacement = CrownDisplacement;
  t_Nmass = Nmass;
  t_Pmass = Pmass;
  t_LMA = LMA;
  t_wsg = wsg;
  t_leafarea = leafarea;
  t_sp_lab = species_label;
  
  //calculate the multipliers 
  t_intraspecific_multiplier_P = t_Pmass/S[t_sp_lab].s_Pmass;
  t_intraspecific_multiplier_N = t_Nmass/S[t_sp_lab].s_Nmass;
  t_intraspecific_multiplier_LMA = t_LMA/S[t_sp_lab].s_LMA;
  t_intraspecific_deviation_wsg = t_wsg/S[t_sp_lab].s_wsg;
  
  //calculate dbh related metrics 
  int dev_rand = int(gsl_rng_uniform_int(gslrand,10000));
  t_intraspecific_multiplier_dbhmax = d_intraspecific_dbhmax[dev_rand];
  
  t_dbhmax = (S[t_sp_lab].s_dbhmax) * t_intraspecific_multiplier_dbhmax;
  t_dbhmax = fmaxf(t_dbhmax,DBH0 * 2.0);
  t_dbhmature=t_dbhmax*0.5; //this correponds to the mean thresholds of tree size to maturity, according to Visser et al. 2016 Functional Ecology (suited to both understory short-statured species, and top canopy large-statured species). NOTE that if we decide to keep it as a fixed species-specific value, this could be defined as a Species calss variable, and computed once in Species::Init. -- v230
  
  if(t_dbh > 1.5 * t_dbhmax){
    Rcout << t_site << " WARNING: t_dbh > 1.5 * t_dbhmax. t_dbh: " << t_dbh << " t_dbhmax: " << t_dbhmax << ". We reset t_dbhmax to t_dbh" << endl;
    t_dbhmax = t_dbh;
    //t_intraspecific_multiplier_dbhmax = t_dbhmax/(t_s->s_dbhmax);
  }
  
#ifdef CROWN_UMBRELLA
  UpdateCrownSlope();
#endif
  
#ifdef Output_ABC
  t_dbh_previous = t_dbh;
#endif
  
  //separate into gaps and nogaps area 
  float crown_area = PI*t_Crown_Radius*t_Crown_Radius;
  float fraction_filled_general = 1.0 - crown_gap_fraction;
  t_fraction_filled = fminf(fraction_filled_general/(t_intraspecific_multiplier_CR * t_intraspecific_multiplier_CR),1.0);
  float crown_area_nogaps = GetCrownAreaFilled(crown_area);
  
  //calculate LAI, which is the LAI of the no gaps area 
  t_LAI = leafarea / crown_area_nogaps;
  
  //calculate variables for leaf module 
  t_Vcmax = CalcVcmaxm() * t_LMA;
  t_Jmax = CalcJmaxm() * t_LMA;
  t_Rdark = CalcRdark();
  
  t_Rstem = 0.0;
  t_Rday = 0.0;
  t_GPP = 0.0;
  t_NPP = 0.0;
  t_NPPneg=0;
  
  if(_BASICTREEFALL) {
    t_Ct = CalcCt();
  }
  
  t_multiplier_seed = 1;
  
  //sapwood area 
  float ddbh = t_dbh;     // in the beginning, all the stem can be treated as "just grown" and the only limit on sapwood is the overall stem cross section --> this may be exaggerated by far for large trees, but sapwood will also be bound by leaf area, so this should not yield unrealistic values
  t_sapwood_area = 0.0;   // the sapwood area is initialized to zero and then updated
  UpdateSapwoodArea(ddbh);
  
  if(_LA_regulation > 0){
    treeLAImax();
    t_carbon_storage = CalcCarbonStorageMax() * 0.5; // initialise with half of maximum of carbon stored
    t_carbon_biometry = 0.0;
  }
  
  UpdateLeafLifespan();
  
  S[t_sp_lab].s_nbind++;
  nblivetrees++;
  
}

//###############################################
// Update of the LAI3D field
//! called by UpdateField
//#################################################
//! - modified in v.2.3: additional contribution to voxels that are not fully occupied by the tree crown. !!!: this does not calculate LAI3D directly, this only calculates the density in each voxel belonging to a tree. The final LAI field is calculated outside of the class Tree 
//! - modified in v.2.4 and v.2.5: introducing an alternative crown shape, "umbrella"-like, inspired by previous shell models and similar to the crown shapes in the PPA. If activated, crowns contain three layers of vegetation that, once the crown goes beyond 3m in depth, will bend downwards on the edges with a linear slope. Since v.2.5 all loops (CalcLAI, Fluxh, leafarea_max) are executed through the same template. This allows to implement other crown shapes in the future and ensures that modifications are carried through across the code 
#ifdef CROWN_UMBRELLA
void Tree::CalcLAI() {
  if(t_age > 0){
    int site_crowncenter = t_site + t_CrownDisplacement;
    int row_crowncenter = site_crowncenter/cols;
    int col_crowncenter = site_crowncenter%cols;
    
    float LA_cumulated = 0.0;   //Currently, an output variable is required by LoopLayerUpdateCrownStatistic_template, we here use LA_cumulated as control variable 
    
    int crown_top = int(t_Tree_Height);
    int crown_base = int(t_Tree_Height - t_Crown_Depth);
    int max_shells = min(crown_top - crown_base + 1, 4);    //since the new crown shapes 
    float fraction_filled_target = t_fraction_filled;
    
    for(int shell_fromtop = 0; shell_fromtop < max_shells; shell_fromtop++){
      LoopLayerUpdateCrownStatistic_template(row_crowncenter, col_crowncenter, t_Tree_Height, t_Crown_Radius, t_Crown_Depth, fraction_filled_target, shell_fromtop, GetRadiusSlope, t_LAI, LA_cumulated, LAI2dens, UpdateLAI3D);
    }
  }
}
#else
void Tree::CalcLAI() {
  if(t_age > 0){
    int crown_base = int(t_Tree_Height-t_Crown_Depth),
      crown_top = int(t_Tree_Height);
    int site_crowncenter = t_site + t_CrownDisplacement;
    int row_crowncenter = site_crowncenter/cols;
    int col_crowncenter = site_crowncenter%cols;
    
    float fraction_abovetop = t_Tree_Height - float(crown_top);
    float fraction_belowbase = float(crown_base + 1) - (t_Tree_Height - t_Crown_Depth);
    
    float crown_area = PI * t_Crown_Radius * t_Crown_Radius;     // floor of crown_area to bound area accumulation
    int crown_intarea = int(crown_area);   // floor of crown_area to bound area accumulation
    crown_intarea = max(crown_intarea,1);                                 // minimum area of crown (1)
    crown_intarea = min(crown_intarea,1963);                              // maximum area of crown (radius 25), int(3.14*25*25), int(3.14*25*25)
    
    float dens_avg = t_LAI/t_Crown_Depth;
    float fraction_filled_target = t_fraction_filled;
    
    for(int h=crown_base;h<=crown_top;h++){
      float fraction_filled_actual = 0.0;
      float dens_layer;
      
      dens_layer = dens_avg;
      if(crown_top == crown_base) dens_layer *= t_Crown_Depth;
      else if(h == crown_top) dens_layer *= fraction_abovetop;
      else if(h == crown_base) dens_layer *= fraction_belowbase;
      
      for(int i = 0; i < crown_intarea; i++){
        if(fraction_filled_actual > fraction_filled_target){
          fraction_filled_actual = (fraction_filled_actual * float(i))/(float(i) + 1.0);
        } else {
          fraction_filled_actual = (fraction_filled_actual * float(i) + 1.0)/(float(i) + 1.0);
          int site_relative = LookUp_Crown_site[i];
          int row = row_crowncenter + site_relative/51 - 25;
          int col = col_crowncenter + site_relative%51 - 25;
          
          if(row >= 0 && row < rows && col >= 0 && col < cols){
            int site=col+cols*row+SBORD;
            LAI3D[h][site] += dens_layer;
          }
        }
      }
    }
  }
}
#endif

#ifdef WATER

//##################################################
// Computation of the tree water availability in the root zone
//####################################################
//! - Tree::Water_availability could be divided into two functions: 
//! -# Water_availability: the equivalent of Tree::Fluxh but for water, ie compute the tree water conditions, and which updates t_phi_root and t_WSF
//! -# another function that could be called UpdateRootDistribution, similar to UpdateLeafDynamics and UpdateTreeBiometry, that would update t_root_depth and t_soil_layer_weight and t_root_biomass, at the end of Tree::Growth (and in Tree::Birth)
//! The current use of one function was chosen for simplicity's sake, but we could shift to a version with 2 functions if we prefer similarity to what has been already done
void Tree::Water_availability() {
  
  if(t_leafarea > 0.0){       // TO BE CHECKED: what happens to a tree with leafarea=0 in the following timesteps ?
    //Tree root biomass
    float total_root_biomass=t_leafarea*(S[t_sp_lab].s_LMA);
    //In the absence of a more explicit allocation scheme, we assume that the total root biomass=the total leaf biomass. This may be discussed and changed. Eg. in Schippers et al. 2015 Functional Plant Biology, they assume that root biomass is 1.4 times leaf biomass... More importantly, the tree-level carbon budget should be closed and balanced. Needs to be specified that this correponds to the total FINE root biomass. Note that similarly, in ED2 (Xu et al. 2016), a constant ratio of 1 is assumed between leaf biomass and fine root biomass (whereas a constant ratio of 0.25 is assumed between coarse root and aboveground stem biomass). But leaf and root phenology follow slightly different dynamics (see Xu et al. 2016 SI) so unclear how this is consistent...?
    
    //Tree root depth
    //t_root_depth=fminf(layer_depth[(nblayers_soil-1)],pow((0.001*total_root_biomass),0.8)*0.2173913);       //this is the tree root depth, computed according to Arora & Boer 2003 Earth interactions (see equ. 11 therein). Instead of assuming a fixed root exponential profile, that typically results in a fixed root depth from recruitment to death, hence overestimating root vertical growth and depth for small saplings, they derived a root distribution profile that depends on root biomass. The simulated root depth and distribution have to be checked. 0.2173913=3/b with b=13.8
    //t_root_depth=fminf(layer_depth[(nblayers_soil-1)],pow((0.001*total_root_biomass),0.1)*0.7);
    //t_root_depth=fmaxf(2.0, fminf(layer_depth[(nblayers_soil-1)],pow((0.001*total_root_biomass),0.1)*0.7));
    //t_root_depth=fminf(layer_depth[(nblayers_soil-1)], 0.06410256*t_Tree_Height+1.435897);  // this dependency of tree root depth with tree height is highly questionable -- cf. eg. Stahl et al. 2013 Oecologia, but see also Brum et al. 2019 Journal of Ecology-- but parcimonious. Note that neither Stahl et al. 2013 nor Brum et al. 2019 reported actual root depth, but the "depth of water uptake". Should probably depends more on the total soil depth, and average soil water availabilty -- cf. distribution of maximal root depth per biomes in Canadell/Jackson et al.
    //t_root_depth=0.22*pow(t_dbh*100, 0.54); // this is the allometry used in ED2, Xu et al. 2016, for evergreen trees. Parameter values were fixed based on Kenzo et al. 2009 Journal of Tropical Biology, which reported on data from excavated trees in wet secondary forests in Malaysia, although Xu et al. 2016 applied it in a seasonally dry tropical forests in Costa Rica. This allometry was then compared against the one obtained from excavated trees in a seasonally dry tropical forests in Costa Rica by Smith-Martin et al. 2019 New Phytologist: although the shape and the differences among the allometries for deciduous and evergreen trees were in overall agreement with observations, the ED2 allometry generally underestimated the observed root depth. As it seems that dry forest species have deeper root than wet forest species (see e.g. Holbrook et al. 1995, cited in Smith-Martin et al. 2019, or Markesteijn & Poorter 2009), this should be better suited to our application to wet forest although this remains to be confronted with data. This allometry gives a root depth of 22cm and 2.65 m for trees with dbh=1cm and 1m respectively. Note that in Xu et al., the first parameter (here 0.22) varied among phenological types, so that root depth of evergreen trees is about twice the ones of deciduous. This link between plant phenology and rooting depth was found empirically in Smith-Martin et al. 2019 New Phytologist, Hasselquist et al. 2010 Oecologia, while Markesteijn & Poorter found a correlation between root depth and stem density on first year seedling in Panama. This link between phenology and rooting depth with higher rooting depth for evergreen species was needed to sustain leaf cover in simulations with ED2, despite their higher leaf and stem drought tolerance (P50 and TLP; Smith-Martin et al. 2019). In absence of data, we here assumed this allometry suits to all species but this has to be discussed !
    t_root_depth=fminf(0.35*pow(t_dbh*100, 0.54), layer_depth[(nblayers_soil-1)]); // this is the allometry used in ED2-hydro, Xu et al. 2016, for evergreen trees, with one parameter, b1=root_depth(dbh=1cm), changed to correct the overall underestimation of tree rootdepth it leads to (cf. Smith-Martin et al. 2019 New phytol.). In absence of data on root depth (in general, and even more at the species level, see email with V. Freycon & B. Ferry january 20th 2020), this should probably be calibrated/fine-tuned.
    float i_root_depth=1.0/t_root_depth;
    
    //Tree water potential in the root zone (or predwan water potential)
    float shallow_bound=1.0;
    float deep_bound=1.0;
    float sumG=0.0;
    t_phi_root=0.0;
    if(isnan(t_phi_root)) {
      Rcout << "nan t_phi_root" << endl;
      Rcout << t_phi_root << endl;
    }
    for(int l=0; l<nblayers_soil; l++) {
      deep_bound=exp(-3.0*i_root_depth*layer_depth[l]);
      //deep_bound=exp(-4.02*i_root_depth*layer_depth[l]); // by consitency with the root depth allometry, we here used the parameter value used by Xu et al. 2016 for evergreen trees. This value (log(beta)) was defined based on Jackson et al. 1996 Oecologia.
      //t_soil_layer_weight[l]=shallow_bound-deep_bound;                        //the weight for each soil layer are the relative tree root biomass in this layer. This is the most classic approach, however probably not fully relevant, inasmuch as trees may equilibrate "preferentially" with the wettest part of the soil, where water is more available and more easily exctractable. Hence another possible weighting integrates the soil-to-root conductance into account (as in de Kauwe et al. 2015; Duursma & Medlyn 2011). See below.
      t_root_biomass[l]=total_root_biomass*(shallow_bound-deep_bound);            // this is the root biomass in layer l, computed from the integration of the root biomass exponential profile in this layer, following Arora & Boer 2003. This also corresponds to the shape of cumulative root fraction used by Jackson et al. 1996 Oecologia, who gathered a large global database of root distributions with this equation. It was thus used in many models, e.g. Duursma & Medlyn 2012; Xu et al. 2016.
      //t_soil_layer_weight[l]=t_root_biomass[l]*10.0/(-log(sqrt(PI*t_root_biomass[l]*10.0)*0.001)); // this soil layer weight integrates the soil-to-root conductance into account (as in de Kauwe et al. 2015; Duursma & Medlyn 2012)
      t_soil_layer_weight[l]=t_root_biomass[l]*10.0/(abs(log(sqrt(PI*t_root_biomass[l]*10.0))*0.001)); // this soil layer weight integrates the soil-to-root conductance into account (as in de Kauwe et al. 2015; Duursma & Medlyn 2012)
      
      t_phi_root+=t_soil_layer_weight[l]*KsPhi[l][site_DCELL[t_site]];   //the water potential in the root zone is computed as the weighted mean of the soil water potential in each soil layer in the DCELL where the tree stands. Note that KsPhi was here not computed as Ks[l][site_DCELL[t_site]]*soil_phi3D[l][site_DCELL[t_site]], to avoid some potential divergence for very low water content, and due to the limit of float type, but directly as the exact power of SWC -- see in UpdateField.
      if(isnan(t_phi_root)) {
        Rcout << "nan t_phi_root: " << endl;
        Rcout << l << "\t" << t_soil_layer_weight[l] << "\t" << KsPhi[l][site_DCELL[t_site]] << endl;
      }
      t_soil_layer_weight[l]*=Ks[l][site_DCELL[t_site]];
      
      //if(t_soil_layer_weight[l]<=0) Rcout << "t_soil_layer_weight[l]=" << t_soil_layer_weight[l] << " t_root_biomass[l]=" << t_root_biomass[l] << " Ks=" << Ks[l][site_DCELL[t_site]] << " Ks*Phi soil =" << KsPhi[l][site_DCELL[t_site]] << " phi soil=" <<   soil_phi3D[l][site_DCELL[t_site]] << endl;
      //t_soil_layer_weight[l]=t_root_biomass[l]*10/(-log(sqrt(PI*t_root_biomass[l]*10)*0.001));
      //t_phi_root+=t_soil_layer_weight[l]*fmaxf(0.0,(KsPhi2[l][site_DCELL[t_site]]-KsPhi[l][site_DCELL[t_site]]*t_s->s_tlp));  //the water potential in the root zone is computed as the weighted mean of the soil water potential in each soil layer
      //t_soil_layer_weight[l]*=fmaxf(0.0,(KsPhi[l][site_DCELL[t_site]]-Ks[l][site_DCELL[t_site]]*t_s->s_tlp));
      sumG+=t_soil_layer_weight[l];
      shallow_bound=deep_bound;
      
      if(t_soil_layer_weight[l]<0.0 || isnan(t_soil_layer_weight[l])) {
        Rcout << "incorrect soil_layer_weight: " << endl;
        Rcout << l << "\t" << t_soil_layer_weight[l] << "\t" << t_root_biomass[l] << "\t" << Ks[l][site_DCELL[t_site]] << "\t" << KsPhi[l][site_DCELL[t_site]] << "\t" << -log(sqrt(PI*t_root_biomass[l]*10)*0.001) << "\t" << deep_bound << "\t" << shallow_bound ;
        Rcout <<endl;
      }
    }
    
    if (sumG>0.0) {
      float isumG=1.0/sumG;
      for (int l=0; l<nblayers_soil; l++) {
        t_soil_layer_weight[l]*=isumG;
      }
      t_phi_root*=isumG;
      if (isnan(t_phi_root)) {
        Rcout << "nan t_phi_root" << endl;
        Rcout << isumG << endl;
      }
    }
    else {
      float iRootB=1.0/total_root_biomass;
      for (int l=0; l<nblayers_soil; l++) {
        t_phi_root+=t_root_biomass[l]*soil_phi3D[l][site_DCELL[t_site]];
        if (isnan(t_phi_root)) {
          Rcout << "nan t_phi_root" << endl;
          Rcout << t_root_biomass[l] << "\t" << soil_phi3D[l][site_DCELL[t_site]] << endl;
        }
        t_soil_layer_weight[l]=t_root_biomass[l]*iRootB;
      }
      t_phi_root*=iRootB;
      if (isnan(t_phi_root)) {
        Rcout << "nan t_phi_root" << endl;
        Rcout << iRootB << endl;
      }
    }
    
    float sum_weight=0.0;
    for (int l=0; l<nblayers_soil; l++) {
      sum_weight+=t_soil_layer_weight[l];
    }
    if (sum_weight>1.5) {
      Rcout << "Weird weights in Water_availability; sum_weight=" << sum_weight << " sumG=" << sumG << " total_root_biomass=" << total_root_biomass << " t_phi_root=" << t_phi_root ;
      if (sumG>0.0) {
        float isumG=1.0/sumG;
        Rcout << " isumG=" << isumG << endl;
      }
      Rcout << "t_soil_layer_weight[l]: ";
      for (int l=0; l<nblayers_soil; l++) {
        Rcout <<t_soil_layer_weight[l] << "\t";
      }
      Rcout << endl;
      Rcout << "soil_phi: ";
      for (int l=0; l<nblayers_soil; l++) {
        Rcout << soil_phi3D[l][site_DCELL[t_site]] << "\t";
      }
      Rcout << endl;
      
    }
    
    t_phi_root-=0.01*t_Tree_Height; // this is to account for the tree height effect on leaf water potential and stress, when the water column and the leaf water potential are not explicitely accounted for. //! !!!: FF, where from? 
    
    if (isnan(t_phi_root)) {
      Rcout << "nan t_phi_root" << endl;
      Rcout << t_Tree_Height << endl;
    }
    
    //! Tree water stress factors
    
    //t_WSF=fminf(1.0, fmaxf(0.0, ((SWC3D[0][site_DCELL[t_site]]-Min_SWC[0])/(Max_SWC[0]-Min_SWC[0])))); // this is the simple linear WSF, with SWC as independent varible, used in lots of model (see Powell et al. 2013 New Phytol, De Kauwe et al. 2015 Biogeosciences, Laio et al. 2001 Advances in Water resources, Egea et al. 2011 AFM....)
    //t_WSF=fminf(1.0, fmaxf(0.0, (t_phi_root-(t_s->s_tlp))*(t_s->s_dWSF))); // this is the linear WSF, with wtare potential as independent variable, used in CLM model (see Powell et al. 2013 New Phytologist, Verhoef & Egea 2011 AFM)
    //t_WSF=exp((log(0.05)/t_s->s_tlp)*t_phi_root); // this is the WSF, to simulate stomatal limitation (ie. hinder g1), drawn from Zhou et al. 2013 AFM, and de Kauwe et al. 2015 Biogeosciences. If this version of WSF is finally adopted, the parameter b=log(0.05)/t_s->s_tlp, should be declare as species variable (instead of s_dWSF).
    t_WSF=exp(S[t_sp_lab].s_b*t_phi_root); // this is the WSF shape used to simulate stomatal limitation (ie. hinder g1), drawn from Zhou et al. 2013 AFM, and de Kauwe et al. 2015 Biogeosciences, but with a different parameterization for the parameter b (using the relationship between phi_gs90 and tlp from Martin-StPaul et al. 2017 Ecology letters, and assuming the WSF=0.9 at phi_gs90).
    //float par=t_s->s_tlp+1;
    //t_WSF_A=(1.0+exp(6.0*par))/(1.0+exp(6.0*(par-t_phi_root))); // this is the WSF, to simulate non-stomatal limitation (ie. hinder Vcmax and Jmax), drawn from Zhou et al. 2013 AFM, and de Kauwe et al. 2015 Biogeosciences
    //t_WSF_A=(1.0+exp(3.0*par))/(1.0+exp(3.0*(par-t_phi_root))); // this is the WSF, to simulate non-stomatal limitation (ie. hinder Vcmax and Jmax), drawn from Zhou et al. 2013 AFM, and de Kauwe et al. 2015 Biogeosciences
    t_WSF_A=1/(1+pow(t_phi_root*S[t_sp_lab].s_itlp, 6)); // this is the WSF used to hinder Vcmax and Jmax in Xu et al. 2016 (equ. S5) (ED2-hydro).
    
    
    if (t_WSF < 0.0 || t_WSF_A < 0.0 || t_WSF>1.0 || t_WSF_A >1.0 ||t_phi_root >0.0 || isnan(t_WSF) || isnan(t_WSF_A) || isnan(t_phi_root)) {
      Rcout << "incorrect value in one of WSF, WSF_A, t_phi_root " << endl;
      Rcout <<t_WSF << "\t" << t_phi_root << "\t" << S[t_sp_lab].s_tlp  << "\t" << t_dbh << "\t" << t_Tree_Height << "\t" << t_age << "\t" << S[t_sp_lab].s_phi_lethal << "\t" << sumG;
      Rcout <<endl;
    }
  }
}


//###################################################################
// Contribution of trees to the stand Transpiration field. Called by UpdateField
//####################################################################
//! - Adds up each tree contribution to the stand Transpiration field, that is the water removed from the soil through all tree transpiration. 
//! - Similar to CalcLAI, that adds up each tree contribution to LAI3D field.
void Tree::Water_uptake() {
  if(t_age > 0.0) {
    int l = 0;
    float depth = 0.0;
    while (t_root_depth > depth && l < nblayers_soil) {
      Transpiration[l][site_DCELL[t_site]] += t_soil_layer_weight[l] * t_transpiration;
      if (t_soil_layer_weight[l] < 0.0 || t_transpiration < 0.0 ) {
        Rcout << setprecision(10);
        Rcout << "Problem with soil_layer_weight and transpiration at site: " << t_site << " layer: " << l << " depth: " << depth << endl;
        Rcout << t_soil_layer_weight[l] << "\t" << t_transpiration << "\n";
      } // debugging, should probably be commented in published code
      depth = layer_depth[l]; // moved by FF so that both indices (depth, l) are updated next to each other
      l++;
    }
  }
}

#endif

//##################################################
// Computation of PPFD right above the tree -- called by Tree::Birth and Tree::Growth 
//####################################################
//! Mean light flux received by the tree crown layer at height h new version (PPFD symmetrical with T and VPD); PPFD, VPD, T and leafarea of a layer are now local variables (not tree variables); v2.3.0, updated in v.2.5
//! - v.2.3.: Tree::Fluxh() computes the average light flux received by a tree crown layer at height h , and also the average VPD and T (modified 1/02/2016) 
//! - modified in v.2.4: weighting of each layer's GPP by the leafarea in the layer, this is also computed here 
//! - modified in v.2.4 and v.2.5: introducing an alternative crown shape, "umbrella"-like, inspired by previous shell models and similar to the crown shapes in the PPA. If activated, crowns contain three layers of vegetation that, once the crown goes beyond 3m in depth, will bend downwards on the edges with a linear slope. Since v.2.5 all loops (CalcLAI, Fluxh, leafarea_max) are executed through the same template. This allows to implement other crown shapes in the future and ensures that modifications are carried through across the code 
#ifdef CROWN_UMBRELLA
void Tree::Fluxh(int h,float &PPFD, float &VPD, float &Tmp, float &leafarea_layer) {
  
  int site_crowncenter = t_site + t_CrownDisplacement;
  int row_crowncenter = site_crowncenter/cols;
  int col_crowncenter = site_crowncenter%cols;
  int shell_fromtop = int(t_Tree_Height)+1 - h;
  
  float canopy_environment_cumulated[4] = {0.0,0.0,0.0,0.0};
  float fraction_filled_target = t_fraction_filled;
  
  LoopLayerUpdateCrownStatistic_template(row_crowncenter, col_crowncenter, t_Tree_Height, t_Crown_Radius, t_Crown_Depth, fraction_filled_target, shell_fromtop, GetRadiusSlope, t_LAI, canopy_environment_cumulated, LAI2dens, GetCanopyEnvironment);
  
  float LA_layer = canopy_environment_cumulated[0];
  float iLA_layer;
  if(LA_layer > 0.0) iLA_layer = 1.0/LA_layer;
  else iLA_layer = 0.0;
  
  leafarea_layer = LA_layer;
  PPFD = canopy_environment_cumulated[1] * iLA_layer;
  VPD = canopy_environment_cumulated[2] * iLA_layer;
  Tmp = canopy_environment_cumulated[3] * iLA_layer;
}

#else
void Tree::Fluxh(int h, float &PPFD, float &VPD, float &Tmp, float &leafarea_layer) {
  
  int site_crowncenter = t_site + t_CrownDisplacement;
  int row_crowncenter = site_crowncenter/cols;
  int col_crowncenter = site_crowncenter%cols;
  
  float crown_area = fmaxf(PI * t_Crown_Radius * t_Crown_Radius,0.0);
  int crown_intarea = int(crown_area);     //floor of crown_area to bound area accumulation
  crown_intarea = max(crown_intarea,1);                                 //minimum area of crown (1)
  crown_intarea = min(crown_intarea,1963);                              //maximum area of crown (radius 25), int(3.14*25*25)
  
  float fraction_filled_target = t_fraction_filled;
  float fraction_filled_actual = 0.0;
  
  int crown_intarea_gaps = 0;
  int crown_intarea_allocated_nogaps = 0;
  
  //loop over LookUp table, until crown_intarea is reached 
  for(int i=0; i < crown_intarea; i++){
    if(fraction_filled_actual > fraction_filled_target){
      fraction_filled_actual = (fraction_filled_actual * float(i))/(float(i) + 1.0);
      crown_intarea_gaps++;
    } else {
      fraction_filled_actual = (fraction_filled_actual * float(i) + 1.0)/(float(i) + 1.0);
      int site_relative = LookUp_Crown_site[i];
      int row = row_crowncenter + site_relative/51 - 25;
      int col = col_crowncenter + site_relative%51 - 25;
      
      if(row >= 0 && row < rows && col >= 0 && col < cols){
        int site=col+cols*row+SBORD;
        float absorb_prev = LAI3D[h][site];
        float absorb_current = LAI3D[h-1][site];
        float absorb_delta = absorb_current - absorb_prev;
        if(absorb_delta < 0.0) absorb_delta = 0.0;    //! eliminate rounding errors 
        int intabsorb = CalcIntabsorb(absorb_prev, absorb_delta);
        PPFD += WDailyMean*LookUp_flux_absorption[intabsorb];
        VPD += VPDDailyMean*LookUp_VPD[intabsorb];
        Tmp += tDailyMean - LookUp_T[intabsorb];
        crown_intarea_allocated_nogaps++;
      }
    }
  }
  
  //weighting has to be done via the actually allocated crown area (integer), not the floating point value 
  float icrown_intarea_allocated = 1.0/float(crown_intarea_allocated_nogaps);
  
  PPFD *= icrown_intarea_allocated;
  VPD  *= icrown_intarea_allocated;
  Tmp  *= icrown_intarea_allocated;
  
  //weighting of each layer 
  float crown_area_nogaps;
  if(fraction_filled_actual > fraction_filled_target) crown_area_nogaps = float(crown_intarea - crown_intarea_gaps);
  else crown_area_nogaps = crown_area - float(crown_intarea_gaps);
  
  int h_index = h - 1;
  int crown_top = int(t_Tree_Height);
  int crown_base = int(t_Tree_Height - t_Crown_Depth);
  float fraction_abovetop = t_Tree_Height - float(crown_top);
  float fraction_belowbase = float(crown_base + 1) - (t_Tree_Height - t_Crown_Depth);
  float dens_avg = t_LAI/t_Crown_Depth;
  
  float dens_layer = dens_avg;
  if(crown_top == crown_base) dens_layer *= t_Crown_Depth;
  else if(h_index == crown_top) dens_layer *= fraction_abovetop;
  else if(h_index == crown_base) dens_layer *= fraction_belowbase;
  
  //given constant density within layer, we just need to multiply by area 
  leafarea_layer = dens_layer * crown_area_nogaps;
}
#endif

//#############################
// Empirical functions (traits)
//#############################
float Tree::CalcVcmaxm(){
  float SLA=10000.0/t_LMA;
  float Vcmaxm = pow(10.0, fminf((-1.56+0.43*log10(t_Nmass*1000.0)+0.37*log10(SLA)), (-0.80+0.45*log10(t_Pmass*1000.0)+0.25*log10(SLA)))); // this is equation 2 in Domingues et al 2010 PCE (coefficients from fig7) which made better fits than equation 1 (without LMA)
  return(Vcmaxm);
}

float Tree::CalcJmaxm(){
  float SLA=10000.0/t_LMA;
  float Jmaxm = pow(10.0, fminf((-1.50+0.41*log10(t_Nmass*1000.0)+0.45*log10(SLA)), (-0.74+0.44*log10(t_Pmass*1000.0)+0.32*log10(SLA)))); // added as a Species member variable 14-04-2015; this is equ 2 in Domingues et al 2010 PCE (coefficients from fig7)
  return(Jmaxm);
}

float Tree::CalcRdark(){
  float Parea = t_Pmass * t_LMA;
  float Narea = t_Nmass * t_LMA;
  float Rdark = (1.3893 + (0.0728 * Narea) + (0.0015 * Parea) + (0.0095 * t_Vcmax) - (0.0358 * 26.2)); //since v.2.5: correction of Atkin et al. 2015 New phytologist formula. The original formula was based on mean-centered values (cf. Atkin et al. 2015, and the clarification/correction published afterwards), unfortunately only one formula (PFT-specific formula) with absolute values is provided and this is the one used here, cf. corrigendum Table6 a): https://nph.onlinelibrary.wiley.com/doi/10.1111/nph.14055 
  // t_Rdark=t_LMA*(8.5341-130.6*t_Nmass-567.0*t_Pmass-0.0137*t_LMA+11.1*t_Vcmaxm+187600.0*t_Nmass*t_Pmass)*0.001; //t_Rdark corresponds to leaf maintenance respiration. From Table 6 in Atkin et al 2015 New phytologist v.2.0
  return(Rdark);
}

//#######################
// Death rate calculation
//#######################
float Tree::DeathRateNDD(float dbh, int nppneg, float ndd) {
  float dr=0;
  float basal=m*(1-t_wsg);
  float dd=deltaD*ndd*(1-2*dbh/t_dbhmax);
  
  dr=basal;
  if(nppneg > t_leaflifespan) dr+=1.0/timestep;
  if(dd > 0) dr+=dd;
  return dr*timestep;
}

#ifdef WATER
// !!!: change by FF, removed PPFD from argument list, as never used
float Tree::DeathRate(float dbh, int nppneg, float phi_root) {
  float dr=0;
  float basal=fmaxf(m-m1*t_wsg,0.0);
  
  dr=basal;
  if (nppneg > t_leaflifespan) dr+=1.0/timestep;
  // if the water availabiliy in the root zone is below the lethal level, the tree dies, !!!: not that deterministic, right?
  if (phi_root < (S[t_sp_lab].s_phi_lethal)) dr+=1.0/timestep;
  if (iter == int(nbiter-1)) output[26]<< t_wsg << "\t" << basal << "\t"  << dbh << "\t"  << dr   <<  "\n";
  return dr*timestep;
}
#else
float Tree::DeathRate(float dbh, int nppneg) {
  float dr=0.0;
  float basal=fmaxf(m-m1*t_wsg,0.0);
  
  dr=basal;
  if (nppneg > t_leaflifespan) dr+=1.0/timestep;
  if (iter == int(nbiter-1) && _OUTPUT_reduced == 0) output[26]<< t_wsg << "\t" << basal << "\t"  << dbh << "\t"  << dr   <<  "\n";
  return dr*timestep;
}
#endif


//###############################################################
// Farquhar von Caemmerer Berry model -- called by  Tree::GPPleaf
//###############################################################

#ifdef WATER
//! - Function Tree::GPPleaf when the WATER option is off, Tree::FluxesLeaf when WATER option is on
//! - Includes Medlyn et al. 2011 model of stomatal conductance: formula for s_fci (ci/ca) -- see also Prentice et al 2014 Ecology Letters and Lin et al 2015 Nature Climate Change; min added in order to prevent ci:ca bigger than 1 (even though Ehleringer et al 1986 reported some values above 1 (Fig3) 
//! - Returns the primary productivity (assimilation) per unit leaf area, i.e. in micromoles C/m^2/s. 
//! - It is converted into gC per m^2 of leaf per timestep by "nbhours_covered*15.7788*timestep" where 15.7788 = 3600*365.25*12/1000000 and nbhours_covered is the duration read in the daily variation file 
//! - NB1: 12 is the molar mass of carbon NB2: timestep is given as fraction of a year, so what is computed is actually the full assimilation per year which, in turn, is multiplied by the fraction per year that is under consideration. 
//! - BEWARE: slight inconsistency through use of 365.25 when daily timestep is likely to be given as 365, but not incorrect. Commented version below was in use prior to version 2.3.0 
//! lookup tables for acceleration of T dependence. cf. Bernacchi et al 2003 PCE; von Caemmerer 2000 
leafFluxes Tree::FluxesLeaf(float PPFD, float VPD, float T) {
  
  // v.2.3.0: theta defined as a global variable 
  //theta=0.7;   // this is the fixed value of theta used by von Caemmerer 2000 
  //float theta=0.76+0.018*T-0.00037*T*T;         // theta, but temperature dependent cf. Bernacchi et al 2003 PCE
  
  //Parameters for the Farquhar model, with temperature dependencies 
  int convT= int(iTaccuracy*T); //temperature data at a resolution of Taccuracy=0.1°C -- stored in lookup tables ranging from 0°C to 50°C ---
  float KmT    =LookUp_KmT[convT];
  float GammaT =LookUp_GammaT[convT];
  
  //in WATER: stomatal conductance parameter (g1) and photosynthetic capacities are now reduced under water stressed conditions ==> stomatal and non-stomatal responses to water stress (see Egea et al. 2011 AFM, Zhou et al. 2013 AFM, Zhou et al. 2014 PCE, etc.). This is done by multipliying the values of g1 and of Vcmax and Jmax in absence of water stress by WSF and WSF_A respectively.
  
  //float g1= t_s->s_g1*t_WSF;
  //float g1= (-3.97 * t_wsg + 6.53)*t_WSF; this is the relationship provided by Lin et al. 2015
  float g1 = (-0.0224 * t_LMA + 4.8278)*t_WSF; // this is the relationship provided by Wu et al. 2019 GCB
  float fci=fmaxf(GammaT,g1/(g1+sqrt(VPD)));
  // TO BE CHECKED !!!! : if t_fci <GammaT, it leads to negative assimilation and transpiration... is it realistic to have such low ci/ca? Looking at values in Prentice et al. 2014 Ecology letters, it seems not... but looking at g1 values in Zhou et al. 2013 AFM, it seems ok to have very low g1 value under strong water stress... so??
  
  float VcmaxT =t_Vcmax*LookUp_VcmaxT[convT]*t_WSF_A;
  float JmaxT  =t_Jmax*LookUp_JmaxT[convT]*t_WSF_A;
  
  //Rcout << "g1=" << g1 << " T=" << T << " VcamxT=" << VcmaxT << " t_WSF_A=" << t_WSF_A << endl;
  
  //Farquhar - -von Caemmerer - Berry model of carbon assimilation rate 
  float I = alpha * PPFD;
  float J = (I+JmaxT-sqrt((JmaxT+I)*(JmaxT+I)-4.0*theta*JmaxT*I))*0.5/theta;
  //float J = fmaxf(0.0,(I+JmaxT-sqrt((JmaxT+I)*(JmaxT+I)-4.0*theta*JmaxT*I))*0.5/theta);
  float A = fminf(VcmaxT/(fci+KmT),0.25*J/(fci+2.0*GammaT))*(fci-GammaT);
  
  //Rcout << "I= " << I << " J=" <<J << " A=" << A << endl;
  
  //Medlyn et al. 2011 model of stomatal conductance 
  
  float lT= (1+ g1/sqrt(VPD))*A*VPD; // this is the Medlyn et al. 2011 's model of stomatal conductance, assuming negligible boundary layer resistance. will be multiplied by 1.6*iCair later.
  
  // float test=(1+ g1/sqrt(VPD))*A*VPD*1.6*iCair;
  leafFluxes F;
  F.carbon_flux = A;
  F.water_flux= lT;
  //Rcout << "carbon_flux=" << F.carbon_flux << " water_flux=" << F.water_flux << endl;
  
  return F;
}

//! - Function Tree::dailyGPPleaf when the WATER option is off, Tree::dailyFluxesLeaf when WATER option is on
//! - Includes Medlyn et al. 2011 model of stomatal conductance: formula for s_fci (ci/ca) -- see also Prentice et al 2014 Ecology Letters and Lin et al 2015 Nature Climate Change; min added in order to prevent ci:ca bigger than 1 (even though Ehleringer et al 1986 reported some values above 1 (Fig3) 
//! - Returns the *daily* primary productivity (assimilation; computed from Tree::GPPleaf)averaged across the daily fluctuations in climatic conditions, per unit leaf area, in micromoles C/m^2/s. 
leafFluxes Tree::dailyFluxesLeaf(float PPFD, float VPD, float T) {
  float dailyA=0.0;
  float dailylT=0.0;
  
  for(int i=0; i < nbsteps_varday; i++) {
    //Rcout << "i=" << i << endl;
    float ppfd_vardaytimestep = PPFD * varday_light[i];
    //Rcout << "i=" << i << " daily_light[i]" << daily_light[i] << " ppfd_haldhour=" <<ppfd_vardaytimestep << endl;
    float vpd_vardaytimestep = VPD * varday_vpd[i];
    //Rcout << "i=" << i << " daily_light[i]=" << daily_light[i] << " ppfd_haldhour=" <<ppfd_vardaytimestep <<  " daily_vpd[i]=" << daily_vpd[i] << " vpd_haldhour=" <<vpd_vardaytimestep <<endl;
    float t_vardaytimestep = T * varday_T[i];
    //Rcout << "i=" << i << " daily_light[i]=" << daily_light[i] << " ppfd_haldhour=" <<ppfd_vardaytimestep <<  " daily_vpd[i]=" << daily_vpd[i] << " vpd_haldhour=" <<vpd_vardaytimestep << " daily_T[i]=" << daily_T[i] << " t_haldhour=" <<t_vardaytimestep <<endl;
    leafFluxes fluxes_vardaytimestep = FluxesLeaf(ppfd_vardaytimestep,vpd_vardaytimestep,t_vardaytimestep);
    
#ifdef FF_todiscuss
    // !!!: FF: quick fix to avoid dealing with negative fluxes, but this needs to be discussed (cf. also IM's discussion in FluxesLeaf regarding Gamma) 
    if(fluxes_vardaytimestep.carbon_flux > 0.0 && fluxes_vardaytimestep.water_flux > 0.0){
      dailyA += fluxes_vardaytimestep.carbon_flux;
      dailylT += fluxes_vardaytimestep.water_flux;
    }
#else
    dailyA += fluxes_vardaytimestep.carbon_flux;
    dailylT += fluxes_vardaytimestep.water_flux;
#endif
    //Rcout << dailyA << endl;
    // deprecated in v.2.4.1: compute GPP only if enough light is available threshold is arbitrary, but set to be low: in full sunlight ppfd is aroung 700 W/m2, and even at dawn, it is ca 3% of the max value, or 20 W/m2. The minimum threshold is set to 0.1 W/m2
    // Future update: compute slightly more efficiently, using 3-hourly values? This will have to be aligned with climate forcing layers (e.g. NCAR)
  }
  dailyA *= inv_nbsteps_varday;
  dailylT *= inv_nbsteps_varday;
  
  leafFluxes dailyF;
  dailyF.carbon_flux =dailyA;
  dailyF.water_flux = dailylT;
  //Rcout << dailyA << endl;
  //Rcout << dailyF.carbon_flux << endl ;
  
  return dailyF;
}

#else

//! - Function Tree::GPPleaf when the WATER option is off, Tree::FluxesLeaf when WATER option is on
//! - Includes Medlyn et al. 2011 model of stomatal conductance: formula for s_fci (ci/ca) -- see also Prentice et al 2014 Ecology Letters and Lin et al 2015 Nature Climate Change; min added in order to prevent ci:ca bigger than 1 (even though Ehleringer et al 1986 reported some values above 1 (Fig3) 
//! - Returns the primary productivity (assimilation) per unit leaf area, i.e. in micromoles C/m^2/s. 
//! - It is converted into gC per m^2 of leaf per timestep by "nbhours_covered*15.7788*timestep" where 15.7788 = 3600*365.25*12/1000000 and nbhours_covered is the duration read in the daily variation file 
//! - NB1: 12 is the molar mass of carbon NB2: timestep is given as fraction of a year, so what is computed is actually the full assimilation per year which, in turn, is multiplied by the fraction per year that is under consideration. 
//! - BEWARE: slight inconsistency through use of 365.25 when daily timestep is likely to be given as 365, but not incorrect. Commented version below was in use prior to version 2.3.0 
//! lookup tables for acceleration of T dependence. cf. Bernacchi et al 2003 PCE; von Caemmerer 2000 
float Tree::GPPleaf(float PPFD, float VPD, float T) {
  //v.2.3.0: theta defined as a global variable 
  //theta=0.7;   // this is the fixed value of theta used by von Caemmerer 2000
  
  //float theta=0.76+0.018*T-0.00037*T*T;         // theta, but temperature dependent cf. Bernacchi et al 2003 PCE
  
  //Parameters for Farquhar model, with temperature dependencies 
  int convT= int(iTaccuracy*T); //temperature data at a resolution of Taccuracy=0.1°C -- stored in lookup tables ranging from 0°C to 50°C ---
  
  //if(convT>500 || isnan(convT) || convT <0) Rcout << t_site << " | convT: " << convT << " | T: " << T << " | PPFD: " << PPFD << " | VPD: " << VPD << endl;
  float KmT    =LookUp_KmT[convT];
  float GammaT =LookUp_GammaT[convT];
  
  // float g1 = -3.97 * t_wsg + 6.53 (Lin et al. 2015)
  
  float t_fci =g1/(g1+sqrt(VPD));
  float VcmaxT =t_Vcmax*LookUp_VcmaxT[convT];
  float JmaxT  =t_Jmax*LookUp_JmaxT[convT];
  
  //Farquhar - -von Caemmerer - Berry model of carbon assimilation rate 
  float I = alpha * PPFD;
  float J = (I+JmaxT-sqrt((JmaxT+I)*(JmaxT+I)-4.0*theta*JmaxT*I))*0.5/theta;
  float A = fminf(VcmaxT/(t_fci+KmT),0.25*J/(t_fci+2.0*GammaT))*(t_fci-GammaT);
  
  return A;
}


//! - Function Tree::dailyGPPleaf when the WATER option is off, Tree::dailyFluxesLeaf when WATER option is on
//! - Includes Medlyn et al. 2011 model of stomatal conductance: formula for s_fci (ci/ca) -- see also Prentice et al 2014 Ecology Letters and Lin et al 2015 Nature Climate Change; min added in order to prevent ci:ca bigger than 1 (even though Ehleringer et al 1986 reported some values above 1 (Fig3) 
//! - Returns the *daily* primary productivity (assimilation; computed from Tree::GPPleaf)averaged across the daily fluctuations in climatic conditions, per unit leaf area, in micromoles C/m^2/s. 
float Tree::dailyGPPleaf(float PPFD, float VPD, float T) {
  float dailyA=0.0;
  
  for(int i=0; i < nbsteps_varday; i++) {
    //Rcout << t_site << " i: " << i << " tempRday: " << tempRday << endl;
    float ppfd_vardaytimestep = PPFD * varday_light[i];
    float vpd_vardaytimestep = VPD * varday_vpd[i];
    float t_vardaytimestep = T * varday_T[i];
    if(ppfd_vardaytimestep > 0.1) dailyA += Tree::GPPleaf(ppfd_vardaytimestep,vpd_vardaytimestep,t_vardaytimestep);
    //else { Rcout << endl << t_site << " species: " << t_s->s_name << " t_age: " << t_age << " PPFD: " << ppfd_vardaytimestep << " vpd_vardaytimestep " << vpd_vardaytimestep << " t_vardaytimestep: " << t_vardaytimestep << " GPPleaf: " << Tree::GPPleaf(ppfd_vardaytimestep,vpd_vardaytimestep,t_vardaytimestep) << endl;}
    // deprecated in v.2.4.1: compute GPP only if enough light is available threshold is arbitrary, but set to be low: in full sunlight ppfd is aroung 700 W/m2, and even at dawn, it is ca 3% of the max value, or 20 W/m2. The minimum threshold is set to 0.1 W/m2
    // Future update: compute slightly more efficiently, using 3-hourly values? This will have to be aligned with climate forcing layers (e.g. NCAR)
    
    //the 6 lines in comment below corresponds to a finer version in which the multiplier is computed and used every 48 half hour, ie. with the corresponding environment instead of assuming a constant multiplier correponding the one at maximum incoming irradiance
    //float hhA=0;
    //hhA=GPPleaf(PPFD*vardaytime_light[i], VPD*vardaytime_vpd[i], T*vardaytime_T[i]);
    //float alpha=phi*PPFD*vardaytime_light[i]/hhA;
    //float D=klight*dens*CD;
    //hhA*=alpha/(D*(alpha-1))*log(alpha/(1+(alpha-1)*exp(-D)));
    //dailyA+=hhA;
  }
  dailyA *= inv_nbsteps_varday;
  return dailyA;
}

//! - Function Tree::dailyGPPcrown similar to Tree::dailyGPPleaf
//! Faster, whole crown GPP calculation 
float Tree::dailyGPPcrown(float PPFD, float VPD, float T, float LAI) {
  float ppfde,dailyA=0.0;
  
  for(int i=0; i < nbsteps_varday; i++) {
    ppfde=PPFD*varday_light[i];
    if(ppfde > 0.1)
      // new v.2.3.0: compute GPP only if enough light is available threshold is arbitrary, but set to be low: in full sunlight ppfd is aroung 700 W/m2, and even at dawn, it is ca 3% of the max value, or 20 W/m2. The minimum threshold is set to 0.1 W/m2
      // Future update: compute slightly more efficiently, using 3-hourly values? This will have to be aligned with climate forcing layers (e.g. NCAR)
      dailyA+=Tree::GPPleaf(ppfde, VPD*varday_vpd[i], T*varday_T[i]);
    //the 6 lines in comment below corresponds to a finer version in which the multiplier is computed and used every 48 half hour, ie. with the corresponding environment instead of assuming a constant multiplier correponding the one at maximum incoming irradiance
    //float hhA=0;
    //hhA=GPPleaf(PPFD*vardaytime_light[i], VPD*vardaytime_vpd[i], T*vardaytime_T[i]);
    //float alpha=phi*PPFD*vardaytime_light[i]/hhA;
    //float D=klight*LAI;
    //hhA*=alpha/(D*(alpha-1))*log(alpha/(1+(alpha-1)*exp(-D)));
    //dailyA+=hhA;
  }
  //vardaytime_light is the averaged (across one year, meteo station Nouragues DZ) and normalized (from 0 to 1) daily fluctuation of light, with half-hour time step, during the day time (from 7am to 7pm, ie 12 hours in total), same for vardaytime_vpd and vardaytime_T. Taking into account these daily variation is necessary considering the non-linearity of FvCB model
  
  float alpha=phi*PPFD/GPPleaf(PPFD, VPD, T);             //alpha is a non-dimensional figure used to compute the multiplier below
  float D=klight*LAI;                                 //D is a non-dimensional figure used to compute the multiplier below, update in v.2.5: replaced dens * CD by LAI
  dailyA*=alpha/(D*(alpha-1))*log(alpha/(1+(alpha-1)*exp(-D)));  // the FvCB assimilation rate computed at the top of the tree crown is multiplied by a multiplier<1, to account for the lower rate at lower light level within the crown depth. This multiplier is computed assuming that change in photosynthetic assimilation rate within a tree crown is mainly due to light decrease due to self-shading following a Michealis-menten relationship (ie. we assume that 1/ the change is not due to changes in VPD or temperature, which are supposed homogeneous at the intra-crown scale, and 2/ that other tree contributions to light decrease is neglected).
  
  dailyA *= inv_nbsteps_varday;
  return dailyA;
}

#endif

//! NEW in v. 2.4.0: Separate function for calculation of Rday 
//! separation necessitates extra function calls, but allows for decoupling of photosynthesis and leaf respiration in special cases (i.e. no photosynthesis at very low ppfd, but still respiration
//! at low light levels, should leaf respiration not be closer to dark respiration than to respiration in full sunlight, i.e. should we not lose the factor 0.4 then?  

#ifdef WATER
// Function Rdayleaf has not been modified when WATER is defined: there is no clear consensus on the effect of water shortage on respiration and how to implement such potential effect in models, but I have to further check this and it could be modified. IM 4 dec 2019.
#endif

float Tree::Rdayleaf(float T) {
  int convT= int(iTaccuracy*T);
  //if(T < 0 || isnan(T) || T > 50) Rcout << t_site << " species: " << t_s->s_name << " convT: " << T << endl;
  float Rday_leaf = t_Rdark*LookUp_Rday[convT];
  return Rday_leaf;
}

float Tree::dailyRdayleaf(float T) {
  float Rdayleaf_daily = 0.0;
  for(int i=0; i < nbsteps_varday; i++) Rdayleaf_daily += Tree::Rdayleaf(T*varday_T[i]);
  Rdayleaf_daily *= 0.0417;
  return Rdayleaf_daily;
}

//Calculation of above ground biomass (in kg)
// !!!: if updated, also update CalcIncrementDBH, cf. below 
float Tree::CalcAGB(){
  // allometric equations from Chave et al. 2014 Global Change Biology to compute above ground biomass (conversion from dbh^2 in cm2 to m2, ie. factor e4)
  //float agb = 0.0673*pow(t_wsg*t_Tree_Height*LV*t_dbh*t_dbh*LH*LH*10000.0, 0.976);
  float agb = 0.0559*t_wsg*t_Tree_Height*LV*t_dbh*t_dbh*LH*LH*10000.0; // simplified assumption of cylinder, 0.0559 accounts for stem taper cf. Chave et al. 2014)
  return(agb);
}

//Calculation of the increment of dbh from assimilated carbon/biomass (in m) 
float Tree::CalcIncrementDBH(float delta_agb){
  float ddbh = fmaxf((delta_agb/(0.559*t_wsg* 1.0e6*t_dbh*LH*t_Tree_Height*LV*(3.0-t_dbh/(t_dbh+t_ah)))),0.0)* NH;
  return(ddbh);
}

//Calculation of the maximum amount of carbon stored in a tree 
float Tree::CalcCarbonStorageMax(){
  // we assume non-structural carbohydrates (NSC) is around 10% of the whole tree's carbon. We assume that only half of NSC (i.e. 5% of whole tree carbon) can be remobilized, since NSC has important metabolic functions or can be stored in tissues that are no longer accessible (heartwood). So NSC for remobilization should never exceed 0.05 * above ground biomass (in units of carbon). Cf. Martínez-Vilalta 2016, Ecological Monographs 
  // to calculate the 5% value, we use CalcAGB(), and reconvert kg into gram (1000.0), then convert biomass into carbon by multiplying with 0.5, and take 5% 
  float carbon_storage_max = 1000.0 * CalcAGB() * 0.5 * 0.05;
  return(carbon_storage_max);
}

//Calculation of the treefall threshold, if _BASICTREEFALL is activated 
float Tree::CalcCt(){
  float hrealmax = t_intraspecific_multiplier_height * 1.5 * t_hmax*t_dbhmax/(1.5 * t_dbhmax+t_ah);   // realized maximum height
  
  //float Ct = hrealmax*flor(1.0 - vC*sqrt(-log(gsl_rng_uniform_pos(gslrand))));
  float vC_intraspecific = vC/1.5 - 1.0/(2.3 * t_intraspecific_multiplier_height) + 1.0/2.3;  //! since v.2.5: adjusting vC to intraspecific height variation. If vC was not modified, tall trees would start falling at much larger heights than smaller trees of the same species and with the same dbh, despite a much worse height/dbh ratio. The default assumption is now that the minimum onset of treefalls should be around the same height threshold irrespective of the height multiplier, but stronger assumptions would be justified too (i.e. tall trees falling more easily). The formula is derived as follows: assuming that the onset of treefall can be described by the 99.5 percentile of the sqrt(-log(uniform)) distribution, which is 2.3, we calculate the corresponding Ct_min and impose the condition that it stays equal irrespective of t_intraspecific_multiplier_height. In this case, we can solve for vC_intraspecific. This is a conservative assumption, likely a stronger dependence on dbh/height ratio would be found, but probably superseded by E-Ping's module anyways 
  float Ct = hrealmax*fmaxf(1.0  - vC_intraspecific * sqrt(-log(gsl_rng_uniform_pos(gslrand))),0.0);
  return(Ct);
}

//Determines leaf life span, either from empirical function or from Kikuzawa model 
void Tree::UpdateLeafLifespan(){
  if(_LL_parameterization == 0){ // prescribed relationship for LL
    t_leaflifespan=12.755*exp(0.007*t_LMA-0.565*t_Nmass);           // expression developed by Sylvain Schmitt, avoids problem of very low leaflifespans at low LMA
    t_leaflifespan = fmaxf(t_leaflifespan,3.0);
    //t_leaflifespan = pow(10,(2.040816*(2.579713-log10(SLA))));    //this is the expression from Reich et al. 1997 PNAS (provides probably more realistic estimates for species with high LMA).
    //t_leaflifespan=1.5+pow(10,(7.18+3.03*log10(t_LMA*0.0001)));   //this is the expression from Reich et al 1991 Oecologia (San Carlos Rio Negro).
    //t_leaflifespan=0.5+pow(10,(-2.509+1.71*log10(t_LMA)));        //this is the expression from Wright et al 2004 Nature (leaf economics spectrum).
  } else { // relationship based on optimal theory for LL
    t_leaflifespan = predLeafLifespanKikuzawa();
  }
  
  t_leaflifespan*=0.08333333*iterperyear;             // Converts leaflifespan from month unit to timestep units, this is needed for UpdateLeafDynamics and nppneg (0.08333333=1/12)
  
  float time_young=fminf(t_leaflifespan/3.0,1.0);
  float time_mature=t_leaflifespan/3.0;
  float time_old=t_leaflifespan-time_mature-time_young;
  
  t_lambda_young = 1.0/time_young;
  t_lambda_mature = 1.0/time_mature;
  t_lambda_old = 1.0/time_old;
  
  t_youngLA = time_young/t_leaflifespan * t_leafarea;
  t_matureLA  = time_mature/t_leaflifespan * t_leafarea;
  t_oldLA = time_old/t_leaflifespan * t_leafarea;
  t_litter = 0.0;
}

//Determine sapwood area, limited by increase in dbh (ddbh) (in m2) 
//! - Options: fixed percentage (option _sapwood == 0) or based on tree's leaf area (option _sapwood > 0). In the case _sapwood>0, a tree cannot retroactively convert heartwood into sapwood, just because it could allocate more leaves
//! - TODO: should there be another limit on leaf area? (i.e. if sapwood cannot grow more than ddbh increment, then leaf area should probably not be allowed to grow beyond what is reasonable through the current sapwood area) 
void Tree::UpdateSapwoodArea(float ddbh){
  if(_sapwood > 0){
    float sapwood_area_new = PI * 0.5 * ddbh * (t_dbh - 0.5 * ddbh); // correction from previous equation in v.3.0, derived from (0.5 * t_dbh) * (0.5 * t_dbh) * PI - (0.5 * (t_dbh - ddbh)) * (0.5 * (t_dbh - ddbh)) * PI, this presupposes that t_dbh has already been updated (i.e. t_dbh += ddbh). If dbh has grown entirely from zero, then the equation reduces to PI * 0.5 * dbh * 0.5 * dbh, i.e. the whole stem area
    t_sapwood_area += sapwood_area_new;
    t_sapwood_area = fminf(t_sapwood_area, 0.0001 * 2.0 * t_leafarea / (0.066 + 0.017 * t_Tree_Height - 0.18 + 1.6 * t_wsg)); // upper bound on sapwood area, either through previously existing sapwood area (sapwood cannot grow quicker than the rest of the tree) or through the amount of sapwood needed from from Fyllas et al. 2014, based on inversion of pipe model, multiplication with 0.0001 to convert cm2 to m2 
    float sapwood_minimum;
    if(t_dbh < 0.01) sapwood_minimum = t_dbh * t_dbh * 0.25 * PI;
    else sapwood_minimum = 0.005 * (t_dbh - 0.005) * PI;
    t_sapwood_area = fmaxf(t_sapwood_area, sapwood_minimum);
  } else {
    float sapthick;
    if (t_dbh < 0.08) sapthick=0.5*t_dbh;
    else sapthick = 0.04;
    t_sapwood_area = PI*sapthick*(t_dbh-sapthick);
  }
}

// Updates t_Tree_Height, based on t_dbh 
void Tree::UpdateHeight(){
  t_Tree_Height = fminf(t_intraspecific_multiplier_height * t_hmax * t_dbh/(t_dbh + t_ah),HEIGHT-1);
}

// Updates t_Crown_Radius, based on t_dbh
void Tree::UpdateCR(){
  if(!_CROWN_MM) t_Crown_Radius = t_intraspecific_multiplier_CR * exp(CR_a + CR_b*log(t_dbh));             // Power law, the default 
  else t_Crown_Radius = t_intraspecific_multiplier_CR * CR_b * t_dbh/(t_dbh + CR_a);                       // Michaelis Menten type allometry !!!: requires CR_b to be the CR_max parameter and CR_a the initial increase 
  t_Crown_Radius = fmaxf(CR_min, t_Crown_Radius);
  //for reference, two crown allometries that are reasonable in French Guiana
  //t_Crown_Radius = t_intraspecific_multiplier_CR * exp(1.9472 + 0.5925*log(t_dbh)); //! crown allometry deduced from Piste Saint-Elie 
  //t_Crown_Radius = t_intraspecific_multiplier_CR * exp(1.8814 + 0.5869*log(t_dbh)); //! this is crown allometry derived from data set compiled by Jucker et al. 2016 (Global Change Biology) 
}

// Updates t_Crown_Depth based on t_Tree_Height
void Tree::UpdateCD(){
  //Since v.2.5, simplification of the computation of the crown depth, in accordance with the Canopy Constructor algorithm 
  t_Crown_Depth = (CD_a + CD_b * t_Tree_Height);
  t_Crown_Depth *= t_intraspecific_multiplier_CD;
  t_Crown_Depth = fminf(t_Crown_Depth, 0.5*t_Tree_Height);
}

#ifdef CROWN_UMBRELLA
// Updates t_Crown_Slope based on t_Crown_Radius and t_Tree_Height
void Tree::UpdateCrownSlope(){
  if(t_Crown_Depth > 3.0){
    float crownshell_base = t_Tree_Height - t_Crown_Depth + 1.5;
    float crown_extent = t_Tree_Height - crownshell_base;
    t_Crown_Slope = t_Crown_Radius * (1.0 - shape_crown) / (crown_extent - 0.5);
  } else {
    t_Crown_Slope = 0.0;
  }
}
#endif


// Determines the maximum LAI that the tree should reach, given the Farquhar model, at a theoretical average day, up to which point leaves can be allocated and until which the costs (self-shading) are lower than the benefits (additional assimilation) 
//! - LAImax (maximum LAI) is assumed to lie between 0 and 10, and is such that adding more leaves results in net loss of carbon
//! - Range of LAImax is narrowed down by bisection (sequentially halving the possible range)
//! - Uses actually absorbed PPFD instead of incident PPFD, since leaves are not perfectly illuminated, but often in the lower canopy layers 
void Tree::treeLAImax(){
  float LAI_lowerbound = 0.0;
  float LAI_upperbound = 10.0;
  float LAImax_temp = 0.5 * (LAI_lowerbound + LAI_upperbound);
  
  //tree LAImax to not be greater 10. 10 is also the number of iterations to get the best value. Since we narrow down in half steps the precision should be ca. 0.5^10 ~ 0.001, which should be high enough for LAImax 
  for(int i=0;i<10;i++){
    float absorb_prev = LAImax_temp;
    float absorb_delta = 0.5;                                           //calculate everything, assuming a medium leaf density in the lower canopy, TODO: if the crown has a non-cylindric shape, this should probably be taken as a third of LAImax, or if LAI_gradient is activated, as 25% of LAImax 
    int intabsorb = CalcIntabsorb(absorb_prev, absorb_delta);
    
    //get PPFD, VPD, and temperature at each discretisation step 
    float PPFD_LAI = WDailyMean_year * LookUp_flux_absorption[intabsorb];
    float VPD_LAI = VPDDailyMean_year * LookUp_VPD[intabsorb];
    float Tmp_LAI = tDailyMean_year - LookUp_T[intabsorb];
    
    //calculate the GPP 
#ifdef WATER
    float GPP_LAI = Tree::dailyFluxesLeaf(PPFD_LAI, VPD_LAI, Tmp_LAI).carbon_flux;
#else
    float GPP_LAI = Tree::dailyGPPleaf(PPFD_LAI, VPD_LAI, Tmp_LAI);
#endif
    float Rday_LAI = Tree::dailyRdayleaf(Tmp_LAI);
    
    float effLA = 0.66 * nbhours_covered * 15.7788 * timestep; //convert  from micromoles C/m^2/s into gC per m^2 of leaf per timestep by "nbhours_covered*15.7788*timestep" where 15.7788 = 3600*365.25*12/1000000 (seconds, days, and mass of carbon) and nbhours_covered is the amount of time that is covered by the daily variation file. We also assume that one third of the leaves are mature and that the rest of the leaves have half the respiration/assimilation rates, so we derive a factor 0.66 
    float effLA_night = 0.66 * (24.0 - nbhours_covered) * 15.7788 * timestep;  //same as during the day, but inverse of hours covered (we assume that non-covered hours are night values)
    
    GPP_LAI *= effLA;
    Rday_LAI *= effLA * 0.4; //inhibition of respiration by ca. 40%, cf. Atkin et al. 2000
    //get the night respiration 
    int convTnight = int(iTaccuracy*Tnight_year);
    
    float Rnight_LAI = t_Rdark * effLA_night * LookUp_Rnight[convTnight];
    
    //add up the two components of leaf respiration, and multiply the result by 1.5 (fine root respiration), since in TROLL, this cannot be separated from leaf respiration 
    
    //calculate effective npp 
    float Rleaf_LAI = 1.5 * (Rday_LAI + Rnight_LAI);    //Rleaf=Rday+Rnight is multiplied by 1.5 to also account for fine root respiration (cf as in Fyllas et al 2014 and Malhi 2012); Rstem is multiplied by 1.5 to account for coarse root respiration (according to the shoot root biomass ratio of 0.2 - Jérôme's paper in prep- and also to branch respiration (Meir & Grace 2002, Cavaleri 2006, Asao 2015).
    float NPP_leaf = 0.7 * (GPP_LAI - Rleaf_LAI);   // growth respiration, v. 2.4.1: 0.75 replaced by 0.7. According to Cannell and Thornley 2000, higher values should be used only if other sources of respiration (phloem loading etc.) are explicitly accounted for. For leaf construction, typically even a lower factor is a assumed (cf. Villar & Merino 2001, avg construction costs of 1.52 gC/gC)
    
    //update boundaries 
    if(NPP_leaf > 0.0) LAI_lowerbound = LAImax_temp;
    else LAI_upperbound = LAImax_temp;
    
    //now update LAImax_temp 
    LAImax_temp = 0.5 * (LAI_lowerbound + LAI_upperbound);
  }
  
  t_LAImax = LAImax_temp;
}

// Determines the maximum LAI that the tree should reach, similar to treeLAImax, but it directly calculates the maximum leafarea for the current light environment the tree experiences rather than for theoretical day
//! - the idea is to compute the LAI that the tree experiences at the crown top and compare it to its maximum LAI, then the tree adjusts its LAI or leaf area accordingly 
//! - importantly, since PPFD is an exponential function of leaf density (nonlinear), averaging LAI across the whole crown would not correspond to the LAI experienced by the tree. A heterogeneous environment with a mixture of light specks and dense spots above the crown will contribute considerably more PPFD (and have a lower effective LAI) than a homogeneously filled crown. We calculate the effective LAI instead of the real LAI above the crown top by first calculating average PPFD and then converting back to LAI 
void Tree::leafarea_max(float &LAIexperienced_eff, float &leafarea_max){
  float crown_area = PI * t_Crown_Radius * t_Crown_Radius;
  float crown_area_nogaps = GetCrownAreaFilled(crown_area);
  
  if(_LA_regulation == 1){
    // this is the case where we limit leaf allocation to the tree's maximum LAI, but have no dynamic reactions to the environment
    LAIexperienced_eff = 0.0;
  } else {
    int site_crowncenter = t_site + t_CrownDisplacement;
    int row_crowncenter = site_crowncenter/cols;
    int col_crowncenter = site_crowncenter%cols;
    
#ifdef CROWN_UMBRELLA
    // get the PPFD that the tree experiences and the area that is looped 
    float ppfd_CA[2] = {0.0,0.0};
    float noinput = 0.0;
    
    int shell_fromtop = 0;
    float fraction_filled_target = t_fraction_filled;
    
    LoopLayerUpdateCrownStatistic_template(row_crowncenter, col_crowncenter, t_Tree_Height, t_Crown_Radius, t_Crown_Depth, fraction_filled_target, shell_fromtop, GetRadiusSlope, noinput, ppfd_CA, ModifyNoinput, GetPPFDabove);
    
    float ppfd_experienced = ppfd_CA[0];
    float crown_area_looped = ppfd_CA[1];
#else
    int height_abovetop = int(t_Tree_Height) + 1;
    int crown_intarea = GetCrownIntarea(t_Crown_Radius);
    
    float fraction_filled_actual = 0.0;
    float fraction_filled_target = t_fraction_filled;
    
    float ppfd_experienced = 0.0;
    int crown_area_looped = 0;
    
    // loop over LookUp table, until crown_intarea is reached 
    for(int i=0; i < crown_intarea; i++){
      if(fraction_filled_actual > fraction_filled_target){
        fraction_filled_actual = (fraction_filled_actual * float(i))/(float(i) + 1.0);
      } else {
        fraction_filled_actual = (fraction_filled_actual * float(i) + 1.0)/(float(i) + 1.0);
        int site_relative = LookUp_Crown_site[i];
        int row = row_crowncenter + site_relative/51 - 25;
        int col = col_crowncenter + site_relative%51 - 25;
        
        if(row >= 0 && row < rows && col >= 0 && col < cols){
          int site=col+cols*row+SBORD;
          // first get voxel field densities 
          float absorb_prev = LAI3D[height_abovetop][site+SBORD];
          int intabsorb = CalcIntabsorb(absorb_prev);
          
          // obtain PPFD for the voxel, and also record the circled area 
          ppfd_experienced += WDailyMean * LookUp_flux[intabsorb];
          crown_area_looped++;
        }
      }
    }
#endif
    if(crown_area_looped > 0){
      float icrown_area_looped = 1.0/crown_area_looped;
      ppfd_experienced *= icrown_area_looped;
    } else {
      // this should not happend, potentially introduce a break or assert() here 
      ppfd_experienced = WDailyMean;
    }
    
    //now calculate the effective LAI above the tree 
    //importantly, we use the yearly average here, as the maximum tree LAI is also defined with respect to yearly averages
    LAIexperienced_eff = -log(ppfd_experienced/WDailyMean_year)/kpar;
    if(LAIexperienced_eff < 0.0001) LAIexperienced_eff = 0.0;
  }
  
  //now take the difference between the maximum value and the actual LAI experienced. Will be positive, if there is still room for leaves, and negative, if it's already too dense 
  //then calculate the relative size of the difference with respect to the tree's own LAI and calculate by how much it can still increase or has to reduce its leafarea 
  float LAIdiff = t_LAImax - LAIexperienced_eff; // this is the amount of LAI the tree has to loose or can add relative to its currently experienced LAI. It translates directly into the relative leafarea the tree can add or should lose, since all the calculations before are linear functions of the density 
  float LAdiff = LAIdiff * crown_area_nogaps;
  
  //in the worst case (if the difference in LAI cannot be compensated for by the tree anymore, i.e. it would have to have negative leafarea), we will get zero leafarea_max, and the tree will allocate everything to storage 
  float LAmaxphysiology = t_LAImax * crown_area_nogaps;
  
  leafarea_max = fminf(LAmaxphysiology,LAdiff);
  leafarea_max = fmaxf(0.0, leafarea_max);
  
#ifdef TRACK_INDIVIDUALS
  if(t_month_born >= 0){
    t_LAIabove_effavgyear += LAIexperienced_eff * timestep;
    t_LAIeffcum += LAIexperienced_eff;
    t_LAIeffsquared_cum += LAIexperienced_eff * LAIexperienced_eff;
  }
#endif
}

// Kikuzawa model for leaf lifespan 
//! - based on Kikuzawa, K. (1991). A cost-benefit analysis of leaf habit and leaf longevity of trees and their geographical pattern. The American Naturalist, 138(5), 1250-1263.
//! - the model assumes that the daily photosynthesis of leaves declines (linearly) with leaf age, then calculates the optimal leaf lifespan, i.e. the one that maximizes lifetime carbon gain for a leaf 
//! - this model is here implemented by calculating the LAI that the tree could have under optimal conditions (i.e. no leaf area above, and a LAI equivalent to LAImax), then calculating the effective daily assimilation (GPP) per unit leaf area (i.e. excluding stem respiration), and solving the model equation for leaf lifespan 
//! - there are a few caveats/things to consider in further developing/exploring this model a) A crucial parameter is the leaf age when carbon assimilation would reach 0, often denoted as b. This parameter has been shown to correlate with LMA in Xu et al. 2017, but, like LL-SLA relations, the relation comes with a lot of unexplained variation (on log scales, so multiplicative), and when I (FF) looked at their data, I obtained a better fit with Vcmax_25_mass, which we calculate anyways in TROLL, so it seemed to be internally consistent to use this relation. However, variation is still enormous (exp(0.6)), and this is clearly far from mechanistic. So while the Kikuzawa model partially solves the problem of deriving LL from LMA and the large uncertainties as well as potentially misleading inferences (i.e. very short leaf lifespans), part of the problem with large variation and weak correlations still remains and is now just shifted onto the derivation of b! Any more research on b and how to derive it for trees would thus be important  b) A second, crucial question is, what exactly the costs are that should be considered in the cost-benefit analysis. While leaf construction cost is uncontroversial (here taken to be LMA, i.e. mass per unit leaf area * 1.5 gC/gC to account for additional respiration in construction), there is a host of other costs that could be considered. We here also consider costs for leaf maintenance and for fine roots transpiration, but do not account for other cost such as building of support structures (Kikuzawa and Ackerly 2002, https://esj-journals.onlinelibrary.wiley.com/doi/abs/10.1046/j.1442-1984.1999.00005.x). Conversely, some papers, discount any respiration (e.g. Wang et al. 2021, https://www.biorxiv.org/content/10.1101/2021.02.07.430028v1.full). This is fundamentally about how modular/interconnected a tree is, and whether leaves could be modelled, at least as a first order approximation, as isolated. c) There is a choice to be made about what kind of environment should be supposed for leaf life span calculations. Are tree leaves optimized for an optimal light environment or the light environment when they are produced?  d) Finally, a minor point: I simply transferred the linear model of decline in leaf photosynthesis from Kikuzawa, etc. into TROLL. But this should probably be checked again, since we have an alternative way of simulating decline in leaf photosynthesis through young, mature and old leaves
float Tree::predLeafLifespanKikuzawa(){
  
  float absorb_prev = 0.0;
  float absorb_delta;
  if(_LA_regulation > 0) absorb_delta = t_LAImax;
  else absorb_delta = 3.0;
  
  int intabsorb = CalcIntabsorb(absorb_prev, absorb_delta);
  float PPFD = WDailyMean_year * LookUp_flux_absorption[intabsorb];
  float VPD = VPDDailyMean_year * LookUp_VPD[intabsorb];
  float T = tDailyMean_year - LookUp_T[intabsorb];
  
#ifdef WATER
  float GPP = Tree::dailyFluxesLeaf(PPFD, VPD, T).carbon_flux;
#else
  float GPP = Tree::dailyGPPleaf(PPFD, VPD, T);
#endif
  float Rday = Tree::dailyRdayleaf(T) * 0.4; // inhibition of respiration by ca. 40%, cf. Atkin et al. 2000
  
  int convTnight = int(iTaccuracy*Tnight_year);
  float Rnight = t_Rdark * LookUp_Rnight[convTnight];
  float GPP_effective = (GPP - 1.5 * Rday - 1.5 * Rnight); // in gC m^-2 day-1  we assume that Rday/Rnight decreases linearly as well, so we will use a GPP_eff = GPP - Rday;
  
  GPP_effective *= nbhours_covered * 3600.0 * 12.0/1000000.0 ; // we convert micromoles C/m^2/s into gC/m^2/day (factor 12.0 for conversion into gC, 3600 for second-hour conversion, and 10^6 to convert micromoles to moles; finally, we divide by the total leaf area that intercepted the light, since
  
  int convT= int(iTaccuracy*25.0); // temperature data at a resolution of Taccuracy=0.1°C -- stored in lookup tables ranging from 0°C to 50°C ---
  float Vcmax_25_mass = t_Vcmax*LookUp_VcmaxT[convT]/t_LMA;
  
  //Core model
  float b = exp(5.467025 - 1.138354 * log(Vcmax_25_mass) + gsl_ran_gaussian(gslrand, 0.6112195)); // parameter as in Xu et al. 2017, fit to their data
  float LL = 1.0 + 0.0333333 * fminf(b,sqrt(3.0 * 0.5 * t_LMA * b/GPP_effective));   // cf. Kikuzawa 1991 for formula, and Xu et al. 2017. As opposed to Xu et al. 2017 we assume decline in respiration rates. Also, LL can never be bigger than b
  LL = fmaxf(LL,3.0);
  
  return(LL);
}

//#############################################
// Tree growth
//#############################################
void Tree::Growth() {
  //update age 
  t_age+= timestep;                               //new v.2.2: increments are not 1 yr, but the duration of the timestep (usually 1 or <1, i.e. 1/12 if monthly, 1/365 if daily 
  
  //Set growth carbon to zero 
  t_carbon_biometry = 0.0;
#ifdef WATER
  t_transpiration=0.0;
#endif
  
  //calculate GPP and respiration
  CalcRespGPP();
  
  //! calculate NPP 
  t_NPP = 0.7 * (t_GPP - 1.5 * (t_Rday+t_Rnight+t_Rstem)); //Growth respiration, v. 2.4.1: 0.75 replaced by 0.7. According to Cannell and Thornley 2000, higher values should be used only if other sources of respiration (phloem loading etc.) are explicitly accounted for. For leaf construction, typically even a lower factor is a assumed (cf. Villar & Merino 2001, avg construction costs of 1.52 gC/gC)
  //Rleaf=Rday+Rnight is multiplied by 1.5 to also account for fine root respiration (cf as in Fyllas et al 2014 and Malhi 2012); Rstem is multiplied by 1.5 to account for coarse root respiration (according to the shoot root biomass ratio of 0.2 - Jérôme's paper in prep- and also to branch respiration (Meir & Grace 2002, Cavaleri 2006, Asao 2015). 
  
#ifdef CHECK_CARBON
  carbon_net_total += t_NPP;
  carbon_assimilated_total += t_GPP;
#endif
  
  if(_LA_regulation == 0){
    //Classic case of no regulation of leaf area, all NPP > 0.0 is converted into leaves 
    if(t_NPP<0.0){
      t_NPPneg++;
#ifdef TRACK_INDIVIDUALS
      if(t_month_born >= 0){
        t_time_carbonstarvation_year++;
        t_time_carbonstarvation++;
      }
#endif
      t_NPP = 0.0;
    } else {
      t_NPPneg = 0;
      //NPP allocation to wood and tree size increment
      UpdateTreeBiometry();
    }
    
    //NPP allocation to leaves
    UpdateLeafDynamics();
  } else {
    //leaf area is dynamically adjusted 
    if(t_NPP<0.0){
      //as long as the tree has enough storage to compensate for negative NPP, we will not count it as negative NPP 
      //the storage will, however, not be used for growth, only for upkeep 
      float carbon_storage_leftover = t_NPP + t_carbon_storage;
      if(carbon_storage_leftover > 0.0) {
        t_NPP = 0.0;
        t_carbon_storage = carbon_storage_leftover;
      } else {
        t_carbon_storage = 0.0;
      }
    }
    
    //if NPP is still negative, even after compensating for it through stored carbon, then carbon starvation sets in 
    if(t_NPP<0.0){
      t_NPPneg++;
      t_NPP=0.0;
      //v.2.3.0 -- Line of code below was odd. If NPP <0.0, then to ensure C balance it should be simply reset to NPP=0 at this stage 
      //t_NPP=t_GPP - 1.5*(t_Rday+t_Rnight+t_Rstem); REMOVED AS OF v.2.3.0.a4
      //NPP allocation to leaves
      UpdateLeafDynamics();
    } else {
      t_NPPneg=0;
      //in v.2.4.1: allocation to leaves is done before updating biometry 
      //idea is: at least leaves need to be sustained, otherwise the tree cannot come back from carbon stress 
      //in future versions: build up a carbon pool as buffer for stress situations !!!UPDATE
      //NPP allocation to wood and tree size increment
      UpdateLeafDynamics();
      UpdateTreeBiometry();
    }
  }
  
  UpdateVolumeDensity();
  
#ifdef WATER
  if (iter == 2) OutputTreeStandard(output[28]);
  if (iter == int(nbiter/2)) OutputTreeStandard(output[29]);
  if (iter == int(nbiter-1)) OutputTreeStandard(output[30]);
  
  if (t_site==4) OutputTreeStandard(output[12]);
  if (t_site==10380) OutputTreeStandard(output[13]);
  if (t_site==100950) OutputTreeStandard(output[14]);
  if (t_site==12090) OutputTreeStandard(output[15]);
  if (t_site==120090) OutputTreeStandard(output[16]);
  if (t_site==150667) OutputTreeStandard(output[17]);
#endif
}

//###################################################
// GPP and respiration calculation, called by Tree::Growth
//#####################################################
//! - new v.2.4: t_GPP and t_Rday are updated in separate function (thus adaptable for different modules) 
void Tree::CalcRespGPP(){
  
  if(t_leafarea > 0.0){
#ifdef WATER
#else
    if(_GPPcrown){
      // v.2.3.1 -- fast GPP calculation option. In addition, the daily computation of the Farquhar model is now the default option (_DAYTIMELIGHT is deprecated) 
      float PPFD = 0.0, VPD = 0.0, Tmp = 0.0, leafarea_layer;
      Fluxh(int(t_Tree_Height)+1, PPFD, VPD, Tmp, leafarea_layer);
      t_GPP = Tree::dailyGPPcrown(PPFD, VPD, Tmp, t_LAI);
      t_Rday = Tree::dailyRdayleaf(Tmp);
    } else {
#endif
      // Photosynthesis and respiration, set to zero 
      t_GPP=0.0;
      t_Rday=0.0;
      
      int crown_above_base=int(t_Tree_Height-t_Crown_Depth)+1; // for flux above crown base
      int crown_above_top=int(t_Tree_Height)+1;                // for flux above crown top
      
#ifdef CROWN_UMBRELLA
      int h_stop = max(crown_above_base,crown_above_top-3);
#else
      int h_stop = crown_above_base;
#endif
      float leafarea_cumulated = 0.0;
      
      for(int h = crown_above_top; h >= h_stop ; h--) {
        float PPFD = 0.0, VPD = 0.0, Tmp = 0.0, leafarea_layer = 0.0;
        Fluxh(h,PPFD, VPD, Tmp, leafarea_layer);
#ifdef WATER
        if(leafarea_layer > 0.0){
          leafFluxes dailyF = Tree::dailyFluxesLeaf(PPFD, VPD, Tmp);
          t_GPP += leafarea_layer * dailyF.carbon_flux;                   // !!!: check consistency! Maybe passing by reference could also be an idea?
          t_transpiration += leafarea_layer * dailyF.water_flux;          // !!!: check consistency! Maybe passing by reference could also be an idea?
        }
        if(isnan(t_transpiration) || t_transpiration < 0.0) Rcout << "Problem at site: " <<  t_site << " transpiration: " << t_transpiration << " leafarea_layer: " << leafarea_layer << " GPP: " << t_GPP << " PPFD: " << PPFD << " VPD: " << VPD << " T: " << Tmp << " t_WSF_A: " << t_WSF_A << " t_WSF: " << t_WSF << endl << endl;
#else
        t_GPP += leafarea_layer * Tree::dailyGPPleaf(PPFD, VPD, Tmp);
        //if(isnan(t_GPP) || t_GPP < 0.0 || PPFD == 0.0) Rcout << "Problem at site: " <<  t_site << " leafarea_layer: " << leafarea_layer << " GPP: " << t_GPP << " PPFD: " << PPFD << " VPD: " << VPD << " T: " << Tmp << endl << endl;
#endif
        t_Rday += leafarea_layer * Tree::dailyRdayleaf(Tmp);
        leafarea_cumulated += leafarea_layer;
        //if(t_dbh > 0.2) Rcout << crown_above_top - h << "PPFD: " << PPFD << " VPD: " << VPD << " Tmp: " << Tmp << " leafarea_layer: " << endl;
      }
      
      // Averaging across layers. To check consistency, leafarea_layer can be added up and the sum compared to t_leafarea 
      float ileafarea_cumulated;
      if(leafarea_cumulated > 0.0) ileafarea_cumulated= 1.0/leafarea_cumulated;
      else ileafarea_cumulated = 0.0;
      
      t_GPP   *= ileafarea_cumulated;
      t_Rday  *= ileafarea_cumulated;
#ifdef WATER
      t_transpiration *= ileafarea_cumulated ;
#endif
#ifdef WATER
#else
    }
#endif
  }
  
  float effLA = 0.5 * (t_leafarea+t_matureLA) * nbhours_covered * 15.7788 * timestep; // we convert  from micromoles C/m^2/s into gC per m^2 of leaf per timestep by "nbhours_covered*15.7788*timestep" where 15.7788 = 3600*365.25*12/1000000 (seconds, days, and mass of carbon) and nbhours_covered is the amount of time that is covered by the daily variation file.
  float effLA_night = 0.5 * (t_leafarea+t_matureLA) * (24.0 - nbhours_covered) * 15.7788 * timestep; // same as during the day, but inverse of hours covered (we assume that non-covered hours are night values)
  
  t_GPP *=effLA;
  
#ifdef WATER
  t_transpiration*=1.6*iCair*1.5*effLA; // this is the amount of water transpired by the tree during the timestep in m3. 1.5 =18/12 where 18 and 12 are molar masses of H20 and C. This amounts to multiply by 0.5*(t_leafarea+t_matureLA)*283.824*timestep, where 283.824 = 18.10^-6 * 3600*12*365, where 18.10^-6 is to convert mol H20 into m3, 3600*365*12 to convert s into year (as timestep is given in year. CHECK: from our WILT data of sapflow of several canopy mature trees, and assuming a constant sapwood thickness of 4cm, this can be 5-110 liter/day, ie. 0.005-0.11 m3/day. See also values in Granier et al. 1996 (0.250-0.3 m3/day for a big dominant canopy tree), or Andrade et al. 1998 Oecologia (46-379 kg/day)
  t_transpiration=fminf(t_transpiration, PET*0.001); // see comments in descriptive doc, this point should be fixed in a proper way. Especillay, GPP should be modified accordingly, and this constraints on total transpiration better grounded and dicussed.
#endif
  
  int convT= int(iTaccuracy*temp); // temperature data at a resolution of Taccuracy=0.1°C -- stored in lookup tables ranging from 0°C to 50°C ---
  t_Rstem = t_sapwood_area * (t_Tree_Height-t_Crown_Depth) * LookUp_Rstem[convT];
  
  int convTnight= int(iTaccuracy*tnight); // temperature data at a resolution of Taccuracy=0.1°C -- stored in lookup tables ranging from 0°C to 50°C ---
  
  // the following two lines should maybe be revised, because the decline in respiration might not be the same as in photosynthetic activity. Kitajima et al. 2002 found no or small reductions in respiration with leaf age, Reich et al. 2009 did (Rdark/Amax constant), but often respiration declines are less steep than photosynthetic capacity, cf. Villar et al. 1995 
  t_Rnight = t_Rdark * effLA_night * LookUp_Rnight[convTnight];
  t_Rday *= effLA * 0.4; // inhibition of respiration by ca. 40%, cf. Atkin et al. 2000
}

//####################################################
// Leaf dynamics and C allocation, called by Tree::Growth
//#####################################################
//! - NPP allocation to leaves
//! - In this current scheme of leaf demography and phenology in three leaf age classes: only the old leaves generate litterfall, and the dynamic of leaves cycle is generated by the dynamic of NPP, with a total leaf biomass varying - as opposed to De Weirdt et al 2012 in ORCHIDEE, but as in Wu et al 2016 but importantly without prescribing litterfall- 
void Tree::UpdateLeafDynamics() {
  float ileafdem_resolution = 1.0/float(leafdem_resolution);
  float flush = 2.0 * fmaxf(t_NPP,0.0) * falloccanopy * 0.68 * 1.0/t_LMA; // 0.68 is the fraction of NPP allocated to leaves (the other 32% being allocated to twigs and fruits), retrieved from Chave et al. 2008, 2010.
  // fine resolution of flush 
  float flush_fine = ileafdem_resolution * flush;
  float lambda_young = ileafdem_resolution * t_lambda_young;
  float lambda_mature = ileafdem_resolution * t_lambda_mature ;
  float lambda_old = ileafdem_resolution * t_lambda_old ;
  
  if(_LA_regulation == 0){
    // litter module, without any dynamic leaf area regulation 
    t_litter = 0.0;
    
    for (int i = 0; i < leafdem_resolution; i++){
      float new_litter = lambda_old * t_oldLA ;
      float new_young = flush_fine;
      float new_mature = t_youngLA * lambda_young ;
      float new_old    = t_matureLA * lambda_mature ;
      
      t_youngLA += new_young - new_mature ;
      t_matureLA += new_mature - new_old ;
      t_oldLA += new_old - new_litter ;
      
      t_litter += new_litter * t_LMA;
    }
  } else {
    // dynamic leaf area regulation (!!!: TODO, literature references) 
    // the main idea is that the fixed fractions with which allocation to leaves and stem are modelled ("falloccanopy", "fallocwood", as fractions of newly assimilated carbon) are only regulative, i.e. they apply if the tree grows in sunlight without much constraint and still has space for new leaves. If a deviation from these conditions occurs, then the allocation patterns will change. 1) Reaching maximum leaf area: If a tree has filled its whole crown with leaves and any additional tree leaf would only result in more self-shading and a net loss of carbon, then the tree will only allocate leaves for keeping up the maximum leaf area and a) allocate excess carbon to a non-structural carbon (NSC) storage b) or allocate it to growth in diameter, if the storage is full already 2) Small or no potential for photosynthesis (maximum leaf area is smaller than the tree's actual leaf area, down to 0). Then, the tree will not allocate any carbon to leaf construction and reserve carbon mostly for respiration or stem growth, until a gap appears. The module as a whole prioritizes leaf upkeep over growth, which is a conversative strategy. In reality trees or species may have more aggressive strategies (i.e. growing faster when shaded), but this would be worth a research project on its own
    // first, get leafarea_max, i.e. the maximum of leafarea that the tree could allocate before creating too much self-shading 
    float LAIexperienced_eff;
    leafarea_max(LAIexperienced_eff, t_leafarea_max);
    t_leafarea_max = fmaxf(0.0,t_leafarea_max);
    
    // carbon intended for tree growth, mobilized as if it was used for flushing leaves (in case it is needed, if not it will be backconverted at the end of the procedure) 
    float flush_biometry = 2.0 * fmaxf(t_NPP,0.0) * fallocwood * 0.6 * 1.0/t_LMA;   // new in v. 2.4.0: only 60% of woody npp is actually used for construction, the rest is for branch fall repair (cf. Malhi et al. 2011)
    float flush_biometry_fine = ileafdem_resolution * flush_biometry;
    
    // storage carbon, mobilized as if it was used for flushing leaves (in case it is needed, if not, backconversion at end of procedure) 
    float flush_storage = 2.0 * t_carbon_storage * 1.0/t_LMA;
    float flush_storage_fine = ileafdem_resolution * flush_storage;
    
    // We have temporarily converted all carbon into carbon for leaf flushing, so we set the pools to zero, they will be filled up again later with all the carbon that was not required. Also overall leaf litter is set to zero 
    t_carbon_biometry = 0.0;
    t_carbon_storage = 0.0;
    t_litter = 0.0;
    
    // get the maximum leafarea the tree can currently support 
    // given that we assume that the leaf density environment only changes once very iteration, this can also be calculated only once every iteration 
    
    //float new_young = 0.0;
    for (int i = 0; i < leafdem_resolution; i++){
      float new_litter = lambda_old * t_oldLA ;
      
      // first check whether tree can still support more leaves (does not have excess leafarea) 
      // then calculate the flush needed, which is at least the amount of leaves to keep up the current leafarea 
      float la_excess = t_leafarea - t_leafarea_max;
      float flush_needed = new_litter;                // flush_needed is set equal to litter, but will be reduced, if there is excess leaves on the tree
      
      float new_young;
      // if leafarea does not exceed the maximum leafarea, just allocate all the flush, otherwise adjust flush_needed 
      if(la_excess < 0.0){
        la_excess = 0.0;
        new_young = flush_fine;
      } else {
        new_young = fminf(flush_fine,flush_needed);
      }
      flush_needed = fmaxf(flush_needed-la_excess,0.0);
      
      // subtract from flush whatever has been allocated 
      flush -= new_young;
      
      // if flush needed is smaller than incoming flush, then get more flush from carbohydrates usually reserved for diameter growth or storage 
      float flush_diff = flush_needed - new_young;
      
      if(flush_diff > 0.0){
        float flush_from_biometry = fminf(flush_biometry_fine, flush_diff);  // flush_diff is the extra flush needed, so limit the amount of flush taken from biometry to what is needed
        new_young += flush_from_biometry;                                   // add extra flush to new leaves
        flush_biometry -= flush_from_biometry;                              // and deduce it from the source
        
        flush_diff = flush_needed - new_young;
        
        if(flush_diff > 0.0){
          float flush_from_storage = fminf(flush_storage_fine, flush_diff);  // flush_diff is the extra flush needed, so limit the amount of flush taken from storage to what is needed
          new_young += flush_from_storage;                                   // add extra flush to new leaves
          flush_storage -= flush_from_storage;                              // and deduce it from the source
        }
      }
      
      // transferring leaves across pools 
      float new_mature = t_youngLA * lambda_young ;
      float new_old    = t_matureLA * lambda_mature ;
      
      t_youngLA += new_young - new_mature ;
      t_matureLA += new_mature - new_old ;
      t_oldLA += new_old - new_litter ;
      
      t_litter += new_litter * t_LMA;
      t_leafarea = t_youngLA + t_matureLA + t_oldLA ;
    }
    
    // This is to ensure that trees do not get infinitely small leaf areas (let's assume minimum size of 2.5cm * 2cm = 0.0005 m2) 
    if(t_leafarea < 0.0005) t_leafarea = 0.0;
    
    // refill storage, if storage has been tapped 
    // in light of floating point calculations not being 100% exact, we have to make sure that quantities are above zero (and not -...e-9) 
    float carbon_storage_max = CalcCarbonStorageMax();
    float carbon_from_flush = 0.5 * (flush_storage + flush) * t_LMA;
    float carbon_excess = carbon_from_flush - carbon_storage_max;
    
    if(carbon_excess > 0.0){
      t_carbon_storage = carbon_storage_max;
      if(_seedsadditional == 0 || t_dbh < t_dbhmature){
        t_carbon_biometry += carbon_excess;         // by default, excess carbon that cannot be stored is allocated to growth
      } else {
        float seedcarbon = t_NPP*falloccanopy*0.08*0.5;
        t_multiplier_seed = int((carbon_excess + seedcarbon)/seedcarbon);
      }
    } else if(carbon_from_flush > 0.0){
      t_carbon_storage = carbon_from_flush;
    }
    float carbon_biometry_total = 0.5 * flush_biometry * t_LMA;
    if(carbon_biometry_total > 0.0) t_carbon_biometry += carbon_biometry_total ;
  }
}

//Compute biometric relations, including allometry
//! - New standalone function in v.2.3.0 
//! - volume in m^3: the first factor of 2 is to convert C into biomass. the 1/s_ wsg to convert biomass into volume (g/cm^3). the 1e-6 term converts cm^3 into m^3 (the sole metric unit in the model). fallocwood is the fraction of biomass allocated to aboveground wood (stem + branches) growth. For the time being, we shall assume that a fixed proportion of NPP is allocated into AGB production. Currently, 0.20=%biomasse allocated to stem increment could be a global variable, even though this % allocation could in fact vary with resouce variation/co-limitation
void Tree::UpdateTreeBiometry(){
  
  //! taking into account wood elements recycling (ex. fallen branches etc...) 
  //! t_ddbh = flor( volume* 4.0/( 3.0*PI*t_dbh*LH*t_Tree_Height*LV ) )* NH; 
  
  float delta_agb;
  if(_LA_regulation == 0){
    delta_agb = 2.0 *t_NPP * fallocwood * 0.6; // new in v. 2.4.0: only 60% of woody npp is actually used for construction, the rest is for branch fall repair (cf. Malhi et al. 2011)
  } else {
    delta_agb = 2.0 * t_carbon_biometry;
  }
  
  if (t_dbh>t_dbhmax) delta_agb *= fmaxf(3.0-2.0*t_dbh/t_dbhmax,0.0);
  
  // Tree dbh increment 
  float ddbh = CalcIncrementDBH(delta_agb); // moved to a separate empirical function CalcIncrementDBH
  // With V=pi*r^2*h, increment of volume = dV = 2*pi*r*h*dr + pi*r^2*dh 
  // With isometric growth assumption (ddbh/dbh=dh/h)and dbh=2*r: dV=3/4*pi*dbh*h*ddbh, ddbh in m, it follows: ddbh = 4/3 * V = 4/3 * 1/(pi*dbh*h)   
  if(t_dbh + ddbh > 0.1 && t_dbh < 0.1) S[t_sp_lab].s_nbind10++;
  if(t_dbh + ddbh > 0.3 && t_dbh < 0.3) S[t_sp_lab].s_nbind30++;
  
  t_dbh += ddbh;
  UpdateSapwoodArea(ddbh);
  
  UpdateHeight();
  UpdateCR();
  UpdateCD();
  
#ifdef CROWN_UMBRELLA
  UpdateCrownSlope();
#endif
  
#ifdef Output_ABC
  S[t_sp_lab].s_dbhmax_realized = fmaxf(S[t_sp_lab].s_dbhmax_realized,t_dbh);
#endif
}

void Tree::UpdateVolumeDensity(){
  float crown_area = PI*t_Crown_Radius*t_Crown_Radius;
  float crown_area_nogaps = GetCrownAreaFilled(crown_area);
  t_LAI = t_leafarea/crown_area_nogaps;
}

//####################################################
// Tree death, called by Tree::Update
//####################################################
//! - This function records basic properties of a tree that has died and empties the variables at the tree site. 
//! - It does not, however, apply the destructor to the tree object 
void Tree::Death() {
  
#ifdef TRACK_INDIVIDUALS
  if(t_month_born >= 0){
    float agb = 1000.0 * 0.0673*pow(t_wsg*t_Tree_Height*LV*t_dbh *t_dbh *LH*LH*10000.0, 0.976); // !!!UPDATE should be a call of agb function now
    if(t_dbh >= 0.1){
      output[29] << t_site << "\t" << t_month_born << "\t" << iter << "\t" << t_age << "\t" << t_seedsproduced_sumyear << "\t" << t_seedsproduced << "\t" << t_time_carbonstarvation_year << "\t" << t_time_carbonstarvation << "\t" << t_dbh << "\t" << t_dbh - t_dbh_tracked << "\t" << t_Tree_Height << "\t"  <<  t_Tree_Height - t_Tree_Height_tracked << "\t" << t_Crown_Radius << "\t"  <<  t_Crown_Radius - t_Crown_Radius_tracked << "\t" << agb << "\t" << agb - t_agb_tracked << "\t" << t_GPP_sumyear << "\t" << t_GPPsquared_sumyear  << "\t" << t_NPP_sumyear << "\t" << t_NPPsquared_sumyear << "\t" << t_Rday_sumyear << "\t" << t_Rnight_sumyear << "\t" << t_Rstem_sumyear << "\t" << t_LAIabove_effavgyear<< "\t" << t_carbon_storage_avgyear << endl;
    }
    
    output[30] << t_site << "\t" << t_month_born << "\t" << iter << "\t" << t_age << "\t" << t_seedsproduced << "\t" << t_time_carbonstarvation << "\t" << t_dbh << "\t" << t_Tree_Height << "\t" << t_Crown_Radius << "\t" << agb << "\t" << t_GPPcum << "\t" << t_NPPcum << "\t" << t_LAIcum << "\t" << t_LAIeffcum << "\t" << t_GPPsquared_cum << "\t" << t_NPPsquared_cum << "\t" << t_LAIsquared_cum  << "\t" << t_LAIeffsquared_cum << endl;
  }
#endif
  
  // new v.2.4: statistics are now calculated inside the Death() function 
  // tree death statistics 
  nbdead_n1++;
  nblivetrees--;
  if ((S[t_sp_lab].s_nbind)>0) (S[t_sp_lab].s_nbind)--;
  if(t_dbh*LH>0.1){
    nbdead_n10++;
    if ((S[t_sp_lab].s_nbind10)>0) (S[t_sp_lab].s_nbind10)--;
#ifdef Output_ABC
    int row = t_site/cols;
    int col = t_site%cols;
    if(row >= row_start && row < row_end && col >= col_start && col < col_end) nbdead_n10_abc++;
#endif
  }
  if(t_dbh*LH>0.3){
    nbdead_n30++;
    if ((S[t_sp_lab].s_nbind30)>0) (S[t_sp_lab].s_nbind30)--;
  }
  // New v.2.2. new outputs 
  if(!_OUTPUT_reduced) {
    if(iter == 2) output[23] << "N\t" << t_sp_lab << "\t" << t_dbh << "\t" << t_age << "\t" << t_Tree_Height <<  "\n";
    if(iter == int(nbiter/2)) output[24]<< "N\t" << t_sp_lab << "\t" << t_dbh << "\t" << t_age << "\t" << t_Tree_Height <<  "\n";
    if(iter == int(nbiter-1)) output[25]<< "N\t" << t_sp_lab << "\t" << t_dbh << "\t" << t_age << "\t" << t_Tree_Height <<  "\n";
  }
  
  t_sp_lab = 0;
  t_age = 0;
  t_hurt = 0;
  t_NPP=t_GPP=t_Rday=t_Rnight=t_Rstem=0.0; //! new v.2.3 
  t_dbh = t_Tree_Height = t_Crown_Radius = t_Crown_Depth= 0.0;
  t_CrownDisplacement = 0;
  
  if(_BASICTREEFALL) t_Ct = 0.0;
  
#ifdef Output_ABC
  t_dbh_previous = 0.0;
#endif
  
}

//################################
// Seed dispersal, called by UpdateField
//#################################
//! - This routine implements the reproduction stage (nbs seeds are produced per tree) and dispersal of the trees
//! - Dispersal is equiprobable in all direction an normally distributed (for 2D normal distribution, the absolute distance follows a Rayleigh distribution)
//! - Reproduction only occurs for mature trees; in the future, a C cost could be added 
//! - New v.2.1 threshold of maturity is defined as a size threshold (and not age as before), following Wright et al 2005 JTE 
void Tree::DisperseSeed(){
  if(t_dbh >= t_dbhmature){
    int nbs;
    if(_SEEDTRADEOFF) nbs=int(t_NPP*2.0*falloccanopy*0.08*0.5*(S[t_sp_lab].s_iseedmass)); //some multiplications could be avoided in this line.
    else nbs=nbs0*t_multiplier_seed;
    //else nbs=int(t_NPP*2*falloccanopy*0.08*0.5); // test 17/01/2017: use a factor to translate NPP into seeds produced, but not species specific, not linked to mass of grains 
    for(int i=0;i<nbs;i++){
      // Loop over number of produced seeds 
      //float rho = 2.0*((t_s->s_ds)+t_Crown_Radius)*float(sqrt(fabs(log(genrand2()*iPi))));    //! s_ds is mean seed dispersal distance. Dispersal distance rho: P(rho) = rho*exp(-rho^2) 
      //update 2.5: rho does not seem to correspond to original 1999 paper anymore and in previous version predicted dispersal with a lower cutoff instead of the Rayleigh distribution 
      //here we restore the previous formulation by using the Rayleigh implementation from the gsl library 
      //for the moment, we do not use the crown radius as an additional dispersal kernel. This would lead to a loss of large tree species locally, because they will have much less seeds within the plot 
      float rho = gsl_ran_rayleigh(gslrand, S[t_sp_lab].s_ds);
      float theta_angle = float(twoPi*gsl_rng_uniform(gslrand)); //Dispersal angle theta 
      int col_tree = t_site%cols;
      int row_tree = t_site/cols;
      int dist_cols = int(rho*cos(theta_angle));
      int dist_rows = int(rho*sin(theta_angle));
      int col_dispersal = dist_cols + col_tree;
      int row_dispersal = dist_rows + row_tree;
      FillSeed(col_dispersal,row_dispersal,t_sp_lab);
    }
#ifdef TRACK_INDIVIDUALS
    if(t_month_born >= 0){
      t_seedsproduced_sumyear += nbs;
      t_seedsproduced += nbs;
    }
#endif
  }
}

//##################################
// Tree death and growth
//##################################
//! - This routine calls the appropriate DeathRate modules; if the death condition is met, function Tree::Death() is called, otherwise function Tree::Growth is called
void Tree::Update() {
  int death;
  if(t_age) {
    if(t_dbh > 0.1) nbtrees_n10++;
    if(t_dbh > 0.3) nbtrees_n30++;
    
#ifdef WATER
    // !!!: changed by FF, t_PPFD has been removed in v.2.5 (along with t_VPD, t_T). It was never updated, because, although formally passed on to DeathRate(), it was never used in the DeathRate() function. Furthermore, these quantities were dangerous, because they were defined at the tree level, but could change throughout the crown. Now passed by reference only where they are needed. This makes checking whether they are actually needed easier as well. Whether this affects any procedures in WATER module, needs to be checked)
    //Start new in v3.0 IM 
    //Fluxh(int(t_Tree_Height)+1);            //strangly, t_PPFD was not updated at the beginning of Tree::Update (as I here suggest to do now),but only in Tree::Growth and CalcRespGPP, even though t_PPFD was already used in the first part of Tree::Update (death).
    //End new in v3.0 IM 
    Water_availability();                   //here, t_phi_root and WSF are updated, which is needed to compute Deathrate, carbon assimilation and transpiration
#endif
    //v.2.4.0: outputs have been moved to Death() function 
    if(_NDD)
      death = int(gsl_rng_uniform(gslrand)+DeathRateNDD(t_dbh, t_NPPneg, t_NDDfield[t_sp_lab]));
    else
#ifdef WATER  // note that I did not include a version with both drought-induced mortality and NDD effect on mortality, to be done if needed.
      death = int(gsl_rng_uniform(gslrand)+DeathRate(t_dbh, t_NPPneg, t_phi_root));
#else
    death = int(gsl_rng_uniform(gslrand)+DeathRate(t_dbh, t_NPPneg));
#endif
    if(death) Death();
    else Growth();   // v.2.4: t_hurt is now updated in the TriggerTreefallSecondary() function
  }
}

//####################################
// Tree falling function, called by TriggerTreefall
//####################################
//! - Tree falling routine, formerly FallTree(),  _BASICTREEFALL, changed in v.2.4.0
//! - Creates a treefall (but no longer treefall probability). Takes angle as argument and can now be used for primary, secondary treefalls, forestry, or other disturbances 
//! - NEW in TROLL v.2.4: FallTree() function has become Treefall() function, calculation of angle and treefall outside of function, and damages are now added up from several treefalls 
void Tree::Treefall(float angle) {
  // treefall statistics 
  nbTreefall1++;
#ifdef Output_ABC
  if(t_dbh*LH>0.1){
    nbTreefall10++;
    int row = t_site/cols;
    int col = t_site%cols;
    if(row >= row_start && row < row_end && col >= col_start && col < col_end) nbTreefall10_abc++;
  }
#else
  if(t_dbh*LH>0.1) nbTreefall10++;
#endif
  if(t_dbh*LH>0.3) nbTreefall30++;
  int xx,yy;
  int row0,col0,h_int, r_int;
  float h_true = t_Tree_Height*LV;
  h_int = int(h_true*NH);
  row0=t_site/cols;
  col0=t_site%cols;
  
  //update of Thurt field at the site of the tree, for consistency 
  //Thurt[0][t_site+sites] = max(int(t_Tree_Height),Thurt[0][t_site+sites]);
  //fallen stem destructs other trees 
  for(int h=1;h<h_int;h++) {                      // loop on the fallen stem (horizontally)
    xx=int(fmaxf(col0+h*cos(angle),0.0));          // get projection in col (= xx) direction, where xx is absolute location
    if(xx<cols){
      yy=   int(row0+h*sin(angle));         // get projection in row (= yy) direction, where yy is absolute location
      Thurt[0][xx+(yy+rows)*cols] = max(int(t_Tree_Height),int(Thurt[0][xx+(yy+rows)*cols]));
      // Thurt[0] where the stem fell, calculation: xx+(yy+rows)*cols= xx + yy*cols + rows*cols = xx + yy*cols + sites / NEW in v.2.4: addition of damage instead of setting equal in order to account for cumulative damage (several treefalls hitting the same site)
    }
  }
  
  //fallen crown destructs other trees, less damaging than stem 
  xx=col0+int((h_true*NH-t_Crown_Radius)*cos(angle));
  yy=row0+int((h_true*NH-t_Crown_Radius)*sin(angle));
  r_int = int(t_Crown_Radius);
  for(int col=max(0,xx-r_int);col<min(cols,xx+r_int+1);col++) { // loop on the fallen crown (horizontally)
    for(int row=max(0,yy-r_int);row<min(rows,yy+r_int+1);row++) {
      if((col-xx)*(col-xx)+(row-yy)*(row-yy)<r_int*r_int) Thurt[0][col+(row+rows)*cols] = max(int((t_Tree_Height-t_Crown_Radius*NV*LH)*0.5),int(Thurt[0][col+(row+rows)*cols])) ; // less severe damage than stem / NEW in v.2.4: max() or addition of damage instead of setting equal in order to account for cumulative damage (several treefalls hitting the same site)
    }
  }
  // v.2.4.0: outputs have been moved to Death() function 
  Death();
}

//####################################################
// Computes Average and OutputField 
//####################################################
// - Short routine that basically only updates the vector s_output_field
void Tree::Average() {
  if(t_age>0) {
    if(t_dbh*LH >= 0.1) {
      (S[t_sp_lab].s_output_field[1])++;
      S[t_sp_lab].s_output_field[6] += t_dbh*LH*t_dbh*LH;
    }
    if(t_dbh*LH >= 0.3) (S[t_sp_lab].s_output_field[2])++;
    S[t_sp_lab].s_output_field[3] += t_dbh*LH*t_dbh*LH;
    S[t_sp_lab].s_output_field[4] += t_NPP*1.0e-6;
    S[t_sp_lab].s_output_field[5] += t_GPP*1.0e-6;
    S[t_sp_lab].s_output_field[7] += CalcAGB();
    S[t_sp_lab].s_output_field[8] += t_Rday*1.0e-6;
    S[t_sp_lab].s_output_field[9] += t_Rnight*1.0e-6;
    S[t_sp_lab].s_output_field[10] += t_Rstem*1.0e-6;
    S[t_sp_lab].s_output_field[11] += t_litter*1.0e-6;
  }
}

// Computation of dbh histograms
void Tree::histdbh() {
  if(t_age) nbdbh[int(100.*t_dbh*LH)]++;
  // where dbh is in cm (it is in number of horizontal cells throughout the code)
  // values are always rounded down (so nbdbh[30] gives you trees with more than 30 cm dbh, and less than 31))
}

#ifdef WATER
// Standard outputs during the simulation -- written to file
void Tree::OutputTreeStandard(fstream& output){
  output << iter << "\t" << t_site << "\t" << t_sp_lab << "\t" << t_Tree_Height << "\t" << t_dbh << "\t" << t_litter << "\t" << t_age << "\t" << t_leafarea << "\t" << t_youngLA<< "\t" << t_matureLA << "\t" << t_oldLA << "\t" << t_Crown_Radius << "\t" << t_Crown_Depth <<"\t" << t_GPP  <<"\t" << t_NPP <<"\t" << t_Rstem <<"\t" << t_Rday  <<"\t" << t_Rnight << "\t" << t_site << "\t" << LAI3D[int(t_Tree_Height)][t_site+SBORD] << "\t" << LAI3D[int(t_Tree_Height-t_Crown_Depth)+1][t_site+SBORD] << "\t" << t_root_depth << "\t" << t_phi_root << "\t" << t_WSF << "\t" << t_WSF_A << "\t" << t_transpiration << "\t" << PET;
  for (int l=0; l<nblayers_soil; l++) output << "\t" << t_root_biomass[l];
  for (int l=0; l<nblayers_soil; l++) output << "\t" << t_soil_layer_weight[l];
  output << endl;
}
// Standard outputs during the simulation -- written to screen in real time
void Tree::OutputTreeStandard(){
  Rcout << iter << "\t" << t_site << "\t" << t_sp_lab << "\t" << t_Tree_Height << "\t" << t_dbh << "\t" << t_litter << "\t" << t_age << "\t" << t_leafarea << "\t" << t_youngLA<< "\t" << t_matureLA << "\t" << t_oldLA << "\t" << t_Crown_Radius << "\t" << t_Crown_Depth <<"\t" << t_GPP  <<"\t" << t_NPP <<"\t" << t_Rstem <<"\t" << t_Rday  <<"\t" << t_Rnight << "\t"  << LAI3D[int(t_Tree_Height)][t_site+SBORD] << "\t" << LAI3D[int(t_Tree_Height-t_Crown_Depth)+1][t_site+SBORD] << "\t" << t_root_depth << "\t" << t_phi_root << "\t" << t_WSF;
  for (int l=0; l<nblayers_soil; l++) Rcout << "\t" << t_root_biomass[l];
  for (int l=0; l<nblayers_soil; l++) Rcout << "\t" << t_soil_layer_weight[l];
  Rcout << endl;
}
#endif

//Calculate the crown area filled by leaves (only relevant for crown gap fractions > 0.0)
float Tree::GetCrownAreaFilled(float crown_area){
  //for now calculated explicitly. Ideally, we would replace the loop by an equation that expresses the underlying logic 
  int crown_intarea = int(crown_area);     // floor of crown_area to bound area accumulation
  crown_intarea = max(crown_intarea,1);    // minimum area of crown (1)
  crown_intarea = min(crown_intarea,1963); // maximum area of crown (radius 25), int(3.14*25*25)
  
  int crown_intarea_gaps = 0;
  float fraction_filled_target = t_fraction_filled;
  float fraction_filled_actual = 0.0;
  
  for(int i = 0; i < crown_intarea; i++){
    if(fraction_filled_actual > fraction_filled_target){
      fraction_filled_actual = (fraction_filled_actual * float(i))/(float(i) + 1.0);
      crown_intarea_gaps++;
    } 
    else fraction_filled_actual = (fraction_filled_actual * float(i) + 1.0)/(float(i) + 1.0);
  }
  
  // now determine crown_area_filled, depending on whether the next voxel is filled or not filled 
  float crown_area_filled;
  if(fraction_filled_actual > fraction_filled_target){
    crown_area_filled= float(crown_intarea - crown_intarea_gaps);
  } 
  else crown_area_filled = crown_area - float(crown_intarea_gaps);
  
  return(crown_area_filled);
}

#ifdef TRACK_INDIVIDUALS
//Diagnostic function to track trees born at a reference year
float Tree::StartTracking(){
  //Only tracks trees born in a mature forest at year 501 
  //currently hardcoded 
  if(iter >= 6000 && iter < 6012) t_month_born = iter%iterperyear;
  else t_month_born = -1;
  
  if(t_month_born >= 0){
    // initialise variables 
    t_seedsproduced = 0;
    t_seedsproduced_sumyear = 0;
    t_time_carbonstarvation = 0;
    t_time_carbonstarvation_year = 0;
    
    t_GPP_sumyear = 0.0;
    t_NPP_sumyear = 0.0;
    t_GPPsquared_sumyear = 0.0;      // for standard deviation
    t_NPPsquared_sumyear = 0.0;      // for standard deviation
    t_Rday_sumyear = 0.0;
    t_Rnight_sumyear = 0.0;
    t_Rstem_sumyear = 0.0;
    t_LAIabove_effavgyear = 0.0;
    t_carbon_storage_avgyear = 0.0;
    
    t_LAIcum = 0.0;
    t_LAIeffcum = 0.0;
    t_GPPcum = 0.0;
    t_NPPcum = 0.0;
    t_LAIsquared_cum = 0.0;
    t_LAIeffsquared_cum = 0.0;
    t_GPPsquared_cum = 0.0;
    t_NPPsquared_cum = 0.0;
    
    t_dbh_tracked = t_dbh;
    t_Tree_Height_tracked = t_Tree_Height;
    t_Crown_Radius_tracked = t_Crown_Radius;
    t_agb_tracked = 1000.0 * 0.0673*pow(t_wsg*t_Tree_Height*LV*t_dbh *t_dbh *LH*LH*10000.0, 0.976); // !!!UPDATE needs to be replaced with AGB function
    
    output[28] << t_site << "\t" << t_month_born << "\t" << t_site%cols << "\t" << t_site/cols << "\t" << t_s->s_name << "\t" << t_dbh << "\t" << t_Crown_Radius << "\t" << t_Tree_Height << "\t" << t_agb_tracked << "\t" << t_intraspecific_multiplier_CR << "\t" << t_intraspecific_multiplier_height << "\t" << t_wsg << "\t" << t_Nmass << "\t" << t_Pmass << "\t" << t_LMA << "\t" << t_intraspecific_deviation_wsg << "\t" << t_intraspecific_multiplier_N << "\t" << t_intraspecific_multiplier_P << "\t" << t_intraspecific_multiplier_LMA << "\t" << t_Vcmax << "\t" << t_Jmax << "\t" << t_Rdark << "\t" << t_LAImax << "\t" << t_leaflifespan << endl;
  }
}
#endif


#ifdef CROWN_UMBRELLA
//! - Global function: the following function is not a member function of the Tree class object, but operates at the tree level and could be converted to member functions. They're therefore defined next to the other tree level functions. Whether conversion to Tree members adds any benefit in terms of performance should be tested. 
//! - the main function is a template to loop through one crown shell (i.e. a 1m layer of the tree) and do something within the tree's canopy, such as allocating leaves or computing the flux. It is supported by a second template that simulates the actual loop. The crown shell can be bent via a function to simulate the umbrella like crown shapes 
//! - variables to be provided to the template:
//! -# crown properties, including the crown position (row, col), the tree height, the crown radius, the crown depth, the fraction of filled voxels (inverse of gap fraction) as well as the layer/shell counting from the tree top 
//! -# a function "GetRadiusLayer" that creates the "umbrella" shape. It defines the change in crown radius between the top of the crown and the second layer above the crown base. Here by default a linear slope 
//! -# crown statistics to be updated, one as input, one as output. These statistics are updated by the function "ModifyCrownStatistic" and then applied across the crown with the function "UpdateCrownStatistic". While they always have to be provided, they do not always have to be used. 
//!
//! EXAMPLE 1: for adding volume to a voxel field, the input statistic would be +1, for removing volume, -1, the ModifyCrownStatistic function empty, and the update function would simply add the input value to the Voxel3D field 
//!
//! EXAMPLE 2: for adding leaves to the LAI field, the input statistic would be the tree LAI, or for removing, -LAI, ModifyCrownStatistic would convert the LAI to density within a specific layer, and the update function would simply add the resulting density values to the LAI3D field 
//! - Input and output variables can be separate types (e.g. int and float) and of different length (e.g. input can be a single variable, output can be a vector. This is needed, for example, to compute PPFD, VPD, Tmp and leafarea in Fluxh) 
//! - In the current implementation, crowns below 3m in crown depth are simply treated as cylinders, this could be changed in future implementations 
template <typename I, typename O, typename M, typename F>
void LoopLayerUpdateCrownStatistic_template(int row_center, int col_center, float height, float CR, float CD, float fraction_filled_target, int shell_fromtop, float GetRadiusLayer(float, float, float), I CrownStatistic_input, O &CrownStatistic_output, M ModifyCrownStatistic_input, F UpdateCrownStatistic_output){
  int crown_top = int(height);
  
  // we start out with 0 actually filled voxels. As a result, the first voxel will always be filled 
  float fraction_filled_actual = 0.0;    
  if(CD <= 3.0){
    I CrownStatistic_input_modified;
    ModifyCrownStatistic_input(CrownStatistic_input, CrownStatistic_input_modified, CD, height, shell_fromtop);
    
    int crown_intarea_previous = 0;
    int crown_intarea = GetCrownIntarea(CR);
    int layer_cylinder = crown_top - shell_fromtop;
    CircleAreaUpdateCrownStatistic_template(row_center, col_center, crown_intarea_previous, crown_intarea, fraction_filled_target, fraction_filled_actual, layer_cylinder, CrownStatistic_input_modified,CrownStatistic_output, UpdateCrownStatistic_output);
  }
  else{
    // This function computes the extent of the crown at every height layer, given a specific function 
    // it separates out the innermost sector (a slowly increasing cylinder), and the surrounding parts of the crown 
    // first the metrics with respect to the internal crown structure (i.e. z coordinate with respect to crown base) 
    float crownshell_base = height - CD + 2.0;              // lower reference point for the crown slope function is two layers up from the crown base 
    float crownshell_extent = height - crownshell_base;          // this is the extent from the "base layer" to the top 
    float crownshell_extent_toplayer = floor(crownshell_extent);      // this is the extent to the lower limit of the toplayer 
    // then we translate the crown coordinates into discretised variables with respect to the absolute location in the voxel field, as needed for location in the voxel field, with layers defined from top to bottom 
    int height_innermost = crown_top - shell_fromtop;
    int height_toplayer = int(crownshell_base + crownshell_extent_toplayer) - shell_fromtop;
    int height_baselayer = int(crownshell_base + 1.0) - shell_fromtop;
    
    // now calculate the two modifications of the input statistic 
    I CrownStatistic_input_innermost;
    I CrownStatistic_input_outer;
    
    ModifyCrownStatistic_input(CrownStatistic_input, CrownStatistic_input_innermost, CD, height, shell_fromtop);
    ModifyCrownStatistic_input(CrownStatistic_input, CrownStatistic_input_outer, CD, crownshell_base, shell_fromtop);
    
    // now do calculations 
    // first the inner crown shell section that grows dynamically 
    int crown_intarea_previous = 0;
    
    float radius_innermost = GetRadiusLayer(CR, crownshell_extent, crownshell_extent_toplayer);
    int crown_intarea_innermost = GetCrownIntarea(radius_innermost);
    CircleAreaUpdateCrownStatistic_template(row_center, col_center, crown_intarea_previous, crown_intarea_innermost, fraction_filled_target, fraction_filled_actual, height_innermost, CrownStatistic_input_innermost,CrownStatistic_output, UpdateCrownStatistic_output);
    crown_intarea_previous = crown_intarea_innermost;
    
    // now loop through the outer crown shell cylinders 
    for(int h_outer = height_toplayer; h_outer >= height_baselayer; h_outer--){
      // calculating the radius of the current layer depending on the respective slopes, to be replaced by function  
      //float radius_height = CR - crown_slope * (h_outer - height_baselayer);    // for the lowest layer, i.e. h == height_baselayer, radius = t_Crown_Radius 
      int extent_layerouter = h_outer - height_baselayer;
      float radius_height = GetRadiusLayer(CR, crownshell_extent, extent_layerouter);
      int crown_intarea = GetCrownIntarea(radius_height);
      CircleAreaUpdateCrownStatistic_template(row_center, col_center, crown_intarea_previous, crown_intarea, fraction_filled_target, fraction_filled_actual, h_outer, CrownStatistic_input_outer, CrownStatistic_output,UpdateCrownStatistic_output);
      crown_intarea_previous = crown_intarea;
    }
  }
}

//! - Global function: the following function is not a member function of the Tree class object, but operates at the tree level and could be converted to member functions. They're therefore defined next to the other tree level functions. Whether conversion to Tree members adds any benefit in terms of performance should be tested. 
//! - this is the template that simulates the actual circling through a crown shell, between a given start and stop position (i.e. between a starting crown area and a stopping crown area) 
//! - If the crown shells are bent, i.e. extend across several canopy layers, the starting position within a lower layer is the stopping position of the layer just above 
template <typename I, typename O, typename F>
void CircleAreaUpdateCrownStatistic_template(int row_center, int col_center, int pos_start, int pos_end, float fraction_filled_target, float &fraction_filled_actual, int height_layer, I CrownStatistic_input, O &CrownStatistic_output, F UpdateCrownStatistic){
  
  for(int i = pos_start; i < pos_end; i++){
    if(fraction_filled_actual > fraction_filled_target){
      fraction_filled_actual = (fraction_filled_actual * float(i))/(float(i) + 1.0);
    }
    else{
      fraction_filled_actual = (fraction_filled_actual * float(i) + 1.0)/(float(i) + 1.0);
      
      int site_relative = LookUp_Crown_site[i];
      int row, col;
      row = row_center + site_relative/51 - 25;
      col = col_center + site_relative%51 - 25;
      
      //            // option for future implementations: choose a direction of the loop (to randomize patterns, etc.) 
      //            // to create less homogeneous and more realistic crowns, this can, for example, be varied between crown layers 
      //            if(direction == 0 || direction == 1){row = row_center + site_relative/51 - 25;}
      //            else {row = row_center - site_relative/51 + 25;}
      //            if(direction == 0 || direction == 2){col = col_center + site_relative%51 - 25;}
      //            else {col = col_center - site_relative%51 + 25;}
      
      if(row >= 0 && row < rows && col >= 0 && col < cols){
        int site=col+cols*row;
        UpdateCrownStatistic(height_layer, site, CrownStatistic_input, CrownStatistic_output);
      }
    }
  }
}

// Global function: linear decrease of crown radius 
float GetRadiusSlope(float CR, float crown_extent, float crown_position){
  float crown_slope = CR * (1.0 - shape_crown) / crown_extent;
  float radius = CR - crown_slope * float(crown_position);
  return(radius);
}

// Global function: not currently used, but returns the input radius 
float GetRadiusCylinder(float CR, float crown_extent, float crown_position){
  return(CR);
}

// Global function: converts floating point crown area into integer value, imposing lower and upper limits 
int GetCrownIntarea(float crown_radius){
  // crown area 
  float crown_area = PI * crown_radius * crown_radius;
  int crown_intarea = int(crown_area);     // floor of crown_area to bound area accumulation
  crown_intarea = max(crown_intarea,1);    // minimum area of crown (1)
  crown_intarea = min(crown_intarea,1963); // maximum area of crown (radius 25), int(3.14*25*25)
  return(crown_intarea);
}

// Global function: deduces within-crown densities from LAI with a gradient from 50% in top layer to 25% in belowtop and 25% in all shells underneath (1 layer for umbrella-like shape) 
void GetDensitiesGradient(float LAI, float CD, float &dens_top, float &dens_belowtop, float &dens){
  if(CD < 2.0){
    dens_top = dens_belowtop = dens = LAI/ CD;
  }
  else if(CD < 3.0){
    dens_top = 0.5 * LAI;
    dens_belowtop = dens = 0.5 * LAI / (CD - 1.0);
  }
  else{
    dens_top = 0.5 * LAI;
    dens_belowtop = 0.25 * LAI;
#ifdef CROWN_UMBRELLA
    dens = 0.25 * LAI;
#else
    dens = 0.25 * LAI / (CD - 2.0);
#endif
  }
}

// Global function: deduces within-crown density from LAI, assuming uniform leaf distribution 
void GetDensityUniform(float LAI, float CD, float &dens){
#ifdef CROWN_UMBRELLA
  float crownshells_limit = fminf(CD, 3.0);
  dens = LAI / crownshells_limit;
#else
  dens = LAI / CD;
#endif
}

// Global function: dummy function when no modification is needed 
void ModifyNoinput(float noinput, float &dens_layer, float CD, float height, int layer_fromtop){
}

// Global function: a modifying function that converts LAI to the density of a specific layer, using the GetDensity functions
//! - modifier for GPP calculation where we need the leaves per layer to weight our results 
//! - LAI is the input, dens_layer the output 
void LAI2dens(float LAI, float &dens_layer, float CD, float height, int layer_fromtop){
  
  int crown_top = int(height);
  int crown_base = int(height - CD);
  float dens_top, dens_belowtop, dens;
#ifdef LAI_gradient
  GetDensitiesGradient(LAI, CD, dens_top, dens_belowtop, dens);
#else
  GetDensityUniform(LAI, CD, dens);
  dens_top = dens_belowtop = dens;
#endif
  
  if(CD < 3.0 && crown_top == crown_base){
    dens_layer = dens_top * CD;
  }
  else if(CD < 3.0 && (crown_top - layer_fromtop == crown_base)){
    float fraction_belowbase = float(crown_base+1) - (height - CD);
    dens_layer = dens * fraction_belowbase;
  }
  else{
    float fraction_layer = height - floor(height); // this is the fraction that each layer apart from the topmost layer will extend into the voxel above 
    float fraction_layer_fromabove = 1.0 - fraction_layer; // the inverse of the fraction above 
    
    if(layer_fromtop == 0) dens_layer = dens_top * fraction_layer;
    else if(layer_fromtop == 1) dens_layer = dens_top * fraction_layer_fromabove + dens_belowtop * fraction_layer;
    else if(layer_fromtop == 2) dens_layer = dens_belowtop * fraction_layer_fromabove + dens * fraction_layer;
    else dens_layer = dens * fraction_layer_fromabove;
  }
}

// Global function: update of LAI3D field, called by CalcLAI() 
void UpdateLAI3D(int height, int site, float dens, float &LA_cumulated){
  LAI3D[height][site+SBORD] += dens;
  LA_cumulated += dens;
}

// Global function: remove outliers in canopy height model (CHM); vector option
#ifdef CHM_SPIKEFREE
void UpdateCHMvector(int height, int site, float noinput, vector<int> &chm){
  if(chm[site] < height) chm[site] = height;
}
// Global function: remove outliers in canopy height model (CHM)
void UpdateCHM(int height, int site, float noinput, int *chm){
  if(chm[site] < height) chm[site] = height;
}
#endif

// Global function: PPFD retrieval for function leafarea_max() 
void GetPPFDabove(int height, int site, float noinput, float (&ppfd_CA)[2]){
  // First get voxel field densities 
  float absorb_prev = LAI3D[height+1][site+SBORD];
  int intabsorb = CalcIntabsorb(absorb_prev);
  
  // Obtain PPFD for the voxel, and also record the circled area 
  ppfd_CA[0] += WDailyMean * LookUp_flux[intabsorb];
  ppfd_CA[1] += 1.0; // add area 
}

// Global function: calculates the canopy environment
//! - this function adds to the environmental variables provided in canopy_environment_cumulated 
//! - the PPFD, VPD, Tmp and leafarea_layer retrieval function for Fluxh() 
void GetCanopyEnvironment(int height, int site, float dens, float (&canopy_environment_cumulated)[4]){
  // first get voxel field densities 
  float absorb_prev = LAI3D[height+1][site+SBORD];
  float absorb_curr = LAI3D[height][site+SBORD];
  float absorb_delta = absorb_curr - absorb_prev;
  if(absorb_delta < 0.0) absorb_delta = 0.0;    // eliminate rounding errors 
  int intabsorb = CalcIntabsorb(absorb_prev, absorb_delta);
  
  // Obtain PPFD, VPD and T for the voxel 
  float PPFD_voxel = WDailyMean * LookUp_flux_absorption[intabsorb];
  float VPD_voxel = VPDDailyMean * LookUp_VPD[intabsorb];
  float T_voxel = tDailyMean - LookUp_T[intabsorb];
  
  // Add the three variables up, weighted by leaf density inside voxel
  canopy_environment_cumulated[0] += dens;
  canopy_environment_cumulated[1] += PPFD_voxel * dens;
  canopy_environment_cumulated[2] += VPD_voxel * dens;
  canopy_environment_cumulated[3] += T_voxel * dens;
}

// Global function: calculates packing densities 
void AddCrownVolumeLayer(int row_center, int col_center, float height, float CR, float CD, int crownvolume[70]){
  int crown_top = int(height);
  int crown_base = int(height-CD);
  if(CD <= 3.0){
    // for the smallest crowns it is simply cylinder rings being filled up 
    int crown_intarea = GetCrownIntarea(CR);
    for(int h = crown_top; h >= crown_base; h--) crownvolume[h] += crown_intarea;
  }
  else{
    // For the rest of the crown, we go through different crown shells. We separate out the innermost sector (a slowly increasing cylinder), and the surrounding parts of the crown 
    // first the metrics with respect to the internal crown structure (i.e. z coordinate with respect to crown base) 
    float crownshell_base = height - CD + 2.0;              // lower reference point for the crown slope function is two layers up from the crown base 
    float crownshell_extent = height - crownshell_base;          // this is the extent from the "base layer" to the top 
    float crownshell_extent_toplayer = floor(crownshell_extent);      // this is the extent to the lower limit of the toplayer 
    // then we translate the crown coordinates into discretised variables with respect to the absolute location in the voxel field, as needed for location in the voxel field, with layers defined from top to bottom 
    int shell_fromtop = 0;
    int height_innermost = crown_top - shell_fromtop;
    int height_toplayer = int(crownshell_base + crownshell_extent_toplayer) - shell_fromtop;
    int height_baselayer = int(crownshell_base + 1.0) - shell_fromtop;
    
    // now do calculations 
    // first the inner crown shell section that grows dynamically 
    float radius_innermost = GetRadiusSlope(CR, crownshell_extent, crownshell_extent_toplayer);
    int crown_intarea_innermost = GetCrownIntarea(radius_innermost);
    for(int h = height_innermost; h >= crown_base; h--){
      crownvolume[h] += crown_intarea_innermost;
    }
    // now loop through the outer crown shell cylinders 
    for(int h_outer = height_toplayer; h_outer >= crown_base; h_outer--){
      // calculating the radius of the current layer depending on the respective slopes, to be replaced by function  
      //float radius_height = CR - crown_slope * (h_outer - height_baselayer);    // for the lowest layer, i.e. h == height_baselayer, radius = t_Crown_Radius 
      int extent_layerouter = max(h_outer - height_baselayer,0);                  // we also fill up underneath the baselayer 
      float radius_height = GetRadiusSlope(CR, crownshell_extent, extent_layerouter);
      int crown_intarea = GetCrownIntarea(radius_height);
      
      crownvolume[h_outer] += (crown_intarea - crown_intarea_innermost);
    }
  }
}
#endif

//###########################################
//###########################################
//###########     MAIN PROGRAM    ###########
//###########################################
//###########################################

//' @title TROLL C++
//' 
//' @description
//' TROLL C++.
//' 
//' @name trollCpp
//' 
//' @param global_file std::string. global parameters file.
//' @param climate_file std::string. climate file.
//' @param species_file std::string. species file.
//' @param day_file std::string. daytime file.
//' @param forest_file std::string. forest file.
//' @param output_file std::string. output folder.
//' 
//' @return int.
//' 
//' @examples
//' \dontrun{
//' trollCpp(global_file = "test/test_input_global.txt", 
//'          climate_file = "test/test_input_climate.txt",
//'          species_file = "test/test_input_species.txt", 
//'          day_file = "test/test_input_daily.txt", 
//'          forest_file = "NULL", 
//'          output_file = "test")
//' }
//' 
//' @export
// [[Rcpp::export]]
void trollCpp(
    std::string global_file,
    std::string climate_file,
    std::string species_file,
    std::string day_file,
    std::string forest_file,
    std::string output_file
) {

  // From Rcpp acceptable input to TROLL char*
  bufi = &global_file[0] ;
  bufi_climate = &climate_file[0] ;
  bufi_species = &species_file[0] ;
  bufi_daytimevar = &day_file[0] ;
  bufi_data = &forest_file[0] ;
  buf = &output_file[0] ;
  
  if(_DAYTIMELIGHT == 1) Rcout << "Activated Module: Dailylight" << endl;
  if(_GPPcrown == 1) Rcout << "Activated Module: FastGPP" << endl;
  if(_BASICTREEFALL == 1) Rcout << "Activated Module: BASICTREEFALL" << endl;
  if(_NDD == 1) Rcout << "Activated Module: NDD" << endl;
  if(_SEEDTRADEOFF == 1) Rcout << "Activated Module: SEEDTRADEOFF" << endl;
  if(_FromData == 1) Rcout << "Activated Module: FromData" << endl;
  if(_OUTPUT_reduced == 1) Rcout << "Activated Module: OUTPUT_reduced" << endl;
  
  //!*********************
  //!** Initializations **
  //!*********************
#ifdef MPI   // Lookup processor number / total number of processors 
  MPI_Init(&argc,&argv);
  MPI_Comm_rank(MPI_COMM_WORLD,&p_rank);
  MPI_Comm_size(MPI_COMM_WORLD,&size);
#else
  mpi_rank = 0;
  mpi_size = 1;
#endif
  easympi_rank = 0;
  
  // for(int argn=1;argn<argc;argn++){ // Arguments of the input and output files 
  //   if(*argv[argn] == '-'){
  //     switch(*(argv[argn]+1)){
  //     case 'i':
  //       bufi = argv[argn]+2;
  //       break;
  //     case 'd':                       // new v.3.0; initialisation of daytime variation in a separate file ('d' for daytime variation) 
  //       bufi_daytimevar = argv[argn]+2;
  //       break;
  //     case 'm':                       // new v.2.4; initialisation of climate parameters from separate file ('m' for meterology) 
  //       bufi_climate = argv[argn]+2;
  //       break;
  //     case 'p':                       // new v.3.0; initialisation of soil parameters from separate file ('p' for pedology) 
  //       bufi_soil = argv[argn]+2;
  //       break;
  //     case 's':                       // new v.3.0; initialisation of species parameters from separate file 
  //       bufi_species = argv[argn]+2;
  //       break;
  //     case 'o':
  //       buf = argv[argn]+2;
  //       break;
  //     case 'f':                      // new v.2.3: initialisation from field, 'f' for "forest", "field data" 
  //       bufi_data = argv[argn]+2;
  //       break;
  //     case 'n':
  //       easympi_rank=atoi(argv[argn]+2); // new v.2.2 
  //     }
  //   }
  // }
  
  // Stuff for constant number generator
  const gsl_rng_type *Trandgsl;
  gsl_rng_env_setup();
  Trandgsl = gsl_rng_default;
  gslrand = gsl_rng_alloc (Trandgsl);
  
  Rcout << "Easy MPI rank: " << easympi_rank << endl;
  
  unsigned long int t = (unsigned long int) time(NULL);
  unsigned long int seed = 3*t + 2*(easympi_rank+1)+1;
  
  if(_NONRANDOM == 1) seed = 1;
  
  gsl_rng_set(gslrand, seed);
  
  Rcout<< "On proc #" << easympi_rank << " seed: " << seed << endl;
  
  // input files
  sprintf(inputfile,"%s",bufi);
  sprintf(inputfile_daytimevar,"%s",bufi_daytimevar);
  sprintf(inputfile_climate,"%s",bufi_climate);
  sprintf(inputfile_soil,"%s",bufi_soil);
  sprintf(inputfile_species,"%s",bufi_species);
  if(_FromData){
    sprintf(inputfile_data,"%s",bufi_data);
  }
  
  sprintf(outputinfo,"%s_%i_par.txt",buf, easympi_rank); // Files with general output info 
  Rcout<< "On proc #" << easympi_rank << " seed: " << seed <<  endl;
  out.open(outputinfo, ios::out);
  if(!out) Rcerr<< "ERROR with par file"<< endl;
  sprintf(outputinfo,"%s_%i_info.txt",buf, easympi_rank);
  out2.open(outputinfo, ios::out);
  if(!out2) Rcerr<< "ERROR with info file"<< endl;
  
  Initialise();           // Read global parameters 
  AllocMem();             // Memory allocation 
  
#ifdef Output_ABC
  InitialiseABC();
#endif
  
  if(_FromData){
    InitialiseFromData();   // Initial configuration of the forest, read from data 
  }
  
  BirthInit();            // Initial configuration of the forest 
  out.close();
  
  Rcout << "klight is: " << klight << endl;
  Rcout << "CO2 concentration is: " << Cair << endl;
  Rcout << "Number of species: " << nbspp << endl << endl;
  
  for(int spp = 1; spp <= nbspp; spp++){
    double prob_species = double(S[spp].s_nbext);
    //Rcout << "prob_species: " << prob_species << endl;
    p_species[spp - 1] = prob_species;
  }
  
  //!*********************
  //!** Evolution loop  **
  //!*********************
  
  double start_time,stop_time, duration=0.0;           // for simulation duration 
  stop_time = clock();
  for(iter=0;iter<nbiter;iter++) {
    start_time = stop_time;
    
    Evolution();
    stop_time = clock();
    duration +=fmaxf(stop_time-start_time,0.0);
    
    
#ifdef Output_ABC
    int timespan_abc = 10 * iterperyear;     // every 10 years, modified in v.3.0 
    int last_abc = ((nbiter+5)/timespan_abc);
    last_abc *= timespan_abc;
    last_abc += -5;
    if((iter+5)%timespan_abc == 0 || iter == last_abc - 43){
      // simulated lidar, based on data from Nouragues ALS, assuming NIR laser 
      float transmittance_nir = 0.4;  // transmittance of leaves in near infrared, used to calculate chances of laser hit
      float mean_beam = 12.0;         // avg number of beams that reach top of canopy
      float sd_beam = 5.0;            // standard deviation of those
      
      UpdateTransmittanceCHM_ABC(mean_beam, sd_beam, klight, transmittance_nir);
      OutputABC();
      UpdateDBHtrackingABC();
    }
#endif
    
    //***** Final Outputs *****
    if(iter == nbiter-1){
      if(!_OUTPUT_reduced) OutputSnapshot(output[10], 1, 0.01);    // Final Pattern, for trees > 0.01 m DBH
      else OutputSnapshot(output[10], 1, 0.1);                     // Final Pattern, for trees > 0.1 m DBH
#ifdef OUTPUT_ABC
#ifdef CHM_SPIKEFREE
      OutputCHM(output[31]);
#endif
#endif
    }
  }
  
  //***********************
  //** End of simulation **
  //***********************
  float durf = duration/double(CLOCKS_PER_SEC);        // output of the effective CPU time 
#ifdef MPI
  MPI_Reduce(&durf,&durf,1,MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD);
#endif
  if(!mpi_rank) {
    Rcout << "\n";
#ifdef MPI
    out2 << "Number of processors : "<< mpi_size << "\n";
#endif
    out2 << "Average computation time : "<< durf/float(mpi_size) << " seconds.\n";
    out2 << "End of simulation.\n";
    Rcout << "\nNumber of processors : "<< mpi_size << "\n";
    Rcout << "Average computation time : "<< durf/float(mpi_size) << " seconds.\n";
    Rcout << "End of simulation.\n";
  }
  out2.close();
  
  //Free dynamic memory  //! added in oct2013 
  FreeMem();
#ifdef easyMPI
  MPI::Finalize();
#endif
  // exit(0);
}

//##########################################
//###########################################
//######  Initialisation routines    ########
//###########################################
//###########################################

//! Global function: assigns input values to parameters 
//! - Also check whether parameters are correctly specified, within limits and replace by default 
//! - !!!: TO IMPLEMENT: possibility of throwing an error instead of default value 
template <typename N>
void SetParameter(string &parameter_name, string &parameter_value, N &parameter, N parameter_min, N parameter_max, N parameter_default, bool quiet) {
  // idea for checking whether float/int from https://stackoverflow.com/questions/447206/c-isfloat-function 
  istringstream iss(parameter_value);
  N numeric;
  iss >> numeric;
  
  // Check if the entire string was consumed and if either failbit or badbit is set
  bool isnumeric = iss.eof() && !iss.fail();
  if(isnumeric){
    if(numeric >= parameter_min && numeric <= parameter_max){
      parameter = numeric;
      if(!quiet) Rcout << parameter_name << ": " << parameter << endl;
    } else {
      parameter = parameter_default;
      Rcout << "Warning. Value provided for '" << parameter_name << "' (" << numeric << ") is outside the allowed range (" << parameter_min << ", " << parameter_max << "). Set to default: " << parameter_default << endl;
    }
  } else {
    parameter = parameter_default;
    Rcout << "Warning. Value provided for '" << parameter_name << "' (" << parameter_value << ") is not a " << typeid(numeric).name() << ". Set to default: " << parameter_default << endl;
  }
}

//! Global function: tests input values for parameters 
//! - for strings, no minimum/maximum and no check for type necessary 
void SetParameter(string &parameter_name, string &parameter_value, string &parameter, string parameter_default, bool quiet) {
  if(!parameter_value.empty()){
    parameter = parameter_value;
    if(!quiet) Rcout << parameter_name << ": " << parameter << endl;
  } else {
    parameter = parameter_default;
    Rcout << "Warning. String for '" << parameter_name << "' is empty" << ". Set to default: '" << parameter_default << "'" << endl;
  }
}

//! Global function: This function specifies limits and defaults for global parameters 
void AssignValueGlobal(string parameter_name, string parameter_value){
  // we set parameters to values that have been read, or to their defaults, if outside of range or not the right type 
  bool quiet = 1; //! only applies to successful initialization, warnings are always given
  
  if(parameter_name == "cols"){
    SetParameter(parameter_name, parameter_value, cols, 0, INT_MAX, 400, quiet);
  } else if(parameter_name == "rows"){
    SetParameter(parameter_name, parameter_value, rows, 0, INT_MAX, 400, quiet);
  } else if(parameter_name == "HEIGHT"){
    SetParameter(parameter_name, parameter_value, HEIGHT, 0, 150, 70, quiet);
  } else if(parameter_name == "length_dcell"){
    SetParameter(parameter_name, parameter_value, length_dcell, 0, INT_MAX, 25, quiet);
  } else if(parameter_name == "nbiter"){
    SetParameter(parameter_name, parameter_value, nbiter, 0, INT_MAX, 6000, quiet);
  } else if(parameter_name == "NV"){
    SetParameter(parameter_name, parameter_value, NV, 0.0f, float(INT_MAX), 1.0f, quiet);
  } else if(parameter_name == "NH"){
    SetParameter(parameter_name, parameter_value, NH, 0.0f, float(INT_MAX), 1.0f, quiet);
  } else if(parameter_name == "nbout"){
    SetParameter(parameter_name, parameter_value, nbout, 0, INT_MAX, 4, quiet);
  } else if(parameter_name == "p_nonvert"){
    SetParameter(parameter_name, parameter_value, p_nonvert, 0.0f, 1.0f, 0.05f, quiet);
  } else if(parameter_name == "SWtoPPFD"){
    SetParameter(parameter_name, parameter_value, SWtoPPFD, 0.0f, 5.0f, 2.27f, quiet);
  } else if(parameter_name == "klight"){
    SetParameter(parameter_name, parameter_value, klight, 0.0f, 1.0f, 0.5f, quiet);
  } else if(parameter_name == "absorptance_leaves"){
    SetParameter(parameter_name, parameter_value, absorptance_leaves, 0.0f, 1.0f, 0.9f, quiet);
  } else if(parameter_name == "theta"){
    SetParameter(parameter_name, parameter_value, theta, 0.0f, 10.0f, 0.7f, quiet); // FF: realistic upper bound?
  } else if(parameter_name == "phi"){
    SetParameter(parameter_name, parameter_value, phi, 0.0f, 1.0f, 0.06f, quiet);
  } else if(parameter_name == "g1"){
    SetParameter(parameter_name, parameter_value, g1, 0.0f, 1000.0f, 3.77f, quiet);
  } else if(parameter_name == "vC"){
    SetParameter(parameter_name, parameter_value, vC, 0.0f, 1.0f, 0.05f, quiet);
  } else if(parameter_name == "DBH0"){
    SetParameter(parameter_name, parameter_value, DBH0, 0.0f, 2.5f, 0.005f, quiet);
  } else if(parameter_name == "H0"){
    SetParameter(parameter_name, parameter_value, H0, 0.0f, 100.0f, 0.95f, quiet);
  } else if(parameter_name == "CR_min"){
    SetParameter(parameter_name, parameter_value, CR_min, 0.0f, 50.0f, 0.2f, quiet);
  } else if(parameter_name == "CR_a"){
    SetParameter(parameter_name, parameter_value, CR_a, 0.0f, 5.0f, 2.13f, quiet);
  } else if(parameter_name == "CR_b"){
    SetParameter(parameter_name, parameter_value, CR_b, 0.0f, 50.0f, 0.63f, quiet);
  } else if(parameter_name == "CD_a"){
    SetParameter(parameter_name, parameter_value, CD_a, 0.0f, 0.5f, 0.0f, quiet);
  } else if(parameter_name == "CD_b"){
    SetParameter(parameter_name, parameter_value, CD_b, 0.0f, 1.0f, 0.2f, quiet);
  } else if(parameter_name == "CD0"){
    SetParameter(parameter_name, parameter_value, CD0, 0.0f, 50.0f, 0.1f, quiet);
  } else if(parameter_name == "shape_crown"){
    SetParameter(parameter_name, parameter_value, shape_crown, 0.0f, 1.0f, 1.0f, quiet);
  } else if(parameter_name == "dens"){
    SetParameter(parameter_name, parameter_value, dens, 0.0f, 10.0f, 1.0f, quiet);
  } else if(parameter_name == "fallocwood"){
    SetParameter(parameter_name, parameter_value, fallocwood, 0.0f, 1.0f, 0.35f, quiet);
  } else if(parameter_name == "falloccanopy"){
    SetParameter(parameter_name, parameter_value, falloccanopy, 0.0f, 1.0f, 0.25f, quiet);
  } else if(parameter_name == "Cseedrain"){
    SetParameter(parameter_name, parameter_value, Cseedrain, 0.0f, 1000000.0f, 50000.0f, quiet);
  } else if(parameter_name == "nbs0"){
    SetParameter(parameter_name, parameter_value, nbs0, 0.0f, 10000.0f, 10.0f, quiet);
  } else if(parameter_name == "sigma_height"){
    SetParameter(parameter_name, parameter_value, sigma_height, 0.0f, 1.0f, 0.19f, quiet);
  } else if(parameter_name == "sigma_CR"){
    SetParameter(parameter_name, parameter_value, sigma_CR, 0.0f, 1.0f, 0.29f, quiet);
  } else if(parameter_name == "sigma_CD"){
    SetParameter(parameter_name, parameter_value, sigma_CD, 0.0f, 1.0f, 0.0f, quiet);
  } else if(parameter_name == "sigma_P"){
    SetParameter(parameter_name, parameter_value, sigma_P, 0.0f, 1.0f, 0.24f, quiet);
  } else if(parameter_name == "sigma_N"){
    SetParameter(parameter_name, parameter_value, sigma_N, 0.0f, 1.0f, 0.12f, quiet);
  } else if(parameter_name == "sigma_LMA"){
    SetParameter(parameter_name, parameter_value, sigma_LMA, 0.0f, 1.0f, 0.24f, quiet);
  } else if(parameter_name == "sigma_wsg"){
    SetParameter(parameter_name, parameter_value, sigma_wsg, 0.0f, 0.5f, 0.06f, quiet);
  } else if(parameter_name == "sigma_dbhmax"){
    SetParameter(parameter_name, parameter_value, sigma_dbhmax, 0.0f, 1.0f, 0.05f, quiet);
  } else if(parameter_name == "corr_CR_height"){
    SetParameter(parameter_name, parameter_value, corr_CR_height, -1.0f, 1.0f, 0.0f, quiet);
  } else if(parameter_name == "corr_N_P"){
    SetParameter(parameter_name, parameter_value, corr_N_P, -1.0f, 1.0f, 0.65f, quiet);
  } else if(parameter_name == "corr_N_LMA"){
    SetParameter(parameter_name, parameter_value, corr_N_LMA, -1.0f, 1.0f, -0.43f, quiet);
  } else if(parameter_name == "corr_P_LMA"){
    SetParameter(parameter_name, parameter_value, corr_P_LMA, -1.0f, 1.0f, -0.39f, quiet);
  } else if(parameter_name == "leafdem_resolution"){
    SetParameter(parameter_name, parameter_value, leafdem_resolution, 0, INT_MAX, 30, quiet);
  } else if(parameter_name == "p_tfsecondary"){
    SetParameter(parameter_name, parameter_value, p_tfsecondary, 0.0f, 1.0f, 1.0f, quiet);
  } else if(parameter_name == "hurt_decay"){
    SetParameter(parameter_name, parameter_value, hurt_decay, 0.0f, 1.0f, 0.0f, quiet);
  } else if(parameter_name == "crown_gap_fraction"){
    SetParameter(parameter_name, parameter_value, crown_gap_fraction, 0.0f, 1.0f, 0.0f, quiet);
  } else if(parameter_name == "m"){
    SetParameter(parameter_name, parameter_value, m, 0.0f, 1.0f, 0.013f, quiet);
  } else if(parameter_name == "m1"){
    SetParameter(parameter_name, parameter_value, m1, 0.0f, 1.0f, 0.013f, quiet);
  } else if(parameter_name == "Cair"){
    SetParameter(parameter_name, parameter_value, Cair, 0.0f, 1000000.0f, 400.0f, quiet);
  } else if(parameter_name == "_LL_parameterization"){
    SetParameter(parameter_name, parameter_value, _LL_parameterization, bool(0), bool(1), bool(1), quiet);
  } else if(parameter_name == "_LA_regulation"){
    SetParameter(parameter_name, parameter_value, _LA_regulation, 0, 2, 2, quiet);
  } else if(parameter_name == "_sapwood"){
    SetParameter(parameter_name, parameter_value, _sapwood, bool(0), bool(1), bool(1), quiet);
  } else if(parameter_name == "_seedsadditional"){
    SetParameter(parameter_name, parameter_value, _seedsadditional, bool(0), bool(1), bool(0), quiet);
  } else if(parameter_name == "_OUTPUT_reduced"){
    SetParameter(parameter_name, parameter_value, _OUTPUT_reduced, bool(0), bool(1), bool(0), quiet);
  } else if(parameter_name == "_FromData"){
    SetParameter(parameter_name, parameter_value, _FromData, bool(0), bool(1), bool(0), quiet);
  } else if(parameter_name == "_NONRANDOM"){
    SetParameter(parameter_name, parameter_value, _NONRANDOM, bool(0), bool(1), bool(0), quiet);
  }
  
  // !!!: TODO, implement NDD parameters
  /* if (_NDD) {
   In >> Rndd; In.getline(buffer,128,'\n');
   In >> deltaR; In.getline(buffer,128,'\n');
   In >> deltaD; In.getline(buffer,128,'\n');
  } */
  
}

//! Global function: This function provides limits and defaults for species-specific parameters 
void AssignValueSpecies(Species &S, string parameter_name, string parameter_value){
  //! we set parameters to values that have been read, or to their defaults, if outside of range or not the right type 
  bool quiet = 1; // only applies to successful initialization, warnings are always given
  
  if(parameter_name == "s_name"){
    SetParameter(parameter_name, parameter_value, S.s_name, "indet_indet", quiet);
  } else if(parameter_name == "s_LMA"){
    SetParameter(parameter_name, parameter_value, S.s_LMA, 0.0f, 1000.0f, 100.0f, quiet);
  } else if(parameter_name == "s_Nmass"){
    SetParameter(parameter_name, parameter_value, S.s_Nmass, 0.0f, 1.0f, 0.02f, quiet);
  } else if(parameter_name == "s_Pmass"){
    SetParameter(parameter_name, parameter_value, S.s_Pmass, 0.0f, 1.0f, 0.0005f, quiet);
  } else if(parameter_name == "s_wsg"){
    SetParameter(parameter_name, parameter_value, S.s_wsg, 0.0f, 1.5f, 0.6f, quiet);
  } else if(parameter_name == "s_dbhmax"){
    SetParameter(parameter_name, parameter_value, S.s_dbhmax, 0.0f, 2.5f, 0.5f, quiet);
  } else if(parameter_name == "s_hmax"){
    SetParameter(parameter_name, parameter_value, S.s_hmax, 0.0f, 100.0f, 50.0f, quiet);
  } else if(parameter_name == "s_ah"){
    SetParameter(parameter_name, parameter_value, S.s_ah, 0.0f, 10.0f, 0.3f, quiet);
  } else if(parameter_name == "s_seedmass"){
    SetParameter(parameter_name, parameter_value, S.s_seedmass, 0.0f, 10000.0f, 1.0f, quiet);
  } else if(parameter_name == "s_regionalfreq"){
    SetParameter(parameter_name, parameter_value, S.s_regionalfreq, 0.0f, 1.0f, 1.0f, quiet);
  } else if(parameter_name == "s_tlp"){
    SetParameter(parameter_name, parameter_value, S.s_tlp, -10.0f, 0.0f, -2.0f, quiet);
  } else if(parameter_name == "s_drymass"){
    SetParameter(parameter_name, parameter_value, S.s_drymass, 0.0f, 100.0f, 0.5f, quiet);
  }
}

//! Global function: This function reads inputs from the general input file 
//! - Also sets up initial values for some parameters computed from input parameters
//! - !!!: TO IMPLEMENT: automatic stop of program under error to avoid running empty simulations on cluster etc. 
//! - !!!: TO IMPLEMENT: separate error messages (i.e. no input file provided, empty input file, etc.)
void ReadInputGeneral(){
  fstream In(inputfile, ios::in);
  if(In){
    string parameter_names[56] = {"cols","rows","HEIGHT","length_dcell","nbiter","NV","NH","nbout","p_nonvert","SWtoPPFD","klight","absorptance_leaves","theta","phi","g1","vC","DBH0","H0","CR_min","CR_a","CR_b","CD_a","CD_b","CD0","shape_crown","dens","fallocwood","falloccanopy","Cseedrain","nbs0","sigma_height","sigma_CR","sigma_CD","sigma_P","sigma_N","sigma_LMA","sigma_wsg","sigma_dbhmax","corr_CR_height","corr_N_P","corr_N_LMA","corr_P_LMA","leafdem_resolution","p_tfsecondary","hurt_decay","crown_gap_fraction","m","m1","Cair","_LL_parameterization","_LA_regulation","_sapwood","_seedsadditional", "_OUTPUT_reduced", "_FromData", "_NONRANDOM"};
    int nb_parameters = 56;
    vector<string> parameter_values(nb_parameters,"");
    
    Rcout << endl << "Reading in file: " << inputfile << endl;
    In.getline(buffer,256,'\n');
    string parameter_name, parameter_value;
    
    while (In >> parameter_name >> parameter_value){
      In.getline(buffer,256,'\n');
      for(int i = 0; i < nb_parameters; i++){
        if(parameter_name == parameter_names[i]) parameter_values[i] = parameter_value;
      }
    }
    // now we assign values 
    for(int i = 0; i < nb_parameters; i++){
      AssignValueGlobal(parameter_names[i], parameter_values[i]);
    }
    // update derived parameters 
    sites = rows*cols;
    sites_per_dcell = length_dcell*length_dcell;
    nbdcells = int(sites/sites_per_dcell);
    linear_nb_dcells = int(cols/length_dcell);
    Rcout << "rows: " << rows << " cols: " << cols << " HEIGHT: " << HEIGHT << endl;
    Rcout << "Number of dcells: " << nbdcells << endl;
    Rcout << "Lin number of dcells: " << linear_nb_dcells << endl;
#ifdef WATER
    i_sites_per_dcell=1.0/float(sites_per_dcell);
#endif
    LV = 1.0/NV;
    LH = 1.0/NH;
    if(nbout) freqout = nbiter/nbout;
    
    kpar = klight * absorptance_leaves; // kpar is the klight factor times the absorptance of leaves 
    // convert correlations to covariances
    cov_N_P = corr_N_P * sigma_N * sigma_P;
    cov_N_LMA = corr_N_LMA * sigma_N * sigma_LMA;
    cov_P_LMA = corr_P_LMA * sigma_P * sigma_LMA;
    
    if(cov_N_P == 0.0 || cov_N_LMA == 0.0 || cov_P_LMA == 0.0){
      Rcerr << "\nCovariance matrix N,P,LMA could not be decomposed. Using uncorrelated variation of trait values instead" << endl;
      covariance_status = 0;
    }
    else{
      Rcout << "Correlation status. corr_N_P: " << corr_N_P << " cov_N_LMA: " << corr_N_LMA << " corr_P_LMA: " << corr_P_LMA << endl;
      covariance_status = 1;
      // Initialise covariance matrix for N, P, LMA
      mcov_N_P_LMA = gsl_matrix_alloc(3,3);
      gsl_matrix_set(mcov_N_P_LMA,0,0,sigma_N*sigma_N);
      gsl_matrix_set(mcov_N_P_LMA,0,1,cov_N_P);
      gsl_matrix_set(mcov_N_P_LMA,0,2,cov_N_LMA);
      gsl_matrix_set(mcov_N_P_LMA,1,0,cov_N_P);
      gsl_matrix_set(mcov_N_P_LMA,1,1,sigma_P*sigma_P);
      gsl_matrix_set(mcov_N_P_LMA,1,2,cov_P_LMA);
      gsl_matrix_set(mcov_N_P_LMA,2,0,cov_N_LMA);
      gsl_matrix_set(mcov_N_P_LMA,2,1,cov_P_LMA);
      gsl_matrix_set(mcov_N_P_LMA,2,2,sigma_LMA*sigma_LMA);
      
      // Cholesky decomposition for multivariate draw
      Rcout << "\nCovariance matrix N,P,LMA: " << endl;
      for(int mrow=0; mrow<3;mrow++) Rcout << gsl_matrix_get(mcov_N_P_LMA, mrow, 0) << "\t" << gsl_matrix_get(mcov_N_P_LMA, mrow, 1) << "\t" << gsl_matrix_get(mcov_N_P_LMA, mrow, 2) << endl;
      gsl_linalg_cholesky_decomp1(mcov_N_P_LMA);
      Rcout << "\nCovariance matrix N,P,LMA (after Cholesky decomposition) " << endl;
      for(int mrow=0; mrow<3;mrow++) Rcout << gsl_matrix_get(mcov_N_P_LMA, mrow, 0) << "\t" << gsl_matrix_get(mcov_N_P_LMA, mrow, 1) << "\t" << gsl_matrix_get(mcov_N_P_LMA, mrow, 2) << endl;
      // allocating mean and result vectors for multivariate draw
      mu_N_P_LMA = gsl_vector_alloc(3);
      for(int j=0;j<3;j++) gsl_vector_set(mu_N_P_LMA,j,0.0); // zero means
      variation_N_P_LMA = gsl_vector_alloc(3);
    }
    crown_gap_fraction = fmaxf(crown_gap_fraction,0.000001);     // crown_gap_fraction is prevented from becoming zero in order to avoid division by zero. Given that crown area is currently limited to 1963 (int(3.14 * 25.0 * 25.0), the lowest crown_gap_fraction that could potentially have an effect would be 1/1963, which is ~ 0.0005
    iCair = 1.0/Cair;
    DBH0 *= NH;
    H0 *= NV;
    CR_min *= NH;
    CD0 *= NV;
    alpha = 4.0*phi;
    // apparent quantum yield to electron transport in mol e-/mol photons see Mercado et al 2009 , the conversion of the apparent quantum yield in micromolCO2/micromol quantum into micromol e-/micxromol quantum is done by multipliyng by 4, since four electrons are needed to regenerate RuBP. alpha is fixed at 0.3 mol e-/mol photons in Medlyn et al 2002, but see equ8 and Appendix 1 in Farquahr et al 1980: it seems that alpha should vary with leaf thickness: there is a fraction of incident light which is lost by absorption by other leaf parts than the chloroplast lamellae, and this fraction f may increase with leaf thickness. With the values of the paper: alpha= 0.5*(1-f)=0.5*(1-0.23)=0.385, but this is a theoretical value and observations often report lower values (see ex discussion in medlyn et al 2005 Tree phsyiology, Lore Veeryckt values, Mercado et al 2009 Table 10, Domingues et al. 2014)
  }
  else{
    Rcout << "ERROR. General input file could not be read." << endl;
  }
}

//! Global function: This function reads inputs from the species input file 
void ReadInputSpecies(){
  Rcout << endl << "Reading in file: " << inputfile_species << endl;
  fstream InSpecies(inputfile_species, ios::in);
  if(InSpecies){
    // possible parameters to initialise vector<string> parameter_names{"s_name","s_LMA","s_Nmass","s_Pmass","s_wsg","s_dbhmax","s_hmax","s_ah","s_seedmass","s_regionalfreq","s_tlp","s_drymass"};
    //        int nb_parameters = int(parameter_names.size()); only works from C++11 onwards
    string parameter_names[12] = {"s_name","s_LMA","s_Nmass","s_Pmass","s_wsg","s_dbhmax","s_hmax","s_ah","s_seedmass","s_regionalfreq","s_tlp","s_drymass"};
    int nb_parameters = 12;
    
    // first get parameter names 
    string line;
    getline(InSpecies,line);
    istringstream firstlinestream(line);
    string parameter_name;
    vector<int> parameter_index;
    int nb_parameters_fromfile = 0;
    
    while(firstlinestream >> parameter_name){
      int index = -1;
      for(int i = 0; i < nb_parameters; i++){
        if(parameter_name == parameter_names[i]){
          index = i;
          nb_parameters_fromfile++;
        }
      }
      if(index == -1){
        Rcout << "Ignoring unknown parameter: " << parameter_name << ". Parameters must be one of:";
        for(int i = 0; i < nb_parameters; i++){
          Rcout << "\t" << parameter_names[i];
        }
        Rcout << endl;
      }
      parameter_index.push_back(index);
    }
    
    int nb_parameterlines = 0;
    Species species_dummy;  // !!!: currently, species at 0 is not initialized, for legacy reasons, maybe to change 
    S.push_back(species_dummy);
    
    // we go through all lines in the input file 
    while(getline(InSpecies, line)){
      istringstream linestream(line);
      
      // first we initialise the potential parameter arrays with empty parameter values 
      vector<string> parameter_values(nb_parameters,"");
      
      // we update the values from the file 
      int s = 0;
      string parameter_value;
      while(linestream >> parameter_value){
        int index = parameter_index[s];
        if(index >= 0){
          parameter_values[index] = parameter_value;
        }
        s++;
      }
      Species species_new;
      // now we assign values 
      for(int i = 0; i < nb_parameters; i++){
        AssignValueSpecies(species_new, parameter_names[i], parameter_values[i]);
      }
      S.push_back(species_new);
      nb_parameterlines++;
    }
    
    if(nb_parameterlines > 0){
      nbspp = nb_parameterlines;
      Rcout << "Successfully initialised " << nbspp << " species from file." << endl;
    } else {
      nbspp = 1;
      Rcout << "WARNING! Species file was empty. A default species is initialized." << endl;
      vector<string> parameter_values(nb_parameters,"");
      Species species_default;
      for(int i = 0; i < nb_parameters; i++){
        AssignValueSpecies(species_default, parameter_names[i], parameter_values[i]);
      }
      S.push_back(species_default);
    }
    
    for(int sp=1;sp<=nbspp;sp++) {
      S[sp].Init();
      //Rcout << S[sp].s_name << " dmax: " << S[sp].s_dbhmax << endl;
    }
    Rcout << "Successfully read in species file." << endl;
  } else {
    Rcout<< "ERROR with the species file" << endl;
  }
  
  InSpecies.close();
}

//! Global function: This function reads inputs from the environmental daily variation input file 
void ReadInputDailyvar(){
  // currently very simple reading in, only basic error checking 
  Rcout << endl << "Reading in file: " << inputfile_daytimevar << endl;
  
  fstream InDaily(inputfile_daytimevar, ios::in);
  
  if (InDaily) {
    InDaily.getline(buffer,256,'\n'); // read in header
    
    // we go through all lines in the input file 
    nbsteps_varday = 0;
    nbhours_covered = 0.0;
    
    string line;
    while(getline(InDaily, line)){
      istringstream linestream(line);
      
      float starttime_current, endtime_current, varday_light_current, varday_vpd_current, varday_T_current;
      linestream >> starttime_current >> endtime_current >> varday_light_current >> varday_vpd_current >> varday_T_current;
      float nbhours_current;
      if(endtime_current > starttime_current) nbhours_current = endtime_current - starttime_current;  // just calculate the time window
      else nbhours_current = 24.0 - (starttime_current - endtime_current); // i.e. if starttime is 23.5 and endtime is 0.5, etc. Calculate the inverse time window and then subtract from 24
      
      nbhours_covered += nbhours_current;
      varday_light.push_back(varday_light_current);
      varday_vpd.push_back(varday_vpd_current);
      varday_T.push_back(varday_T_current);
      //Rcout << "at: " << starttime_current << " vardaytime_light: " << varday_light[nbsteps_varday] << " vardaytime_vpd: " << varday_vpd[nbsteps_varday] << " vardaytime_T: "<< varday_T[nbsteps_varday] << endl;
      nbsteps_varday++;
    }
    
#ifdef WATER
    totday_light = 0.0;
    for (int i = 0; i < nbsteps_varday; i++) totday_light += varday_light[i];
#endif
    Rcout << "Read in: " << nbsteps_varday << " timesteps per day, covering " << nbhours_covered << " hours of the day." << endl;
    Rcout << "Successfully read in daytime variation file" << endl;
    inv_nbsteps_varday = 1.0/float(nbsteps_varday);
  }
  else{
    Rcout<< "ERROR with the daily variation file" << endl;
  }
}

//! Global function: This function reads inputs from the environmental monthly variation input file 
//! - v.3.0 of the code suppose that environment is periodic (a period = a year), if one want to make climate vary, with interannual variation and climate change along the simulation, one just need to provide the full climate input of the whole simulation (ie number of columns=iter and not iterperyear) and change iterperyear by nbiter here.
void ReadInputClimate(){
  Rcout << endl << "Reading in file: " << inputfile_climate << endl;
  
  fstream InClim(inputfile_climate, ios::in);
  
  if(InClim){
    // we go through all lines in the input file 
    iterperyear = 0;
    InClim.getline(buffer,256,'\n');
    //Rcout << "Header line: " << buffer << endl;
    
    string line;
    while(getline(InClim, line)){
      istringstream linestream(line);
      
      float Temperature_current, DailyMeanTemperature_current, NightTemperature_current, Rainfall_current, WindSpeed_current, DailyMeanIrradiance_current, MeanIrradiance_current, SaturatedVapourPressure_current, VapourPressure_current, VapourPressureDeficit_current, DailyVapourPressureDeficit_current, DailyMeanVapourPressureDeficit_current;
      linestream >> Temperature_current >> DailyMeanTemperature_current >> NightTemperature_current >> Rainfall_current >> WindSpeed_current >> DailyMeanIrradiance_current >> MeanIrradiance_current >> SaturatedVapourPressure_current >> VapourPressure_current >> VapourPressureDeficit_current >> DailyVapourPressureDeficit_current >> DailyMeanVapourPressureDeficit_current;
      Temperature.push_back(Temperature_current);
      DailyMeanTemperature.push_back(DailyMeanTemperature_current);
      NightTemperature.push_back(NightTemperature_current);
      Rainfall.push_back(Rainfall_current);
      WindSpeed.push_back(WindSpeed_current);
      DailyMeanIrradiance.push_back(DailyMeanIrradiance_current);
      MeanIrradiance.push_back(MeanIrradiance_current);
      SaturatedVapourPressure.push_back(SaturatedVapourPressure_current);
      VapourPressure.push_back(VapourPressure_current);
      VapourPressureDeficit.push_back(VapourPressureDeficit_current);
      DailyVapourPressureDeficit.push_back(DailyVapourPressureDeficit_current);
      DailyMeanVapourPressureDeficit.push_back(DailyMeanVapourPressureDeficit_current);
      //Rcout << Temperature_current << "\t" <<  DailyMeanTemperature_current << "\t" <<  NightTemperature_current << "\t" <<  Rainfall_current << "\t" <<  WindSpeed_current << "\t" <<  DailyMeanIrradiance_current << "\t" <<  MeanIrradiance_current << "\t" <<  SaturatedVapourPressure_current << "\t" <<  VapourPressure_current << "\t" <<  VapourPressureDeficit_current << "\t" <<  DailyVapourPressureDeficit_current << "\t" <<  DailyMeanVapourPressureDeficit_current << endl;
      iterperyear++;
    }
    
    timestep=1.0/float(iterperyear);
    Rcout << "Read in climate data for " << iterperyear << " iterations per year." << endl;
    // choose average conditions 
    WDailyMean_year = tDailyMean_year = VPDDailyMean_year = Tnight_year = temp_year = 0.0;
    
    for (int i=0; i<iterperyear; i++) {
      WDailyMean_year += DailyMeanIrradiance[i]*SWtoPPFD;
      tDailyMean_year += DailyMeanTemperature[i];
      VPDDailyMean_year += DailyMeanVapourPressureDeficit[i];
      Tnight_year += NightTemperature[i];
      temp_year += Temperature[i];
    }
    
    WDailyMean_year *= 1.0/float(iterperyear);
    tDailyMean_year *= 1.0/float(iterperyear);
    VPDDailyMean_year *= 1.0/float(iterperyear);
    
    //Rcout << "WDailyMean: " << WDailyMean_year << endl;
    Tnight_year *= 1.0/float(iterperyear);
    temp_year *= 1.0/float(iterperyear);
    
    temp=Temperature[iter%iterperyear];
    tnight=NightTemperature[iter%iterperyear];
    precip=Rainfall[iter%iterperyear];
    WS=WindSpeed[iter%iterperyear];
    WDailyMean=DailyMeanIrradiance[iter%iterperyear]*SWtoPPFD;
    tDailyMean=DailyMeanTemperature[iter%iterperyear];
    VPDDailyMean=DailyMeanVapourPressureDeficit[iter%iterperyear];
    Wmean=MeanIrradiance[iter%iterperyear];            // still in W/m2
    e_s=SaturatedVapourPressure[iter%iterperyear];
    e_a=VapourPressure[iter%iterperyear];
    VPDbasic=VapourPressureDeficit[iter%iterperyear];
    VPDday=DailyVapourPressureDeficit[iter%iterperyear];
    
    Rcout << "Successfully read the climate file" << endl;
  }
  else{
    Rcout<< "ERROR with the climate file" << endl;
  }
  InClim.close();
}

#ifdef WATER
//! Global function: This function reads inputs from the soil input file 
//! - in v.3.0, all soil parameters (Sat_SWC, Res_SWC) are computed from soil texture data (%clay, %silt, %sand) provided in input for each layer. If additional information is available from the field (soil pH, organic content, dry bulk density, cation exchange capacity), this could be also provided in input and used to refine the computation of these soil parameters (see Table 2 in Marthews et al. 2014 Geoscientific Model Development and Hodnett & Tomasella 2002 Geoderma -- for tropical soils). Alternatively, if no local field soil data is available, these soil parameters (Sat_SWC, Res_SWC) should be drawn from global maps and databases --see Marthews et al. 2014, and directly provided in input. ==> ccl: to standardize the input file, the soil parameters (Sat_SWC, Res_SWC) should probably be provided in input, and the computation of those properties from the available local data (here %clay, %silt, %sand) made using a new function of RconTROLL (and not here)
//! - Sat_SWC and Res_SWC are here computed according Tomasella & Hodnett 1998 from soil texture information (see Table 2 in Marthews et al. 2014)
void ReadInputSoil(){
  Rcout << endl << "Reading in file: " << inputfile_soil << endl;
  fstream InSoil(inputfile_soil, ios::in);
  
  if(InSoil){
    InSoil.getline(buffer,256,'\n');
    vector<float> layer_thickness, proportion_Silt, proportion_Clay, proportion_Sand;
    layer_thickness.reserve(100);
    proportion_Silt.reserve(100);
    proportion_Clay.reserve(100);
    proportion_Sand.reserve(100);
    nblayers_soil = 0;
    
    // we go through all lines in the input file 
    string line;
    while(getline(InSoil, line)){
      istringstream linestream(line);
      
      float thickness_current, proportion_Silt_current, proportion_Clay_current, proportion_Sand_current;
      
      linestream >> thickness_current >> proportion_Silt_current >> proportion_Clay_current >> proportion_Sand_current;
      
      layer_thickness.push_back(thickness_current);
      proportion_Silt.push_back(proportion_Silt_current);
      proportion_Clay.push_back(proportion_Clay_current);
      proportion_Sand.push_back(proportion_Sand_current);
      
      nblayers_soil++;
    }
    Rcout << "Read in: " << nblayers_soil << " soil layers" << endl;
    
    if(NULL==(layer_depth=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc layer_depth" << endl;
    float cumulative_depth=0.0;
    for (int l=0; l<nblayers_soil; l++) {
      cumulative_depth+=layer_thickness[l];
      layer_depth[l]=cumulative_depth;
    }
    // (added in the header but kept in here) in this version, all soil parameters (Sat_SWC, Res_SWC) are computed from soil texture data (%clay, %silt, %sand) provided in input for each layer. If additional information is available from the field (soil pH, organic content, dry bulk density, cation exchange capacity), this could be also provided in input and used to refine the computation of these soil parameters (see Table 2 in Marthews et al. 2014 Geoscientific Model Development and Hodnett & Tomasella 2002 Geoderma -- for tropical soils). Alternatively, if no local field soil data is available, these soil parameters (Sat_SWC, Res_SWC) should be drawn from global maps and databases --see Marthews et al. 2014, and directly provided in input. ==> ccl: to standardize the input file, the soil parameters (Sat_SWC, Res_SWC) should probably be provided in input, and the computation of those properties from the available local data (here %clay, %silt, %sand) made using a new function of RconTROLL (and not here)
    // (added in the header but kept in here) Sat_SWC and Res_SWC are here computed according Tomasella & Hodnett 1998 from soil texture information (see Table 2 in Marthews et al. 2014)
    if(NULL==(Sat_SWC=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc Sat_SW" << endl;
    if(NULL==(Max_SWC=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc Max_SW" << endl;
    
    for (int l=0; l<nblayers_soil; l++) {
      Sat_SWC[l]= 0.01*(40.61+(0.165*proportion_Silt[l])+(0.162*proportion_Clay[l])+(0.00137*proportion_Silt[l]*proportion_Silt[l])+(0.000018*proportion_Silt[l]*proportion_Silt[l]*proportion_Clay[l])); // this is the Tomasella & Hodnett 1998 tropical texture-based pedotransfer function, as reported in Table 2 of Marthews et al. 2014. in m3.m-3
      Max_SWC[l]=Sat_SWC[l]*sites_per_dcell*LH*LH*layer_thickness[l]; // in m3
      Rcout << "layer " << l << " Vol=" << sites_per_dcell*LH*LH*layer_thickness[l]<< " m3; Sat_SWC =" << Sat_SWC[l] << " MAX_SWC =" << Max_SWC[l] << " m3." << endl;
    }
    
    if(NULL==(Ksat=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc Ksat" << endl;
    for (int l=0; l<nblayers_soil; l++) {
      Ksat[l]=0.007055556*pow(10,(-0.60-(0.0064*proportion_Clay[l])+(0.0126*proportion_Sand[l]))); // according to Cosby et al. 1984 (the only expression of k_sat reported in Table 2 of Marthews et al. 2014). k_sat is here in mm/s or equivalently in kg/m2/s.
      Rcout << "layer " << l << " Ksat=" << Ksat[l] <<"mm/s or kg/m2/s  "<< Ksat[l]*9.8/18 << endl;
    }
    
    if(NULL==(Res_SWC=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc Res_SW" << endl;
    if(NULL==(Min_SWC=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc Max_SW" << endl;
    
    for (int l=0; l<nblayers_soil; l++) {
      Res_SWC[l]= 0.01*fmax(0.0,(-2.094+(0.047*proportion_Silt[l])+(0.431*proportion_Clay[l])-(0.00827*proportion_Silt[l]*proportion_Clay[l]))); // this is the Tomasella & Hodnett 1998 tropical texture-based pedotransfer function, as reported in Table 2 of Marthews et al. 2014.
      Min_SWC[l]=Res_SWC[l]*sites_per_dcell*LH*LH*layer_thickness[l];
      Rcout << "layer " << l << " Vol=" << sites_per_dcell*LH*LH*layer_thickness[l] << "m3; Res=" << Res_SWC[l]<<  " MIN_SWC =" << Min_SWC[l] << " m3" << endl;
    }
    
    if(NULL==(phi_e=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc phi_e" << endl;
    if(NULL==(b=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc b" << endl;
    for (int l=0; l<nblayers_soil; l++) {
      //phi_e[l]=-0.001*(0.285+(0.000733*proportion_Silt[l]*proportion_Silt[l])-(0.00013*proportion_Silt[l]*proportion_Clay[l])+(0.0000036*proportion_Silt[l]*proportion_Silt[l]*proportion_Clay[l])); // according to Tomasella & Hodnett 1998 tropical texture-based pedotransfer function, as reported in Table 2 of Marthews et al. 2014. phi_e is here in MPa.
      //b[l]=exp(1.197+(0.00417*proportion_Silt[l])-(0.0045*proportion_Clay[l])+(0.000894*proportion_Silt[l]*proportion_Clay[l])-(0.00001*proportion_Silt[l]*proportion_Silt[l]*proportion_Clay[l])); // according to Tomasella & Hodnett 1998 tropical texture-based pedotransfer function, as reported in Table 2 of Marthews et al. 2014.
      phi_e[l]=-0.00000001*pow(10.0,(2.17-(0.0063*proportion_Clay[l])-(0.0158*proportion_Sand[l])))*(1000*9.80665); // according to Cosby et al. 1984, non -tropical and texture-based but widely used, as reported in Table 2 in Marthews et al. 2014. In MPa.
      b[l]=3.10+0.157*proportion_Clay[l]-0.003*proportion_Sand[l]; // according to Cosby et al. 1984, non -tropical and texture-based but widely used, as reported in Table 2 in Marthews et al. 2014. Dimensionless.
      Rcout << "layer " << l << " phi_e=" << phi_e[l] << "\t" << "b=" << b[l] <<endl;
    }
    Rcout << "Successfully read the soil file" << endl;
  } else {
    Rcout<< "ERROR with the soil file" << endl;
  }
  Rcout << endl;
}
#endif

//! Global function: initialise intraspecific variables
void InitialiseIntraspecific(){
  float max_intraspecific_height=0.0, min_intraspecific_height=1000.0,
    max_intraspecific_CR=0.0, min_intraspecific_CR = 1000.0,
    max_intraspecific_CD=0.0, min_intraspecific_CD = 1000.0,
    max_intraspecific_P=0.0, min_intraspecific_P = 1000.0,
    max_intraspecific_N=0.0, min_intraspecific_N = 1000.0,
    max_intraspecific_LMA=0.0, min_intraspecific_LMA = 1000.0,
    max_intraspecific_wsg=0.0, min_intraspecific_wsg = 1000.0,
    max_intraspecific_dbhmax=0.0, min_intraspecific_dbhmax = 1000.0;
  
  double variation_height=0.0, variation_CR=0.0, variation_CD=0.0, variation_P=0.0, variation_N=0.0, variation_LMA=0.0,variation_wsg=0.0,variation_dbhmax=0.0;
  for(int i=0;i<100000;i++){
    if(covariance_status == 0){
      variation_N = gsl_ran_gaussian(gslrand, sigma_N);
      variation_P = gsl_ran_gaussian(gslrand, sigma_P);
      variation_LMA = gsl_ran_gaussian(gslrand, sigma_LMA);
    } else {
      gsl_ran_multivariate_gaussian(gslrand, mu_N_P_LMA, mcov_N_P_LMA, variation_N_P_LMA);
      variation_N = gsl_vector_get(variation_N_P_LMA, 0);
      variation_P = gsl_vector_get(variation_N_P_LMA, 1);
      variation_LMA = gsl_vector_get(variation_N_P_LMA, 2);
    }
    gsl_ran_bivariate_gaussian(gslrand, sigma_height, sigma_CR, corr_CR_height, &variation_height, &variation_CR);
    // variation_height = gsl_ran_gaussian(gslrand, sigma_height);
    // variation_CR = gsl_ran_gaussian(gslrand, sigma_CR);
    variation_CD = gsl_ran_gaussian(gslrand, sigma_CD);
    variation_wsg = gsl_ran_gaussian(gslrand, sigma_wsg);
    variation_dbhmax = gsl_ran_gaussian(gslrand, sigma_dbhmax);
    
    // limit extent of variation, some coarse biological limits for variation around allometries 
    if(variation_N > 0.5) variation_N = 0.5;
    if(variation_N < -0.5) variation_N = -0.5;
    if(variation_P > 0.5) variation_P = 0.5;
    if(variation_P < -0.5) variation_P = -0.5;
    if(variation_LMA > 0.5) variation_LMA = 0.5;
    if(variation_LMA < -0.5) variation_LMA = -0.5;
    if(variation_CD > 0.5) variation_CD = 0.5;
    if(variation_CD < -0.5) variation_CD = -0.5;
    if(variation_CR > 0.5) variation_CR = 0.5;
    if(variation_CR < -0.5) variation_CR = -0.5;
    if(variation_height > 0.5) variation_height = 0.5;
    if(variation_height < -0.5) variation_height = -0.5;
    
    d_intraspecific_height[i] = float(exp(variation_height));
    d_intraspecific_CR[i] = float(exp(variation_CR));
    d_intraspecific_N[i] = float(exp(variation_N));
    d_intraspecific_P[i] = float(exp(variation_P));
    d_intraspecific_LMA[i] = float(exp(variation_LMA));
    d_intraspecific_CD[i] = float(exp(variation_CD));
    d_intraspecific_wsg[i] = float(variation_wsg);             // normal, not log-normal
    d_intraspecific_dbhmax[i] = float(exp(variation_dbhmax));
    // d_intraspecific_height[i] = exp(float(gsl_ran_gaussian(gslrand, sigma_height)));
    max_intraspecific_height = fmaxf(max_intraspecific_height,d_intraspecific_height[i]);
    min_intraspecific_height = fminf(min_intraspecific_height,d_intraspecific_height[i]);
    max_intraspecific_CR = fmaxf(max_intraspecific_CR,d_intraspecific_CR[i]);
    min_intraspecific_CR = fminf(min_intraspecific_CR,d_intraspecific_CR[i]);
    max_intraspecific_N = fmaxf(max_intraspecific_N,d_intraspecific_N[i]);
    min_intraspecific_N = fminf(min_intraspecific_N,d_intraspecific_N[i]);
    max_intraspecific_P = fmaxf(max_intraspecific_P,d_intraspecific_P[i]);
    min_intraspecific_P = fminf(min_intraspecific_P,d_intraspecific_P[i]);
    max_intraspecific_LMA = fmaxf(max_intraspecific_LMA,d_intraspecific_LMA[i]);
    min_intraspecific_LMA = fminf(min_intraspecific_LMA,d_intraspecific_LMA[i]);
    max_intraspecific_CD = fmaxf(max_intraspecific_CD,d_intraspecific_CD[i]);
    min_intraspecific_CD = fminf(min_intraspecific_CD,d_intraspecific_CD[i]);
    max_intraspecific_wsg = fmaxf(max_intraspecific_wsg,d_intraspecific_wsg[i]);
    min_intraspecific_wsg = fminf(min_intraspecific_wsg,d_intraspecific_wsg[i]);
    max_intraspecific_dbhmax = fmaxf(max_intraspecific_dbhmax,d_intraspecific_dbhmax[i]);
    min_intraspecific_dbhmax = fminf(min_intraspecific_dbhmax,d_intraspecific_dbhmax[i]);
  }
  Rcout << endl << "Intraspecific variation initialisation: " << endl;
  Rcout << "Max and min allometry deviation, lognormal (height): " << max_intraspecific_height << " | " << min_intraspecific_height << endl;
  Rcout << "Max and min allometry deviation, lognormal (crown radius): " << max_intraspecific_CR << " | " << min_intraspecific_CR << endl;
  Rcout << "Max and min trait deviation, lognormal (N): " << max_intraspecific_N << " | " << min_intraspecific_N << endl;
  Rcout << "Max and min trait deviation, lognormal (P): " << max_intraspecific_P << " | " << min_intraspecific_P << endl;
  Rcout << "Max and min trait deviation, lognormal (LMA): " << max_intraspecific_LMA << " | " << min_intraspecific_LMA << endl;
  Rcout << "Max and min allometry deviation, normal (crown depth): " << max_intraspecific_CD << " | " << min_intraspecific_CD << endl;
  Rcout << "Max and min trait deviation, normal (wsg): " << max_intraspecific_wsg << " | " << min_intraspecific_wsg << endl;
  Rcout << "Max and min trait deviation, lognormal (dmax): " << max_intraspecific_dbhmax << " | " << min_intraspecific_dbhmax << endl;
}

//! Global function: initialise lookup tables
//! - this contains a number of empirical functions including:
//! -# temperature dependence on Farquhar model parameters
//! -# flux averaging with the canopy
//! -# sites within a crown in order of distance from the center
void InitialiseLookUpTables(){
  nbTbins=500;
  float Taccuracy=0.1;
  iTaccuracy=1.0/Taccuracy;
  Rcout << endl << "Built-in maximal temperature: " << float(nbTbins)*Taccuracy <<endl;
  if(NULL==(LookUp_KmT=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_KmT" << endl;
  if(NULL==(LookUp_GammaT=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_GammaT" << endl;
  if(NULL==(LookUp_VcmaxT=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_VcmaxT" << endl;
  if(NULL==(LookUp_JmaxT=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_JmaxT" << endl;
  if(NULL==(LookUp_Rday=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_Rday" << endl;
  if(NULL==(LookUp_Rstem=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_Rstem" << endl;
  if(NULL==(LookUp_Rnight=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_Rnight" << endl;
  for(int i=0;i<nbTbins;i++) { // loop over "T" in GPPleaf()
    float temper=float(i)*Taccuracy;
    // !!!UPDATE provide references for these eqautions
    LookUp_KmT[i] = 404.0*exp(((temper-25.0)/(298*0.00831*(273+temper)))*59.36)*
      (1+210*1.0/248.0*exp(-(temper-25.0)/(298*0.00831*(273+temper))*35.94))*iCair;
    LookUp_GammaT[i]=37.0*exp(((temper-25.0)/(298*0.00831*(273+temper)))*23.4)*iCair;
    LookUp_VcmaxT[i]=exp(26.35-65.33/(0.00831*(temper+273.15)));
    LookUp_JmaxT[i]=exp(17.57-43.54/(0.00831*(temper+273.15)));
    LookUp_Rday[i]=exp((temper-25.0)*0.1*log(3.09-0.0215*(25.0+temper)));
    LookUp_Rstem[i]=39.6*378.7*timestep*exp(((temper-25.0)/10.0)*log(2.0));
    LookUp_Rnight[i]=exp((temper-25.0)*0.1*log(3.09-0.0215*(25.0+temper)));
    // exp((temp-25)/10*log(2)) is the temperature dependency of Rstem, supposing a constant Q10=2, according to Ryan et al 1994 and Meir & Grace 2002 exp((tnight-25)*0.1*log(3.09-0.0215*(25+tnight))) is the temperature dependencies used by Atkin 2015 (equ1) 
  }
  
  // look up table for flux averaging/integration 
  // division into absorption prior to current voxel (absorb_prev) and absorption in current voxel (absorb_delta) 
  // prior absorption has a maximum of 20 m2/m3, while absorption within one voxel can maximally reach 10 m2/m3
  if(NULL==(LookUp_flux_absorption=new float[80000])) Rcerr<<"!!! Mem_Alloc LookUp_flux" << endl;
  if(NULL==(LookUp_flux=new float[80000])) Rcerr<<"!!! Mem_Alloc LookUp_flux" << endl;
  if(NULL==(LookUp_VPD=new float[80000])) Rcerr<<"!!! Mem_Alloc LookUp_VPD" << endl;
  if(NULL==(LookUp_T=new float[80000])) Rcerr<<"!!! Mem_Alloc LookUp_VPD" << endl;
  for(int i=0;i<400;i++) { // loop over "absorb" in Fluxh()
    for(int j=0;j<200;j++){
      float absorb_prev=float(i)/20.0;
      float absorb_delta=float(j)/20.0;
      if(absorb_delta==0){
        // flux is now computed simply as absorption on a per m2 plant matter basis (cf. explanation below) but since there is no plant matter in the case of absorb_delta == 0, PPFD should be zero here
        LookUp_flux_absorption[i+400*j] = 0.0;    // in this case
        
        // if the voxel does not contain any plant matter, values are constant across the voxel, e.g. just the top value calculated from absorb_prev
        LookUp_flux[i+400*j] = exp(-kpar*absorb_prev);
        LookUp_VPD[i+400*j] = 0.25 + sqrt(fmaxf(0.0 , 0.08035714*(7.0-absorb_prev)));
        LookUp_T[i+400*j] = 0.4285714 * (fminf(7.0,absorb_prev));
      } else {
        // Flux is now computed simply as absorption on a per m2 plant matter basis, and not incident flux, since the FvCB model requires a transformation of incident into absorbed flux (cf. original Farquhar 1980 paper, or Medlyn et al. 2002, Plant, Cell & Environment). For example, Medlyn et al. use 0.093 quantum yield, 4 mol electron/mol photon and an absorptance of leaves of 0.8 to arrive at a factor of 0.3 to be multiplied with incident PPFD. Now, in a dense forest, kpar modifies the absorbed PPFD per leaf area. For simplicity, we separate the 0.093 and 4 mol electrion/mol photon from the absorptance and calculate the latter directly from an effective kpar which includes a general k (i.e. leaf angle distribution) and a leaf absorptance factor, e.g. 0.9. The main effect of this scheme is that lowering k (reflecting, for example, steeper leaf angles) may result in more incident light per m2 ground, but absorption also gets lower, since leaves are not perfectly illuminated
        // To calculate absorbed PPFD, a formula can either be derived through integration or be motivated as follows: 1) incoming flux is exp(-kpar * absorb_prev), equivalent to what was previously computed as flux, 2) the absorbed fraction of the incoming flux in a layer of "absorb_delta" is (1.0 - exp(-kpar * absorb_delta), and 3) the amount of leaf area per ground area is absorb_delta, which is needed as divisor to convert to absorption per m2 leaf area
        // Since PPFD is a density (i.e. given relative to m2 leaf area), a lower absorb_delta results in slightly higher PPFD. A lower absorb_delta implies that leaves are less densely distributed in space, so there is a slight increase in absorbed photons per leaf area, even though overall absorbed photon numbers decrease. While an absorb_delta = 0 implies zero absorption, in the limit of very low absorb_delta (-> 0), the absorption approaches kpar (Taylor expansion: exp(x) ~ 1 + x, so (1 - exp(-kpar*x))/x ~ kpar*x/x ~ kpar). This is not realistic, since leaves cannot get infinitesimally small and the assumptions of Beer-Lambert breaks down beforehand. But since the linear approximation should be justified in low density layers, maybe this could be used to accelerate the computation?
        LookUp_flux_absorption[i+400*j] = exp(-kpar * absorb_prev) * (1.0 - exp(-kpar * absorb_delta)) / absorb_delta;
        
        // an alternative to calculating the absorbed flux density, is to calculate the average flux density
        // for voxels of 1 unit length depth, this corresponds just to the integral over LAI, which can be decomposed into a constant absorb_prev and a linearly increasing absorb_delta
        // once LAI reaches the critical value of 7.0, VPD And T do not decrease anymore, hence the distinction between two cases
        
        LookUp_flux[i+400*j] = exp(-kpar * absorb_prev) * (1.0 - exp(-kpar * absorb_delta)) / (kpar * absorb_delta);
        if(absorb_prev+absorb_delta >= 7){// This is related to the empirical description of VPD and temperature decrease through the canopy. These empirical description were derived from literature and data from HOBO on the COPAS tower, and assumed that above a LAI of 7, the VPD and temperature are pretty constant (e.g. Within the understory).
          LookUp_VPD[i+400*j] = 0.25;
          LookUp_T[i+400*j] = 3.0;  // 0.4285714 * 7.0
        } else {
          LookUp_VPD[i+400*j] = 0.25 + (0.188982/absorb_delta) * (pow((7.0 - absorb_prev),1.5) - pow((7.0 - absorb_prev - absorb_delta),1.5));
          LookUp_T[i+400*j] = 0.4285714 * (absorb_prev + 0.5 * absorb_delta);
        }
      }
    }
  }
  
  // new in v.2.4: LookUp table that gives the sites within a crown in order of distance from the center 
  // crowns can thus be assembled from inside out, in a radial fashion 
  int Crown_dist[2601];                                           // this saves the distances from the center of the crown
  int extent = 25;                                                // maximum extent of crowns (after test: either enlarge or allocate dynamically)
  int extent_full = 2*25 + 1;
  int index_crown = 0, xx, yy, site_rel, dist;
  Crown_dist[index_crown] = 0;                                    // this is the distance of the center of the crown from the center (0)
  LookUp_Crown_site[index_crown] = extent + extent_full * extent;      // this is the label of the site at the center of the crown ( x = extent, y = extent)
  
  // loop over crown 
  for(int col = 0; col < extent_full; col++){
    for(int row = 0; row < extent_full; row++){
      xx = col - extent;                                      // distance from center (x = extent) in x direction
      yy = row - extent;                                      // distance from center (y = extent) in y direction
      if(!(xx == 0 & yy == 0)){
        site_rel = col + extent_full * row;
        dist = xx*xx + yy*yy;
        // now order the arrays according to distance from center 
        // index_crown saves last filled position in array 
        // for every voxel we run through the array from position zero to last filled position, and check where to put the new value 
        for(int i=0; i < index_crown + 1; i++){
          int temp = Crown_dist[i];
          int site_rel_temp = LookUp_Crown_site[i];
          if(dist <= temp){                               // if distance is smaller than at current array position, move everything up
            Crown_dist[i] = dist;
            LookUp_Crown_site[i] = site_rel;
            dist = temp;
            site_rel = site_rel_temp;
          }
        }
        Crown_dist[index_crown+1] = dist;                   // the last value that has been pushed into the dist variable fills a new position
        index_crown = index_crown + 1;
      }
    }
  }
  
}

//! Global function: initialise output streams
void InitialiseOutputStreams(){
  char nnn[200];
  if(!mpi_rank) {
    if(_OUTPUT_reduced){
      sprintf(nnn,"%s_%i_outputs.txt",buf, easympi_rank);
      output[0].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_final_pattern.txt",buf, easympi_rank);
      output[10].open(nnn, ios::out);
#ifdef Output_ABC
      sprintf(nnn,"%s_%i_abc_traitconservation.txt",buf, easympi_rank);
      output[11].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_ground.txt",buf, easympi_rank);
      output[12].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_chm.txt",buf, easympi_rank);
      output[13].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_chmALS.txt",buf, easympi_rank);
      output[14].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_transmittance.txt",buf, easympi_rank);
      output[15].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_transmittanceALS.txt",buf, easympi_rank);
      output[16].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_PAIfield.txt",buf, easympi_rank);
      output[17].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_PAIfieldALS.txt",buf, easympi_rank);
      output[18].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_chmpotential.txt",buf, easympi_rank);
      output[19].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_LAI.txt",buf, easympi_rank);
      output[20].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_species.txt",buf, easympi_rank);
      output[23].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_species10.txt",buf, easympi_rank);
      output[24].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_traits.txt",buf, easympi_rank);
      output[25].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_traits10.txt",buf, easympi_rank);
      output[26].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abc_biomass.txt",buf, easympi_rank);
      output[27].open(nnn, ios::out);
#endif
#ifdef WATER
      sprintf(nnn,"%s_%i_water_balance.txt",buf, easympi_rank);
      output[11].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_site1.txt",buf, easympi_rank);
      output[12].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_site2.txt",buf, easympi_rank);
      output[13].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_site3.txt",buf, easympi_rank);
      output[14].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_site4.txt",buf, easympi_rank);
      output[15].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_site5.txt",buf, easympi_rank);
      output[16].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_site6.txt",buf, easympi_rank);
      output[17].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_state_begin.txt",buf, easympi_rank);
      output[28].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_state_mid.txt",buf, easympi_rank);
      output[29].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_state_end.txt",buf, easympi_rank);
      output[30].open(nnn, ios::out);
#endif
      
#ifdef TRACK_INDIVIDUALS
      // these are the "cases" of trees that are followed 
      sprintf(nnn,"%s_%i_trees_fortracking.txt",buf, easympi_rank);
      output[28].open(nnn, ios::out);
      output[28] << "site" << "\t" << "month_born" << "\t" << "col" << "\t" << "row" << "\t" << "species" << "\t" << "dbh" << "\t" << "cr" << "\t" << "height"  << "\t" << "agb" << "\t" << "multiplier_cr" << "\t" << "multiplier_height" << "\t" << "wsg" << "\t" << "Nmass" << "\t" << "Pmass" << "\t" << "LMA" << "\t" << "deviation_wsg" << "\t" << "multiplier_Nmass" << "\t" << "multiplier_Pmass" << "\t" << "multiplier_lma" << "\t" << "Vcmax" << "\t" << "Jmax" << "\t" << "Rdark" << "\t" << "LAImax" << "\t" << "leaflifespan" << endl;
      // these are the "activities" that are recorded for each tree 
      sprintf(nnn,"%s_%i_trees_tracked.txt",buf, easympi_rank);
      output[29].open(nnn, ios::out);
      output[29] << "site" << "\t" << "month_born" << "\t" << "Iter" << "\t" << "age" << "\t" << "seeds" << "\t" << "seedstotal" << "\t" << "carbstarv" << "\t" << "carbstarvtotal" << "\t" << "dbh" << "\t" << "dbhgrowth" << "\t" << "height" << "\t" << "heightgrowth" << "\t" << "cr" << "\t" << "crgrowth" << "\t" << "agb" << "\t" << "agbgrowth" << "\t" << "GPP" << "\t" << "GPPsq" << "\t" << "NPP" << "\t" << "NPPsq" << "\t" << "Rday" << "\t" << "Rnight" << "\t" << "Rstem" << "\t" << "LAIabove_avg" << "\t" << "LAIabove_effavg" << "\t" << "carbstore_avg" << endl;
      // this is to get information on the dead trees > 10cm 
      sprintf(nnn,"%s_%i_trees_aftertracking.txt",buf, easympi_rank);
      output[30].open(nnn, ios::out);
      output[30] << "site" << "\t" << "month_born" << "\t" << "Iter" << "\t" << "age" << "\t" << "seedstotal" << "\t" << "carbstarvtotal" << "\t" << "dbh" << "\t" << "height" << "\t" << "cr" << "\t" << "agb" << "\t" << "GPP" << "\t" << "NPP" << "\t" << "LAIabove_avg" << "\t" << "LAIabove_effavg" << "\t" << "GPPsquared" << "\t" << "NPPsquared" << "\t" << "LAIabovesquared_avg" << "\t" << "LAIabovesquared_effavg" << endl;
#endif
      
#ifdef CHM_SPIKEFREE
      sprintf(nnn,"%s_%i_chm.txt",buf, easympi_rank);
      output[31].open(nnn, ios::out);
#endif
      //sprintf(nnn,"%s_%i_final_pattern_10cm.txt",buf, easympi_rank);
      //output[18].open(nnn, ios::out);
    }
    else{
      sprintf(nnn,"%s_%i_abund.txt",buf, easympi_rank);
      output[0].open(nnn, ios::out);
      if (!output[0]) {
        Rcout<< "ERROR with abund file"<< endl;
      }
      sprintf(nnn,"%s_%i_abu10.txt",buf, easympi_rank);
      output[1].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_abu30.txt",buf, easympi_rank);
      output[2].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_ba.txt",buf, easympi_rank);
      output[3].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_npp.txt",buf, easympi_rank);
      output[4].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_gpp.txt",buf, easympi_rank);
      output[5].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_ba10.txt",buf, easympi_rank);
      output[6].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_ppfd0.txt",buf, easympi_rank);
      output[7].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_death.txt",buf, easympi_rank);
      output[8].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_state.txt",buf, easympi_rank);
      output[9].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_final_pattern.txt",buf, easympi_rank);
      output[10].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_site1.txt",buf, easympi_rank);
      // used to be added to explore potential link with Belassen curve, could be suppressed, but maybe useful to have an idea of the magnitude and distribution of increment of dbh
      output[11].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_site2.txt",buf, easympi_rank);
      output[12].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_site3.txt",buf, easympi_rank);
      output[13].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_site4.txt",buf, easympi_rank);
      output[14].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_site5.txt",buf, easympi_rank);
      output[15].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_site6.txt",buf, easympi_rank);
      output[16].open(nnn, ios::out);
      //output[17] for parameter space is valid both for reduced and standard output, defined below
      sprintf(nnn,"%s_%i_sp_par.txt",buf, easympi_rank);
      output[18].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_agb.txt",buf, easympi_rank);
      output[19].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_Rday.txt",buf, easympi_rank);
      output[20].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_Rnight.txt",buf, easympi_rank);
      output[21].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_Rstem.txt",buf, easympi_rank);
      output[22].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_death1.txt",buf, easympi_rank);
      output[23].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_death2.txt",buf, easympi_rank);
      output[24].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_death3.txt",buf, easympi_rank);
      output[25].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_deathrate.txt",buf, easympi_rank);
      output[26].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_litterfall.txt",buf, easympi_rank);
      output[27].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_leafdens1.txt",buf, easympi_rank);
      output[28].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_leafdens2.txt",buf, easympi_rank);
      output[29].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_leafdens3.txt",buf, easympi_rank);
      output[30].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_dbh.txt",buf, easympi_rank);
      output[31].open(nnn,ios::out);
      sprintf(nnn,"%s_%i_vertd.txt",buf, easympi_rank);
      output[32].open(nnn,ios::out);
      sprintf(nnn,"%s_%i_100yearsofsolitude.txt",buf, easympi_rank);
      output[33].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_NDDfield.txt",buf, easympi_rank);
      output[34].open(nnn, ios::out);
      sprintf(nnn,"%s_%i_cica.txt",buf, easympi_rank);
      output[35].open(nnn, ios::out);
    }
  }
}

//######################################
// Global function: initialisation with bare ground conditions
//######################################
void Initialise() {
  //** Initialization of the simulation parameters **
  //*************************************************
  ReadInputGeneral();
  ReadInputSpecies();
  ReadInputDailyvar();
  ReadInputClimate();
  
#ifdef WATER
  ReadInputSoil();
#endif
  
  //** Information in file info **
  //******************************
  if(!mpi_rank){
    out2 << "\nTROLL simulator\n\n";
    out2 << "\n   2D discrete network: horizontal step = " << LH
         << " m, one tree per "<< LH*LH << " m^2 \n\n";
    out2 << "\n   Tree : (t_dbh,t_Tree_Height,t_Crown_Radius,t_Crown_Depth) \n\n";
    out2 << "\n            + one species label \n\n";
    out2 << " Number of sites      : "<<rows<<"x"<<cols<<"\n";
    out2 << " Number of iterations : "<<nbiter<<"\n";
    out2 << " Duration of timestep : "<<timestep<<" years\n";
    out2 << " Number of Species    : "<<nbspp << "\n\n";
    out2.flush();
  }
  
  //** Initialization of trees **
  //*****************************
  T.reserve(sites);
  for(int site = 0; site < sites; site++){
    Tree T_site;
    T.push_back(T_site);
#ifdef WATER
    // FF: not sure this check is necessary anymore (it's a check for memory allocation problems, I presume?), but I kept it just in case
    if (&T_site.t_soil_layer_weight[0] == &T_site.t_root_biomass[4]) {
      Rcout << "Warning mem_alloc root biomass and soil layer weight at site " << site << ": " << endl;
      Rcout << "t_soil_layer_weight adresses: " << &T_site.t_soil_layer_weight[0] << "\t" << &T_site.t_soil_layer_weight[1] << "\t" << &T_site.t_soil_layer_weight[2] << "\t" << &T_site.t_soil_layer_weight[3] << "\t" << &T_site.t_soil_layer_weight[4] << endl;
      Rcout << "t_root biomass adresses: " << &T_site.t_root_biomass[0] << "\t" << &T_site.t_root_biomass[1] << "\t" << &T_site.t_root_biomass[2] << "\t" << &T_site.t_root_biomass[3] << "\t" << &T_site.t_root_biomass[4] << endl;
    }
#endif
  }
  InitialiseIntraspecific();
  InitialiseLookUpTables();
  InitialiseOutputStreams();
}

#ifdef Output_ABC
//######################################
// Global ABC function: initialise ABC conditions
//######################################
//! - determine area to be taken into consideration for ABC and margin not to be taken into consideration (artefacts along border) 
void InitialiseABC(){
  margin = 0;
  row_start = margin;
  col_start = margin;
  row_end = rows-margin;
  col_end = cols-margin;
  sites_abc = (row_end - row_start) * (col_end - col_start);
  isites_abc = 1.0/float(sites_abc);
  Rcout << "row start: " << row_start << " | row end: " << row_end << " | sites_abc: " << sites_abc << endl;
  // counter for recursive function (gap patches) 
  nbvisited = 0;
  // distributions for simulated and empirical CHM 
  // precomputed, not directly deduced from empirical fields, as a separate algorithm is used (from LAStools) 
  if (NULL==(chm_field_previous=new int[sites])) Rcerr<<"!!! Mem_Alloc\n";
  if (NULL==(chm_field_current=new int[sites])) Rcerr<<"!!! Mem_Alloc\n";
  if (NULL==(chm_field_previous_ALS=new int[sites])) Rcerr<<"!!! Mem_Alloc\n";
  if (NULL==(chm_field_current_ALS=new int[sites])) Rcerr<<"!!! Mem_Alloc\n";
  if (NULL==(chm_field_changes=new int[sites])) Rcerr<<"!!! Mem_Alloc\n";
  if (NULL==(chm_field_changes_ALS=new int[sites])) Rcerr<<"!!! Mem_Alloc\n";
  for(int s=0;s<sites;s++){
    chm_field_previous[s] = 0;
    chm_field_current[s] = 0;
    chm_field_previous_ALS[s] = 0;
    chm_field_current_ALS[s] = 0;
    chm_field_changes[s] = 0;
    chm_field_changes_ALS[s] = 0;
  }
  // field for simulated transmittance 
  if (NULL==(transmittance_simulatedALS=new float*[HEIGHT+1])) Rcerr<<"!!! Mem_Alloc\n";
  if (NULL==(transmittance_direct=new float*[HEIGHT+1])) Rcerr<<"!!! Mem_Alloc\n";
  if (NULL==(transmittance_simulatedALS_sampling=new int*[HEIGHT+1])) Rcerr<<"!!! Mem_Alloc\n";
  for(int h=0;h<(HEIGHT+1);h++){
    if (NULL==(transmittance_simulatedALS[h]=new float[sites])) Rcerr<<"!!! Mem_Alloc\n";
    if (NULL==(transmittance_direct[h]=new float[sites])) Rcerr<<"!!! Mem_Alloc\n";
    if (NULL==(transmittance_simulatedALS_sampling[h]=new int[sites])) Rcerr<<"!!! Mem_Alloc\n";
  }
  // set it to default 1 (full visibility) 
  for(int row=0;row<rows;row++){
    for(int col=0;col<cols;col++){
      for(int h=0;h<(HEIGHT+1);h++){
        transmittance_simulatedALS[h][col+row*cols] = 1.0;
        transmittance_direct[h][col+row*cols] = 1.0;
        transmittance_simulatedALS_sampling[h][col + row*cols] = 0;
      }
    }
  }
  // initialise output streams 
  OutputABCWriteHeaders(output[11], output[12], output[13], output[14], output[15], output[16], output[17], output[18], output[19],output[23], output[24],output[25], output[26], output[27]);
  
}
#endif

//######################################
// Global function: initialisation from inventories
//######################################
void InitialiseFromData(){
  // prepare data set beforehand: col/x_dim, row/y_dim, dbh_measured, species_label
  
  fstream In(inputfile_data, ios::in);  // input stream
  int data_read=0, data_initialised=0; // diagnostics
  float height_max=0; // diagnostics
  nblivetrees=0;
  Rcout << "Reading from file " << inputfile_data << "\n";
  In.getline(buffer,256,'\n'); // skip header lines
  Rcout << "Header line skipped \n";
  float dbh, height, CR, CD, var_height,var_CR,var_CD,CrownDisplacement,Nmass,Pmass,LMA,wsg,leafarea;
  int row, col, species_label;// s_fd = t_s... pointer 
  string species_name;
  
  // cycle through the file and read out the important parameters 
  while ((In >> col >> row >> dbh  >> height >> CR >> CD >> var_height >> var_CR >> var_CD >> CrownDisplacement >> Nmass >> Pmass >> LMA >> wsg >> leafarea >> species_label >> species_name) && data_read < sites){  // restricting to data sets with a maximum number of values corresponding to no. of sites
    In.getline(buffer, 256, '\n'); // reads additional information into buffer
    
    if((species_label > 0) && (species_label <= nbspp) && (col >= 0) && (col < cols) && (row >= 0) && (row < rows)){
      int site = col + row * cols;
      if(T[site].t_age==0) T[site].BirthFromInput(species_label, col + row*cols, dbh, height, CR, CD, var_height, var_CR, var_CD, CrownDisplacement, Nmass, Pmass, LMA, wsg, leafarea);
      if(height_max<T[site].t_Tree_Height) height_max = T[site].t_Tree_Height;
      // first attempt: simple, only trees with coordinates, only known species. Other possibilities: not spatially explicit and/or assign species randomnly to trees whose species are not known
      data_initialised++;
    }
    else{
      Rcout << "WARNING: tree of species: " << species_name << " at: " << col << " | " << row << " could not be initialized." << endl;
    }
    data_read++;
  }
  Rcout << "\n" << data_read << " rows read from file. " << data_initialised << " rows usable for initialisation from data. \n";
  Rcout << "Maximum height of trees included is: " << height_max << "\n";
  Rcout << "NBtrees from Data:\t" << nblivetrees << "\n";
  Rcout << "Initialisation from data finished \n";
  In.close();
}

//######################################
// Global function: Field dynamic memory allocation
//######################################
void AllocMem() {
  // this needs better commenting and probably a rethink (mainly has to do with MPI) 
  float d = 0.0;  // maximum diameter possible
  for(int spp=1;spp<=nbspp;spp++){
    d = fmaxf(d,S[spp].s_dbhmax*1.5);
  }
  float r = 25.0; // simply set to maximum crown radius possible in simulations
  
  RMAX = int(r+p_nonvert*NH*LV*HEIGHT);
  //  RMAX = int(r);
  SBORD = cols*RMAX;
  dbhmaxincm = int(100.*d);
  Rcout << "SBORD: " << SBORD << endl;
  if(!mpi_rank){
    //Rcout << "HEIGHT : " << HEIGHT << " RMAX : " << RMAX << " DBH : " << DBH <<"\n"; Rcout.flush();
    if(RMAX>rows){
      // Consistency tests 
      Rcerr << "Error : RMAX > rows \n";
      // exit(-1);
    }
    if(HEIGHT > rows){
      Rcerr << "Error : HEIGHT > rows \n";
      // exit(-1);
    }
  }
  
  //** Initialization of dynamic Fields **
  //**************************************
  if(NULL==(nbdbh=new int[dbhmaxincm])) Rcerr<<"!!! Mem_Alloc\n";                         // Field for DBH histogram 
  if(NULL==(layer=new float[HEIGHT+1])) Rcerr<<"!!! Mem_Alloc\n";                         // Field for variables averaged by vertical layer 
#ifdef Output_ABC
  if(NULL==(abundances_species=new int[nbspp+1])) Rcerr<<"!!! Mem_Alloc\n";               // vector to save species abundances every recorded timestep 
  if(NULL==(abundances_species10=new int[nbspp+1])) Rcerr<<"!!! Mem_Alloc\n";               // vector to save species abundances every recorded timestep (dbh > 10cm) 
  if(NULL==(biomass_species=new float[nbspp+1])) Rcerr<<"!!! Mem_Alloc\n";               // vector to save species abundances every recorded timestep (dbh > 10cm) 
  
  if(NULL==(traits_species=new float*[nbspp+1])) Rcerr<<"!!! Mem_Alloc\n";               // vector to save species traits every recorded timestep 
  
  for(int spp=0;spp<(nbspp+1);spp++)
    if(NULL==(traits_species[spp]=new float[10]))
      Rcerr<<"!!! Mem_Alloc\n";
    if(NULL==(traits_species10=new float*[nbspp+1])) Rcerr<<"!!! Mem_Alloc\n";               // vector to save species traits every recorded timestep  (dbh > 10cm) 
    for(int spp=0;spp<(nbspp+1);spp++)
      if(NULL==(traits_species10[spp]=new float[10]))
        Rcerr<<"!!! Mem_Alloc\n";
#endif
      if(NULL==(SPECIES_GERM=new int[nbspp+1])) Rcerr<<"!!! Mem_Alloc\n";  // Field for democratic seed germination 
      if(NULL==(SPECIES_SEEDS=new int*[sites]))  Rcerr<<"!!! Mem_Alloc\n"; // Field of seeds 
      for(int site=0;site<sites;site++)                           // For each processor, we define a stripe above (labelled 0) and a stripe below (1). Each stripe is SBORD in width.
        if (NULL==(SPECIES_SEEDS[site]=new int[nbspp+1]))       // ALL the sites need to be updated.
          Rcerr<<"!!! Mem_Alloc\n";
        for(int site=0;site<sites;site++)
          for(int spp=0;spp<=nbspp;spp++)
            SPECIES_SEEDS[site][spp] = 0;
        if(NULL==(p_seed=new double[sites])) Rcerr<<"!!! Mem_Alloc\n";
        if(NULL==(n_seed=new unsigned int[sites])) Rcerr<<"!!! Mem_Alloc\n";
        
        double prob_seed = 1.0/double(sites);
        for(int i = 0; i < sites; i++) p_seed[i] = prob_seed;
        for(int i = 0; i < sites; i++) n_seed[i] = 0;
        
        if(NULL==(p_species=new double[nbspp])) Rcerr<<"!!! Mem_Alloc\n";
        if(NULL==(n_species=new unsigned int[nbspp])) Rcerr<<"!!! Mem_Alloc\n";
        
        for(int i = 0; i < nbspp; i++) n_species[i] = 0;
        
        if(_SEEDTRADEOFF) if (NULL==(PROB_S=new float[nbspp+1])) Rcerr<<"!!! Mem_Alloc\n";
        if(_NDD) if (NULL==(PROB_S=new float[nbspp+1])) Rcerr<<"!!! Mem_Alloc\n";
        //  if (NULL==(persist=new long int[nbiter])) Rcerr<<"!!! Mem_Alloc\n";                  // Field for persistence 
        //  if (NULL==(distr=new int[cols])) Rcerr<<"!!! Mem_Alloc\n";
        
        if(NULL==(LAI3D=new float*[HEIGHT+1]))                                                   // Field 3D 
          Rcerr<<"!!! Mem_Alloc\n";                                                            // Trees at the border of the simulated forest need to know the canopy occupancy by trees in the neighboring processor.
        for(int h=0;h<(HEIGHT+1);h++)                                                          // For each processor, we define a stripe above (labelled 0) and a stripe below (1). Each stripe is SBORD in width.
          if (NULL==(LAI3D[h]=new float[sites+2*SBORD]))                                   // ALL the sites need to be updated.
            Rcerr<<"!!! Mem_Alloc\n";
          for(int h=0;h<(HEIGHT+1);h++)
            for(int site=0;site<sites+2*SBORD;site++)
              LAI3D[h][site] = 0.0;
          if (NULL==(Thurt[0]=new unsigned short[3*sites]))                                       // Field for treefall impacts 
            Rcerr<<"!!! Mem_Alloc\n";
          for(int i=1;i<3;i++)
            if (NULL==(Thurt[i]=new unsigned short[sites]))
              Rcerr<<"!!! Mem_Alloc\n";
            
#ifdef WATER
            if(NULL==(SWC3D=new float*[nblayers_soil])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(soil_phi3D=new float*[nblayers_soil])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(Ks=new float*[nblayers_soil])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(KsPhi=new float*[nblayers_soil])) Rcerr<<"!!! Mem_Alloc\n";
            //if (NULL==(KsPhi2=new float*[nblayers_soil])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(Transpiration=new float*[nblayers_soil])) Rcerr<<"!!! Mem_Alloc\n";
            for(int l=0;l<nblayers_soil;l++) {
              if(NULL==(SWC3D[l]=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
              if(NULL==(soil_phi3D[l]=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
              if(NULL==(Ks[l]=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
              if(NULL==(KsPhi[l]=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
              //if (NULL==(KsPhi2[l]=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
              if(NULL==(Transpiration[l]=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
              for(int dcell=0; dcell<nbdcells; dcell++) {
                SWC3D[l][dcell]=Max_SWC[l];
                soil_phi3D[l][dcell]=0.0;
                Ks[l][dcell]=0.0;
                KsPhi[l][dcell]=0.0;
                //KsPhi2[l][dcell]=0.0;
                Transpiration[l][dcell]=0.0;
                if (SWC3D[l][dcell]<=0.0) {
                  Rcout <<SWC3D[l][dcell] << "\t" <<Max_SWC[l] << "\n";
                }
              }
            }
            if(NULL==(LAI_DCELL=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(Interception=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(Throughfall=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(Runoff=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(Leakage=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(Evaporation=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            for(int dcell=0; dcell<nbdcells; dcell++) {
              LAI_DCELL[dcell]=0.0;
              Interception[dcell]=0.0;
              Throughfall[dcell]=0.0;
              Runoff[dcell]=0.0;
              Leakage[dcell]=0.0;
              Evaporation[dcell]=0.0;
            }
            if(NULL==(site_DCELL=new int[sites])) Rcerr<<"!!! Mem_Alloc\n";
            for(int site=0;site<sites;site++) {
              int x = site%cols;
              int y = site/cols;
              int dcol = x/length_dcell;
              int drow = y/length_dcell;
              site_DCELL[site] = dcol + linear_nb_dcells * drow;
            }
#endif
            
#ifdef MPI // Fields for MPI operations 
            for(i=0;i<2;i++){ //  Two fields: one for the CL north (0) one for the CL south (1) 
              if(NULL==(LAIc[i]=new unsigned short*[HEIGHT+1])) // These fields contain the light info in the neighboring procs (2*SBORD in width, not SBORD !). They are used to update local fields 
                Rcerr<<"!!! Mem_Alloc\n";
              for(h=0;h<(HEIGHT+1);h++)
                if (NULL==(LAIc[i][h]=new unsigned short[2*SBORD]))
                  Rcerr<<"!!! Mem_Alloc\n";
            }
#endif
}

//######################################
// Global function: Initial non-local germination 
//######################################
void BirthInit() {
  if(!_FromData) nblivetrees=0;
#ifdef MPI
  MPI_Barrier(MPI_COMM_WORLD);
#endif
  Rcout<<endl;
}


//######################################
// Global function: Evolution at each timestep
//######################################
void Evolution() {
#ifdef CHECK_CARBON
  if(iter == 0){
    carbon_assimilated_total = 0.0;
    carbon_net_total = 0.0;
  }
#endif
  
  UpdateField();  // Update light fields and seed banks 
  nbtrees_n10=nbtrees_n30=nbdead_n1=nbdead_n10=nbdead_n30=0;
  nbtrees_carbstarv_n1 = nbtrees_carbstarv_n10 = nbtrees_carbstarv_n30 = 0;
  
#ifdef Output_ABC
  nbdead_n10_abc=0;
#endif
  if(_BASICTREEFALL){
    // secondary treefalls are triggered first, since they have been caused in the previous iteration 
    TriggerTreefallSecondary(); // Compute and distribute Treefall events, caused by treefalls in the previous iteration 
    TriggerTreefall();          // Compute and distribute Treefall events, caused by wind drag 
  }
  
  for(int site=0;site<sites;site++) {
    //**** Tree evolution: Growth or death ****
    T[site].Update();
  }
  // Update trees 
  Average();                          //! Compute averages for outputs 
  if(!_OUTPUT_reduced) OutputField(); //! Output the statistics 
}


//#################################
// Global function: Compute field Seed 
//#################################
void UpdateSeeds() {
  // With MPI option: Pass seeds across processors => two more fields to be communicated between n.n. (nearest neighbor) processors. NB: dispersal distance is bounded by the value of 'rows'. At least 99 % of the seeds should be dispersed within the stripe or on the n.n. stripe. Hence rows > 4.7*max(dist_moy_dissemination),for an exponential dispersal kernel.
  // dispersal only once a year 
  if(iter%iterperyear == 0){
    // acceleration, using the multinomial distribution 
    int ha = sites/10000;
    gsl_ran_multinomial(gslrand, sites, Cseedrain * ha, p_seed, n_seed);
    Rcout << sites << " Seedrain: " << Cseedrain * ha << endl;
    int seedsadded = 0;
    for(int s = 0; s < sites; s++){
      //if(T[s].t_age == 0){
      int nbseeds = n_seed[s];
      //Rcout << "Site: " << s << " nbseeds: " << nbseeds << " nbspp: " << nbspp << endl;
      gsl_ran_multinomial(gslrand, nbspp, nbseeds, p_species, n_species);
      for(int spp = 1; spp <= nbspp; spp++){
        int nbseeds_species = n_species[spp-1];
        //Rcout << "Site: " << s << " Species: " << spp << " nbseeds: " << nbseeds_species << endl;
        if(nbseeds_species > 0){
          SPECIES_SEEDS[s][spp] = 1;
          seedsadded++;
        } else SPECIES_SEEDS[s][spp] = 0;
      }
      //}
    }
    
    // now disperse seeds from the trees on site 
    // dispersion comes after seedrain calculation, because seedrain automatically removes seeds from places where there was no incoming seed 
    int trees_mature = 0;
    for(int site=0;site<sites;site++) {  // disperse seeds produced by mature trees 
      if(T[site].t_age){
        if(T[site].t_dbh >= T[site].t_dbhmature) trees_mature++;
        T[site].DisperseSeed();
      }
    }
    
    int nbspecies_affected = 0;
    int seedsadded_effective = 0;
    for(int spp = 1; spp <= nbspp; spp++){
      int seedsadded_species = 0;
      for(int s = 0; s < sites; s++){
        seedsadded_species += SPECIES_SEEDS[s][spp];
      }
      if(seedsadded_species > 0) nbspecies_affected++;
      seedsadded_effective += seedsadded_species;
    }
    Rcout << "Trees_mature: " << trees_mature << " Nbseedsadded: " << seedsadded << " effective: " << seedsadded_effective << " nbspeciesaffected: " << nbspecies_affected << endl;
  }
}

//#################################
// Global function: Update all fields
//#################################
//! - This is an important function for TROLL -- Includes many of the operations
//! - set the iteration environment -- nb: the current structure of code suppose that environment is periodic (a period = a year), if one wants to input a variable climate, with interannual variation and climate change along the simulation, a full climatic input needs to be input (ie number of columns=iter and not iterperyear) and change iterperyear by nbiter here. 
void UpdateField() {
  
  //CURRENTLY NOT USED: precip, WS, Wmean, e_s, e_a,VPDbasic,VPDday
  temp=Temperature[iter%iterperyear];
  tnight=NightTemperature[iter%iterperyear];
  precip=Rainfall[iter%iterperyear];
  WS=WindSpeed[iter%iterperyear];
  WDailyMean=DailyMeanIrradiance[iter%iterperyear]*SWtoPPFD;
  tDailyMean=DailyMeanTemperature[iter%iterperyear];
  VPDDailyMean=DailyMeanVapourPressureDeficit[iter%iterperyear];
  Wmean=MeanIrradiance[iter%iterperyear];            // still in W/m2
  e_s=SaturatedVapourPressure[iter%iterperyear];
  e_a=VapourPressure[iter%iterperyear];
  VPDbasic=VapourPressureDeficit[iter%iterperyear];
  VPDday=DailyVapourPressureDeficit[iter%iterperyear];
  
#ifdef WATER
  float delta=17.27*237.3*e_s/((temp+237.3)*(temp+237.3));
  PET=1.36*(1-0.12)*Wmean*0.0864*(delta/(delta+0.0673645))*0.408; //WARNING !!!!!! this should be thoroughly checked (UNITS + formula) (and include a wind speed term) I just quickly take a simplified expression from an old script from Jérôme. + if finally used, make a look up table or provide it in input.
#ifdef FF_todiscuss
  PET = fmaxf(PET,0.0); // PET should have a lower bound of 0 right?
#endif
#endif
  
  UpdateSeeds();
  
  if(_NDD){
    // Evolution of the field NDDfield

    float normBA=10000.0/(0.001+PI*Rndd*Rndd*BAtot);
    for(int site=0;site<sites;site++) {

      for(int spp=1;spp<=nbspp;spp++) {
        //if ((iter == int(nbiter-1))&&(site>80000)&&(site<85000))  { sor[142]<< T[site].t_NDDfield[spp] << "\t" ;}
        T[site].t_NDDfield[spp]=0;
      }
      //if (iter == int(nbiter-1))  sor[142]<< "\n";

      int row0=T[site].t_site/cols;
      int col0=T[site].t_site%cols;
      for(int col=max(0,int(col0-Rndd));col<=min(cols-1,int(col0+Rndd));col++) {
        for(int row=max(0,int(row0-Rndd));row<=min(rows-1,int(row0+Rndd));row++) {  // loop over the neighbourhood
          int xx=col0-col;
          int yy=row0-row;
          float d=sqrt(xx*xx+yy*yy);
          if((d<=Rndd)&&(d>0))  {  // is the voxel within the neighbourhood?
            int j=cols*row+col;
            if(T[j].t_age) T[site].t_NDDfield[T[j].t_sp_lab]+= PI* T[j].t_dbh * T[j].t_dbh*0.25*normBA;
          }
        }
      }
    }
  }
  
  RecruitTree();
  //  Compute Field LAI3D
#ifdef MPI
  // Reinitialize field LAI3D 
  for(int i=0;i<2;i++)
    for(int h=0;h<(HEIGHT+1);h++)
      for(int site=0;site<2*SBORD;site++)
        LAIc[i][h][site] = 0;
#endif
  
  // v.2.4.0: no changes, but proposition: move CalcLAI() further downwards? after T.[site].Birth()? Currently freshly born trees do not allocate their plant material to LAI3Dfield (but of course, only for the first iteration) 
  for(int h=0;h<(HEIGHT+1);h++)
    for(int sbsite=0;sbsite<sites+2*SBORD;sbsite++)
      LAI3D[h][sbsite] = 0.0;
  for(int site=0;site<sites;site++) T[site].CalcLAI(); // Each tree contribues to LAI3D 
  
  for(int h=HEIGHT;h>0;h--){ // LAI is computed by summing LAI from the canopy top to the ground 
    for(int site=0;site<sites;site++){
      int sbsite=site+SBORD;
      LAI3D[h-1][sbsite] += LAI3D[h][sbsite];
    }
  }
  
#ifdef WATER
  // LAI_DCELL[dcell] provide the average LAI at ground level in dcell, to estimate water interception and evaporation in each dcell at each timestep.
  for(int site=0;site<sites;site++) LAI_DCELL[site_DCELL[site]]+=LAI3D[0][site+SBORD];
  for (int d=0; d<nbdcells; d++) LAI_DCELL[d]*=i_sites_per_dcell;
#endif
  
#ifdef MPI
  // Communicate border of field 
  //MPI_ShareField(LAI3D,LAIc,2*SBORD);
  This MPI command no longer exists in openMPI
    Action 20/01/2016 TODO: FIX THIS 
    MPI_ShareField(LAI3D,LAIc,2*SBORD);
  for(int h=0;h<(HEIGHT+1);h++){
    //! Add border effects in local fields 
    if(mpi_rank)
      for(site=0;site<2*SBORD;site++)
        LAI3D[h][site] += LAIc[0][h][site];
    if(mpi_rank<mpi_size-1)
      for(int site=0;site<2*SBORD;site++)
        LAI3D[h][site+sites] += LAIc[1][h][site];
  }
#endif
  
#ifdef WATER
  //**  Evolution of belowground hydraulic fields: Soil bucket model
  for (int d=0; d<nbdcells; d++) {
    Runoff[d]=0.0;
    Interception[d]=0.0;
    Throughfall[d]=0.0;
    Evaporation[d]=0.0;
    Leakage[d]=0.0;
    for (int l=0;l<nblayers_soil;l++) {
      Transpiration[l][d]=0.0;
    }
  }
  
  for(int site=0;site<sites;site++) T[site].Water_uptake(); // Update of Transpiration: tree water uptake, each tree will deplete soil water content through its transpiration 
  
  for (int d=0; d<nbdcells; d++) {
    //****   BUCKET MODEL in each dcell   ****
    // the unit used for water volume throughout the bucket model is m3.
    // NOTE: under the assumption of a flat terrain and no lateral fluxes, as it is assumed here for a first implementation, the order with which dcells are visited during the loop does not matter. With topography, we will need to visit the soil voxels (ie. dcells*layer) from highest to lowest elevation so that run-off from highest voxels contribute to the water flux entering the lowest ones.
    //  to be investigated: does the order in which transpiration and evaporation are retrieved from the soil affect the overall outcome? which one should be retrieved first?
    
    // Water uptake through tree transpiration
    float w_uptake=0.0;
    for (int l=0; l<nblayers_soil; l++) {
      w_uptake=fminf(Transpiration[l][d],(SWC3D[l][d]-Min_SWC[l]));
      if (Transpiration[l][d]<0.0 ||  isnan(Transpiration[l][d]) || isnan(w_uptake) || (SWC3D[l][d]-Min_SWC[l])<0 ) {
        Rcout << "l=" << l << " d= "<< d <<" transpiration=" << Transpiration[l][d] << " and SWC3D[l][d]=" << SWC3D[l][d] << " and Min_SWC[l]=" << Min_SWC[l] << " and SWC3D[l][d]-Min_SWC[l]=" << SWC3D[l][d]-Min_SWC[l] << endl;
      }
      SWC3D[l][d]-=w_uptake;
      SWC3D[l][d]=fmaxf(SWC3D[l][d],Min_SWC[l]);
      if (Transpiration[l][d]<0.0 ||  isnan(Transpiration[l][d]) || isnan(w_uptake) || (SWC3D[l][d]-Min_SWC[l])<0 ) {
        Rcout << "After w_uptake, l=" << l << " d= "<< d <<" transpiration=" << Transpiration[l][d] << " and SWC3D[l][d]=" << SWC3D[l][d] << " and Min_SWC[l]=" << Min_SWC[l] << " and SWC3D[l][d]-Min_SWC[l]=" << SWC3D[l][d]-Min_SWC[l] << endl;
      }
    }
    
    // Evaporation from soil
    
    // if this should be negligible in dense forest understory, it should have a more important effect in open areas, especially through species filtering at germination stage, at the beginning of a succession or in gaps in drier conditions; see Marthews et al. 2008 Ecological Modelling
    // However it is sometimes neglected and not represented in models, eg. Laio et al. 2001, Guterriez et al. 2014, Fischer et al. 2014..
    // here, we use a phenomenological approach, following Granier et al. 1999 Ecological Modelling and Wagner et al. 2011 AFM, which assumed that evaporation is proportional to the energy reaching the soil.[this is an approximation as as the soil gets drier, more energy would be needed to remove the same amount of water from the soil as water molecules should be more tighly bound to soil particules and cavitation also occur in the soil...] ==> see if a model under which evaporation also depends on the soil water potential would not be better -- I guess so.
    // parameter values are not so clear, so TO BE CHECKED.
    // note that, in this version, evaporation only depletes the most superficial soil layer. This could be changed, especially if the superficial soil layer is particularly thin and the energy reaching the soil high.
    
    float e = Wmean * totday_light * 0.001072706 * exp(-klight*LAI_DCELL[d]) * 0.1 * sites_per_dcell*LH*LH*0.001; 
    // !!!: new formulation of climate file uses Wmean instead of Wmax, more stable 
    //0.001072706=3600*0.5*10^-6/1.678, where 1.678 is to convert Wmax in micromol of PAR /s /m2 into W/m2, 3600*0.5 to convert W in Joule, as daily_light is given with a half-hourly timestep, and 10^-6 to MJoule as in Wagner et al. 2011 (however the value provided by Wagner et al. 2011 seems really weird -too high-, and the values we obtained here are in agreement with the ones reported in Marthews et al. 2014.
    //the value 0.1 is drawn from Wagner et al. 2011, but not really explained... to be checked!
    //sites_per_dcell*LH*LH*0.001 is to convert te amount of water in mm, ie. in 10-3 m3/m2, to the amount of water evaporated for the focal dcell in m3
    
    Evaporation[d]=fminf(e,(SWC3D[0][d]-Min_SWC[0])); // the amount of water evaporated from the soil cannot result in a water content below the residual water content. A model depending on soil matric potential would not need this.
    if(Evaporation[d]<0 || isnan(Evaporation[d])  || (SWC3D[0][d]-Min_SWC[0])<0) Rcout << "evaporation=" << Evaporation[d] << " and e=" << e << " and SWC3D[0][d]=" << SWC3D[0][d] << " and Min_SWC[0]=" << Min_SWC[0] << "and SWC3D[0][d]-Min_SWC[0]=" << SWC3D[0][d]-Min_SWC[0] << endl;
    
    SWC3D[0][d]-=Evaporation[d];
    
    // Refilling by rainfall
    
    Interception[d]=fminf(precip, 0.2*LAI_DCELL[d]);      // This is the amount of rainfall - in mm, as rainfall -, intercepted by vegetation cover, following the approach used in Liang et al. 1994 Journal of Geophysical Reserach, and also used by Laio et al. 2001 Advances in Water Resources and Fischer et al. 2014 Environmental Modelling & Software (FORMIX3, Madagascar). More complex approach can be used however - see eg. Gutierrez et al. 2014 Plos One (FORMIND, Chili), or Wagner et al. 2011 AFM (Paracou) 
    Throughfall[d]=precip-Interception[d];
    Throughfall[d]*=sites_per_dcell*LH*LH*0.001; // to convert in absolute amount of water entering the soil voxel in m3
    
    if(isnan(Throughfall[d]) || (Throughfall[d]) <0 ) {
      Rcout << "Incorrect throughfall" << endl;
      Rcout << precip << "\t" << Interception[d] << "\t" << LAI_DCELL[d] << endl;
    }
    
    float in=Throughfall[d];
    if(SWC3D[0][d]<Max_SWC[0]) {
      int l=0;
      while((l<nblayers_soil) && (in>0.0)) {
        if(in>(Max_SWC[l]-SWC3D[l][d])) {
          in-=(Max_SWC[l]-SWC3D[l][d]);
          SWC3D[l][d]=Max_SWC[l];
          if(isnan(SWC3D[l][d]) || (SWC3D[l][d]-Min_SWC[l])<=0) {
            Rcout << "incorrect SWC3D, Min/Max_SWC" << endl;
            Rcout <<Max_SWC[l] << endl;
          }
        }
        else{
          SWC3D[l][d]+=in;
          if (isnan(SWC3D[l][d]) || (SWC3D[l][d]-Min_SWC[l])<0) {
            Rcout << "incorrect SWC3D, Min/Max_SWC" << endl;
            Rcout << Throughfall[d] << "\t" <<in <<"\t" <<  precip << "\t" << Interception[d] << "\t" << LAI_DCELL[d] << endl;
          }
          in=0.0;
        }
        l++;
      }
    }
    else{ //if the top soil layer is already saturated (eg. inundated forest), throughfall -> runoff
      Runoff[d]=Throughfall[d];
    }
    // Leakage 
    Leakage[d]=in;
  }
  // END of the BUCKET MODEL.
  
  // Update of soil water potential field
  for (int d=0; d<nbdcells; d++) {
    for (int l=0; l<nblayers_soil; l++) {
      //soil_phi3D[l][d]=phi_e[l]*pow((SWC3D[l][d]/Max_SWC[l]), -b[l]);
#ifdef FF_todiscuss
      // I think this could be dangerous. There is already a global theta variable for the Farquhar model. theta here will be save within the loop, but maybe rename?
#endif
      float theta=(SWC3D[l][d]-Min_SWC[l])/(Max_SWC[l]-Min_SWC[l]);
      soil_phi3D[l][d]=phi_e[l]*pow(theta, -b[l]); // this is the soil water characteristic of Brooks & Corey-Mualem (as in Table 1 in Marthews et al. 2014)
      Ks[l][d]=Ksat[l]*pow(theta, 2.5+2*b[l]); // this is the hydraulic conductivity curve of Brooks & Corey-Mualem (as in Table 1 in Marthews et al. 2014)
      KsPhi[l][d]=Ksat[l]*phi_e[l]*pow(theta, 2.5+b[l]); //Ks times soil_phi3D, computed directly as the exact power of theta.
      
      if (isnan(soil_phi3D[l][d]) || isnan(Ks[l][d]) ||  isnan(KsPhi[l][d]) || (SWC3D[l][d]-Min_SWC[l])<0) //|| KsPhi[l][d]==0.0 || Ks[l][d]==0.0 || soil_phi3D[l][d]==0.0)
        Rcout << "In bucket model, layer " << l << " dcell " << d << " theta=" << theta << " SWC3D[l][d]-Min_SWC[l]=" << (SWC3D[l][d]-Min_SWC[l]) << " soil_phi3D[l][d]=" << soil_phi3D[l][d] << " Ksat=" << Ksat[l] << " phi_e=" << phi_e[l] <<" b[l]=" << b[l] << " KsPhi[l][d]=" << KsPhi[l][d] << " Ks[l][d]=" << Ks[l][d] << endl ;
      //KsPhi2[l][d]=Ksat[l]*phi_e[l]*pow(theta, 2.5);
      // we may want to shift to the van Genuchten-Mualem expressions of soil_phi3D and Ks, as the van genuchten-Mualem model is currently defacto the more santard soil hydraulic model (see ref in Table 1 in Marthews et al. 2014). To do so, see if we have data of soil pH, cation exchange capacity, organic carbon content, to explicitly compute the parameters with Hodnett & Tomasella 2002 (as recommended by Marthews et al. 2014 -- Table 2; or instead directly use the parameter provided by the map in Marthews et al. 2014.
    }
  }
#endif
}

//#############################
// Global function: update SPECIES_SEEDS field
//#############################
void FillSeed(int col, int row, int spp) {
  if((col >= 0) && (col < cols)) {
    if((row >=0) && (row < rows)) {
      int site = col + cols * row;
      //if(T[site].t_age == 0){
      if(_SEEDTRADEOFF) SPECIES_SEEDS[site][spp]++; // ifdef SEEDTRADEOFF, SPECIES_SEEDS[site][spp] is the number of seeds of this species at that site 
      else SPECIES_SEEDS[site][spp] = 1;     // If s_Seed[site] = 0, site is not occupied, if s_Seed[site] > 1, s_Seed[site] is the age of the youngest seed  
      //Rcout << "site: " << site << " spp: " << spp << " Seed added!!! " << endl;
      //}
    }
  }
}

//#############################
// Global function: tree germination module
//#############################
void RecruitTree(){
  for(int site=0;site<sites;site++) {  //**** Local germination ****
    if(T[site].t_age == 0) {
      int spp_withseeds = 0;
      for(int spp=1;spp<=nbspp;spp++){  // lists all the species with a seed present at given site... 
        if(SPECIES_SEEDS[site][spp] > 0) {
          // write species that are present to an extra array 
          SPECIES_GERM[spp_withseeds]=spp;
          spp_withseeds++;
        }
      }
      if(spp_withseeds > 0) {  // ... and then randomly select one of these species 
        
        // new in v.2.4.1: for consistency use genrand2() instead of rand(), since v.2.5: use gsl RNG 
        int spp_index = int(gsl_rng_uniform_int(gslrand,spp_withseeds));
        int spp = SPECIES_GERM[spp_index];
        // otherwise all species with seeds present are equiprobable 
        float flux = WDailyMean*exp(-fmaxf(LAI3D[0][site+SBORD],0.0)*kpar);
#ifdef WATER
        if(flux>(S[spp].s_LCP) && soil_phi3D[0][site_DCELL[site]]>0.5*S[spp].s_tlp) T[site].Birth(spp,site);
        // in addition to a condition of light availability- hence light demanding species may not be able to grow in deep sahde conditions in understorey -, a condition on water availability is added - hence drought-intolerant species may not be recruited in water-stressd conditions 
#else
        // If enough light, germination, initialization of NPP (LCP is the species light compensation point
        // here, light is the sole environmental resources tested as a limiting factor for germination, but we should think about adding nutrients (N,P) and water conditions... 
        if(flux>(S[spp].s_LCP))  T[site].Birth(spp,site);
#endif
      }
    }
  }
}

//#############################
// Global function: Treefall gap formation
//#############################
//! change in v.2.4: resetting Thurt[0] field is done in TriggerSecondaryTreefall() at the beginning of each iteration. Further changes: rewriting of Tree::FallTree() which is now Tree::Treefall(angle). t_hurt can now persist longer, so new treefall events are added to older damages (that, in turn are decaying) 
void TriggerTreefall(){
  for(int site=0;site<sites;site++)
    if(T[site].t_age) {
      // treefall is triggered given a certain flexural force 
      // _BASICTREEFALL: just dependent on height threshold + random uniform distribution 
      float angle = 0.0, c_forceflex = 0.0;
      if(_BASICTREEFALL){
        c_forceflex = gsl_rng_uniform(gslrand)*T[site].t_Tree_Height;     // probability of treefall = 1-t_Ct/t_Tree_Height, compare to genrand2(): genrand2() < 1 - t_Ct/t_Tree_Height, or: genrand2() > t_Ct/t_Tree_Height
        angle = float(twoPi*gsl_rng_uniform(gslrand));                    // random angle
      }
      // above a given stress threshold the tree falls 
      if(c_forceflex > T[site].t_Ct){
        T[site].Treefall(angle);
      }
    }
#ifdef MPI
    // Treefall field passed to the n.n. procs 
    MPI_ShareTreefall(Thurt, sites);
#endif
    for(int site=0;site<sites;site++){
      // Update of Field hurt 
      if(T[site].t_age) {
        T[site].t_hurt = max(Thurt[0][site+sites],T[site].t_hurt);                 // NEW in v.2.4: addition of damages, alternative: max()
#ifdef MPI
        if(mpi_rank) T[site].t_hurt = max(T[site].t_hurt,Thurt[1][site]);               // ? v.2.4: Update needed, Thurt[1], why max?
        if(mpi_rank<mpi_size-1) T[site].t_hurt = max(T[site].t_hurt,Thurt[2][site]);
#endif
      }
    }
}

//#############################
// Global function: Secondary treefall gap formation
//#############################
//! - NEW in v.2.4: TriggerSecondaryTreefall(), called at the beginning of each iteration 
//! - translates damages from previous round into tree deaths, partly treefalls, partly removing them only (e.g. splintering) 
//! - in the limit of p_tfsecondary = 0.0, this is equivalent to the previous computation 
void TriggerTreefallSecondary(){
  nbTreefall1 = 0;
  nbTreefall10 = 0;
  nbTreefall30 = 0;
#ifdef Output_ABC
  nbTreefall10_abc = 0;
#endif
  for(int site=0;site<sites;site++){
    Thurt[0][site] = Thurt[0][site+2*sites] = 0;
    Thurt[0][site+sites] = 0;
  }
  for(int site=0;site<sites;site++){
    if(T[site].t_age){
      float height_threshold = T[site].t_Tree_Height/T[site].t_intraspecific_multiplier_height;  // since 2.5: a tree's stability is defined by its species' average height, i.e. we divide by the intraspecific height multiplier to account for lower stability in quickly growing trees; otherwise slender, faster growing trees would be treated preferentially and experience less secondary treefall than more heavily built trees
      if(2.0*T[site].t_hurt*gsl_rng_uniform(gslrand) > height_threshold) {        // check whether tree dies: probability of death is 1.0-0.5*t_Tree_Height/t_hurt, so gslrand <= 1.0 - 0.5 * t_Tree_Height/t_hurt, or gslrand > 0.5 * t_Tree_Height/t_hurt; modified in v.2.5: probability of death is 1.0 - 0.5*t_Tree_Height/(t_intraspecific_multiplier_height * t_hurt), so the larger the height deviation (more slender), the higher the risk of being thrown by another tree
        if(p_tfsecondary > gsl_rng_uniform(gslrand)){                              // check whether tree falls or dies otherwise
          float angle = float(twoPi*gsl_rng_uniform(gslrand));                    // random angle
          T[site].Treefall(angle);
        } else {
          T[site].Death();
        }
      } else {
        T[site].t_hurt = short(hurt_decay * float(T[site].t_hurt));                                // reduction of t_hurt according to hurt_decay, could be moved to Tree::Growth() function and made dependent on the tree's carbon gain
      }
    }
  }
  
#ifdef MPI
  //! Treefall field passed to the n.n. procs 
  MPI_ShareTreefall(Thurt, sites);
#endif
}

// Helper function 
//! - upper bound on LAI within one voxel and above voxel (beyond 9.95 and 19.95, none of the environmental variables should change), needed for LookUp tables
int CalcIntabsorb(float absorb_prev, float absorb_delta){
  absorb_delta = fminf(absorb_delta,9.95);
  absorb_prev = fminf(absorb_prev,19.95);
  int intabsorb = int(absorb_prev*20.0) + 400*int(absorb_delta*20.0);
  return(intabsorb);
}
// Helper function 
//! - upper bound on LAI within one voxel and above voxel, needed for LookUp tables, for cases where there won't be any absorption within the voxel (absorb_delta = 0.0)
int CalcIntabsorb(float absorb_prev){
  absorb_prev = fminf(absorb_prev,19.95);
  int intabsorb = int(absorb_prev*20.0);
  return(intabsorb);
}

//##############################################
//######        Output routines         ########
//##############################################

//##############################################
// Global function: calculation of the global averages every timestep
//##############################################
void Average(void){
  
#ifdef TRACK_INDIVIDUALS
  TrackingData_andOutput();
#endif
  
#ifdef Output_ABC
  UpdateMovingAveragesABC();
#endif
  
  int site,spp,i;
  float sum1=0.0,sum10=0.0,sum30=0.0,ba=0.0,npp=0.0,gpp=0.0, ba10=0.0, agb=0.0, rday=0.0, rnight=0.0, rstem=0.0, litterfall=0.0;
  
  if(!mpi_rank) {
    for(spp=1;spp<=nbspp;spp++)
      for(i=0;i<12;i++)
        S[spp].s_output_field[i]=0;
    float inbcells = 1.0/float(sites*mpi_size);
    float inbhectares = inbcells*NH*NH*10000.0;
    
    if(_OUTPUT_reduced){
      output[0] << iter << "\t";
      //for(spp=1;spp<=nbspp;spp++) output[0] << float(S[spp].s_nbind)*inbhectares << "\t";
    }
    else{
      for(i=0;i<7;i++) output[i] << iter << "\t";
      for(i=20;i<23;i++) output[i] << iter << "\t";
      for(spp=1;spp<=nbspp;spp++) output[0] << float(S[spp].s_nbind)*inbhectares << "\t";
    }
    for(site=0;site<sites;site++)T[site].Average();
    
    for(spp=1;spp<=nbspp;spp++) {
      S[spp].s_output_field[1] *= inbhectares; //species number of trees with dbh>10cm
      S[spp].s_output_field[2] *= inbhectares; //species number of trees with dbh>30cm
      S[spp].s_output_field[3] *= 3.1415*0.25*inbhectares; //species basal area
      S[spp].s_output_field[4] *= inbhectares; //species total NPP (sum of t_NPP) in MgC (per timestep)
      S[spp].s_output_field[5] *= inbhectares; //species total GPP (sum of t_GPP) in MgC (per timestep)
      S[spp].s_output_field[6] *= 3.1415*0.25*inbhectares; //species basal area; with only trees with dbh>10cm
      S[spp].s_output_field[7] *= inbhectares; //species aboveground biomass
      S[spp].s_output_field[8] *= inbhectares; //species leaf Rday in MgC (per timestep) 
      S[spp].s_output_field[9] *= inbhectares; //species leaf Rnight in MgC (per timestep)
      S[spp].s_output_field[10] *= inbhectares; //species Rstem  in MgC (per timestep)
      S[spp].s_output_field[11] *= inbhectares; //species litterfall  in MgC (per timestep)
      sum1+= float(S[spp].s_nbind)*inbhectares;
      sum10 += S[spp].s_output_field[1];
      sum30 += S[spp].s_output_field[2];
      ba += S[spp].s_output_field[3];
      npp += S[spp].s_output_field[4];
      gpp += S[spp].s_output_field[5];
      ba10 += S[spp].s_output_field[6];
      agb += S[spp].s_output_field[7];
      rday += S[spp].s_output_field[8];
      rnight += S[spp].s_output_field[9];
      rstem += S[spp].s_output_field[10];
      litterfall += S[spp].s_output_field[11];
      
      if(!_OUTPUT_reduced){
        for(i=1;i<7;i++) output[i] << S[spp].s_output_field[i] << "\t";
        output[19] << S[spp].s_output_field[7] << "\t";
        output[20] << S[spp].s_output_field[8] << "\t";
        output[21] << S[spp].s_output_field[9] << "\t";
        output[22] << S[spp].s_output_field[10] << "\t";
        output[27] << S[spp].s_output_field[11] << "\t";
      }
    }
    
    if (_NDD) BAtot=ba;
    
    for(int site = 0; site < sites; site++){
      if(T[site].t_age > 0){
        if(T[site].t_NPP <= 0.0){
          nbtrees_carbstarv_n1++;
          if(T[site].t_dbh >= 0.1) nbtrees_carbstarv_n10++;
          if(T[site].t_dbh >= 0.3) nbtrees_carbstarv_n30++;
        }
      }
    }
    
    Rcout.setf(ios::fixed,ios::floatfield);
    Rcout.precision(5);
    
#ifdef CHECK_CARBON
    float carbon_stored_leaves_previous, carbon_stored_trunk_previous = 0.0,carbon_stored_free_previous = 0.0, carbon_assimilated_total_previous = 0.0, carbon_net_total_previous = 0.0;
    
    if(iter == 0){
      carbon_stored_leaves_previous = 0.0;
      carbon_stored_trunk_previous = 0.0;
      carbon_stored_free_previous = 0.0;
    }
    else{
      carbon_stored_leaves_previous = carbon_stored_leaves;
      carbon_stored_trunk_previous = carbon_stored_trunk;
      carbon_stored_free_previous = carbon_stored_free;
    }
    carbon_stored_leaves = 0.0;
    carbon_stored_trunk = 0.0;
    carbon_stored_free = 0.0;
    for(int s = 0; s < sites; s++){
      if(T[s].t_age > 0){
        float agb = 1000.0 * T[s].CalcAGB(); // convert to g
        float carbon_trunk = agb * 0.5;
        carbon_stored_trunk += carbon_trunk;
        
        float carbon_free = T[s].t_carbon_storage;
        carbon_stored_free += carbon_free;
        
        float carbon_leaves = T[s].t_leafarea * T[s].t_LMA * 0.5;
        carbon_stored_leaves += carbon_leaves;
      }
    }
    
    float factor_weight = 0.000001; // factor to convert carbon from g to tons
    
    Rcout << iter << "\tTrunkC: " << carbon_stored_trunk *  factor_weight << " LeavesC: " << carbon_stored_leaves *  factor_weight  << " FreeC: " << carbon_stored_free *  factor_weight << " Total AssimC: " << carbon_assimilated_total *  factor_weight << " Total NetC: " << carbon_net_total *  factor_weight << endl;
    Rcout << iter << "\tTrunkC change: " << (carbon_stored_trunk - carbon_stored_trunk_previous) *  factor_weight << " LeavesC: " << (carbon_stored_leaves - carbon_stored_leaves_previous) *  factor_weight  << " FreeC: " << (carbon_stored_free - carbon_stored_free_previous) *  factor_weight << endl;
#endif
    Rcout.setf(ios::fixed,ios::floatfield);
    Rcout.precision(2);
    
    Rcout << iter << "\tTrees (1/ha): " << sum1 << " | " << sum10 << " | " << sum30 << " *** nbdead (%): " << 100.0*nbdead_n1 * inbhectares/sum1 << " | " << 100.0*nbdead_n10 * inbhectares/sum10 << " | " << 100.0*nbdead_n30 * inbhectares/sum30 << " *** AGB (t/ha): " << round(agb/1000.0) << " GPP (MgC/ha/yr) " << gpp*iterperyear << " NPP " << npp*iterperyear << " litterfall (Mg/ha/yr) " << litterfall*iterperyear << endl;
    
    if(_OUTPUT_reduced){
      output[0] << sum1 << "\t";                                                     //total number of trees (dbh>1cm=DBH0)
      //for(spp=1;spp<=nbspp;spp++) output[0] << S[spp].s_output_field[1] << "\t";
      output[0] << sum10 << "\t";                                                    //total number of trees (dbh>10cm=DBH0)
      //for(spp=1;spp<=nbspp;spp++) output[0] << S[spp].s_output_field[2] << "\t";
      output[0] << sum30 << "\t";                                                    //total number of trees (dbh>30cm=DBH0)
      //for(spp=1;spp<=nbspp;spp++) output[0] << S[spp].s_output_field[6] << "\t";
      output[0] << ba10 << "\t";
      //for(spp=1;spp<=nbspp;spp++) output[0] << S[spp].s_output_field[4] << "\t";
      output[0] << npp << "\t";
      //for(spp=1;spp<=nbspp;spp++) output[0] << S[spp].s_output_field[5] << "\t";
      output[0] << gpp << "\t";
      //for(spp=1;spp<=nbspp;spp++) output[0] << S[spp].s_output_field[7] << "\t";
      output[0] << agb << endl;
    }
    else{
      output[0] << sum1 << endl;          //total number of trees (dbh>1cm=DBH0)
      output[1] << sum10 << endl;         //total number of trees with dbh>10cm
      output[2] << sum30 << endl;         //total number of trees with dbh>30cm
      output[3] << ba << endl;            //total basal area
      output[4] << npp << endl;           //total NPP in MgC per ha (per time step)
      output[5] << gpp << endl;           //total GPP in MgC par ha (per time step)
      output[6] << ba10 << endl;          //total basal area with only trees with dbh>10cm
      output[19] << agb << endl;          //total above ground biomass
      output[20] << rday << endl;         //total leaf day respiration
      output[21] << rnight << endl;       //total leaf night respiration
      output[22] << rstem << endl;        //total stem respiration
      output[27] << litterfall << endl;   //total litterfall
      
      float tototest=0.0, tototest2=0.0, flux;
      for(int site=0;site<sites;site++) {
        flux = WDailyMean*exp(-fmaxf(LAI3D[0][site+SBORD],0.0)*kpar);
        tototest += flux;
        tototest2 += flux*flux;
      }
      tototest /=float(sites*LH*LH);                              // Average light flux (PPFD) on the ground
      tototest2 /=float(sites*LH*LH);
      if(iter) output[7] << iter<< "\tMean PPFDground\t"<< tototest << "\t" << sqrt(tototest2-tototest*tototest) << "\n";
      
      if(!_OUTPUT_reduced){
        if(_BASICTREEFALL) output[8] << iter << "\t" << nbdead_n1*inbhectares << "\t" << nbdead_n10*inbhectares<< "\t" << nbTreefall1*inbhectares << "\t" << nbTreefall10*inbhectares << "\t" << endl;
        else output[8] << iter << "\t" << nbdead_n1*inbhectares << "\t" << nbdead_n10*inbhectares<< "\t" << endl;
      }
    }
  }
  
#ifdef WATER
  
  float evapo=0.0, runoff=0.0, leak=0.0, interception=0.0, throughfall=0.0;
  for (int d=0; d<nbdcells;d++) {
    evapo+=Evaporation[d]; // in m3
    interception+=Interception[d]; // in mm, as rainfall
    throughfall+=Throughfall[d]; // in m3
    runoff+=Runoff[d]; // in m3
    leak+=Leakage[d]; // in m3
  }
  
  float isites=1.0/float(sites*LH*LH);
  float icells=1.0/float(nbdcells);
  evapo*=isites; // in m
  runoff*=isites; // in m
  leak*=isites; // in m
  throughfall*=isites; // in m
  interception*=isites*0.001; // in m
  
  output[11] << precip << "\t" << interception << "\t" << throughfall << "\t" << runoff << "\t" << leak << "\t" << evapo << "\t";
  
  for(int l=0; l<nblayers_soil; l++) {
    float transpi=0.0;                  // FF: now local variable, more secure
    for(int d=0; d<nbdcells;d++) {
      transpi+=Transpiration[l][d];  // in m3
    }
    transpi*=isites; // in m
    output[11] << transpi << "\t";
  }
  
  float layer_depth_previous = 0.0;        // FF: since the layer depths and layer thickness contain redundant information, they are now computed from the same data line
  for(int l=0; l<nblayers_soil; l++) {
    float soilWC=0.0;                    // FF: now local variable, more secure
    for (int d=0; d<nbdcells;d++) {
      soilWC+=SWC3D[l][d]; // in m3
    }
    float layer_depth_current = layer_depth[l];
    float layer_thickness = layer_depth_current - layer_depth_previous;
    soilWC*=isites/layer_thickness;  // in m3/m3
    output[11] << soilWC << "\t";
    layer_depth_previous = layer_depth_current;
  }
  for(int l=0; l<nblayers_soil; l++) {
    float soilPhi=0.0;                  // FF: now local variable, more secure
    for(int d=0; d<nbdcells;d++) {
      soilPhi+=soil_phi3D[l][d];  //in MPa
    }
    soilPhi*=icells; // in MPa
    output[11] << soilPhi << "\t";
    soilPhi=0.0;
  }
  
  output[11] <<"\n";
#endif
  
#ifdef MPI
  MPI_Reduce(&(S[spp].s_nbind),&sind,1,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD);
  MPI_Reduce(S[spp].s_output_field,S[spp].s_output_field,5,MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD);
  MPI_Reduce(Mortality,Mortality,4,MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD);
  MPI_Reduce(&S[spp].s_output_field[6],&S[spp].s_output_field[6],5,MPI_FLOAT,MPI_MAX,0,MPI_COMM_WORLD);
#endif
  
#ifdef MPI
  // This section corresponds to the parallel version of
  // the reporting of the global diagnostic variables. Since much work has been done
  // on routine Average over the past years, this would need a full rewrite
  // !!!!Action 20/01/2016: rework the parallel version of function Average!!!!
  
  MPI_Reduce(&(S[spp].s_nbind),&sind,1,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD);
  MPI_Reduce(S[spp].s_output_field,S[spp].s_output_field,5,MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD);
  MPI_Reduce(Mortality,Mortality,4,MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD);
  MPI_Reduce(&S[spp].s_output_field[6],&S[spp].s_output_field[6],5,MPI_FLOAT,MPI_MAX,0,MPI_COMM_WORLD);
#endif
  Rcout.flush();
}

//##############################################
// Global function: output of the field variables every timestep
//##############################################
void OutputField(){
  int site,h;
  if((nbout)&&((iter%freqout)==freqout-1)) {
    // output fields, nbout times during simulation (every freqout iterations)
    int d;
    for(d=0;d<dbhmaxincm;d++) nbdbh[d]=0;
    for(site=0;site<sites;site++) T[site].histdbh();
    
    for(h=0;h<(HEIGHT+1);h++){
      layer[h] = 0;
      for(site=0;site<sites;site++) layer[h] += LAI3D[h][site+SBORD];
    }
    
#ifdef MPI
    MPI_Status status;
    MPI_Reduce(nbdbh,nbdbh,dbhmaxincm,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD);
    MPI_Reduce(layer,layer,HEIGHT,MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD);
#endif
    if(!mpi_rank) {
      // output of the dbh histograms (output[31])
      for(d=1;d<dbhmaxincm;d++) output[31] << d << "\t" << nbdbh[d]  << "\n";
      output[31] <<  "\n";
      // output of the mean LAI per height class (output[32])
      float norm = 1.0/float(sites*LH*LH*mpi_size);
      for(h=0;h<(HEIGHT+1);h++) output[32] << h*LV << "\t" << layer[h]*norm << "\n";
      output[32] <<  "\n";
    }
  }
}

//##############################################
// Global function: output snapshots of the scene at one point in time
//##############################################
//! - This can be used to take snapshots of the forest in more detail and track its development over time. Currently used for 100 years development analysis. 
void OutputSnapshot(fstream& output, bool header, float dbh_limit){
  if(header == 1){
    output << "iter\tcol\trow\tage\ts_name\tdbh\theight\tCR\tCD\tAGB\tLAImax\tLA_max\tLA\tdens\tGPP\tNPP\tLMA\tPmass\tNmass\twsg\tdev_height\tdev_CR\tdev_CD\tdev_P\tdev_N\tdev_LMA\tdev_wsg";
#ifdef Output_ABC
    output << "\tdbh_previous";
#endif
    output << "\tleaflifespan\tyoungLA\tmatureLA\toldLA\ttime_young\ttime_mature\ttime_old\tlitter\tCt\tdmax\tcarbon_storage\tcarbon_biometry\tLAIexperienced_eff\tleafarea_maxrecalc\tVcmax\tJmax";
    output <<  endl;
  }
  //output.setf(ios::fixed,ios::floatfield);
  output.precision(5);
  for(int row=0;row<rows;row++)
    for(int col=0;col<cols;col++){
      if(T[col + cols*row].t_age > 0 && T[col + cols*row].t_dbh >= dbh_limit){
        float LAIexperienced_eff, leafarea_max;
        T[col + cols*row].leafarea_max(LAIexperienced_eff, leafarea_max);
        output << iter << "\t" << col << "\t" << row << "\t" << T[col + cols*row].t_age << "\t" << S[T[col + cols*row].t_sp_lab].s_name << "\t" << T[col + cols*row].t_dbh << "\t" << T[col + cols*row].t_Tree_Height << "\t" << T[col + cols*row].t_Crown_Radius << "\t" << T[col + cols*row].t_Crown_Depth << "\t" << 0.0673*pow(T[col + cols*row].t_wsg*T[col + cols*row].t_Tree_Height*LV*T[col + cols*row].t_dbh*T[col + cols*row].t_dbh*LH*LH*10000, 0.976) << "\t" << T[col + cols*row].t_LAImax << "\t" << T[col + cols*row].t_leafarea_max<< "\t" << T[col + cols*row].t_leafarea << "\t" << T[col + cols*row].t_LAI << "\t" << T[col + cols*row].t_GPP << "\t" << T[col + cols*row].t_NPP << "\t" << T[col + cols*row].t_LMA<< "\t" << T[col + cols*row].t_Pmass << "\t" << T[col + cols*row].t_Nmass << "\t" << T[col + cols*row].t_wsg  << "\t" << log(T[col + cols*row].t_intraspecific_multiplier_height) << "\t" << log(T[col + cols*row].t_intraspecific_multiplier_CR) << "\t" << log(T[col + cols*row].t_intraspecific_multiplier_CD) << "\t" << log(T[col + cols*row].t_intraspecific_multiplier_P) << "\t" << log(T[col + cols*row].t_intraspecific_multiplier_N) << "\t" << log(T[col + cols*row].t_intraspecific_multiplier_LMA) << "\t" << T[col + cols*row].t_intraspecific_deviation_wsg;
#ifdef Output_ABC
        output << "\t" << T[col + cols*row].t_dbh_previous;
#endif
        output << "\t" << T[col + cols*row].t_leaflifespan << "\t" << T[col + cols*row].t_youngLA << "\t" << T[col + cols*row].t_matureLA << "\t" << T[col + cols*row].t_oldLA << "\t" << T[col + cols*row].t_lambda_young << "\t" << T[col + cols*row].t_lambda_mature << "\t" << T[col + cols*row].t_lambda_old << "\t" << T[col + cols*row].t_litter << "\t" << T[col + cols*row].t_Ct << "\t" << T[col + cols*row].t_dbhmax << "\t" << T[col + cols*row].t_carbon_storage << "\t" << T[col + cols*row].t_carbon_biometry << "\t" << LAIexperienced_eff << "\t" << leafarea_max << "\t" << T[col + cols*row].t_Vcmax << "\t" << T[col + cols*row].t_Jmax;
        output << endl;
      }
    }
}

#ifdef Output_ABC
//##############################################
// Global ABC function: creates a Canopy Height Model and LAD profile
//##############################################
void OutputCHM(fstream& output_CHM){
  
#ifdef CHM_SPIKEFREE
  vector<int> chm_temporary(sites,0);
  
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int s = c + r*cols;
      if(T[s].t_age > 0){
#ifdef CROWN_UMBRELLA
        float height = T[s].t_Tree_Height;
        float CR = T[s].t_Crown_Radius;
        float CD = T[s].t_Crown_Depth;
        float fraction_filled_target = 1.0;         // to remove spikes, we assume a fully filled crown for all trees
        int shell_fromtop = 0;                      // toplayer
        float noinput = 0.0;
        
        LoopLayerUpdateCrownStatistic_template(r, c, height, CR, CD, fraction_filled_target, shell_fromtop, GetRadiusSlope, noinput, chm_temporary, ModifyNoinput, UpdateCHMvector);
#else
        int crown_top = int(T[s].t_Tree_Height);
        
        float crown_area = PI * T[s].t_Crown_Radius * T[s].t_Crown_Radius;     // floor of crown_area to bound area accumulation
        int crown_intarea = int(crown_area);   // floor of crown_area to bound area accumulation
        crown_intarea = max(crown_intarea,1);                                 // minimum area of crown (1)
        crown_intarea = min(crown_intarea,1963);                              // maximum area of crown (radius 25), int(3.14*25*25)
        
        int row_crowncenter = s/cols;
        int col_crowncenter = s%cols;
        
        for(int i = 0; i < crown_intarea; i++){
          int site_relative = LookUp_Crown_site[i];
          int row = row_crowncenter + site_relative/51 - 25;
          int col = col_crowncenter + site_relative%51 - 25;
          if(row >= 0 && row < rows && col >= 0 && col < cols){
            int site = col + row * cols;
            if(chm_temporary[site] < crown_top) chm_temporary[site] = crown_top;
          }
        }
#endif
      }
    }
  }
  
  output_CHM  << "site" << "\t" << "row" << "\t" << "col" << "\t"  << "height" << "\t" << "LAI" << "\t" << "height_current" << "\t" << "height_spikefree" << endl;
  for(int s=0;s<sites;s++){
    int height_canopy=0;
    for(int h=0;h<(HEIGHT+1);h++)
      if(LAI3D[h][s+SBORD] > 0.0) height_canopy = max(h,height_canopy);
      output_CHM << s << "\t" << int(s/cols) << "\t" << int(s%cols) << "\t"  << height_canopy+1 << "\t" << LAI3D[0][s+SBORD] << "\t" << chm_field_current[s] << "\t" << chm_temporary[s] << endl;
  }
#else
  Rcout << "site" << "\t" << "row" << "\t" << "col" << "\t"  << "height" << "\t" << "LAI" << endl;
  for(int s=0;s<sites;s++){
    int height_canopy=0;
    for(int h=0;h<(HEIGHT+1);h++)
      if(LAI3D[h][s+SBORD] > 0.0) height_canopy = max(h,height_canopy);
      output_CHM << s << "\t" << int(s/cols) << "\t" << int(s%cols) << "\t"  << height_canopy+1 << "\t" << LAI3D[0][s+SBORD] << endl;
  }
#endif
}
#endif

//##############################################
// Global function: writes the whole 3D LAI voxel field to file 
//##############################################
void OutputLAI(fstream& output_transmLAI3D){
  for(int s=0;s<sites;s++)
    for(int h=0; h<HEIGHT;h++)
      output_transmLAI3D << s << "\t" << int(s/cols) << "\t" << int(s%cols) << "\t" << h << "\t" << LAI3D[h][s+SBORD] << endl;
}

#ifdef TRACK_INDIVIDUALS
//##############################################
// Global function: tree level tracking of key variables 
//##############################################
void TrackingData_andOutput(){
  for(int site = 0; site < sites; site++){
    // we start the accounting the year after the trees have been born 
    if(T[site].t_age > 0){
      int month_born = T[site].t_month_born;
      
      if(month_born >= 0){
        // these are the yearly figures, reset to zero every year 
        T[site].t_GPP_sumyear += T[site].t_GPP;
        T[site].t_GPPsquared_sumyear += T[site].t_GPP * T[site].t_GPP;
        T[site].t_NPP_sumyear += T[site].t_NPP;
        T[site].t_NPPsquared_sumyear += T[site].t_NPP * T[site].t_NPP;
        T[site].t_Rday_sumyear += T[site].t_Rday;
        T[site].t_Rnight_sumyear += T[site].t_Rnight;
        T[site].t_Rstem_sumyear += T[site].t_Rstem;
        T[site].t_carbon_storage_avgyear += T[site].t_carbon_storage * timestep;
        // these are the whole lifetime cumulated figures, never reset to zero and put out at tree death 
        T[site].t_GPPcum += T[site].t_GPP;
        T[site].t_NPPcum += T[site].t_NPP;
        T[site].t_GPPsquared_cum += T[site].t_GPP * T[site].t_GPP;
        T[site].t_NPPsquared_cum += T[site].t_NPP * T[site].t_NPP;
        
        float agb = 1000.0 * 0.0673*pow(T[site].t_wsg*T[site].t_Tree_Height*LV*T[site].t_dbh *T[site].t_dbh *LH*LH*10000.0, 0.976); // !!!UPDATE
        
        // write to output every year and then reset to zero 
        if(iter%iterperyear == month_born){
          // write to output 
          if(T[site].t_dbh >= 0.1) output[29] << T[site].t_site << "\t" << month_born << "\t" << iter << "\t" << T[site].t_age << "\t" << T[site].t_seedsproduced_sumyear << "\t" << T[site].t_seedsproduced << "\t" << T[site].t_time_carbonstarvation_year << "\t" << T[site].t_time_carbonstarvation << "\t" << T[site].t_dbh << "\t" << T[site].t_dbh - T[site].t_dbh_tracked << "\t" << T[site].t_Tree_Height << "\t"  <<  T[site].t_Tree_Height - T[site].t_Tree_Height_tracked << "\t" << T[site].t_Crown_Radius << "\t"  <<  T[site].t_Crown_Radius - T[site].t_Crown_Radius_tracked << "\t" << agb << "\t" << agb - T[site].t_agb_tracked << "\t" << T[site].t_GPP_sumyear << "\t" << T[site].t_GPPsquared_sumyear  << "\t" << T[site].t_NPP_sumyear << "\t" << T[site].t_NPPsquared_sumyear << "\t" << T[site].t_Rday_sumyear << "\t" << T[site].t_Rnight_sumyear << "\t" << T[site].t_Rstem_sumyear << "\t" << T[site].t_LAIabove_effavgyear<< "\t" << T[site].t_carbon_storage_avgyear << endl;
          // reset 
          T[site].t_time_carbonstarvation_year = 0;
          T[site].t_seedsproduced_sumyear = 0;
          T[site].t_GPP_sumyear = 0.0;
          T[site].t_GPPsquared_sumyear = 0.0;
          T[site].t_NPP_sumyear = 0.0;
          T[site].t_NPPsquared_sumyear = 0.0;
          T[site].t_Rday_sumyear = 0.0;
          T[site].t_Rnight_sumyear = 0.0;
          T[site].t_Rstem_sumyear = 0.0;
          T[site].t_LAIabove_effavgyear = 0.0;
          T[site].t_carbon_storage_avgyear = 0.0;
          T[site].t_dbh_tracked = T[site].t_dbh;
          T[site].t_Tree_Height_tracked = T[site].t_Tree_Height;
          T[site].t_Crown_Radius_tracked = T[site].t_Crown_Radius;
          T[site].t_agb_tracked = agb;
        }
      }
    }
  }
}
#endif

#ifdef Output_ABC
//##############################################
// Global ABC function: yearly statistics
//##############################################
//! - Statistics averaged over one year for ten years and then used for the point estimates in the OutputABC(...) routine 
//! - !!!: TODO needs to be checked for different timesteps than 1 month
void UpdateMovingAveragesABC(){
  
  int nbtrees_abc = 0;
  float inbhectares_abc = 10000.0/float(sites_abc);
  float GPP_abc = 0.0, litter_abc = 0.0;
  
  for(int row = row_start; row < row_end; row++){
    for(int col = col_start; col < col_end; col++){
      int site = col + row * cols;
      GPP_abc += T[site].t_GPP*1.0e-6;
      litter_abc += T[site].t_litter*1.0e-6;
      if(T[site].t_dbh >= 0.1) nbtrees_abc++;
    }
  }
  
  GPP_abc *= inbhectares_abc;
  litter_abc *= inbhectares_abc;
  
  float mortality_abc;
  float treefall_abc;
  if(nbtrees_abc > 0){
    mortality_abc = float(nbdead_n10_abc)/float(nbtrees_abc);
    treefall_abc = float(nbTreefall10_abc)/float(nbtrees_abc);
  }
  else{
    mortality_abc = 0.0;
    treefall_abc = 0.0;
  }
  if(iter < 120){
    GPP_MA[iter] = GPP_abc;
    Litterfall_MA[iter] = litter_abc;
    Mortality_MA[iter] = mortality_abc;
    Treefall_MA[iter] = treefall_abc;
  }
  else{
    // move values down 
    for(int i=0; i<119;i++){
      GPP_MA[i] = GPP_MA[i+1];
      Litterfall_MA[i] = Litterfall_MA[i+1];
      Mortality_MA[i] = Mortality_MA[i+1];
      Treefall_MA[i] = Treefall_MA[i+1];
    }
    // new value at the top 
    GPP_MA[119] = GPP_abc;
    Litterfall_MA[119] = litter_abc;
    Mortality_MA[119] = mortality_abc;
    Treefall_MA[119] = treefall_abc;
  }
}

//##############################################
// Global ABC function: update DBH function for ABC routines
//##############################################
void UpdateDBHtrackingABC(){
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int s = c + r * cols;
      if(T[s].t_age > 0) T[s].t_dbh_previous = T[s].t_dbh;
    }
  }
}

//##############################################
// Global function: calculating the TROLL transmittance field from simulated LiDAR 
//##############################################
//! - First draws from a distribution to calculate the sampling density, i.e. the number of beams per voxel column 
//! - Then loop over voxel column from top to bottom and calculate the number of hits given the density of the respective voxel 
//! - If beams are all extinct, NAs are returned (-1), if beams hit ground, all produce guaranteed returns 
//! - The parameters used is the k assumed for the laser, which is only based on leaf geometry and should thus be taken equal to klight 
//! - The difference to kpar is accounted for by parameterizing the transmittance of leaves, which, in the NIR spectrum, is much larger than for visible light (0.4 vs. 0.1) 
//! - Results are saved in transmittance_simulatedALS_sampling for the number of beams, transmittance_simulatedALS for the transmittance 
void UpdateTransmittanceCHM_ABC(int mean_beam, float sd_beam, float klaser, float transmittance_laser){
  //loop over the LAI3D field 
  for(int r = row_start; r < row_end; r++){
    for(int c = col_start; c < col_end; c++){
      int site = c + r * cols;
      int nbbeams = int(mean_beam + gsl_ran_gaussian(gslrand, sd_beam));    // always rounding up
      nbbeams = max(nbbeams,1);
      
      //loop over the field from maximum height to 0 and iteratively update voxels from top to bottom, following the beam. An alternative version, also allowing for ground returns, can be activated to extending the loop to h >= -1. In this case, when a beam is not extinguished before it reaches the ground (h >= 0), then it is counted as a ground return 
      for(int h = HEIGHT - 1; h >= 0; h--){
        //for(int h = HEIGHT - 1; h >= -1; h--){
        int hits;
        float transmittance;
        transmittance_simulatedALS_sampling[h][site] = nbbeams;
        if(nbbeams == 0){
          //If there is no beam reaching the voxel, transmittance and hits are set to NA (i.e. -1.0) 
          hits = 0;
          transmittance = -1.0;
        }
        else{
          if(h >= 0){
            //returns due to vegetation 
            float LAI_above = LAI3D[h+1][site + SBORD];
            float LAI_current = LAI3D[h][site + SBORD];
            
            float prob_hit;
            if(LAI_above == 100.0 & LAI_current == 100.0){
              //stem returns 
              hits = nbbeams;
              nbbeams = 0;
              transmittance = 0.0;
            }
            else{
              //leaf/twig returns 
              float LAD = LAI_current - LAI_above;
              if(LAD > 0.0) prob_hit = 1.0 - exp(-klaser * LAD);
              else prob_hit = 0.0;
              hits = gsl_ran_binomial(gslrand, prob_hit, nbbeams);
              //transmittance = exp(-klaser * LAD);
              if(hits == 0){
                transmittance = 1.0;
              }
              else{
                transmittance = float(nbbeams - hits)/float(nbbeams);
                nbbeams -= hits;
                //nbbeams += int(0.1*float(hits));                            // 10% of intercepted beams are not getting extinct
                // now simulate transmittance of beam through the leaves 
                int hits_notextinct = gsl_ran_binomial(gslrand, transmittance_laser, hits);
                nbbeams += hits_notextinct;
              }
            }
          }
          else{
            //ground returns 
            hits = nbbeams;
            //nbbeams = 0;
            transmittance = -1.0;
          }
        }
        transmittance_simulatedALS[h][site] = transmittance;
      }
    }
  }
  //Also output the "direct"/"actual" transmittance of each voxel, i.e. simply based on inversing the Beer Lambert law and estimating transmittance from the leaf area density 
  //This can be used to compare actual vs. lidar-derived transmittance estimates 
  for(int h=0;h<(HEIGHT+1);h++){
    for(int r=row_start;r<row_end;r++){
      for(int c=col_start;c<col_end;c++){
        int s = c + r*cols;
        float LAD = 0.0;
        if(h < HEIGHT) LAD = LAI3D[h][s+SBORD]-LAI3D[h+1][s+SBORD];
        transmittance_direct[h][s] = exp(-klight*LAD);
      }
    }
  }
  
  //Now calculate CHM fields, both with and without ALS simulation 
  //Set previous CHM fields 
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int s = c + r*cols;
      chm_field_previous[s] = chm_field_current[s];
      chm_field_previous_ALS[s] = chm_field_current_ALS[s];
      chm_field_current[s] = 0;
      chm_field_current_ALS[s] = 0;
    }
  }
  
#ifdef CHM_SPIKEFREE
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int s = c + r*cols;
      if(T[s].t_age > 0){
#ifdef CROWN_UMBRELLA
        float height = T[s].t_Tree_Height;
        float CR = T[s].t_Crown_Radius;
        float CD = T[s].t_Crown_Depth;
        float fraction_filled_target = 1.0;        //to remove spikes, we assume a fully filled crown for all trees
        int shell_fromtop = 0;              //toplayer
        float noinput = 0.0;
        
        LoopLayerUpdateCrownStatistic_template(r, c, height, CR, CD, fraction_filled_target, shell_fromtop, GetRadiusSlope, noinput, chm_field_current, ModifyNoinput, UpdateCHM);
#else
        int crown_top = int(T[s].t_Tree_Height);
        
        float crown_area = PI * T[s].t_Crown_Radius * T[s].t_Crown_Radius;     //floor of crown_area to bound area accumulation
        int crown_intarea = int(crown_area);   //floor of crown_area to bound area accumulation
        crown_intarea = max(crown_intarea,1);                                 //minimum area of crown (1)
        crown_intarea = min(crown_intarea,1963);                              //maximum area of crown (radius 25), int(3.14*25*25)
        
        int row_crowncenter = s/cols;
        int col_crowncenter = s%cols;
        
        for(int i = 0; i < crown_intarea; i++){
          int site_relative = LookUp_Crown_site[i];
          int row = row_crowncenter + site_relative/51 - 25;
          int col = col_crowncenter + site_relative%51 - 25;
          if(row >= 0 && row < rows && col >= 0 && col < cols){
            int site = col + row * cols;
            if(chm_field_current[site] < crown_top) chm_field_current[site] = crown_top;
          }
        }
#endif
      }
    }
  }
#else
  
  //calculate current CHM fields 
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int s = c + r*cols;
      int height_canopy=0;
      for(int h=0;h<(HEIGHT+1);h++){
        if(LAI3D[h][s+SBORD]>0.0) height_canopy = max(h, height_canopy);
      }
      chm_field_current[s] = height_canopy;
    }
  }
#endif
  
  //calculate CHM based on simulated lidar 
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int s = c + r*cols;
      int height_canopy_ALS = 0;
      for(int h=0;h<(HEIGHT+1);h++){
        if(transmittance_simulatedALS[h][s] >= 0.0 && transmittance_simulatedALS[h][s] < 1.0) height_canopy_ALS = max(h, height_canopy_ALS);
      }
      chm_field_current_ALS[s] = height_canopy_ALS;
    }
  }
  
  //compute CHM changes, both for full simulation and simulated ALS 
  for(int s = 0; s < sites; s++){
    chm_field_changes[s] = chm_field_previous[s] - chm_field_current[s];
    chm_field_changes_ALS[s] = chm_field_previous_ALS[s] - chm_field_current_ALS[s];
  }
}

//##############################################
// Global ABC function: output general ABC statistics
//##############################################
void OutputABC(){
  Rcout << " ABC: Conservation of Traits " << endl;
  OutputABCConservationTraits(output[11]);
  Rcout << " ABC: Ground data " << endl;
  OutputABC_ground(output[12]);
  Rcout << " ABC: CHM simulation " << endl;
  OutputABC_CHM(output[13], output[14], output[19]);
  Rcout << " ABC: Transmittance simulation " << endl;
  OutputABC_transmittance(output[15], output[16]);
  Rcout << " ABC: Species outputs " << endl;
  OutputABC_species(output[23], output[24],output[25], output[26], output[27]);
}

//##############################################
// Global ABC function: write headers for ABC outputs 
//##############################################
void OutputABCWriteHeaders(fstream& output_traitconservation, fstream& output_field, fstream& output_CHM, fstream& output_CHM_ALS, fstream& output_transmittance, fstream& output_transmittance_ALS, fstream& output_LAIfield, fstream& output_LAIfield_ALS, fstream& output_chmpotential, fstream& output_species, fstream& output_species10, fstream& output_traits, fstream& output_traits10, fstream& output_biomass){
  //Write headers for trait conservation metrics 
  output_traitconservation << "Iter\t" << "mean_ran\t" << "sd_ran\t" << "Height_output\t" << "Heightsd_output\t"  << "CR_output\t" << "CRsd_output\t" << "CD_output\t" << "CDsd_output\t" << "P_output\t" <<  "Psd_output\t" << "N_output\t" << "Nsd_output\t" << "LMA_output\t" << "LMAsd_output\t" << "wsg_output\t" << "wsgsd_output\t" << "dmax_output\t" << "dmaxsd_output\t" << "Height_input\t" << "Heightsd_input\t"  << "CR_input\t" << "CRsd_input\t" << "CD_input\t" << "CDsd_input\t" << "P_input\t" <<  "Psd_input\t" << "N_input\t" << "Nsd_input\t" << "LMA_input\t" << "LMAsd_input\t" << "wsg_input\t" << "wsgsd_input\t" << "dmax_input\t" << "dmaxsd_input" << endl;
  //Write headers for ground metrics 
  output_field << "Iter\t" << "Nbsites\t" << "NBspecies_realized10\t" << "NBspecies\t" << "NBspecies10\t" << "Shannon\t" << "Shannon10\t" << "Simpson\t" << "Simpson10\t" << "Abu\t" << "Abu10\t" << "Abu30\t" << "Abu10_retained\t" << "Abu30_retained\t" << "AGB\t" << "AGB10\t" << "BA\t" << "BA10\t" << "LoreyH\t" << "LoreyH10\t" << "YearlyGPP\t" << "YearlyLitterfall\t" << "YearlyMortality\t" << "YearlyTreefall\t" << "mean_LMA\t" << "mean_Nmass\t" << "mean_Pmass\t" << "mean_wsg\t" << "mean_CR\t" << "mean_LMA10\t" << "mean_Nmass10\t" << "mean_Pmass10\t" << "mean_wsg10\t" << "mean_CR10\t" << "DBH_mean\t" << "DBH_sd";
  
  for(int d=0;d<50;d++) output_field << "\tDBH_hist";
  output_field << "\tDBHgrowth_mean" << "\tDBHgrowth_sd";
  for(int d=0;d<50;d++) output_field << "\tDBHgrowth_hist";
  output_field << "\tDBHgrowth_yearly_mean" << "\tDBHgrowth_yearly_sd";
  for(int d=0;d<50;d++) output_field << "\tDBHgrowth_yearly_hist";
  for(int d=-50;d<0;d++) output_field << "\tDBHshrinkage_hist_werr";
  for(int d=0;d<50;d++) output_field << "\tDBHgrowth_hist_werr";
  for(int d=-50;d<0;d++) output_field << "\tDBHshrinkage_yearly_hist_werr";
  for(int d=0;d<50;d++) output_field << "\tDBHgrowth_yearly_hist_werr";
  output_field << endl;
  
  //Write second row, giving additional information on binning etc. 
  output_field << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t";
  for(int d=0;d<50;d++) output_field << "\t" << float(d)/20.0 ;
  output_field << "\t" << "\t";
  for(int d=0;d<50;d++) output_field << "\t" << float(d)/250.0;
  output_field << "\t" << "\t";
  for(int d=0;d<50;d++) output_field << "\t" << float(d)/1000.0 ;
  for(int d=-50;d<0;d++) output_field << "\t" << float(d)/250.0;
  for(int d=0;d<50;d++) output_field << "\t" << float(d)/250.0;
  for(int d=-50;d<0;d++) output_field << "\t" << float(d)/1000.0;
  for(int d=0;d<50;d++) output_field << "\t" << float(d)/1000.0;
  output_field << endl;
  
  //Write headers for chm metrics 
  output_CHM << "Iter\t" << "Sites_abc\t" << "CHM_mean\t" << "CHM_sd\t";
  for(int h=0;h<70;h++) output_CHM << "CHM_hist\t";
  output_CHM << "CHMchange_mean\t" << "CHMchange_sd\t";
  for(int h=0;h<140;h++) output_CHM << "CHMchange_hist\t";
  output_CHM << endl;
  
  //Second line, information on bin size etc. 
  output_CHM << "\t" << "\t" << "\t" << "\t";
  for(int h=0;h<70;h++) output_CHM << h << "\t";
  output_CHM << "\t" << "\t";
  for(int h=0;h<140;h++) output_CHM << h << "\t";
  output_CHM << endl;
  
  //Same for ALS derived metrics 
  output_CHM_ALS << "Iter\t" << "Sites_abc\t" << "CHM_mean\t" << "CHM_sd\t";
  for(int h=0;h<70;h++) output_CHM_ALS << "CHM_hist\t";
  output_CHM_ALS << "CHMchange_mean\t" << "CHMchange_sd\t";
  for(int h=0;h<140;h++) output_CHM_ALS << "CHMchange_hist\t";
  output_CHM_ALS << endl;
  
  //Second line, information on bin size etc. 
  output_CHM_ALS << "\t" << "\t" << "\t" << "\t";
  for(int h=0;h<70;h++) output_CHM_ALS << h << "\t";
  output_CHM_ALS << "\t" << "\t";
  for(int h=0;h<140;h++) output_CHM_ALS << h << "\t";
  output_CHM_ALS << endl;
  
  //Write headers for transmittance metrics 
  output_transmittance << "Iter\t" << "Sites_abc\t";
  output_transmittance << "maxheight\t" << "voxcanopy_total\t" << "voxmaxheight_total\t" << "voxcanopy_filled_total\t" << "voxcanopy_greater2\t" << "voxmaxheight_greater2\t" << "voxcanopy_filled_greater2\t" << "voxcrown_total\t" << "voxcrown_greater2\t";
  for(int h = 0; h < 70; h++) output_transmittance << "voxcrown\t";
  for(int h = 0; h < 70; h++) output_transmittance << "transm_nogaps_z\t";
  for(int h = 0; h < 70; h++) output_transmittance << "transm_incanopy_z\t";
  for(int h = 0; h < 70; h++) output_transmittance << "transm_full_z\t";
  for(int h = 0; h < 70; h++) output_transmittance << "voxcanopy_z\t";
  for(int h = 0; h < 70; h++) output_transmittance << "voxcanopy_na_z\t";
  for(int h = 0; h < 70; h++) output_transmittance << "voxcanopy_empty_z\t";
  for(int h = 0; h < 70; h++) output_transmittance << "voxcanopy_filled_z\t";
  
  for(int d = 0; d < 70; d++) output_transmittance << "transm_nogaps_d\t";
  for(int d = 0; d < 70; d++) output_transmittance << "transm_full_d\t";
  for(int d = 0; d < 70; d++) output_transmittance << "voxna_d\t";
  for(int d = 0; d < 70; d++) output_transmittance << "voxempty_d\t";
  for(int d = 0; d < 70; d++) output_transmittance << "voxfilled_d\t";
  
  output_transmittance << "nb_aggregates\t";
  for(int bin = 0; bin < 80; bin++)  output_transmittance << "LAIhist\t";
  output_transmittance << endl;
  output_transmittance << "\t" << "\t";
  output_transmittance << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t" << "\t";
  for(int i=0; i < 8; i++)
    for(int h = 0; h < 70; h++) output_transmittance << h << "\t";
  for(int i=0; i < 5; i++)
    for(int d = 0; d < 70; d++) output_transmittance << d << "\t";
  output_transmittance << "\t";
  for(int bin = 0; bin < 80; bin++) output_transmittance << bin << "\t";
  output_transmittance << endl;
  output_transmittance_ALS << "Iter\t" << "Sites_abc\t";
  
  for(int h = 0; h < 70; h++) output_transmittance_ALS << "transm_nogaps_z\t";
  for(int h = 0; h < 70; h++) output_transmittance_ALS << "transm_incanopy_z\t";
  for(int h = 0; h < 70; h++) output_transmittance_ALS << "transm_full_z\t";
  for(int h = 0; h < 70; h++) output_transmittance_ALS << "voxcanopy_z\t";
  for(int h = 0; h < 70; h++) output_transmittance_ALS << "voxcanopy_na_z\t";
  for(int h = 0; h < 70; h++) output_transmittance_ALS << "voxcanopy_empty_z\t";
  for(int h = 0; h < 70; h++) output_transmittance_ALS << "voxcanopy_filled_z\t";
  
  for(int d = 0; d < 70; d++) output_transmittance_ALS << "transm_nogaps_d\t";
  for(int d = 0; d < 70; d++) output_transmittance_ALS << "transm_full_d\t";
  for(int d = 0; d < 70; d++) output_transmittance_ALS << "voxna_d\t";
  for(int d = 0; d < 70; d++) output_transmittance_ALS << "voxempty_d\t" ;
  for(int d = 0; d < 70; d++) output_transmittance_ALS << "voxfilled_d\t";
  
  output_transmittance_ALS << "nb_aggregates\t";
  for(int bin = 0; bin < 80; bin++)  output_transmittance_ALS << "LAIhist\t";
  output_transmittance_ALS << endl;
  output_transmittance_ALS << "\t" << "\t";
  for(int i=0; i < 7; i++)
    for(int h = 0; h < 70; h++) output_transmittance_ALS << h << "\t";
  for(int i=0; i < 5; i++)
    for(int d = 0; d < 70; d++) output_transmittance_ALS << d << "\t";
  output_transmittance_ALS << "\t";
  for(int bin = 0; bin < 80; bin++) output_transmittance_ALS << bin << "\t";
  output_transmittance_ALS << endl;
  
  //Now the species headers
  output_species << "Iter";
  output_species10 << "Iter";
  //    output_traits << "Iter\tTrait";
  //    output_traits10 << "Iter\tTrait";
  output_biomass << "Iter";
  
  for(int spp = 1; spp < nbspp + 1; spp++){
    output_species  << "\t" << S[spp].s_name;
    output_species10 << "\t" << S[spp].s_name;
    //        output_traits << "\t" << S[spp].s_name;
    //        output_traits10 << "\t" << S[spp].s_name;
    output_biomass << "\t" << S[spp].s_name;
  }
  
  output_species << endl;
  output_species10 << endl;
  //    output_traits << endl;
  //    output_traits10 << endl;
  output_biomass << endl;
}


//##############################################
// Global ABC function: assesses the conservation of traits between input and output in TROLL 
//##############################################
//! - for example, it may be that a particular species is more successful than expected from its relative frequency at the parameterized site, and thus its abundance will exceed the real abundance. This could then lead to important shifts in the overall community trait patterns between the trait pattern that is provided to TROLL and the one that is found in the simulated community 
//! - There is a potentially important ecological question behind it, namely: to what extent do the trait distributions that we observe for seeds/seedlings/saplings (i.e. input from seedrain) differ from those of the mature community 
void OutputABCConservationTraits(fstream& output_traitconservation){
  
  //Trait input/output variation 
  int nb_trees_counted = 0;
  
  //Summary statistics for initial, random configuration (i.e. randomly drawn variance around trait means) 
  float mu_random = 0.0, moment2_random = 0.0;
  
  float mu_height_varinput = 0.0, mu_CR_varinput = 0.0, mu_CD_varinput = 0.0, mu_P_varinput = 0.0, mu_N_varinput = 0.0, mu_LMA_varinput = 0.0, mu_wsg_varinput = 0.0, mu_dbhmax_varinput = 0.0;
  float mu_height_varoutput = 0.0, mu_CR_varoutput = 0.0, mu_CD_varoutput = 0.0, mu_P_varoutput = 0.0, mu_N_varoutput = 0.0, mu_LMA_varoutput = 0.0, mu_wsg_varoutput = 0.0,  mu_dbhmax_varoutput = 0.0;
  
  float moment2_height_varinput = 0.0, moment2_CR_varinput = 0.0, moment2_CD_varinput = 0.0, moment2_P_varinput = 0.0, moment2_N_varinput = 0.0, moment2_LMA_varinput = 0.0, moment2_wsg_varinput = 0.0, moment2_dbhmax_varinput = 0.0;
  float moment2_height_varoutput = 0.0, moment2_CR_varoutput = 0.0, moment2_CD_varoutput = 0.0, moment2_P_varoutput = 0.0, moment2_N_varoutput = 0.0, moment2_LMA_varoutput = 0.0, moment2_wsg_varoutput = 0.0,  moment2_dbhmax_varoutput = 0.0;
  
  //Means and 2nd moment for calculation of sd later on 
  for(int s=0;s<sites;s++){
    if(T[s].t_age > 0 && T[s].t_dbh >= 0.1){
      int dev_rand = int(gsl_rng_uniform_int(gslrand,100000));
      
      nb_trees_counted++;
      mu_random += dev_rand;
      
      mu_height_varinput += log(d_intraspecific_height[dev_rand]);
      mu_CR_varinput += log(d_intraspecific_CR[dev_rand]);
      mu_CD_varinput += log(d_intraspecific_CD[dev_rand]);
      mu_P_varinput += log(d_intraspecific_P[dev_rand]);
      mu_N_varinput += log(d_intraspecific_N[dev_rand]);
      mu_LMA_varinput += log(d_intraspecific_LMA[dev_rand]);
      mu_dbhmax_varinput += log(d_intraspecific_dbhmax[dev_rand]);
      mu_wsg_varinput += log(d_intraspecific_wsg[dev_rand]);
      
      mu_height_varoutput += log(T[s].t_intraspecific_multiplier_height);
      mu_CR_varoutput += log(T[s].t_intraspecific_multiplier_CR);
      mu_CD_varoutput += log(T[s].t_intraspecific_multiplier_CD);
      mu_P_varoutput += log(T[s].t_intraspecific_multiplier_P);
      mu_N_varoutput += log(T[s].t_intraspecific_multiplier_N);
      mu_LMA_varoutput += log(T[s].t_intraspecific_multiplier_LMA);
      mu_dbhmax_varoutput += log(T[s].t_intraspecific_multiplier_dbhmax);
      mu_wsg_varoutput += log(T[s].t_intraspecific_deviation_wsg);
      
      moment2_random += dev_rand;
      moment2_height_varinput += log(d_intraspecific_height[dev_rand]) * log(d_intraspecific_height[dev_rand]);
      moment2_CR_varinput += log(d_intraspecific_CR[dev_rand]) * log(d_intraspecific_CR[dev_rand]);
      moment2_CD_varinput += log(d_intraspecific_CD[dev_rand]) * log(d_intraspecific_CD[dev_rand]);
      moment2_P_varinput += log(d_intraspecific_P[dev_rand]) * log(d_intraspecific_P[dev_rand]);
      moment2_N_varinput += log(d_intraspecific_N[dev_rand]) * log(d_intraspecific_N[dev_rand]);
      moment2_LMA_varinput += log(d_intraspecific_LMA[dev_rand]) * log(d_intraspecific_LMA[dev_rand]);
      moment2_dbhmax_varinput += log(d_intraspecific_dbhmax[dev_rand]) * log(d_intraspecific_dbhmax[dev_rand]);
      moment2_wsg_varinput += log(d_intraspecific_wsg[dev_rand]) * log(d_intraspecific_wsg[dev_rand]);
      
      moment2_height_varoutput += log(T[s].t_intraspecific_multiplier_height) * log(T[s].t_intraspecific_multiplier_height);
      moment2_CR_varoutput += log(T[s].t_intraspecific_multiplier_CR) * log(T[s].t_intraspecific_multiplier_CR);
      moment2_CD_varoutput += log(T[s].t_intraspecific_multiplier_CD) * log(T[s].t_intraspecific_multiplier_CD);
      moment2_P_varoutput += log(T[s].t_intraspecific_multiplier_P) * log(T[s].t_intraspecific_multiplier_P);
      moment2_N_varoutput += log(T[s].t_intraspecific_multiplier_N) * log(T[s].t_intraspecific_multiplier_N);
      moment2_LMA_varoutput += log(T[s].t_intraspecific_multiplier_LMA) * log(T[s].t_intraspecific_multiplier_LMA);
      moment2_dbhmax_varoutput += log(T[s].t_intraspecific_multiplier_dbhmax) * log(T[s].t_intraspecific_multiplier_dbhmax);
      moment2_wsg_varoutput += log(T[s].t_intraspecific_deviation_wsg) * log(T[s].t_intraspecific_deviation_wsg);
    }
  }
  
  //Calculation of standard deviation 
  float sd_random;
  
  float sd_height_varinput = 0.0, sd_CR_varinput = 0.0, sd_CD_varinput = 0.0, sd_P_varinput = 0.0, sd_N_varinput = 0.0, sd_LMA_varinput = 0.0, sd_wsg_varinput = 0.0, sd_dbhmax_varinput = 0.0;
  float sd_height_varoutput = 0.0, sd_CR_varoutput = 0.0, sd_CD_varoutput = 0.0, sd_P_varoutput = 0.0, sd_N_varoutput = 0.0, sd_LMA_varoutput = 0.0, sd_wsg_varoutput = 0.0,  sd_dbhmax_varoutput = 0.0;
  
  if(nb_trees_counted > 0){
    mu_random *= 1.0/float(nb_trees_counted);
    
    mu_height_varinput *= 1.0/float(nb_trees_counted);
    mu_CR_varinput *= 1.0/float(nb_trees_counted);
    mu_CD_varinput *= 1.0/float(nb_trees_counted);
    mu_P_varinput *= 1.0/float(nb_trees_counted);
    mu_N_varinput *= 1.0/float(nb_trees_counted);
    mu_LMA_varinput *= 1.0/float(nb_trees_counted);
    mu_wsg_varinput *= 1.0/float(nb_trees_counted);
    mu_dbhmax_varinput *= 1.0/float(nb_trees_counted);
    
    mu_height_varoutput *= 1.0/float(nb_trees_counted);
    mu_CR_varoutput *= 1.0/float(nb_trees_counted);
    mu_CD_varoutput *= 1.0/float(nb_trees_counted);
    mu_P_varoutput *= 1.0/float(nb_trees_counted);
    mu_N_varoutput *= 1.0/float(nb_trees_counted);
    mu_LMA_varoutput *= 1.0/float(nb_trees_counted);
    mu_dbhmax_varoutput *= 1.0/float(nb_trees_counted);
    mu_wsg_varoutput *= 1.0/float(nb_trees_counted);
    
    sd_random = sqrt(moment2_random/float(nb_trees_counted) - mu_random * mu_random);
    
    sd_height_varinput = sqrt(moment2_height_varinput/float(nb_trees_counted) - mu_height_varinput * mu_height_varinput);
    sd_CR_varinput = sqrt(moment2_CR_varinput/float(nb_trees_counted) - mu_CR_varinput * mu_CR_varinput);
    sd_CD_varinput = sqrt(moment2_CD_varinput/float(nb_trees_counted) - mu_CD_varinput * mu_CD_varinput);
    sd_P_varinput = sqrt(moment2_P_varinput/float(nb_trees_counted) - mu_P_varinput * mu_P_varinput);
    sd_N_varinput = sqrt(moment2_N_varinput/float(nb_trees_counted) - mu_N_varinput * mu_N_varinput);
    sd_LMA_varinput = sqrt(moment2_LMA_varinput/float(nb_trees_counted) - mu_LMA_varinput * mu_LMA_varinput);
    sd_wsg_varinput = sqrt(moment2_wsg_varinput/float(nb_trees_counted) - mu_wsg_varinput * mu_wsg_varinput);
    sd_dbhmax_varinput = sqrt(moment2_dbhmax_varinput/float(nb_trees_counted) - mu_dbhmax_varinput * mu_dbhmax_varinput);
    
    sd_height_varoutput = sqrt(moment2_height_varoutput/float(nb_trees_counted) - mu_height_varoutput * mu_height_varoutput);
    sd_CR_varoutput = sqrt(moment2_CR_varoutput/float(nb_trees_counted) - mu_CR_varoutput * mu_CR_varoutput);
    sd_CD_varoutput = sqrt(moment2_CD_varoutput/float(nb_trees_counted) - mu_CD_varoutput * mu_CD_varoutput);
    sd_P_varoutput = sqrt(moment2_P_varoutput/float(nb_trees_counted) - mu_P_varoutput * mu_P_varoutput);
    sd_N_varoutput = sqrt(moment2_N_varoutput/float(nb_trees_counted) - mu_N_varoutput * mu_N_varoutput);
    sd_LMA_varoutput = sqrt(moment2_LMA_varoutput/float(nb_trees_counted) - mu_LMA_varoutput * mu_LMA_varoutput);
    sd_wsg_varoutput = sqrt(moment2_wsg_varoutput/float(nb_trees_counted) - mu_wsg_varoutput * mu_wsg_varoutput);
    sd_dbhmax_varoutput = sqrt(moment2_dbhmax_varoutput/float(nb_trees_counted) - mu_dbhmax_varoutput * mu_dbhmax_varoutput);
  }
  else{
    sd_random = 0.0;
  }
  //Write to output file 
  output_traitconservation << iter << "\t" << mu_random << "\t" << sd_random << "\t" <<  mu_height_varoutput << "\t" << sd_height_varoutput << "\t" << mu_CR_varoutput << "\t" << sd_CR_varoutput << "\t" << mu_CD_varoutput << "\t" << sd_CD_varoutput << "\t" << mu_P_varoutput << "\t" << sd_P_varoutput << "\t" << mu_N_varoutput << "\t" << sd_N_varoutput << "\t" << mu_LMA_varoutput << "\t" << sd_LMA_varoutput << "\t" << mu_wsg_varoutput << "\t" << sd_wsg_varoutput << "\t" << mu_dbhmax_varoutput << "\t" << sd_dbhmax_varoutput << "\t" << mu_height_varinput << "\t" << sd_height_varinput << "\t" << mu_CR_varinput << "\t" << sd_CR_varinput << "\t" << mu_CD_varinput << "\t" << sd_CD_varinput << "\t" << mu_P_varinput << "\t" << sd_P_varinput << "\t" << mu_N_varinput << "\t" << sd_N_varinput << "\t" << mu_LMA_varinput << "\t" << sd_LMA_varinput << "\t" << mu_wsg_varinput << "\t" << sd_wsg_varinput << "\t" << mu_dbhmax_varinput << "\t" << sd_dbhmax_varinput << "\t" << endl;
}

//##############################################
// Global ABC function: returns ABC ground outputs
//##############################################
//! - field measured summary statistics: total number of species, abundances > 10cm, >30cm (provided for convenience, information already included in distribution below), AGB 
void OutputABC_ground(fstream& output_field){
  
  //Self-explanatory 
  int NBspecies_realized10 = 0;
  int NBspecies = 0, NBspecies10 = 0;
  float Shannon = 0.0, Shannon10 = 0.0, Simpson = 0.0, Simpson10 = 0.0;
  float Abu = 0, Abu10 = 0, Abu30 = 0, Abu10_prev = 0, Abu30_prev = 0, Abu10_retained = 0, Abu30_retained = 0;
  float AGB = 0.0, AGB10 = 0.0, ba = 0.0, ba10 = 0.0, LoreyH = 0.0, LoreyH10 = 0.0;
  
  float mean_LMA = 0.0, mean_Nmass = 0.0, mean_Pmass = 0.0, mean_wsg = 0.0, mean_CR = 0.0;
  float mean_LMA10 = 0.0, mean_Nmass10 = 0.0, mean_Pmass10 = 0.0, mean_wsg10 = 0.0, mean_CR10 = 0.0;
  
  //This is just a check whether the initialized species are preserved in the model 
  for(int spp=1;spp<=nbspp;spp++) if(S[spp].s_dbhmax_realized > 0.1) NBspecies_realized10++;
  //and calculate the number of species 
  for(int spp = 0; spp < nbspp+1; spp++){
    abundances_species[spp] = 0;
    abundances_species10[spp] = 0;
  }
  //Compute summary statistics, but only over the respective area 
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int s = c + r*cols;
      if(T[s].t_age > 0){
        Abu++;
        float dbh_tree = T[s].t_dbh;
        float dbh_track = T[s].t_dbh_previous;
        float height_tree = T[s].t_Tree_Height;
        float ba_tree = dbh_tree * dbh_tree * 0.25 * PI;
        float AGBtree;
        AGBtree = 0.001 * T[s].CalcAGB(); // convert from kg to tons
        AGB += AGBtree;
        LoreyH += ba_tree * height_tree;
        ba += ba_tree;
        mean_LMA += T[s].t_LMA;
        mean_Nmass += T[s].t_Nmass;
        mean_Pmass += T[s].t_Pmass;
        mean_wsg += T[s].t_wsg;
        mean_CR += T[s].t_Crown_Radius;
        
        int sp_lab = T[s].t_sp_lab;
        abundances_species[sp_lab]++;
        
        if(dbh_tree >= 0.1){
          abundances_species10[sp_lab]++;
          Abu10++;
          LoreyH10 += ba_tree * height_tree;
          ba10 += ba_tree;
          AGB10 += AGBtree;
          
          mean_LMA10 += T[s].t_LMA;
          mean_Nmass10 += T[s].t_Nmass;
          mean_Pmass10 += T[s].t_Pmass;
          mean_wsg10 += T[s].t_wsg;
          mean_CR10 += T[s].t_Crown_Radius;
          
          if(dbh_tree >= 0.3) Abu30++;
        }
        if(dbh_track >= 0.1){
          Abu10_prev++;
          if(dbh_track >= 0.3) Abu30_prev++;
        }
      }
    }
  }
  
  //Divide means by abundances 
  float inv_abu, inv_abu10;
  if(Abu > 0) inv_abu = 1.0/Abu;
  else inv_abu = 0.0;
  if(Abu10 > 0) inv_abu10 = 1.0/Abu10;
  else inv_abu10 = 0.0;
  
  mean_LMA *= inv_abu;
  mean_Nmass *= inv_abu;
  mean_Pmass *= inv_abu;
  mean_wsg *= inv_abu;
  mean_CR *= inv_abu;
  
  mean_LMA10 *= inv_abu10;
  mean_Nmass10 *= inv_abu10;
  mean_Pmass10 *= inv_abu10;
  mean_wsg10 *= inv_abu10;
  mean_CR10 *= inv_abu10;
  
  //Denominator of Lorey's height 
  if(ba > 0.0) LoreyH *= 1.0/ba;
  if(ba10 > 0.0) LoreyH10 *= 1.0/ba10;
  
  //Abundance growth 
  Abu10_retained = Abu10 - Abu10_prev;
  Abu30_retained = Abu30 - Abu30_prev;
  
  //Now calculate the first three orders of diversity 
  for(int spp = 1; spp < nbspp+1; spp++){
    int abu_spp = abundances_species[spp];
    int abu_spp10 = abundances_species10[spp];
    
    if(abu_spp > 0){
      float prop_spp = float(abu_spp)/float(Abu);
      
      NBspecies++;
      Shannon -= prop_spp * log(prop_spp);
      Simpson += prop_spp * prop_spp;
    }
    
    if(abu_spp10 > 0){
      float prop_spp10 = float(abu_spp10)/float(Abu10);
      
      NBspecies10++;
      Shannon10 -= prop_spp10 * log(prop_spp10);
      Simpson10 += prop_spp10 * prop_spp10;
    }
  }
  
  //Standardize output metrics 
  float isites_abc = 10000.0/float(sites_abc);
  AGB *= isites_abc;
  AGB10 *= isites_abc;
  Abu10 *= isites_abc;
  Abu30 *= isites_abc;
  Abu10_retained *= isites_abc;
  Abu30_retained *= isites_abc;
  ba *= isites_abc;
  ba10 *= isites_abc;
  
  //Now add the process metrics from moving averages 
  float YearlyGPP = 0.0, YearlyLitterfall = 0.0, YearlyMortality = 0.0, YearlyTreefall = 0.0;
  float nb_years = 120.0 / float(iterperyear);
  float inb_years = 1.0 / nb_years;
  
  for(int i=0; i < 120; i++){
    YearlyGPP += GPP_MA[i];
    YearlyLitterfall += Litterfall_MA[i];
    YearlyMortality += Mortality_MA[i];
    YearlyTreefall += Treefall_MA[i];
  }
  
  YearlyGPP *= inb_years;
  YearlyLitterfall *= inb_years;
  YearlyMortality *= inb_years;
  YearlyTreefall *= inb_years;
  
  //Create DBH histogram, mean and sd 
  int dbh_abc[50] = {0};
  float mean_dbh = 0.0;
  float sd_dbh = 0.0;
  float icount_dbh = 0.0;
  
  if(Abu10 > 1){
    icount_dbh = 1.0/float(Abu10);
    
    for(int r=row_start;r<row_end;r++){
      for(int c=col_start;c<col_end;c++){
        int s = c + r*cols;
        float dbh_tree = T[s].t_dbh;
        if(T[s].t_age > 0 && dbh_tree >= 0.1) {
          mean_dbh += dbh_tree;
          int dbh_bin = int(dbh_tree*20.0);
          if(dbh_bin>49) dbh_bin = 49;
          dbh_abc[dbh_bin]++;
        }
      }
    }
    mean_dbh *= icount_dbh;
    for(int r=row_start;r<row_end;r++){
      for(int c=col_start;c<col_end;c++){
        int s = c + r*cols;
        float dbh_tree = T[s].t_dbh;
        if(T[s].t_age > 0 && dbh_tree >= 0.1) {
          sd_dbh += (dbh_tree - mean_dbh)*(dbh_tree - mean_dbh);
        }
      }
    }
    float icount_dbh_sd = 0.0;
    icount_dbh_sd = 1.0/float(Abu10-1);
    sd_dbh = sqrt(sd_dbh * icount_dbh_sd);
  }
  
  //Create DBH growth histogram, mean and sd / CHM analogous period and yearly values 
  int dbhgrowth_abc[50] = {0};
  int dbhgrowth_abc_yearly[50] = {0};
  float mean_dbhgrowth = 0.0;
  float mean_dbhgrowth_yearly = 0.0;
  float sd_dbhgrowth = 0.0;
  float sd_dbhgrowth_yearly = 0.0;
  float icount_dbhgrowth = 0;
  
  //The dbhgrowth witherror variables also simulate errors in measurement, taken from Chave et al. 2004 (all given in cm). See also Réjou-Méchain 2017 
  // this can be used to compare distributions with negative diameter growth 
  // this is, however, empirical, and does not account for actual negative diameter growth, i.e. trees shrinking slightly under different environmental conditions etc. 
  int dbhgrowth_witherror_abc[50] = {0};
  int dbhgrowth_witherror_abc_yearly[50] = {0};
  int dbhshrinkage_witherror_abc[50] = {0};
  int dbhshrinkage_witherror_abc_yearly[50] = {0};
  int count_dbhgrowth_witherror = 0;
  float icount_dbhgrowth_witherror = 0;
  
  if(Abu10_prev > 1){
    icount_dbhgrowth = 1.0/float(Abu10_prev);
    
    for(int r=row_start;r<row_end;r++){
      for(int c=col_start;c<col_end;c++){
        int s = c + r*cols;
        float dbh_previous = T[s].t_dbh_previous;
        //Errors taken from Chave et al. 2004 (all given in cm). See also Réjou-Méchain 2017 
        // slight modification by assuming that measurement errors cannot go beyond a third of the diameter, e.g. for a stem of 10cm not be above or below 3.3 cm, and for a stem of 100cm not be below or above 33cm), also prevents negative diameters 
        float sd1 = (0.0062 * dbh_previous * 100.0 + 0.0904)*0.01;
        float sd2 = 0.0464;
        
        float prob = gsl_rng_uniform(gslrand);
        float error;
        if(prob < 0.95) error = gsl_ran_gaussian(gslrand, sd1);
        else error = gsl_ran_gaussian(gslrand, sd2);
        
        float dbh_previous_witherror = fmaxf(dbh_previous + error, 0.66 * dbh_previous);
        dbh_previous_witherror = fminf(dbh_previous_witherror, 1.33 * dbh_previous);
        
        if(dbh_previous >= 0.1){
          float dbh = T[s].t_dbh;
          float dbh_growth =  dbh - dbh_previous;
          float dbh_growth_yearly = dbh_growth * 12.0/float(chmchange_iter);
          
          mean_dbhgrowth += dbh_growth;
          mean_dbhgrowth_yearly += dbh_growth_yearly;
          int dbhgrowth_bin = int(dbh_growth * 250.0);              // with a limit at 50 bins, this implies maximum growth of 20cm (in census intervals of 10 years)
          int dbhgrowth_bin_yearly = int(dbh_growth_yearly * 1000.0);       // with a limit at 50 bins, this implies that there is no further increase above 0.05m, i.e. maximum yearly growth is 5cm
          
          if(dbhgrowth_bin>=50) dbhgrowth_bin = 50-1;
          if(dbhgrowth_bin_yearly>=50) dbhgrowth_bin_yearly = 50-1;
          dbhgrowth_abc[dbhgrowth_bin]++;
          dbhgrowth_abc_yearly[dbhgrowth_bin_yearly]++;
        }
        
        if(dbh_previous_witherror >= 0.1){
          count_dbhgrowth_witherror++;
          float dbh = T[s].t_dbh;
          //Errors taken from Chave et al. 2004 (all given in cm). See also Réjou-Méchain 2017 
          // slight modification by assuming that measurement errors cannot go beyond a third of the diameter, e.g. for a stem of 10cm not be above or below 3.3 cm, and for a stem of 100cm not be below or above 33cm), also prevents negative diameters 
          float sd1 = (0.0062 * dbh_previous * 100.0 + 0.0904)*0.01;
          float sd2 = 0.0464;
          
          float prob = gsl_rng_uniform(gslrand);
          float error;
          if(prob < 0.95) error = gsl_ran_gaussian(gslrand, sd1);
          else error = gsl_ran_gaussian(gslrand, sd2);
          
          float dbh_witherror = fmaxf(dbh + error, 0.66 * dbh);
          dbh_witherror = fminf(dbh_witherror, 1.33 * dbh);
          
          float dbh_growth_witherror =  dbh_witherror - dbh_previous_witherror;
          float dbh_growth_yearly_witherror = dbh_growth_witherror * 12.0/float(chmchange_iter);
          
          if(dbh_growth_witherror >= 0.0){
            int dbhgrowth_witherror_bin = int(dbh_growth_witherror * 250.0);              // with a limit at 50 bins, this implies maximum growth of 50cm (in census intervals of 10 years)
            int dbhgrowth_witherror_bin_yearly = int(dbh_growth_yearly_witherror * 1000.0);       // with a limit at 50 bins, this implies that there is no further increase above 0.05m, i.e. maximum yearly growth is 5cm
            
            if(dbhgrowth_witherror_bin>=50) dbhgrowth_witherror_bin = 50-1;
            if(dbhgrowth_witherror_bin_yearly>=50) dbhgrowth_witherror_bin_yearly = 50-1;
            dbhgrowth_witherror_abc[dbhgrowth_witherror_bin]++;
            dbhgrowth_witherror_abc_yearly[dbhgrowth_witherror_bin_yearly]++;
          } else {
            dbh_growth_witherror *= -1.0;
            dbh_growth_yearly_witherror *= -1.0;
            
            int dbhgrowth_witherror_bin = int(dbh_growth_witherror * 250.0);              // with a limit at 50 bins, this implies maximum growth of 50cm (in census intervals of 10 years)
            int dbhgrowth_witherror_bin_yearly = int(dbh_growth_yearly_witherror * 1000.0);       // with a limit at 50 bins, this implies that there is no further increase above 0.05m, i.e. maximum yearly growth is 5cm
            
            if(dbhgrowth_witherror_bin>=50) dbhgrowth_witherror_bin = 50-1;
            if(dbhgrowth_witherror_bin_yearly>=50) dbhgrowth_witherror_bin_yearly = 50-1;
            dbhshrinkage_witherror_abc[dbhgrowth_witherror_bin]++;
            dbhshrinkage_witherror_abc_yearly[dbhgrowth_witherror_bin_yearly]++;
          }
        }
      }
    }
    
    if(count_dbhgrowth_witherror > 0) icount_dbhgrowth_witherror = 1.0/float(count_dbhgrowth_witherror);
    else icount_dbhgrowth_witherror = 0.0;
    
    mean_dbhgrowth *= icount_dbhgrowth;
    mean_dbhgrowth_yearly *= icount_dbhgrowth;
    
    //Now standard deviation of dbh growth 
    for(int r=row_start;r<row_end;r++){
      for(int c=col_start;c<col_end;c++){
        int s = c + r*cols;
        
        float dbh_previous = T[s].t_dbh_previous;
        
        if(dbh_previous >= 0.1){
          float dbh_growth = T[s].t_dbh - dbh_previous;
          float dbh_growth_yearly = dbh_growth * 12.0/float(chmchange_iter);
          
          sd_dbhgrowth += (dbh_growth - mean_dbhgrowth)*(dbh_growth - mean_dbhgrowth);
          sd_dbhgrowth_yearly += (dbh_growth_yearly - mean_dbhgrowth_yearly)*(dbh_growth - mean_dbhgrowth_yearly);
        }
      }
    }
    
    float icount_dbhgrowth_sd = 0.0;
    icount_dbhgrowth_sd = 1.0/float(Abu10_prev-1);
    
    sd_dbhgrowth = sqrt(sd_dbhgrowth * icount_dbhgrowth_sd);
    sd_dbhgrowth_yearly = sqrt(sd_dbhgrowth_yearly * icount_dbhgrowth_sd);
  }
  
  // Ripley's K(r), transformed to L estimator through L(r) = sqrt(K/Pi) - r 
  //    float itrees_ripley = 0.0;
  //    int area_ripley = cols * rows;
  //    int sum_ripley[25] = {0};
  //
  //    if(Abu10 > 0){
  //        int trees_ripley = Abu10;
  //        itrees_ripley = 1.0/trees_ripley;
  //
  //        for(int r=row_start;r<row_end;r++){
  //            for(int c=col_start;c<col_end;c++){
  //                int site = c + r*cols;
  //                if(T[site].t_age > 0 && T[site].t_dbh > 0.1){
  //
  //                    for(int r = 0; r < 25; r++){
  //                        int t = r + 1;
  //                        int t_squared = t * t;
  //                        for(int site_compare = 0; site_compare < sites; site_compare++){
  //                            if(T[site_compare].t_age > 0 && T[site_compare].t_dbh > 0.1 && site != site_compare){
  //                                int row = site/cols;
  //                                int col = site%cols;
  //                                int row_compare = site_compare/cols;
  //                                int col_compare = site_compare%cols;
  //                                int dist_squared = (row - row_compare) * (row - row_compare) + (col - col_compare) * (col - col_compare);
  //                                if(dist_squared < t_squared){
  //                                    sum_ripley[r]++;
  //                                }
  //                            }
  //                        }
  //                    }
  //                }
  //            }
  //        }
  //    }
  
  //Write to file 
  output_field << iter << "\t" << sites_abc << "\t" << NBspecies_realized10 << "\t" << NBspecies << "\t" << NBspecies10 << "\t" << Shannon << "\t" << Shannon10 << "\t" << Simpson << "\t" << Simpson10 << "\t" << Abu << "\t" << Abu10 << "\t" << Abu30 << "\t" << Abu10_retained << "\t" << Abu30_retained << "\t" << AGB << "\t" << AGB10 << "\t" << ba << "\t" << ba10 << "\t" << LoreyH << "\t" << LoreyH10 << "\t" << YearlyGPP << "\t" << YearlyLitterfall << "\t" << YearlyMortality << "\t" << YearlyTreefall << "\t" << mean_LMA << "\t" << mean_Nmass << "\t" << mean_Pmass << "\t" << mean_wsg << "\t" << mean_CR << "\t" << mean_LMA10 << "\t" << mean_Nmass10 << "\t" << mean_Pmass10 << "\t" << mean_wsg10 << "\t" << mean_CR10 << "\t" << mean_dbh << "\t" << sd_dbh;
  
  for(int d=0;d<50;d++){
    float density_DBH = float(dbh_abc[d])*icount_dbh;
    output_field << "\t" << density_DBH;
  }
  
  //Write to file 
  output_field << "\t" << mean_dbhgrowth << "\t" << sd_dbhgrowth;
  
  for(int d=0;d<50;d++){
    float density_dbhgrowth = float(dbhgrowth_abc[d])*icount_dbhgrowth;
    output_field << "\t" << density_dbhgrowth;
  }
  
  output_field << "\t" << mean_dbhgrowth_yearly << "\t" << sd_dbhgrowth_yearly;
  
  for(int d=0;d<50;d++){
    float density_dbhgrowth_yearly = float(dbhgrowth_abc_yearly[d])*icount_dbhgrowth;
    output_field << "\t" << density_dbhgrowth_yearly;
  }
  
  for(int d=50-1;d>=0;d--){
    float density_dbhshrinkage_witherror = float(dbhshrinkage_witherror_abc[d])*icount_dbhgrowth_witherror;
    output_field << "\t" << density_dbhshrinkage_witherror;
  }
  
  for(int d=0;d<50;d++){
    float density_dbhgrowth_witherror = float(dbhgrowth_witherror_abc[d])*icount_dbhgrowth_witherror;
    output_field << "\t" << density_dbhgrowth_witherror;
  }
  
  for(int d=50-1;d>=0;d--){
    float density_dbhshrinkage_yearly_witherror = float(dbhshrinkage_witherror_abc_yearly[d])*icount_dbhgrowth_witherror;
    output_field << "\t" << density_dbhshrinkage_yearly_witherror;
  }
  
  for(int d=0;d<50;d++){
    float density_dbhgrowth_yearly_witherror = float(dbhgrowth_witherror_abc_yearly[d])*icount_dbhgrowth_witherror;
    output_field << "\t" << density_dbhgrowth_yearly_witherror;
  }
  
  //    for(int r = 0; r < 25; r++){
  //        int t = r + 1;
  //        float K_ripley = float(area_ripley) * float(sum_ripley[r]) * itrees_ripley * itrees_ripley;
  //        output_field << sqrt(K_ripley / PI) - t << "\t";
  //    }
  
  output_field << endl;
}

//##############################################
// Global ABC function: returns ABC outputs for species
//##############################################
void OutputABC_species(fstream& output_species, fstream& output_species10,fstream& output_traits, fstream& output_traits10, fstream& output_biomass){
  
  //Empty the vectors 
  for(int spp = 0; spp < nbspp+1; spp++){
    abundances_species[spp] = 0;
    abundances_species10[spp] = 0;
    biomass_species[spp] = 0.0;
    for(int trait = 0; trait < 10; trait++){
      traits_species[spp][trait] = 0.0;
      traits_species10[spp][trait] = 0.0;
    }
  }
  
  //Now calculate 
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int s = c + r*cols;
      if(T[s].t_age > 0){
        //First abundance, then mean and standard deviation of the variation in the 5 traits (leaf level traits, wood density, crown radius), mean and standard deviation are calculated in one pass from sum of traits and sum of squares of traits 
        int sp_lab = T[s].t_sp_lab;
        abundances_species[sp_lab]++;
        
        float agb_tree = 0.001 * T[s].CalcAGB(); // convert from kg to tons
        biomass_species[sp_lab] += agb_tree;
        
        float LMA = log(T[s].t_intraspecific_multiplier_LMA);
        float LMA2 = LMA * LMA;
        float Nmass = log(T[s].t_intraspecific_multiplier_N);
        float Nmass2 = Nmass * Nmass;
        float Pmass = log(T[s].t_intraspecific_multiplier_P);
        float Pmass2 = Pmass * Pmass;
        float wsg = T[s].t_intraspecific_deviation_wsg;
        float wsg2 = wsg * wsg;
        float crown = log(T[s].t_intraspecific_multiplier_CR);
        float crown2 = crown * crown;
        
        traits_species[sp_lab][0] += LMA;
        traits_species[sp_lab][1] += LMA2;
        traits_species[sp_lab][2] += Nmass;
        traits_species[sp_lab][3] += Nmass2;
        traits_species[sp_lab][4] += Pmass;
        traits_species[sp_lab][5] += Pmass2;
        traits_species[sp_lab][6] += wsg;
        traits_species[sp_lab][7] += wsg2;
        traits_species[sp_lab][8] += crown;
        traits_species[sp_lab][9] += crown2;
        
        if(T[s].t_dbh >= 0.1){
          abundances_species10[sp_lab]++;
          
          traits_species10[sp_lab][0] += LMA;
          traits_species10[sp_lab][1] += LMA2;
          traits_species10[sp_lab][2] += Nmass;
          traits_species10[sp_lab][3] += Nmass2;
          traits_species10[sp_lab][4] += Pmass;
          traits_species10[sp_lab][5] += Pmass2;
          traits_species10[sp_lab][6] += wsg;
          traits_species10[sp_lab][7] += wsg2;
          traits_species10[sp_lab][8] += crown;
          traits_species10[sp_lab][9] += crown2;
        }
        
      }
    }
  }
  
  //Create a vector of traits 
  vector<string> trait_names;
  
  trait_names.push_back("LMA");
  trait_names.push_back("LMA_sd");
  trait_names.push_back("Nmass");
  trait_names.push_back("Nmass_sd");
  trait_names.push_back("Pmass");
  trait_names.push_back("Pmass_sd");
  trait_names.push_back("wsg");
  trait_names.push_back("wsg_sd");
  trait_names.push_back("crown");
  trait_names.push_back("crown_sd");
  
  //Now calculate the means and sds properly 
  for(int spp = 1; spp < nbspp+1; spp++){
    int abu = abundances_species[spp];
    int abu10 = abundances_species10[spp];
    float inv_abu, inv_abu10;
    if(abu > 0) inv_abu = 1.0/float(abu);
    else inv_abu = 0.0;
    if(abu10 > 0) inv_abu10 = 1.0/float(abu10);
    else inv_abu10 = 0.0;
    
    //First divide both quantities by the number of individuals 
    for(int trait = 0; trait < 10; trait++){
      traits_species[spp][trait] *= inv_abu;
      traits_species10[spp][trait] *= inv_abu10;
    }
    
    //Then subtract the squared means from the standard deviations 
    traits_species[spp][1] -= traits_species[spp][0] * traits_species[spp][0];
    traits_species[spp][3] -= traits_species[spp][2] * traits_species[spp][2];
    traits_species[spp][5] -= traits_species[spp][4] * traits_species[spp][4];
    traits_species[spp][7] -= traits_species[spp][6] * traits_species[spp][6];
    traits_species[spp][9] -= traits_species[spp][8] * traits_species[spp][8];
    
    traits_species10[spp][1] -= traits_species10[spp][0] * traits_species10[spp][0];
    traits_species10[spp][3] -= traits_species10[spp][2] * traits_species10[spp][2];
    traits_species10[spp][5] -= traits_species10[spp][4] * traits_species10[spp][4];
    traits_species10[spp][7] -= traits_species10[spp][6] * traits_species10[spp][6];
    traits_species10[spp][9] -= traits_species10[spp][8] * traits_species10[spp][8];
    
  }
  
  //Now create outputs 
  output_species << iter;
  for(int spp = 1; spp < nbspp+1; spp++) output_species  << "\t" << abundances_species[spp];
  output_species << endl;
  
  output_species10 << iter;
  for(int spp = 1; spp < nbspp+1; spp++) output_species10  << "\t" << abundances_species10[spp];
  output_species10 << endl;
  
  output_biomass << iter;
  for(int spp = 1; spp < nbspp+1; spp++) output_biomass  << "\t" << biomass_species[spp];
  output_biomass << endl;
  
  //    for(int trait = 0; trait < 10; trait++){
  //
  //        output_traits << iter << "\t" << trait_names[trait];
  //        for(int spp = 1; spp < nbspp+1; spp++) output_traits  << "\t" << traits_species[spp][trait];
  //        output_traits << endl;
  //
  //        output_traits10 << iter << "\t" << trait_names[trait];
  //        for(int spp = 1; spp < nbspp+1; spp++) output_traits10  << "\t" << traits_species10[spp][trait];
  //        output_traits10 << endl;
  //    }
  
}

//##############################################
// Global function: returns ABC outputs for canopy height model (CHM)
//##############################################
void OutputABC_CHM(fstream& output_CHM, fstream& output_CHM_ALS, fstream& output_chmpotential){
  //Compute CHM changes 
  for(int s = 0; s < sites; s++){
    chm_field_changes[s] = chm_field_previous[s] - chm_field_current[s];
    chm_field_changes_ALS[s] = chm_field_previous_ALS[s] - chm_field_current_ALS[s];
  }
  
  //Compute CHM distribution 
  int chm_abc[70] = {0}, chm_abc_ALS[70] = {0};
  float mean_chm = 0.0, mean_chm_ALS = 0.0;
  float sd_chm = 0.0, sd_chm_ALS = 0.0;
  
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int s = c + r*cols;
      int height_chm = chm_field_current[s];
      mean_chm += float(height_chm);
      chm_abc[height_chm]++;
      int height_chm_ALS = chm_field_current_ALS[s];
      mean_chm_ALS += float(height_chm_ALS);
      chm_abc_ALS[height_chm_ALS]++;
    }
  }
  
  mean_chm *= isites_abc;
  mean_chm_ALS *= isites_abc;
  
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int s = c + r*cols;
      int height_chm = chm_field_current[s];
      sd_chm += (height_chm - mean_chm)*(height_chm - mean_chm);
      int height_chm_ALS = chm_field_current_ALS[s];
      sd_chm_ALS += (height_chm_ALS - mean_chm_ALS)*(height_chm_ALS - mean_chm_ALS);
    }
  }
  
  sd_chm = sqrt(sd_chm * isites_abc);
  sd_chm_ALS = sqrt(sd_chm_ALS * isites_abc);
  
  //Compute CHM change distribution 
  int chmchange_abc[140] = {0}, chmchange_abc_ALS[140] = {0};
  float mean_chmchange = 0.0, mean_chmchange_ALS = 0.0;
  float sd_chmchange = 0.0, sd_chmchange_ALS = 0.0;
  
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int s = c + r*cols;
      int height_chmchange = chm_field_changes[s];
      mean_chmchange += float(height_chmchange);
      chmchange_abc[height_chmchange+70]++;
      int height_chmchange_ALS = chm_field_changes_ALS[s];
      mean_chmchange_ALS += float(height_chmchange_ALS);
      chmchange_abc_ALS[height_chmchange+70]++;
    }
  }
  
  mean_chmchange *= isites_abc;
  mean_chmchange_ALS *= isites_abc;
  
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int s = c + r*cols;
      int height_chmchange = chm_field_changes[s];
      sd_chmchange += (height_chmchange - mean_chmchange)*(height_chmchange - mean_chmchange);
      int height_chmchange_ALS = chm_field_changes_ALS[s];
      sd_chmchange_ALS += (height_chmchange_ALS - mean_chmchange_ALS)*(height_chmchange_ALS - mean_chmchange_ALS);
    }
  }
  
  sd_chmchange = sqrt(sd_chmchange * isites_abc);
  sd_chmchange_ALS = sqrt(sd_chmchange * isites_abc);
  
  //    //! create variogram for both chmchange fields (final and change) 
  //    
  //    float chm_variogram[50] = {0.0}, chm_variogram_ALS[50] = {0.0};
  //    int chm_variogram_nb[50] = {0};
  //    
  //    float chmchange_variogram[50] = {0.0}, chmchange_variogram_ALS[50] = {0.0};
  //    int chmchange_variogram_nb[50] = {0};
  //    
  //    for(int r=row_start;r<row_end;r++){
  //        for(int c=col_start;c<col_end;c++){
  //            int height_canopy = chm_field_current[c + r * cols];
  //            int height_canopychange = chm_field_changes[c + r * cols];
  //            int height_canopy_ALS = chm_field_current_ALS[c + r * cols];
  //            int height_canopychange_ALS = chm_field_changes_ALS[c + r * cols];
  //            
  //            for(int r_compare=row_start;r_compare<row_end;r_compare++){
  //                for(int c_compare=col_start;c_compare<col_end;c_compare++){
  //                    //! only compare if you have not already compared 
  //                    if(r_compare > r && c_compare > c){
  //                        int height_canopy_compare = chm_field_current[c_compare + r_compare * cols];
  //                        int height_canopychange_compare = chm_field_changes[c_compare + r_compare * cols];
  //                        int height_canopy_compare_ALS = chm_field_current_ALS[c_compare + r_compare * cols];
  //                        int height_canopychange_compare_ALS = chm_field_changes_ALS[c_compare + r_compare * cols];
  //                        
  //                        int dist = int(sqrt(float((r_compare - r) * (r_compare - r) + (c_compare - c) * (c_compare - c))));
  //                        
  //                        //! only compute semivariance for distances < 50 
  //                        if(dist < 50){
  //                            chm_variogram[dist] += float((height_canopy_compare - height_canopy) * (height_canopy_compare - height_canopy));
  //                            chm_variogram_ALS[dist] += float((height_canopy_compare_ALS - height_canopy_ALS) * (height_canopy_compare_ALS - height_canopy_ALS));
  //                            chm_variogram_nb[dist]++;
  //                            
  //                            chmchange_variogram[dist] += float((height_canopychange_compare - height_canopychange) * (height_canopychange_compare - height_canopychange));
  //                            chmchange_variogram_ALS[dist] += float((height_canopychange_compare_ALS - height_canopychange_ALS) * (height_canopychange_compare_ALS - height_canopychange_ALS));
  //                            chmchange_variogram_nb[dist]++;
  //                        }
  //                        
  //                    }
  //                }
  //            }
  //        }
  //    }
  
  //Write to file 
  output_CHM << iter << "\t" << sites_abc << "\t" << mean_chm << "\t" << sd_chm << "\t";
  
  for(int h=0;h<70;h++){
    float density_CHM = float(chm_abc[h])*isites_abc;
    output_CHM << density_CHM << "\t";
  }
  
  output_CHM << mean_chmchange << "\t" << sd_chmchange << "\t";
  
  for(int h=0;h<140;h++){
    float density_chmchange = float(chmchange_abc[h])*isites_abc;
    output_CHM << density_chmchange << "\t";
  }
  
  output_CHM << endl;
  output_CHM_ALS << iter << "\t" << sites_abc << "\t" << mean_chm_ALS << "\t" << sd_chm_ALS << "\t";
  
  for(int h=0;h<70;h++){
    float density_CHM = chm_abc_ALS[h]*isites_abc;
    output_CHM_ALS << density_CHM << "\t";
  }
  output_CHM_ALS << mean_chmchange_ALS << "\t" << sd_chmchange_ALS << "\t";
  
  for(int h=0;h<140;h++){
    float density_chmchange = chmchange_abc_ALS[h]*isites_abc;
    output_CHM_ALS << density_chmchange << "\t";
  }
  output_CHM_ALS << endl;
  //In the future: compute canopy extension (i.e. lateral growth, vertical growth etc.) to test model of Kellner & Asner 2014  
}


//##############################################
// Global ABC function: ABC outputs
//##############################################
//! - this function creates various transmittance and lidar metrics, from the LAI field and the simulated lidar 
//! - the output variables are: avg transmittance per layer within all voxels of that layer (i.e. transmittance_full), only within voxels of that layer that are also inside the canopy (i.e. transmittance_incanopy, voxels below the CHM, if there is a gap with an average canopy height of 3m for example, then voxelsat 10m won't be counted for the transmittance avg at 10m), and transmittance only in filled voxes (i.e. only voxels with leaves, transmittance_nogaps) 
//! - additional metrics are the total number of voxels that have been considered for the averages 
//! - all metrics will be computed normalized to the ground (suffix z) and normalized to the canopy (suffix d), and both for the actual 3D canopy (no additional suffix) and a simulated lidar (additional suffix ALS) 
//! - !!!: TODO, detailed documentation 
void OutputABC_transmittance(fstream& output_transmittance, fstream& output_transmittance_ALS){
  
  //Compute height normalized metrics (z stands for height dimension) from it, both directly and with ALS simulation 
  // filled and empty voxels are only taken within the canopy 
  float transmittance_nogaps_z[70] = {0.0}, transmittance_nogaps_zALS[70] = {0.0};
  float transmittance_incanopy_z[70] = {0.0}, transmittance_incanopy_zALS[70] = {0.0};
  float transmittance_full_z[70] = {0.0}, transmittance_full_zALS[70] = {0.0};
  int voxcrown[70] = {0};
  int voxcanopy_z[70] = {0}, voxcanopy_zALS[70] = {0};
  int voxcanopy_empty_z[70] = {0}, voxcanopy_empty_zALS[70] = {0};
  int voxcanopy_filled_z[70] = {0}, voxcanopy_filled_zALS[70] = {0};
  int voxcanopy_na_z[70] = {0}, voxcanopy_na_zALS[70] = {0};
  
  //Calculate the volume filled by crowns (for packing densities) 
  // to be consistent with other estimates, we do not only use leaf-filled area, but the whole area used up by the crown (even when overlapping) 
  for(int site = 0; site < sites; site++){
    if(T[site].t_age > 0){
      int row_center = site/cols;
      int col_center = site%cols;
      
      AddCrownVolumeLayer(row_center, col_center, T[site].t_Tree_Height, T[site].t_Crown_Radius, T[site].t_Crown_Depth, voxcrown);
    }
  }
  
  //Compute the voxels for heights greater 2 separately, as the lowest layer in TROLL can be a bit difficult to interpret (lots of seedlings by default) 
  int voxcrown_total = voxcrown[0] + voxcrown[1];
  int voxcrown_greater2 = 0;
  
  for(int h = 2; h < 70; h++){
    voxcrown_total += voxcrown[h];
    voxcrown_greater2 += voxcrown[h];
  }
  
  //Update statistics based on 3D fields, including the simulated transmittance field 
  for(int h = min(70, HEIGHT)-1; h >= 0; h--){
    for(int r=row_start;r<row_end;r++){
      for(int c=col_start;c<col_end;c++){
        int site = c + r * cols;
        int height_canopy = chm_field_current[site];
        int height_canopy_ALS = chm_field_current_ALS[site];
        
        float transmittance = transmittance_direct[h][site];
        float transmittanceALS = transmittance_simulatedALS[h][site];
        
        if(h <= height_canopy){
          //Without ALS simulation, "real" 
          if(transmittance < 0.0){
            voxcanopy_na_z[h]++;
          } else {
            if(transmittance == 1.0){
              voxcanopy_empty_z[h]++;
            } else {
              voxcanopy_filled_z[h]++;
              transmittance_nogaps_z[h] += transmittance;
            }
            transmittance_incanopy_z[h] += transmittance;
            voxcanopy_z[h]++;
          }
          //From ALS simulation 
        }
        
        if(h <= height_canopy_ALS){
          if(transmittanceALS < 0.0){
            voxcanopy_na_zALS[h]++;
          } else {
            if(transmittanceALS == 1.0){
              voxcanopy_empty_zALS[h]++;
            } else {
              voxcanopy_filled_zALS[h]++;
              transmittance_nogaps_zALS[h] += transmittanceALS;
            }
            
            transmittance_incanopy_zALS[h] += transmittanceALS;
            voxcanopy_zALS[h]++;
            
          }
        }
      }
    }
    
    //Now calculate the mean 
    if(voxcanopy_filled_z[h] > 0) transmittance_nogaps_z[h] *= 1.0/float(voxcanopy_filled_z[h]);
    else transmittance_nogaps_z[h] = -1.0;
    if((voxcanopy_filled_z[h] + voxcanopy_empty_z[h]) > 0) transmittance_incanopy_z[h] *= 1.0/float(voxcanopy_filled_z[h] + voxcanopy_empty_z[h]);
    else transmittance_incanopy_z[h] = -1.0;
    
    if(voxcanopy_filled_zALS[h] > 0) transmittance_nogaps_zALS[h] *= 1.0/float(voxcanopy_filled_zALS[h]);
    else transmittance_nogaps_zALS[h] = -1.0;
    if((voxcanopy_filled_zALS[h] + voxcanopy_empty_zALS[h]) > 0) transmittance_incanopy_zALS[h] *= 1.0/float(voxcanopy_filled_zALS[h] + voxcanopy_empty_zALS[h]);
    else transmittance_incanopy_zALS[h] = -1.0;
    
    //Now the full LAI profile, including non-canopy areas 
    int voxeltotal = 0;
    
    for(int r=row_start;r<row_end;r++){
      for(int c=col_start;c<col_end;c++){
        int site = c + r * cols;
        
        float transmittance = transmittance_direct[h][site];
        float transmittanceALS = transmittance_simulatedALS[h][site];
        
        if(transmittance < 0.0) transmittance_full_z[h] += transmittance_incanopy_z[h];
        else transmittance_full_z[h] += transmittance;
        
        if(transmittanceALS < 0.0) transmittance_full_zALS[h] += transmittance_incanopy_zALS[h];
        else transmittance_full_zALS[h] += transmittanceALS;
        
        voxeltotal++;
        
      }
    }
    
    if(voxeltotal > 0){
      transmittance_full_z[h] *= 1.0/float(voxeltotal);
      transmittance_full_zALS[h] *= 1.0/float(voxeltotal);
    } else {
      transmittance_full_z[h] = -1.0;
      transmittance_full_zALS[h] = -1.0;
    }
  }
  
  //Now compute metrics normalized with regard to top of canopy (d for depth) 
  float transmittance_nogaps_d[70] = {0.0}, transmittance_nogaps_dALS[70] = {0.0};
  float transmittance_full_d[70] = {0.0}, transmittance_full_dALS[70] = {0.0};
  int voxempty_d[70] = {0}, voxempty_dALS[70] = {0};
  int voxfilled_d[70] = {0}, voxfilled_dALS[70] = {0};
  int voxna_d[70] = {0}, voxna_dALS[70] = {0};
  
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int site = c + r * cols;
      int height_canopy = chm_field_current[site];
      int height_canopy_ALS = chm_field_current_ALS[site];
      
      for(int d = 0; d <= height_canopy; d++){
        float transmittance = transmittance_direct[height_canopy - d][site];
        
        if(transmittance < 0.0){
          voxna_d[d]++;
        }
        else{
          if(transmittance == 1.0){
            voxempty_d[d]++;
          }
          else{
            voxfilled_d[d]++;
            transmittance_nogaps_d[d] += transmittance;
          }
          transmittance_full_d[d] += transmittance;
        }
      }
      
      for(int d = 0; d <= height_canopy_ALS; d++){
        float transmittanceALS = transmittance_simulatedALS[height_canopy_ALS - d][site];
        
        if(transmittanceALS < 0.0){
          voxna_dALS[d]++;
        } else {
          if(transmittanceALS == 1.0){
            voxempty_dALS[d]++;
          } else {
            voxfilled_dALS[d]++;
            transmittance_nogaps_dALS[d] += transmittanceALS;
          }
          transmittance_full_dALS[d] += transmittanceALS;
        }
      }
    }
  }
  
  for(int d = 0; d < 70; d++){
    if(voxfilled_d[d] > 0) transmittance_nogaps_d[d] *= 1.0/float(voxfilled_d[d]);
    else transmittance_nogaps_d[d] = -1.0;
    
    int voxnona = voxfilled_d[d] + voxempty_d[d];
    if(voxnona > 0) transmittance_full_d[d] *= 1.0/float(voxnona);
    else transmittance_full_d[d] = -1.0;
    
    if(voxfilled_dALS[d] > 0) transmittance_nogaps_dALS[d] *= 1.0/float(voxfilled_dALS[d]);
    else transmittance_nogaps_dALS[d] = -1.0;
    
    int voxnonaALS = voxfilled_dALS[d] + voxempty_dALS[d];
    if(voxnonaALS > 0) transmittance_full_dALS[d] *= 1.0/float(voxnonaALS);
    else transmittance_full_dALS[d] = -1.0;
  }
  
  //Finally we create PAI distributions, similar to efforts to estimate PAI from lidar simulations (cf. Greg's work) 
  // interpretation outside of an ABC context is a bit problematic, since we have many NA values that need to be imputated (otherwise PAI estimate is biased downwards), and since mean(log(x)) != log(mean(x)) 
  // we fill up voxels within canopy with mean canopy layer value, this decreases variance, but it does so for both empirical and simulated distributions, and the effect can be investigated given that we also have the direct measurements form LAI3D field 
  // furthermore, the canopy height will probably have a stronger effect on the distribution of PAI values than local variation within one layer 
  // as above, we will calculate the actual distribution and the ALS-inferred one 
  int stepsize = 10;
  int PAIhist[80] = {0}, PAIhist_ALS[80] = {0};  // for values going from 0 to 20, anything beyond is put into the 20 bin 
  int nb_aggregates = 0;
  
  for(int r=row_start;r<row_end;r+=stepsize){
    for(int c=col_start;c<col_end;c+=stepsize){
      float PAIcolumn = 0.0, PAIcolumn_ALS = 0.0;
      nb_aggregates++;
      
      //Loop over the whole canopy except for 0 height where there is no transmittance value 
      for(int h = HEIGHT; h > 0; h--){
        int nbvoxels = 0;
        float transmittance_avg = 0.0, transmittance_avgALS = 0.0;
        
        for(int r_avg=r;r_avg<r+stepsize;r_avg++){
          for(int c_avg=c;c_avg<c+stepsize;c_avg++){
            
            if(r_avg >= row_start && r_avg < row_end && c_avg >= col_start && c_avg < col_end){
              int site_avg = c_avg + r_avg * cols;
              float transmittance = transmittance_direct[h][site_avg];
              float transmittanceALS = transmittance_simulatedALS[h][site_avg];
              
              if(transmittance < 0.0) transmittance_avg += transmittance_incanopy_z[h];
              else transmittance_avg += transmittance;
              if(transmittanceALS < 0.0) transmittance_avgALS += transmittance_incanopy_zALS[h];
              else transmittance_avgALS += transmittanceALS;
              
              nbvoxels++;
            }
          }
        }
        if(nbvoxels > 0){
          if(transmittance_avg >= 0.0){
            transmittance_avg *= 1.0/float(nbvoxels);
            float PAI = -log(transmittance_avg)/klight;
            PAIcolumn += PAI;
          }
          if(transmittance_avgALS >= 0.0){
            transmittance_avgALS *= 1.0/float(nbvoxels);
            float PAI_ALS = -log(transmittance_avgALS)/klight;
            PAIcolumn_ALS += PAI_ALS;
          }
        }
      }
      //We bin in bins of 0.25 LAI difference 
      int PAIbin = min(int(4.0*PAIcolumn),79);
      int PAIbin_ALS = min(int(4.0*PAIcolumn_ALS),79);
      PAIhist[PAIbin]++;
      PAIhist_ALS[PAIbin_ALS]++;
    }
  }
  
  //Calculate summary statistics for crown packing analysis, i.e. the crown volume per voxel, average per layer and canopy height 
  // first compute maxheight 
  int height_max = 0;
  for(int r=row_start;r<row_end;r++){
    for(int c=col_start;c<col_end;c++){
      int site = c + r * cols;
      int height_canopy = chm_field_current[site];
      if(height_max < height_canopy) height_max = height_canopy;
    }
  }
  
  // number of voxels to consider 
  int voxmaxheight_total = height_max * sites_abc, voxmaxheight_greater2 = (height_max - 2) * sites_abc;
  int voxcanopy_total = 0, voxcanopy_greater2 = 0;
  int voxcanopy_filled_total = 0, voxcanopy_filled_greater2 = 0;
  
  voxcanopy_total += voxcanopy_z[0] + voxcanopy_z[1];
  voxcanopy_filled_total += voxcanopy_filled_z[0] + voxcanopy_filled_z[1];
  
  for(int h = 2; h < 70; h++){
    voxcanopy_total += voxcanopy_z[h];
    voxcanopy_greater2 += voxcanopy_z[h];
    voxcanopy_filled_total += voxcanopy_filled_z[h];
    voxcanopy_filled_greater2 += voxcanopy_filled_z[h];
  }
  
  //Write to file 
  output_transmittance << iter << "\t" << sites_abc << "\t";
  
  //Summary statistics 
  output_transmittance << height_max << "\t" << voxcanopy_total << "\t" << voxmaxheight_total << "\t" << voxcanopy_filled_total << "\t" << voxcanopy_greater2 << "\t" << voxmaxheight_greater2 << "\t" << voxcanopy_filled_greater2 << "\t" << voxcrown_total << "\t" << voxcrown_greater2 << "\t";
  
  //Now the more detailed metrics 
  // from the ground upwards 
  for(int h = 0; h < 70; h++) output_transmittance << voxcrown[h] << "\t";
  for(int h = 0; h < 70; h++) output_transmittance << transmittance_nogaps_z[h] << "\t";
  for(int h = 0; h < 70; h++) output_transmittance << transmittance_incanopy_z[h] << "\t";
  for(int h = 0; h < 70; h++) output_transmittance << transmittance_full_z[h] << "\t";
  for(int h = 0; h < 70; h++) output_transmittance << voxcanopy_z[h] << "\t";
  for(int h = 0; h < 70; h++) output_transmittance << voxcanopy_na_z[h] << "\t";
  for(int h = 0; h < 70; h++) output_transmittance << voxcanopy_empty_z[h] << "\t";
  for(int h = 0; h < 70; h++) output_transmittance << voxcanopy_filled_z[h] << "\t";
  
  //From the canopy top downwards 
  for(int d = 0; d < 70; d++) output_transmittance << transmittance_nogaps_d[d] << "\t";
  for(int d = 0; d < 70; d++) output_transmittance << transmittance_full_d[d] << "\t";
  for(int d = 0; d < 70; d++) output_transmittance << voxna_d[d] << "\t";
  for(int d = 0; d < 70; d++) output_transmittance << voxempty_d[d] << "\t";
  for(int d = 0; d < 70; d++) output_transmittance << voxfilled_d[d] << "\t";
  
  output_transmittance << nb_aggregates << "\t";
  
  for(int bin = 0; bin < 80; bin++){
    if(nb_aggregates > 0) output_transmittance << PAIhist[bin]/float(nb_aggregates) << "\t";
    else output_transmittance << "NA\t";
  }
  
  output_transmittance << endl;
  
  //Same as before, but for the simulated ALS scan
  output_transmittance_ALS << iter << "\t" << sites_abc << "\t";
  
  for(int h = 0; h < 70; h++) output_transmittance_ALS << transmittance_nogaps_zALS[h] << "\t";
  for(int h = 0; h < 70; h++) output_transmittance_ALS << transmittance_incanopy_zALS[h] << "\t";
  for(int h = 0; h < 70; h++) output_transmittance_ALS << transmittance_full_zALS[h] << "\t";
  for(int h = 0; h < 70; h++) output_transmittance_ALS << voxcanopy_zALS[h] << "\t";
  for(int h = 0; h < 70; h++) output_transmittance_ALS << voxcanopy_na_zALS[h] << "\t";
  for(int h = 0; h < 70; h++) output_transmittance_ALS << voxcanopy_empty_zALS[h] << "\t";
  for(int h = 0; h < 70; h++) output_transmittance_ALS << voxcanopy_filled_zALS[h] << "\t";
  
  for(int d = 0; d < 70; d++) output_transmittance_ALS << transmittance_nogaps_dALS[d] << "\t";
  for(int d = 0; d < 70; d++) output_transmittance_ALS << transmittance_full_dALS[d] << "\t";
  for(int d = 0; d < 70; d++) output_transmittance_ALS << voxna_dALS[d] << "\t";
  for(int d = 0; d < 70; d++) output_transmittance_ALS << voxempty_dALS[d] << "\t";
  for(int d = 0; d < 70; d++) output_transmittance_ALS << voxfilled_dALS[d] << "\t";
  
  output_transmittance_ALS << nb_aggregates << "\t";
  
  for(int bin = 0; bin < 80; bin++){
    if(nb_aggregates > 0) output_transmittance_ALS << PAIhist_ALS[bin]/float(nb_aggregates) << "\t";
    else output_transmittance_ALS << "NA\t";
  }
  
  output_transmittance_ALS << endl;
}
#endif


#ifdef MPI
// MPI Routines 

//##############################################
// Global MPI function: Communication of border fields in the parallel version of the code
//##############################################
//! - Only if the MPI option has been enabled 
void MPI_ShareSeed(unsigned char **c, int n) {
  
  MPI_Status status;
  
  if(p_rank == size-1) MPI_Sendrecv(c[0],n,MPI_UNSIGNED_CHAR,size-2,0,c[3],n,MPI_UNSIGNED_CHAR,0,0,MPI_COMM_WORLD,&status);
  if(p_rank == 0) MPI_Sendrecv(c[0],n,MPI_UNSIGNED_CHAR,size-1,0,c[3],n,MPI_UNSIGNED_CHAR,1,0,MPI_COMM_WORLD,&status);
  if((p_rank) && (p_rank < size-1)) MPI_Sendrecv(c[0],n,MPI_UNSIGNED_CHAR,p_rank-1,0,c[3],n,MPI_UNSIGNED_CHAR,p_rank+1,0,MPI_COMM_WORLD,&status);
  
  if(p_rank == 0) MPI_Sendrecv(c[1],n,MPI_UNSIGNED_CHAR,1,1,c[2],n,MPI_UNSIGNED_CHAR,size-1,1,MPI_COMM_WORLD,&status);
  if(p_rank == size-1) MPI_Sendrecv(c[1],n,MPI_UNSIGNED_CHAR,0,1,c[2],n,MPI_UNSIGNED_CHAR,size-2,1,MPI_COMM_WORLD,&status);
  if((p_rank) && (p_rank < size-1)) MPI_Sendrecv(c[1],n,MPI_UNSIGNED_CHAR,p_rank+1,1,c[2],n,MPI_UNSIGNED_CHAR,p_rank-1,1,MPI_COMM_WORLD,&status);
}

//##############################################
// Global MPI function: Communication of fields
//##############################################
void MPI_ShareField(unsigned short **cl, unsigned short ***cp, int n) {
  
  MPI_Status status;
  for(int h=0;h<(HEIGHT+1);h++) {
    if(p_rank == 0) MPI_Sendrecv(cl[h],n,MPI_UNSIGNED_SHORT,size-1,h,cp[1][h],n,MPI_UNSIGNED_SHORT,1,h,MPI_COMM_WORLD,&status);
    if(p_rank == size-1) MPI_Sendrecv(cl[h],n,MPI_UNSIGNED_SHORT,size-2,h,cp[1][h],n,MPI_UNSIGNED_SHORT,0,h,MPI_COMM_WORLD,&status);
    if((p_rank) && (p_rank < size-1)) MPI_Sendrecv(cl[h],n,MPI_UNSIGNED_SHORT,p_rank-1,h,cp[1][h],n,MPI_UNSIGNED_SHORT,p_rank+1,h,MPI_COMM_WORLD,&status);
    
    if(p_rank == 0) MPI_Sendrecv(cl[h]+sites,n,MPI_UNSIGNED_SHORT,1,h+HEIGHT,cp[0][h],n,MPI_UNSIGNED_SHORT,size-1,h+HEIGHT,MPI_COMM_WORLD,&status);
    if(p_rank == size-1) MPI_Sendrecv(cl[h]+sites,n,MPI_UNSIGNED_SHORT,0,h+HEIGHT,cp[0][h],n,MPI_UNSIGNED_SHORT,size-2,h+HEIGHT,MPI_COMM_WORLD,&status);
    if((p_rank) && (p_rank < size-1)) MPI_Sendrecv(cl[h]+sites,n,MPI_UNSIGNED_SHORT,p_rank+1,h+HEIGHT,cp[0][h],n,MPI_UNSIGNED_SHORT,p_rank-1,h+HEIGHT,MPI_COMM_WORLD,&status);
  }
}

//##############################################
// Global MPI function: Communication of treefalls
//##############################################
void MPI_ShareTreefall(unsigned short **c, int n) {
  
  MPI_Status status;
  if(p_rank == 0) MPI_Sendrecv(c[0],n,MPI_UNSIGNED_SHORT,size-1,0,c[2],n,MPI_UNSIGNED_SHORT,1,0,MPI_COMM_WORLD,&status);
  if(p_rank == size-1) MPI_Sendrecv(c[0],n,MPI_UNSIGNED_SHORT,size-2,0,c[2],n,MPI_UNSIGNED_SHORT,0,0,MPI_COMM_WORLD,&status);
  if((p_rank) && (p_rank < size-1)) MPI_Sendrecv(c[0],n,MPI_UNSIGNED_SHORT,p_rank-1,0,c[2],n,MPI_UNSIGNED_SHORT,p_rank+1,0,MPI_COMM_WORLD,&status);
  
  if(p_rank == 0) MPI_Sendrecv(c[0]+2*n,n,MPI_UNSIGNED_SHORT,1,1,c[1],n,MPI_UNSIGNED_SHORT,size-1,1,MPI_COMM_WORLD,&status);
  if(p_rank == size-1) MPI_Sendrecv(c[0]+2*n,n,MPI_UNSIGNED_SHORT,0,1,c[1],n,MPI_UNSIGNED_SHORT,size-2,1,MPI_COMM_WORLD,&status);
  if((p_rank) && (p_rank < size-1)) MPI_Sendrecv(c[0]+2*n,n,MPI_UNSIGNED_SHORT,p_rank+1,1,c[1],n,MPI_UNSIGNED_SHORT,p_rank-1,1,MPI_COMM_WORLD,&status);
}
#endif

//!  Free dynamic memory  
void FreeMem () {
  delete [] nbdbh;
  delete [] layer;
  delete [] SPECIES_GERM;
#ifdef WATER
  delete [] site_DCELL;
#endif
  for(int site=0; site < sites; site++) delete [] SPECIES_SEEDS[site];
  delete [] SPECIES_SEEDS;
  delete [] p_seed;
  delete [] n_seed;
  delete [] p_species;
  delete [] n_species;
  
  if(_SEEDTRADEOFF || _NDD) delete [] PROB_S;
  for (int h=0; h<(HEIGHT+1); h++) delete [] LAI3D[h];
  delete [] LAI3D;
  
  for (int i=0; i<3; i++) delete [] Thurt[i];
  
#ifdef Output_ABC
  delete [] chm_field_previous;
  delete [] chm_field_previous_ALS;
  delete [] chm_field_current;
  delete [] chm_field_current_ALS;
  delete [] chm_field_changes;
  
  for (int h=0; h<(HEIGHT+1); h++) {
    delete [] transmittance_simulatedALS[h];
    delete [] transmittance_direct[h];
    delete [] transmittance_simulatedALS_sampling[h];
  }
  
  delete [] transmittance_simulatedALS;
  delete [] transmittance_direct;
  delete [] transmittance_simulatedALS_sampling;
  
  delete [] abundances_species;
  delete [] abundances_species10;
  delete [] biomass_species;
  
  for (int spp = 0; spp < (nbspp+1); spp++) {
    delete [] traits_species[spp];
    delete [] traits_species10[spp];
  }
  delete [] traits_species;
  delete [] traits_species10;
  
#endif
  delete [] LookUp_T;
  delete [] LookUp_KmT;
  delete [] LookUp_VPD;
  delete [] LookUp_flux_absorption;
  delete [] LookUp_flux;
  delete [] LookUp_Rday;
  delete [] LookUp_JmaxT;
  delete [] LookUp_Rstem;
  delete [] LookUp_GammaT;
  delete [] LookUp_Rnight;
  delete [] LookUp_VcmaxT;
}
