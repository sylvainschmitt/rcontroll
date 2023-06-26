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
 *    - Version 3.1: Isabelle Maréchaux (May-June 2021)
 *    - Version 4.0: Isabelle Maréchaux (Feb 2023)
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
 *     - Basic uniform sampling functions (for more information: c:
 *        -# gsl_rng_uniform(gsl_rand) which samples range [0,1)
 *        -# gsl_rng_uniform_pos(gsl_rand) which samples range (0,1) to avoid 0 for log transformations
 *        -# gls_rng_uniform_int(gsl_rand, unsigned long int n) samples long integers from 0 to n-1
 */
//////////////////////////////////////////////////////////////////////////////////

#define LCP_alternative  //!< new in v.3.1.4: new way of checking whether light environment supports tree birth, dependent on individual seed/seedling's intraspecific variation >
#define G0 // test with positive g0 in stomatal conductance model
#define FF_todiscuss     //!< small additions and quick fixes that need to be checked/discussed very carefully
#undef MPI               //!< MPI = Message Passing Interface. Software for sharing information across processors in parallel computers. If global variable MPI is not defined, TROLL functions on one processor only. if flag MPI defined, parallel routines (MPI software) are switched on. WARNING!!!: MPI has not been maintained since v.2.2, several functions need updating
#define WATER             //!< new in v.3.0: If defined, an explicit water cycle is added, with an explicit belowground space. The horizontal resolution of the soil field is currently set by DCELL
#define PHENO_DROUGHT
#define CROWN_UMBRELLA   //!< new in v.2.4.1, modified in v.2.5: If activated, crowns are assumed to grow cylindrical until they reach 3m in depth, then the center of the crown will experience quicker height growth than the outer parts of the crown, thus leading to an umbrella like crown shape (with slope depending on crown depth and width). Depending on the slope parameter, trees will look more like cylinders or more like cones. Contrary to previous versions, however, the crown will not fill its shape underneath the first three layers. The crown will simply become umbrella-like, with a dense layer that spans out from its stem, and empty space underneath. This allows for a very simple computation of just three crown layers, while retaining their spread across the crown depth, enough realism for LiDAR derived CHMs and for eventual non vertical light penetration. New in v.2.5: Entirely modelled through one template that simulates loops across a crown layer/shell and can be applied to any calculation that requires to calculate crown properties (CalcLAI, Fluxh, etc.). Furthermore, enhanced flexibility for future incorporation of other crown shape functions (e.g. spherical, etc.)
#undef LAI_gradient      //!< new in v.2.4.1, modified in v.2.5: a tree's leaf density changes from top to bottom. Broadly based on Kitajima 2004 (Ann Bot), we assume that trees allocate 50% of their leaves to the first meter of their crown, 25% to the second meter layer, and the rest evenly spread across the crown. In case of the umbrella-like crowns, layers correspond to depth from canopy top. In case of a gap in the top layers, there is no shift of leaf density to a lower layer. This ensures that the gaps are real gaps. New in v.2.5: activated within a single function (LAI2dens) as part of the crown template scheme. Small effect in this case, however, since all leaves are concentrated in three shells anyways, with a shift from (0.33,0.33,0.33) to (0.5, 0.25,0.25) likely no strong effect. If implemented for cylindric crowns with full depth, this might, however, have a strong effect, as upper leaves are less impacted by crown overlap than lower leaves.
#undef Output_ABC       //!< new in v.2.4.1, refined in v.2.5: PARAMETERIZATION/OUTPUT TOOL, inclusion of ABC routines for comprehensive parameter inference with TROLL
#define CHM_SPIKEFREE    //!< new in v.2.5: PARAMETERIZATION/OUTPUT TOOL. Since our empirical CHMs are typically derived with a spike-free/pit-free algorithm (e.g. from LAStools), we should also remove spikes/pits from the TROLL-generated CHM. Here, this is simply implemented as considering all crown gaps as filled. A more sophisticated version could use a simulated lidar point cloud and then apply the spike-free/pit-free algorithms or compare the raw CHMs of simulated lidar and actual lidar. Since we are, however, unlikely to capture the microstructure of crowns well (i.e. crown gaps are likely to be smaller, but more frequent than the simulated gaps), the current approach is probably a good compromise.
#undef TRACK_INDIVIDUALS //!< new in v.2.5: DIAGNOSTIC TOOL, individual-based tracking of trees for PBA (process-based analytics, an accounting-like monitoring of model behavior through individual states and processes). PLEASE BE AWARE: onset of tracking is currently hardcoded to comprise trees born in year 501 in forest regeneration. Also: Filesizes can get VERY VERY LARGE if more trees are included.
#undef CHECK_CARBON      //!< new in v.2.5: DIAGNOSTIC TOOL, checking of carbon budgets, could potentially be extended for nutrient budget checking in the future. The idea is to keep track of carbon stocks and carbon fluxes every timestep to see whether there are any deviations from expectations - to do so, differences between stocks are computed at each timestep, and can be compared to the gross and net assimilation of carbon
#define FULL_CLIMATE



// LIBRAIRIES
#include <cstdio>
#include <iostream>
#include <iomanip> //new IM (for setprecision)
#include <fstream>
#include <cstdlib>
#include <string>
#include <limits>
#include <ctime>
#include <cmath>
#include <vector>
#include <sstream>
#include <algorithm>
#include <typeinfo>


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

// Global constants (e.g. PI and derivatives, physical constant etc)
# define PI 3.141592654  //!< Global constant: Pi
# define twoPi 6.2831853071 //!< Global constant: 2.0*Pi
# define Pis2 1.570796327 //!< Global constant: Pi/2.0
# define iPi 0.3183099 //!< Global constant: 1.0/Pi

// Constants used only when WATER is defined
#define ABSZERO -273.15    //!< Minimal temperature (in °C)
#define AIRMA 0.02896279  //!< molelcular mass of air (kg/mol)
#define CPAIR 1010.0      //!< heat capacity of air (J/kg/K)
#define EMLEAF 0.95       //!< emissivity of thermal radiation by leaf
#define SIGMA 5.67e-8     //!< Stefan-Boltzmann constant (W m-2 K-4)
#define H2OLV0 2.501e6    //!< latent heat H2O (J/kg)
#define H2OMW 18.0e-3     //!< mol mass H2O (kg/mol)
#define RCONST 8.314      //!< universal gas constant (J/mol/K)
#define DHEAT 21.5e-6     //!< molecular diffusivity to heat (m2/s)
#define GBHGBC 1.32       //!< Ratio of Gbh:Gbc (value consistent with Vezy et al. 2018, double-checked by R. Vezy (cf. mail 21/10/2020))
#define GBVGBH 1.075      //!< Ratio of Gbw:Gbh (relative diffusivities of heat and water vapor in air) (cf. equ. A7 in Medlyn et al. 2007; equ. E5 in Leuning et al. 1995)
#define GSVGSC 1.57       //!< Ratio of Gsw:Gsc (relative diffusivities of CO2 and water vapor in air)
#define LSQ 0.15          //!< Light spectral quality: effective spectral quality of light
#define DAYRESP 0.4       //!< Inhibition of leaf dark respiration in day light by ca. 40%, cf. Atkin et al. 2000. Note: should be given in the input file probably

// the following parameter values could be provided in input
#define Rtot_by_Rabove 1.5 //!< Multiplier coefficient used to roughly account for belowground respiration (coarse and fine root respiration)
#define Rtotleaf_by_Rdark 1.4 //!< Multiplier coefficient used to roughly account for leaf respiration during the day
#define alphaInoue 3.0 //!< alpha coefficient in the model of wind speed profile by Inoue (1963). Values provided in Raupach et al. 1991, Shaw and Pereira (1982), Shuttleworth et al. (1989) (see Rau et al's manuscript)
#define MeteoStation_Height 55.0 //!< Height of the meteo station where the input climate data are taken from. Here supposed to be above the canopy.

char buffer[256], inputfile[256], inputfile_daytimevar[256], inputfile_climate[256], inputfile_soil[256], outputinfo[256],inputfile_inventory[256], inputfile_pointcloud[256], *bufi(0), *bufi_daytimevar(0), *bufi_climate(0), *bufi_soil(0), *buf(0), *bufi_data(0), *bufi_pointcloud(0); //!< Global variable: character strings used to read file names, or other features
#ifdef WATER
char inputfile_SWC[256], *bufi_dataSWC(0);

#endif
char inputfile_species[256], *bufi_species(0); //!< Global variable: vector of input files

// FILE OUTPUT STREAMS. Updated in v.3.1 to reduce number of streams and increase clarity
fstream output_info;                //!< Global variable:  basic simulation information
fstream output_basic[4];            //!< Global variable:  default output streams, always used
fstream output_extended[9];         //!< Global variable:  extended TROLL outputs, preserved from previous versions, might need further clean-up
fstream output_visual[2];           //!< Global variable: outputs for visualization/gif creation, new in v.3.1.2
fstream output_pointcloud;          //!< Global variable: optional las file output, new in v.3.1.6

#ifdef Output_ABC
fstream output_abc[11];             //!< Global variable: output streams for Approximate Bayesian Computation
#endif

#ifdef WATER
fstream output[40];//!< Global variable: output files
//fstream output_water[10];           //!< Global variable: output streams for water module (in development), to be modified
#endif

#ifdef TRACK_INDIVIDUALS
fstream output_track[3];            //!< Global variable: output streams for tracking of trees
#endif

// USER CONTROLS. Options that can be turned on (1) or off (0). This comes at computational cost: where routines have to be called frequently, if-conditioning should be done as far outside the loop as possible (e.g. for DAYTIMELIGHT outside voxel loops) .Options are set below, but inclusion in parameter sheet needed (for control from R)
bool _NONRANDOM;     //!< User control: If _NONRANDOM == 1, the seeds for the random number generators will be kept fixed at 1, for bug fixing
bool _GPPcrown;      //!< User control: This defines an option to compute only GPP from the topmost value of PPFD and GPP, instead of looping within the crown.
bool _BASICTREEFALL; //!< User control: if defined: treefall is a source of tree death (and if TREEFALL not defined, this is modeled through simple comparison between tree height and a threshold t_Ct, if not defined, treefall is not represented as a separated and independent source of death, but instead, all tree death are due to the deathrate value)
bool _SEEDTRADEOFF;  //!< User control: if defined: the number of seeds produced by each tree is determined by the tree NPP allocated to reproduction and the species seed mass, otherwise the number of seeds is fixed; besides, seedling recruitment in one site is not made by randomly and 'equiprobably' picking one species among the seeds present at that site but the probability of recruitment among the present seeds is proportional to the number of seeds (in s_Seed[site]) time the seed mass of each species
bool _NDD;           //!< User control: if defined, negative density dependant processes affect both the probability of seedling recruitment and the local tree death rate. The term of density-dependance is computed as the sum of conspecific tree basal area divided by their distance to the focal tree within a neighbourhood (circle of radius 15m)
bool _CROWN_MM;      //!< User control: new in v.2.4.1: Michaelis Menten allometry for crowns instead of power law, since v.2.5: not a macro anymore, but set at runtime (little overhead since queried only once at birth and once per timestep), !!!: power law is the default. If Michaelis Menten type allometry is activated, the parameters have to be changed in input sheet accordingly
bool _OUTPUT_extended;//!< User control: changed in v.3.1 from _OUTPUT_reduced to _OUTPUT_extended, uses extended set of output streams
bool _OUTPUT_inventory;//!< User control: added in v4.0 to ease stop and start from inventories.
bool _FromInventory;      //!< User control: if defined, an additional input file can be provided to start simulations from an existing data set or a simulated data set (5 parameters are needed: x and y coordinates, dbh, species_label, species
bool _sapwood;         //!< User control: two ways of parameterising sapwood density: constant thickness (0), Fyllas, but with lower limit (1)
bool _seedsadditional; //!< User control: excess carbon into seeds? no/yes=(0/1)
bool _LL_parameterization;   //!< User control: two ways for parameterising leaf lifespan: empirical (derived by Sylvain Schmitt, TODO: from which data?), Kikuzawa model (0,1)

int _LA_regulation;     //!< User control: updated v.3.1: potentially three ways of parameterising leaf dynamic allocation, but currently using only two ways: no regulation (0), never exceed LAImax, i.e. the maximum LAI under full sunlight (1), adjust LAI to the current light environment (2). To switch between option 1 and 2, only one line is necessary in CalcLAmax()
int _OUTPUT_pointcloud;  //!<User control: ATTENTION! At the moment assumes a little-endian system (most personal computers, but not necessarily server systems), because LAS fles are in little-endian! If == 1, creates a point cloud from a simplified ALS simulation;

int _SOIL_LAYER_WEIGHT; // !< User control: three ways of computing the fraction of transpiration supplied by each soil layer in individual tree total transpiration, and to weight the tree average water potential in the root zone. (0: root biomass only, 1: relative root-to soil conductance, 2: relative estimated maximal transpiration as in Duursma & Medlyn 2012).
int _WATER_RETENTION_CURVE; // !< User control: different water retention curves can be used. So far two are have been implemented: either brooks & Corey (0), either van Genuchten-Mualem (1). To each wtare retention cirve option is associated a different set of pedo-transfer functions, see Table 2 (texture-based Tomasella & Hodnett 1998

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
#ifdef FULL_CLIMATE
int nbdays;     //!< Global variable: number of days with explicit daily climate variation provided in input
#endif // FULL_CLIMATE

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
#ifdef WATER
float PRESS; //!< Global variable: atmospheric CO_2 pressure (in kPa). If in the future PRESS should vary, it should have the same status as other climatic variables
#endif
float iCair; //!< Global variable: inverse of Cair

float crown_gap_fraction; //!< Global variable: fraction of gaps in a crown, new in v.2.4.1: adds gaps according to a predetermined gap_fraction in tree crown. Gaps inserted regularly (i.e. every 4 pixels for a gap fraction of 0.25). In v.2.5:  gap fraction is now given as a target fraction and, in filling up a tree's crown, gaps are inserted when fraction_filled is too high: more accurate approximation of gap fraction (e.g., gap fractions > 0.5 are now possible). Also in v.2.5: intraspecific variation in crown radius affects gap fraction: trees with larger crowns also have more gaps, so large crowns get more open and smaller crowns denser, while keeping the same leaf area. !!!IMPROVE_IN_FUTURE
float shape_crown; //!< Global variable: percentage of crown radius that reaches the crown top and defines how steep the crown slope is; only required if CROWN_UMBRELLA is activated, but added to main code so that input sheet does not have to be modified.

// NEGATIVE DENSITY DEPENDENCE (NDD)
float Rndd; //!< Global variable: distance beyond which negative density dependence (NDD) effect is not accounted anymore
float deltaR; //!< Global variable: negative density dependence (NDD) strength parameter in recruitment rate
float deltaD; //!< Global variable: negative density dependence (NDD) strength parameter in death rate
float BAtot; //!< Global variable: !!!UPDATE

// VISUALIZATION parameter
int extent_visual; //!< Global variable: extent of visualization outputs, bounded by extent of simulation
int mincol_visual, maxcol_visual, minrow_visual, maxrow_visual, minrow_visual_slice, maxrow_visual_slice; //!<! Global variables: computed from extent, defining the two visualization outputs


//! ENVIRONMENTAL VARIABLES

// Daily variation of environmental variables. Defined since version 2.2, updated 2.5 and 3.0: now parameterised as variation around daily mean value, with arbitrary length of step. Users should ensure that climate file provides the correct average (i.e. if they provide only daytime variation, then the climate file needs to provide daytime averages, if they provide a 24h cycle, then the climate file needs to provide 24h averages)
vector<float> varday_light;     //!< Global vector: light irradiance variation during an average day, since v.2.5: standardized with respect to the mean and summing to 0 (adimensional)
vector<float> varday_vpd;       //!< Global vector: Vapour pressure deficit (VPD) variation during an average day, since v.2.5:  standardized with respect to the mean and summing to 0 (adimensional)
vector<float> varday_T;         //!< Global vector: Temperature variation during an average day, since v.2.5:  standardized with respect to the mean and summing to 0 (adimensional)
vector<float> varday_WS;        //!< Global vector: Wind speed variation during an average day, since v.2.5:  standardized with respect to the mean and summing to 0 (adimensional)
int nbsteps_varday;             //!< Global variable: number of steps for environmental variables
float inv_nbsteps_varday;       //!< Global variable: the inverse of the step number for environmental variables
float nbhours_covered;          //!< Global variable: number of hours of environmental variables in the environmental input file


// Climate input data; as provided in the input file. File structure depends on the timestep and scenario used for the simulation
// new version 2.2, updated in 2.5: use mean temperatures instead of maxima as reference point. This corrects an overestimation of all environmental variables (daily cycle in input file has been adjusted accordingly)
vector<float> DailyMeanTemperature;             //!< Global vector: daily mean temperature in degree Celsius
vector<float> DailyMeanIrradiance;              //!< Global vector: daily mean irradiance (W/m^2)
vector<float> DailyMeanVapourPressureDeficit;   //!< Global vector: daily mean vapour pressure deficit (kPa)
vector<float> NightTemperature;                 //!< Global vector: night temperature in degree Celsius
vector<float> Rainfall;                         //!< Global vector: rainfall (mm)
vector<float> DailyMeanWindSpeed;                        //!< Global vector: wind speed (m/s)


// LOOKUP TABLES
// Complex temperature-dependent functions used in the Farquhar model are computed once at 'Taccuracy' resolution. Leaf temperature must be comprised between 0°C and 60°C, Values are stored every 0.5°C step in Tleaf, so 120 values in total
int nbTbins;                            //!< Global variable: number of bins for the temperature lookup tables
float iTaccuracy;                       //!< Global variable: inverse of accuracy of a temperature bin (e.g. if Taccuracy is 0.1 or 0.5 °C, then iTaccuracy is 10.0 or 2.0, respectively)
float *LookUp_KmT(0);                   //!< Global vector: lookup table for Km(T) in Farquhar model
float *LookUp_GammaT(0);                //!< Global vector: lookup table for Gamma(T) in Farquhar model
float *LookUp_VcmaxT(0);                //!< Global vector: lookup table for Vcmax(T) in Farquhar model
float *LookUp_JmaxT(0);                 //!< Global vector: lookup table for Jmax(T) in Farquhar model
//float *LookUp_Rday(0);                  //!< Global vector: lookup table for Rday(T) in Farquhar model //new IM: no redundancy anymore between LookUp_Rday and LookUp_Rnight
float *LookUp_Rleaf(0);                  //!< Global vector: lookup table for Rleaf(T) in Farquhar model //new IM: no redundancy anymore between LookUp_Rday and LookUp_Rnight
float *LookUp_flux_absorption(0);       //!< Global vector: lookup table for faster computation of PPFD. New in v.2.4: absorption flux
float *LookUp_flux(0);                  //!< Global vector: lookup table for faster computation of PPFD. New in v.2.4: averaging instead of top value, largely replaced by LookUp_flux_absorption, but still needed, for example to compute light above crown without explicit computation of absorption
float *LookUp_ExtinctLW(0);                  //!< Global vector: lookup table for faster computation of extinction of thermal radiation, following Leuning et al. 1995 equ D1
float *LookUp_VPD(0);                   //!< Global vector: lookup table for faster computation of vapour pressure deficit (VPD). New in v.2.4:averaging instead of top value
float *LookUp_T(0);                     //!< Global vector: lookup table for faster computation of temperature. New in v.2.4: averaging instead of top value
float *LookUp_Rstem(0);                 //!< Global vector: lookup table for faster computation of Rstem
//float *LookUp_Rnight(0);                //!< Global vector: lookup table for faster computation of Rstem //new IM: no redundancy anymore between LookUp_Rday and LookUp_Rnight
int LookUp_Crown_site[2601];            //!< Global vector: new in v.2.4: lookup table to fill crown cylinder sequentially from inside to outside, allowing for smooth crown growth
#ifdef WATER
int nbVPDbins;                         //!< Global variable: number of bins for the VPD lookup tables
float iVPDaccuracy;                         //!< Global variable: inverse of accuracy of a vpd bin (e.g. if VPDaccuracy is 0.01 kPa, then iVPDaccuracy is 100.0)
float **LookUp_INLR(0);                   //!< Global vector: lookup table for the isothermal net radiation computation in Penman-Monteith equation
float *LookUp_SLOPE(0);                   //!< Global vector: lookup table for constant s in Penman-Monteith equation
float *LookUp_GRADN(0);                   //!< Global vector: lookup table for radiation conductance in Penman-Monteith equation
int nbHbins;                            //!< Global variable: number of bins for the Wind lookup tables
float iHaccuracy;
float *LookUp_Wind(0);
#endif

// ENVIRONMENTAL VARIABLES
float tnight; //!< Global variable: Night mean temperature (degree C)
float precip; //!< Global variable: Rainfall  (mm)
float WSDailyMean; //!< Global variable: WindSpeed (m/s)
float WDailyMean; //!< Global variable: Daily mean irradiance (average for timestep) (micromol PAR photon/m^2/s), used in the photosynthesis part. !!! W/m2 is the common unit of meteorological stations ==> need to be converted using SWtoPPFD
float tDailyMean; //!< Global variable: Daily mean temperature (degree C)
float VPDDailyMean; //!< Global variable: Daily mean VapourPressureDeficit (kPa)
float WDailyMean_year; //!< Global variable: average WDailyMean per year
float tDailyMean_year; //!< Global variable: average tDailyMean per year
float VPDDailyMean_year; //!< Global variable: average VPDDailyMean per year
#ifdef WATER
float windDailyMean_year; //!< Global variable: average WindSpeed per year
#endif
float Tnight_year; //!< Global variable: average tnight per year
#ifdef FULL_CLIMATE
float *WDailyMean_all(0); //!< Global variable: average daily irradiance across the whole periode
float *VPDDailyMean_all(0); //!< Global variable: average daily VPD across the whole periode
float *tDailyMean_all(0); //!< Global variable: average daily temperature across the whole periode
float *windDailyMean_all(0); //!< Global variable: average daily wind across the whole periode
#endif


// GLOBAL VARIABLES ACROSS SPECIES
float SWtoPPFD; //!< Global variable: conversion factor for shortwave irradiance measured in W/m2 to PPFD in micromol of PAR (micromol/s/m^2, as used in the Farquhar model). Around 2.0-2.5 in the tropics. Data at Nouragues (comparing photon count and irradiance) give a value: 2.27. Depends on cloudiness (in non-cloudy areas, the fraction of PAR in irradiance arriving on the ground may be much lower). This is typically equals to ca 0.5*4.57, where 0.5 stands for the fact that ca 50% of the total solar energy reaching the Earth’s surface corresponds to PAR, and 4.57 umol J-1 is basically equal to 10^6/(Emean*Avogadro number) where Emean is the average energy of a photon across PAR wavelength range: Ephoton=h*c/wavelength).
#ifdef WATER
float PPFDtoSW; //!< Global variable: conversion factor from PPFD in micromol of PAR to shortwave irradiance measured in J/ Inverse of SWtoPPFD.
#endif
float klight; //!< Global variable: light absorption rate or extinction coefficient used in Beer-Lambert law to compute light within the canopy
float kpar; //!< Global variable: new in v.2.5: effective light absorption rate or extinction coefficient used in Beer-Lambert law to compute light within the canopy; kpar = klight * absorptance_leaves
float phi; //!< Global variable: true quantum yield; previously was apparent quantum yield (in micromol C/micromol photon): quantum yield multiplied by leaf absorptance. Quantum yield is often provided per absorbed light flux, so one should multiply incident PPFD by leaf absorptance (Poorter et al American Journal of Botany). For some authors, it should be species-dependent or environment dependent, but these options are not implemented here (see eg Domingues et al 2014 Plant Ecology & Diversity). As of v2.5: phi is the quantum yield, without multiplication with absorptance (see absorptance_leaves)
float theta; //!< Global variable: parameter of the Farquhar model set to 0.7 in this version. For some authors, it should be species-dependent or environment dependent, but these options are not implemented here
float absorptance_leaves; //!< Global variable: absorptance of leaves (close to 0.91 for tropical tree species)
float g1; //!< Global variable: g1 parameter of Medlyn et al's model of stomatal conductance. v230: defined as a global parameter shared by species, instead of a Species class variable. !!!UPDATE A species-specific value of g1 (cf Lin et al. 2015 NCC or Wu et al. 2019 GCB) is needed to simulate functional shift through a regeneration. Different values of g1 across PFT are also used by Xu et al. 2016 New Phytologist using ED2+SPA applied on tropical dry forests
#ifdef G0
float g0;   //!< Global variable: minimum leaf conductance, in mmolH20 m-2 s-1, parameter of stomatal conductance model. Defined as a global parameter shared by species, in absence of a clear variation with other traits (see Duursma et al. 2018 New Phytologist, Slot et al. 2021 New Phytologist, METRADICA data)
#endif
#ifdef PHENO_DROUGHT
float pheno_thres;  //!< Global variable: threshold beyond which a change in t_pheno_factor (that controls old leaves shedding) is triggered, in proportion of leaf TLP
float pheno_delta;  //!< Global variable: amplitude of change in t_pheno_factor per timestep beyond pheno_thres
#endif
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

// LookUp_tables for intraspecific variation, modified FF v.3.1.5 (reduced to 10000)
float d_intraspecific_height[10000]; //!< Global vector: distribution of intraspecific values for maximal tree height
float d_intraspecific_CR[10000]; //!< Global vector: distribution of intraspecific values for maximal crown radius
float d_intraspecific_CD[10000]; //!< Global vector: distribution of intraspecific values for maximal crown depth
float d_intraspecific_P[10000]; //!< Global vector: distribution of intraspecific values for leaf phosphorus
float d_intraspecific_N[10000]; //!< Global vector: distribution of intraspecific values for leaf nitrogen
float d_intraspecific_LMA[10000]; //!< Global vector: distribution of intraspecific values for leaf mass per area (LMA)
float d_intraspecific_wsg[10000]; //!< Global vector: distribution of intraspecific values for wood density (wsg)
float d_intraspecific_dbhmax[10000];//!< Global vector: distribution of intraspecific values for maximal trunk diameter

#ifdef LCP_alternative
vector<float> LookUpLAImax; // new v.3.1.5: array to save the LAImax per species and intraspecific deviation
#endif

// THREE DIMENSIONAL LAI FIELD
float **LAI3D(0);   //!< Global 3D field: leaf density (per volume unit)
// TREEFALL IMPACT ON TREE HEALTH
unsigned short *Thurt[3];  //!<  Global vector:Treefall field

#ifdef WATER
int nblayers_soil; //!< Global variable: number of soil layers (for water module)
float *layer_depth(0); //!< Global vector: depth of each layer (m) !!!UPDATE

// soil parameters (Sat_SWC, Res_SWC) are computed from soil texture data (%clay, %silt, %sand) provided in input. If additional information is available from the field (soil pH, organic content, dry bulk density, cation exchange capacity), this should be also provided in input and used to refine the computation of these soil parameters (see Table 2 in Marthews et al. 2014 Geoscientific Model Development and Hodnett & Tomasella 2002 Geoderma -- for tropical soils, and comments in the code). Alternatively, if no local field soil data is available, these soil parameters (Sat_SWC, Res_SWC) should be drawn from global maps and databases --see Marthews et al. 2014, and directly provided in input. ==> ccl: to standardize the input file, the soil parameters (Sat_SWC, Res_SWC) should probably be provided in input, and the computation of those properties from the available local data made using a new function of RconTROLL, if unearthed.
// since soil layers (silt, clay, sand) are only needed locally, they are now coded as vectors
//float *proportion_Silt(0);             //!soil layer silt fraction
//float *proportion_Clay(0);             //!soil layer clay fraction
//float *proportion_Sand(0);             //!soil layer sand fraction
float *Sat_SWC(0);          //!< Global vector: soil layer saturated water content, in m3/m3 -- this is often assumed similar to porosity, even though it is usually 5-10% lower than total porosity due to entrapped or dissolved air -- see comment Table 1 in Marthews et al. 2014
float *Max_SWC(0);          //!< Global vector: soil layer maximum absolute water content (m^3)
float *FC_SWC(0);          //!< Global vector: soil layer maximum absolute water content (m^3)
float *Res_SWC(0);          //!< Global vector: soil layer residual water content (m^3/m^3)
float *Min_SWC(0);          //!< Global vector: soil layer minimum absolute water content (m^3)
float *Ksat(0);             //!< Global vector: soil layer saturated conductivity (m^3)
float *a_vgm(0);            //!< Global vector: parameter for the van Genuchten_mualem soil water retention curves
float *b_vgm(0);            //!< Global vector: parameter for the van Genuchten_mualem soil water retention curves
float *c_vgm(0);            //!< Global vector: parameter for the van Genuchten_mualem soil water retention curves
float *m_vgm(0);            //!< Global vector: parameter for the van Genuchten_mualem soil water retention curves
float *phi_e(0);            //!< Global vector: parameter for the Campbell-Mualem soil water retention curves (possible update: replace with a Genuchten parameter)
float *b(0);                //!< Global vector: parameter for the Campbell-Mualem soil water retention curves (possible update: replace with a Genuchten parameter)
float **SWC3D(0);           //!< Global 3D field: soil water content in each soil voxel (layer * DCELL)
float **soil_phi3D(0);      //!< Global 3D field: soil water potential (in MPa) in each soil voxel (layer * DCELL)
float **Ks(0);              //!< Global 3D field: soil hydraulic conductivity in each soil voxel (layer * DCELL)
float **KsPhi(0);           //!< Global vector: soil hydraulic conductivity * soil water potential for each soil voxel (layer * DCELL), useful to ease computation
float **LAI_DCELL(0);        //!< Global vector: total leaf area index (LAI), averaged per DCELL
float *LAI_young(0);        //!< Global vector: total young leaf area index (LAI), averaged across all sites
float *LAI_mature(0);        //!< Global vector: total mature leaf area index (LAI), averaged across all sites
float *LAI_old(0);        //!< Global vector: total old leaf area index (LAI), averaged across all sites
float *Canopy_height_DCELL(0); //!< Global vector: mean top canopy height, averaged per DCELL
int *HSum_DCELL(0);       //!< Global vector: number of sites covered by vegetation per DCELL, used to compute mean top canopy height per DCELL
float *TopWindSpeed_DCELL(0); //!< Global vector: wind speed at the top of the canopy, per DCELL
float *Interception(0);     //!< Global vector: water interception by the canopy, per DCELL !!!UNITS
float *Throughfall(0);      //!< Global vector: throughfall, per DCELL !!!UNITS
float *Runoff(0);           //!< Global vector: water run-off, per DCELL !!!UNITS
float *Leakage(0);          //!< Global vector: water leakage, per DCELL !!!UNITS
float *Evaporation(0);      //!< Global vector: water evaporation (physical process), per DCELL
float **Transpiration(0);   //!< Global vector: water uptake by trees, in each soil voxel (layer * DCELL)
float transpiration_1016; //variable used only to compare with eddy-flux tower data.

float abund_phi_root; //!< summary statistic output: abundance-weighted phi-root
float abund10_phi_root; //!< summary statistic output: abundance-weighted phi-root, including only tree with dbh>10cm
float agb_phi_root; //!< summary statistic output: agb-weighted phi-root
#endif


//SEED DENSITIES
int **SPECIES_SEEDS (0);    //!< Global 3D field: seed density per site
double *p_seed(0);          //!< Global vector: probability to draw at a particular site (1/sites)
unsigned int *n_seed(0);    //!< Global vector: number of seeds distributed on each site
double *p_species(0);       //!< Global vector: relative frequency of species (not normalised, as it is normalised by the gsl multinomial function)
unsigned int *n_species(0); //!< Global vector: number of seeds assigned to each species

int    *SPECIES_GERM (0); //!< Global vector: !!!TO_DOCUMENT
float  *PROB_S (0); //!< Global vector: !!!TO_DOCUMENT _SEEDTRADEOFF

// point cloud output, v.3.1.6
float mean_beam_pc; // the mean number of shots per m2 for the point cloud sampling
float sd_beam_pc;   // the standard deviation of the shots per m2 for the point cloud sampling
float klaser_pc;    // the klight for the point cloud sampling, needs to be multiplied with the transmittance_laser to get effective klaser (i.e. 0.5 * 0.4 = 0.2)
float transmittance_laser;  // transmittance of the laser when hitting something (or probability of continuing)
int iter_pointcloud_generation; // iteration at which point cloud should be generated

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
#ifdef WATER
void GetCanopyEnvironment(int height, int site, float dens, float (&canopy_environment_cumulated)[6]); //!< Global function: calculates the canopy environment
#else
void GetCanopyEnvironment(int height, int site, float dens, float (&canopy_environment_cumulated)[4]); //!< Global function: calculates the canopy environment
#endif
void AddCrownVolumeLayer(int row_center, int col_center, float height, float CR, float CD, int crownvolume[70]); //!< Global function: calculates packing densities
void UpdateLAI3D(int height, int site, float dens, float &LA_cumulated); //!< Global function: update of LAI3D field, called by CalcLAI()
#ifdef CHM_SPIKEFREE
void UpdateCHMvector(int height, int site, float noinput, vector<int> &chm); //!< Global function: remove outliers in canopy height model (CHM); vector option
void UpdateCHM(int height, int site, float noinput, int *chm); //!< Global function: remove outliers in canopy height model (CHM)
#endif
void OutputCrownSliced(int height, int site, int row_slice, vector<float> &output_statistics); //!< Global function: write a slice of a crown to file
void KeepFloatAsIs(float input, float &output, float CD, float height, int layer_fromtop); //!< Global function: dummy function when no modification is needed
void KeepIntAsIs(int input, int &output, float CD, float height, int layer_fromtop); //!< Global function: dummy function when no modification is needed
void LAI2dens(float LAI, float &dens_layer, float CD, float height, int layer_fromtop); //!< Global function: a modifying function that converts LAI to the density of a specific layer, using the GetDensity functions
void LAI2dens_cumulated(float LAI, float &dens_layer, float CD, float height, int layer_fromtop);//!< Global function: a modifying function that converts LAI to percentage LAI in and above the current layer, using the GetDensity functions; can be used to directly allocate LAI without looping over LAI3D field; new in v.3.1
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

float CalcVcmaxm(float lma, float nmass, float pmass);     //!< Returns Vcmaxm, in micromol C g-1 s-1
float CalcRdark(float lma, float nmass, float pmass, float vcmax); //!< Returns Rdark, in micromol C m-2 s-1
#ifdef G0
float QUAD(float A, float B, float C, int ROOT);  //!< Returns solution of quadratic equation (larger root of ROOT=1, smaller root if ROOT=-1)
#endif

// GLOBAL FUNCTIONS
void ReadInputGeneral();  //!< Global function: read in global parameter sheet
void Initialise(void); //!< Global function: initialisation with bare ground conditions
void InitialiseOutputStreams(void); //!< Global function: initialisation of output streams
void ReadInputInventory(void); //!< Global function: updated in v.3.1: initialisation from inventories
void ReadInputPointcloud(void); //!< Global function: introduced in v.3.1.6: reads the parameter sheet for the point cloud simulation
void AllocMem(void); //!< Global function: Field dynamic memory allocation
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
void OutputCHM(fstream& output_CHM); //!< Global function: Outputs CHM
void OutputVisual();    //!< Global function: Output function for visualization purposes
void CloseOutputs();
void FreeMem(void);

void ExportPointcloud(float mean_beam, float sd_beam, float klaser, float transmittance_laser, fstream& output_pointcloud); //!<Global function: point cloud output, v.3.1.6; kept separately from other output functions, as we write to a dedicated external file format (.las)

// HELPER FUNCTIONS
int GetTimeofyear();    //!< Helper function, new in v.3.1: converts current iteration into time of year, also works backwards (negative iterations)
float CalcHeightBaseline(float &ah, float &hmax, float &dbh);   //!< Helper function: calculates mean predicted height from allometry
float CalcCDBaseline(float &height);    //!< Helper function, new in v.3.1: calculates mean predicted crown radius from allometry
float CalcCRBaseline(float &dbh);   //!< Helper function new in v.3.1: calculates mean predicted crown diameter from allometry
int CalcIntabsorb(float absorb_prev, float absorb_delta); //!< Helper function: returns index for LookUpTables of absorbed flux (considering leaves above and within voxel)
int CalcIntabsorb(float absorb_prev); //!< Helper function: returns index for LookUpTables of absorbed flux (only considering leaves above voxel)


// DATA READING FUNCTIONS
// update v.3.1: already defined here to enable calling from functions of class Tree()
template <typename N>
void SetParameter(string &parameter_name, string &parameter_value, N &parameter, N parameter_min, N parameter_max, N parameter_default, bool quiet);
void SetParameter(string &parameter_name, string &parameter_value, string &parameter, string parameter_default, bool quiet);


#ifdef WATER
//! Global structure: For GPPleaf FUNCTION
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
    
#ifdef LCP_alternative
#else
    float s_LCP;            //!< Light compensation point  (micromol photon/m^2/s)
#endif
    float s_ds;             //!< Mean seed dispersal distance (m)
    float s_dbhmax;         //!< Maximal dbh (m) !! data usually report dbh in cm
    float s_hmax;           //!< Maximal height (m)
    float s_LMA;            //!< Leaf mass per area (g/m^2)
    float s_Nmass;          //!< Leaf nitrogen concentration (g/g) v.2.01
    float s_Pmass;          //!< Leaf phosphorous concentration (g/g) v.2.01
    float s_wsg;            //!< Wood specific gravity (g/cm^3)
    float s_ah;             //!< Parameter for allometric height-dbh equation
    float s_regionalfreq;   //!< Regional frequency; v.3.0 !!!UPDATE
    float s_leafarea;        //!< Drymass; v.3.0  !!!UPDATE
    float s_seedmass;       //!< Seed mass (g); See Baraloto & Forget 2007 dataset v.2.3; deprecated in v.2.2, but still necessary for SEEDTRADEOFF
    float s_iseedmass;      //!< Inverse of seed mass (1/g), v.2.3
    //float s_output_field[12];         // scalar output fields, deprecated since v.3.1, replaced by actual sumstats for readability/code accessibility
    float s_sum1, s_sum10, s_sum30, s_ba, s_ba10, s_agb, s_gpp, s_npp, s_rday, s_rnight, s_rstem, s_litterfall; // species level summary statistics, to be provided to output streams
    
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
    
    
#ifdef LCP_alternative
#else
    //Computation of the light compensation point from dark respiration and the quantum yield phi
    //By definition, Rdark is in micromolC/m^2/s and it is used in the Species::NPP() routine
    float Vcmax = CalcVcmaxm(s_LMA, s_Nmass, s_Pmass)*s_LMA;
    s_LCP = (Rtot_by_Rabove * Rtotleaf_by_Rdark * CalcRdark(s_LMA, s_Nmass, s_Pmass, Vcmax))/phi;
#endif
    
    s_sum1 = s_sum10 = s_sum30 = s_ba = s_ba10 = s_agb = s_gpp = s_npp = s_rday = s_rnight = s_rstem = s_litterfall = 0.0; // new in v.3.1
    
    
#ifdef WATER
    //s_g1=-3.97*s_wsg+6.53;                      // from Lin et al. 2015 Nature Climate Change
    //s_dWSF=1.0/(-0.00395145-0.3626778*s_tlp);         // this is the denominator of WSF= phi_sc - s_tlp, where phi_sc (for 'stomatal closure') corresponds to the onset of water stress, derived from the relationship between leaf phi at 50%loss of stomatal closure (ie. WSF~0.5 considering only stomatal responses to water stress) and turgor loss point (drawn from Bartlett et al. 2016 PNAS), and given the shape of WSF factor here assumed
    s_phi_lethal= -0.9842 + 3.1795*s_tlp;       // Inferred from data provided in Bartlett et al. 2016 PNAS
    s_itlp=1/s_tlp;
    s_b=-2.23*s_itlp; // this results from the following assumption: (i) phi(TLP)=0.97*phi(gs90), from Martin-StPaul et al. 2017 Ecology letters; (ii) WSF=0.1 at phi(gs90)
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
    float t_height;      //!< Total tree height (m) beware: this scales with NV, the vertical size of voxels, renamed v.3.1 for convenience
    float t_CD;          //!< crown depth (m) beware: this scales with NV, the vertical size of voxels, renamed v.3.1 for convenience
    float t_CR;          //!< crown radius (m) beware: this scales with NH, the horizontal size of voxels, renamed v.3.1 for convenience
    float t_Ct;          //!< flexural force threshold, _BASICTREEFALL
    float t_GPP;         //!< Gross primary productivity of the tree (gC/timestep)
    float t_NPP;         //!< Net primary productivity of the tree (gC/timestep)
    float t_Rday;        //!< Daytime leaf respiration of the tree (gC/timestep)
    float t_Rnight;      //!< Nighttime leaf respiration of the tree (gC/timestep)
    float t_Rstem;       //!< Stem respiration (gC/timestep)
    float t_LA;          //!< Total crown leaf area (m^2); v.2.2, renamed in v.3.1 for convenience
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
    vector<float> t_root_biomass;   //!< Tree root biomass in each soil layer, in g
    vector<float> t_soil_layer_weight;  //!< Soil layer weight (to compute t_phi_root), if different from root biomass
    float t_WSF;            //!< Tree water stress factor for stomatal conductance, unitless, between 0 and 1
    float t_WSF_A;          //!< Tree water stress factor for photosynthetic capacities, unitless, between 0 and 1
    float t_transpiration;  //!< Amount of water taken up from the soil and transpired at each timestep !!!UPDATE: which unit?
    float t_g1_0;
    float t_g1;
    
#ifdef PHENO_DROUGHT
    int t_Ndays_dry;
    int t_Ndays_wet;
    float t_pheno_factor;
#endif
    
#endif
    
    // new in v.2.4.0: traits defined at  individual level
    float t_Pmass;        //!< Leaf phosphorus content, defined at tree scale (g/g)
    float t_Nmass;        //!< Leaf nitrogen content, defined at tree scale (g/g))
    float t_LMA;          //!< Leaf mass per area (LMA), defined at tree scale (g/m^2)
#ifdef WATER
    float t_wleaf;        //!< Leaf width (in m)
#endif
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
    float t_lambda_old;    //!< Residence time in the old leaf class, inverse of old leaf lifespan; v.2.5 !!!UPDATE: which unit?
    
    float t_mult_height;  //!< Intraspecific multiplier for height (due to intraspp lognormal variation); v.2.4.0, renamed v.3.1 for convenience
    float t_mult_CR;      //!< Intraspecific multiplier for crown radius (due to intraspp lognormal variation); v.2.4.0, renamed v.3.1 for convenience
    float t_mult_CD;      //!< Intraspecific multiplier for crown depth (due to intraspp lognormal variation); v.2.4.0, renamed v.3.1 for convenience
    float t_mult_P;       //!< Intraspecific multiplier for leaf phosphorus concentration (due to intraspp lognormal variation); v.2.4.0, renamed v.3.1 for convenience
    float t_mult_N;       //!< Intraspecific multiplier for leaf nitrogen concentration (due to intraspp lognormal variation); v.2.4.0, renamed v.3.1 for convenience
    float t_mult_LMA;     //!< Intraspecific multiplier for leaf mass per area (due to intraspp lognormal variation); v.2.4.0, renamed v.3.1 for convenience
    float t_mult_dbhmax;  //!< Intraspecific multiplier for dbh maximum (due to intraspp lognormal variation); v.2.4.0, renamed v.3.1 for convenience
    float t_dev_wsg;      //!< Intraspecific absolution deviation for wood specific gravity (due to intraspp normal variation); v.2.4.0, renamed v.3.1 for convenience
    
    float t_LAImax;           //!< Maximal LAI; Dynamic adjustment of leaf allocation, based on light environment !!!UPDATE
    float t_LAmax;           //!< Maximal leaf area; Dynamic adjustment of leaf allocation, based on light environment  !!!UPDATE, renamed in v.3.1 for convenience
    float t_carbon_storage;   //!< Persistent C storage pool. If leaf area is optimal, surplus carbon is allocated to a storage pool
    float t_carbon_biometry;  //!< Temporary biometry C storage pool. In case a tree is suffering from carbon stress, it will tap this pool to renew its leaves and slow down growth
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
    float t_height_tracked;    //!< Stores previous height to compute height increment
    float t_CR_tracked;   //!< Stores previous CR (crown radius) to compute CR increment
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
        t_dbh = t_height = t_CR = t_CD= 0.0;
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
    int BirthFromInventory(int site, vector<string> &parameter_names, vector<string> &parameter_values, int &nb_speciesrandom);  //!< Tree initialisation from field data, completely updated in v.3.1
    void Death();                   //!< Tree death, called by Tree::Update
    void Growth();                  //!< Tree growth
#ifdef WATER
    void Fluxh(int h, float &PPFD, float &VPD, float &Tmp, float &leafarea_layer, float &PPFD_incident, float &ExtinctLW); //!< Computation of PPFD right above the tree -- called by Tree::Birth and Tree::Growth
#else
    void Fluxh(int h, float &PPFD, float &VPD, float &Tmp, float &leafarea_layer); //!< Computation of PPFD right above the tree -- called by Tree::Birth and Tree::Growth
#endif
    
#ifdef WATER
    void Water_availability();      //!< Computation of the tree water availability in the root zone
    //compute root depth, root biomass in each layer, soil water potential in the root zone, and water stress factor
    //see comments at Tree::Water_availability
    //void UpdateRootDistribution();  //compute root depth, root biomass in each layer, soil water potential in the root zone, and water stress factor
    void Water_uptake();            //!< Contribution of trees to the stand Transpiration field -- called by UpdateField
#endif
    
    void CalcRespGPP();             //!< GPP and respiration calculation, called by Tree::Growth
    void CalcNPP();                 //!< NPP calculation, called by Tree::Growth, new in v.3.1
    void UpdateLeafDynamics();      //!< Leaf dynamics and C allocation, called by Tree::Growth
    void UpdateTreeBiometry();      //!< Compute biometric relations, including allometry
    void UpdateVolumeDensity();     //!< Follows updates to leaf dynamics and biometry and computes the new leaf density; v.2.4.1
    //!< - For plastic and shy crowns, moved inside allocation step, otherwise computed straight after other update functions
    
    void DisperseSeed();            //!< Seed dispersal, called by UpdateField
    void Treefall(float angle);     //!< Tree falling function, called by TriggerTreefall
    void Update();                  //!< Tree death and growth
    void Average();                 //!< Local computation of the averages
    void CalcLAI();                 //!< Update of the LAI3D field
    //void CalcLAinitial();            //!< Initialise leaf area and related variables for trees that could not be initialized from data, new in v.3.1, not used yet
    void histdbh();                 //!< Computation of dbh histograms
    
    //! empirical functions for trait calculation and tree level variables (Calc functions return the specific parameter, Update functions update a specific variable at tree level)
    float CalcJmaxm();                          //!< Returns Jmax for for Farquhar model
    float CalcAGB();                            //!< Calculation of above ground biomass (in kg) !!!: if updated, also update CalcIncrementDBH, cf. below
    float CalcIncrementDBH(float delta_agb);    //!< Calculation of the increment of dbh from assimilated carbon/biomass (in m)
    float CalcCarbonStorageMax();               //!< Calculation of the maximum amount of carbon stored in a tree
    float CalcCt();                             //!< Calculation of the treefall threshold, if _BASICTREEFALL is activated
    
    void CalcLeafLifespan();                  //!< Determines leaf life span, either from empirical function or from Kikuzawa model
    void InitialiseLeafPools();                 //!< Initialises leaf pools for newly germinated trees, formerly part of CalcLeafLifespan(), new in v.3.1 to clear up code
    void UpdateSapwoodArea(float ddbh);         //!< Determine sapwood area, limited by increase in dbh (ddbh) (in m2)
    void UpdateHeight();                        //!< Updates t_height, based on t_dbh
    void UpdateCR();                            //!< Updates t_CR, based on t_dbh
    void UpdateCD();                            //!< Updates t_CD based on t_height
    
    
    // GPP functions are now calculated at tree level
    float DeathRateNDD(float, float, float); //!< Death rate function, including negative density dependence mortality, called when option _NDD is on.
#ifdef WATER
    float DeathRate(float, float, float);      //!< Death rate function, including drought-induced mortality
    leafFluxes FluxesLeaf(float, float, float, float, float, float, float, float, float); //!< Determines the leaf-level temperature, CO2 concentration, and vapour pressure deficit using and iterative scheme, and provides the corresponding leaf-level photosyntehtic assimilation rate (call to Photosyn) and transpiration rate (using penman-Monteith equation) -- depends on light (PPFD), vapor pressure deficit (VPD), temperature (T), and wind (W) //IMmarch 2022: try to account for extinction of net thermal radiation in Rnet iso and of actually absorbed NIR radiation, hence the last two arguments
    leafFluxes dailyFluxesLeaf(float, float, float, float, float, float, float&); //!< Computation of average C assimilation rate and water evapotranspiration per leaf area across daily variation in light (PPFD), vapor pressure deficit (VPD), temperature (T), and wind (W); //IMmarch 2022: try to account for extinction of net thermal raditiion in Rnet iso, and of actuel NIR absorbed energy, hence the last two arguments
    leafFluxes Photosyn(float, float, float, float); //!< Implements the Farquhar von Caemmerer Berry model of photosynthesis together with the model of stomatal conductance provided by Medlyn et al. (2011).
    void OutputTreeStandard(fstream& output); //!< Standard outputs during the simulation -- written to file
    void OutputTreeStandard(); //!< Standard outputs during the simulation -- written to screen in real time
    // !!!UPDATE: question, why are these two functions needed? the first function with Rcout as an argument is the same as the second function, no? question2: why are these functions only available in WATER mode?
#else
    float DeathRate(float, float);  //!< Death rate function
    float GPPleaf(float, float, float);    //!< Farquhar von Caemmerer Berry model -- computation of the light-limited leaf-level average C assimilation rate per m^2 (micromol/m^2/s) -- depends on daily variation in light (PPFD), vapor pressure deficit (VPD) and temperature (T)
    float dailyGPPleaf(float, float, float);    //!< Computation of average C assimilation rate per leaf area across daily variation in light (PPFD), vapor pressure deficit (VPD) and temperature (T)
    float dailyGPPcrown(float, float, float, float);  //!< Farquhar von Caemmerer Berry model -- proposition of reinsertion of fastdailyGPPleaf() as dailyGPPcrown(), main reason: despite sharing some code with dailyGPPleaf, structurally very different, updated in v.2.5, replaced dens * CD by LAI
#endif
    float Rdayleaf(float T); //!< !!!UPDATE
    float dailyRdayleaf(float T); //!< !!!UPDATE
    
    void CalcLAImax();             //!< Determines the maximum LAI that the tree should reach, given the Farquhar model, at a theoretical average day, up to which point leaves can be allocated and until which the costs (self-shading) are lower than the benefits (additional assimilation), renamed in v.3.1
    void CalcLAmax(float &LAIexperienced_eff, float &LAmax);  //!<  Unlike CalcLAImax, calculates the maximum leafarea for the current light environment the tree experiences rather than for a theoretical day, renamed in v.3.1
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
   
    int dev_rand = int(gsl_rng_uniform_int(gslrand,10000));    // modified FF v.3.1.5 (reduced to 10000)

    
#ifdef LCP_alternative
    
    //######################
    //# first test LAImax ##
    //######################
    int index_LAImax = dev_rand + (nume - 1) * 10000;
    float LAImax_precomputed = LookUpLAImax[index_LAImax];
    
    if(LAI3D[0][site0+SBORD] < LAImax_precomputed){
        
#endif
        
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
        
        t_mult_height = d_intraspecific_height[dev_rand];
        t_mult_CR = d_intraspecific_CR[dev_rand];
        t_mult_N = d_intraspecific_N[dev_rand];
        t_mult_P = d_intraspecific_P[dev_rand];
        t_mult_LMA = d_intraspecific_LMA[dev_rand];
        t_mult_CD = d_intraspecific_CD[dev_rand];
        t_dev_wsg = d_intraspecific_wsg[dev_rand];
        t_mult_dbhmax = d_intraspecific_dbhmax[dev_rand];
        
        //#####################
        //# leaf/wood traits ##
        //#####################
        t_Pmass = S[t_sp_lab].s_Pmass * t_mult_P;
        t_Nmass = S[t_sp_lab].s_Nmass * t_mult_N;
        t_LMA = S[t_sp_lab].s_LMA * t_mult_LMA;
        t_wsg = fmaxf(S[t_sp_lab].s_wsg + t_dev_wsg, 0.05); // updated in v.3.1: Lower cutoff for normal distribution set to 0.05 instead of 0.1 (some wsg measurements in Global Wood Density Database go below 0.1)
#ifdef WATER
        t_wleaf=sqrt(S[t_sp_lab].s_leafarea*0.0001);
#endif
        
        t_Vcmax =  CalcVcmaxm(t_LMA, t_Nmass, t_Pmass) * t_LMA; // in micromolC m-2 s-1
        t_Jmax = CalcJmaxm() * t_LMA; // in micromolC m-2 s-1
        t_Rdark = CalcRdark(t_LMA, t_Nmass, t_Pmass, t_Vcmax); // in micromolC m-2 s-1
#ifdef WATER
        //t_g1_0=-0.0224 * t_LMA + 4.8278; // this is the relationship provided by Wu et al. 2019 GCB
        t_g1_0= (-3.97 * t_wsg + 6.53); //this is the relationship provided by Lin et al. 2015
        t_g1=t_g1_0;
        if(t_g1<0) {
            Rcout << "Warning: t_g1 <0 in Tree::Birth ! t_LMA=" << t_LMA << "; s_LMA=" <<  S[t_sp_lab].s_LMA << "; multiplierLMA=" << t_mult_LMA << endl;
        }
#endif
        
        //#################$####
        //# biometry/allometry ##
        //##################$###
        
        t_hmax = S[t_sp_lab].s_hmax;
        t_ah = S[t_sp_lab].s_ah;
        
        t_dbh = DBH0;
        t_dbhmax = S[t_sp_lab].s_dbhmax;
        t_dbhmax *= t_mult_dbhmax;
        t_dbhmax = fmaxf(t_dbhmax,DBH0 * 2.0);
        t_dbhmature = t_dbhmax*0.5; //Mean threshold of tree size to maturity - see Visser et al. 2016 Functional Ecology (suited to both understory and top-canopy species). NOTE that if we decide to keep it as a fixed species-specific value, this could be defined as a Species calss variable, and computed once in Species::Init. -- v230
        
        UpdateHeight();
        UpdateCR();
        UpdateCD();
        
        t_CrownDisplacement = 0;
        
        if(_BASICTREEFALL) {
            t_Ct = CalcCt();
        }
        
        //##################
        //# leaf dynamics ##
        //##################
        
#ifdef WATER
        t_WSF = t_WSF_A = 1.0; // Tree leaf lifespan and LAImax are computed under no water stress and with average climatic conditions (radiation, temperature).
        //This could/should be updated later, so that these two quantities change under water stress, and also seasonally.
        //t_WSF and t_WSF_A are then updated later in Tree::Birth to account for the real water conditions at birth.
#endif
        float crown_area = PI * t_CR * t_CR;
        float fraction_filled_general = 1.0 - crown_gap_fraction;
        t_fraction_filled = fminf(fraction_filled_general/(t_mult_CR * t_mult_CR),1.0);
        float crown_area_nogaps = GetCrownAreaFilled(crown_area);
        
        if(_LA_regulation > 0){
            
            //In this case, we determine both maximum LAI (theoretical if fully exposed to sunlight) and maximum leaf area (actual, i.e. also considering shading by neighboring trees)
            // v.3.1.5, we can simply set t_LAImax to the precomputed value
            //CalcLAImax();
            //Rcout << "LAImax: " << t_LAImax << " precomputed: " << LAImax_precomputed << endl; // check consistency
#ifdef LCP_alternative
            t_LAImax = LAImax_precomputed;
#else
            CalcLAImax();
#endif
            float LAIexperienced_eff;
            CalcLAmax(LAIexperienced_eff, t_LAmax);
            //t_LA = 0.5 * t_LAmax;    //tets post_fusion 27/01/2023 IM
            t_LA = 0.25 * t_LAmax;    //Initially trees are set to have a quarter of their maximum leaf area

            t_LAI = t_LA/crown_area_nogaps;
            t_carbon_storage = CalcCarbonStorageMax() * 0.5; //Initial value with half of the maximum storage
            t_carbon_biometry = 0.0;
        } else {
#ifdef CROWN_UMBRELLA
            t_LAI = dens * fminf(t_CD, 3.0);
#else
            t_LAI = dens * t_CD;
#endif
            t_LA = t_LAI * crown_area_nogaps;
            
        }
        CalcLeafLifespan();
        InitialiseLeafPools(); // new in v.3.1, formerly part of CalcLeafLifespan()
        
#ifdef PHENO_DROUGHT
        t_Ndays_dry=0;
        t_Ndays_wet=0;
        t_pheno_factor=1.0;
#endif
        
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
        
#ifdef LCP_alternative
        SPECIES_SEEDS[site0][nume]=0; // newIM nov2021, to adjust to the yearly update of Species_seeds
    }
#endif
    
}

//#############################################
// Tree initialisation from field data
//##############################################
//! - Modelled for compatibility with Tree::Birth. For comments cf. Tree::Birth
//! - For comments regarding allometries and t_LA cf. Tree::Growth.
string GetParameter(string &parameter_name, vector<string> &parameter_names, vector<string> &parameter_values){
    unsigned int parameter_index = 0;
    while(parameter_index < parameter_names.size() && parameter_names[parameter_index] != parameter_name) {
        parameter_index++;
    }
    
   /* if (parameter_name=="s_name") {
        for (int i=0; i<=parameter_names.size(); i=i+1) Rcout << parameter_names[i] << "=" << parameter_values[i] << "\t";
        Rcout << endl;
        Rcout << parameter_names[parameter_index] << " ="  << parameter_values[parameter_index] << endl;
        Rcout << parameter_names.size() << "\t"  << parameter_values.size() << endl;

    }*/
    string parameter_value;
    if(parameter_index < parameter_names.size()) parameter_value = parameter_values[parameter_index];
    else {
        parameter_value = ""; // should not happen, potentially throw warning
        Rcout << "Problem finding parameter " << parameter_name << " please check input string in parameter definitions." << endl;
    }
    return(parameter_value);
}

template <typename N>
bool AreEqual(N value1, N value2, int precision){
    return abs(value1 - value2) < pow(10,-precision); // make sure that namespace std is activated, otherwise abs is only defined for integers
}

template <typename N>
void CompareParameters(N &parameter_value, N parameter_min, N parameter_max, N parameter_default, string &parameter_name, vector<string> &parameter_names, vector<string> &parameter_values, bool quiet){
    // function could be used to systematically compare empirically provided values and computed values
    // since this could result in a lot of computation and output during initialisation, we rather print out a Snapshot of the initial configuration, to be compared to input
    N parameter_value_emp;
    string parameter_value_emp_string = GetParameter(parameter_name, parameter_names, parameter_values);
    SetParameter(parameter_name, parameter_value_emp_string, parameter_value_emp, parameter_min, parameter_max, parameter_default, quiet);
    if(parameter_value_emp != parameter_default){
        int precision = 4;
        bool equal = AreEqual(parameter_value, parameter_value_emp, precision);
        if(!equal) Rcout << "Supplied parameter " << parameter_name << " does not correspond to calculated value at precision " << precision << endl;
    }
}


int Tree::BirthFromInventory(int site, vector<string> &parameter_names, vector<string> &parameter_values, int &nb_speciesrandom){
    int success = 0;
    bool quiet = 1;
    int dev_rand = int(gsl_rng_uniform_int(gslrand,10000)); // in case traits have to be drawn at random, modified FF v.3.1.5
    
    //*############################################*/
    //*## First diameter (minimum data condition ##*/
    //*############################################*/
    
    string parameter_name = "dbh";
    string parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
    SetParameter(parameter_name, parameter_value, t_dbh, 0.001f, 15.0f, 0.0f, quiet);
    
    if(t_dbh > 0.0){
        success = 1;
        
        //*####################*/
        //*## General traits ##*/
        //*####################*/
        
        nblivetrees++;
        // assign site, by default initialized to site of tree
        t_site = site;
        
        // get fromData indicator, 1 by default
        t_from_Data = 1;
        
        // get species label first
        parameter_name = "s_name";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        int species_exists = 0;
        // species run from 1 to nbspp!
        for(int s = 1; s <= nbspp; s++){
            if(parameter_value == S[s].s_name){
                t_sp_lab = s;
                species_exists = 1;
            }
        }
        if(species_exists == 0){
            t_sp_lab = int(gsl_rng_uniform_int(gslrand,nbspp)) + 1;
            nb_speciesrandom++;
            //Rcout << "Species: " << parameter_value << " not found. Initializing as random species: " << S[t_sp_lab].s_name << endl;
        }
        
        // update species counter
        S[t_sp_lab].s_nbind++;
        
        // CrownDisplacement
        parameter_name = "CrownDisplacement";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_CrownDisplacement, 0, 25, 0, quiet); // maximum displacement of 25m is unrealistic, but upper end where even largest possible crowns would still overlap with stem, defaults to 0
        
        //*#############################*/
        //*## Basic functional traits ##*/
        //*#############################*/
        
        // !!!: in future versions, this could be condensed by creating a template
        parameter_name = "Pmass";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_Pmass, 0.0f, 0.01f, 0.0f, quiet); // maximum value of 0.01 based on typical values in the range of 0.0001-0.001
        
        if(t_Pmass == 0.0){
            // draw random trait
            t_mult_P = d_intraspecific_P[dev_rand];
            t_Pmass = S[t_sp_lab].s_Pmass * t_mult_P;
        } else {
            // infer multiplier trait and check against supplied values if available
            t_mult_P = t_Pmass/S[t_sp_lab].s_Pmass;
            // example for how parameters could be checked
            //        string parameter_name_emp = "mult_P";
            //        CompareParameters(t_mult_P, 0.0f, 10.0f, 0.0f, parameter_name_emp, parameter_names, parameter_values, quiet);
        }
        
        parameter_name = "Nmass";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_Nmass, 0.0f, 0.1f, 0.0f, quiet); // maximum value of 0.1 based on typical values in the range of 0.01-0.05
        
        if(t_Nmass == 0.0){
            // draw random trait
            t_mult_N = d_intraspecific_N[dev_rand];
            t_Nmass = S[t_sp_lab].s_Nmass * t_mult_N;
        } else {
            // infer multiplier trait and check against supplied values if available
            t_mult_N = t_Nmass/S[t_sp_lab].s_Nmass;
        }
        
        parameter_name = "LMA";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_LMA, 0.0f, 1000.0f, 0.0f, quiet); // maximum value of 1000 is based on assumption that some trees may reach LMA as high as 500 (gymnosperms, some palm trees, etc.)
        
        if(t_LMA == 0.0){
            // draw random trait
            t_mult_LMA = d_intraspecific_LMA[dev_rand];
            t_LMA = S[t_sp_lab].s_LMA * t_mult_LMA;
        } else {
            // infer multiplier trait and check against supplied values if available
            t_mult_LMA = t_LMA/S[t_sp_lab].s_LMA;
        }
        
        parameter_name = "wsg";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_wsg, 0.0f, 1.5f, 0.0f, quiet);  // hard upper limit of 1.5 based on density of woody substance
        
        if(t_wsg == 0.0){
            // draw random trait
            t_dev_wsg = d_intraspecific_wsg[dev_rand];
            t_wsg = fmaxf(S[t_sp_lab].s_wsg + t_dev_wsg,0.05);
        } else {
            // infer deviation trait and check against supplied values if available
            t_dev_wsg = t_wsg - S[t_sp_lab].s_wsg;
        }
        
        parameter_name = "dbhmax";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_dbhmax, 0.0f, 15.0f, 0.0f, quiet);  // maximum value of 15m (Baobabs, Sequoias and Taxodium mucronatum can reach somewhere around 10m)
        
        if(t_dbhmax == 0.0){
            t_dbhmax = S[t_sp_lab].s_dbhmax;
            t_mult_dbhmax = d_intraspecific_dbhmax[dev_rand];
            t_dbhmax *= t_mult_dbhmax;
            if(t_dbhmax < t_dbh * 1.5){
                t_dbhmax = t_dbh * 1.5;
                t_mult_dbhmax = t_dbhmax/S[t_sp_lab].s_dbhmax;
            }
        } else {
            // infer multiplier trait and check against supplied values if available
            t_mult_dbhmax = t_dbhmax/S[t_sp_lab].s_dbhmax;
        }
        
        t_dbhmature = t_dbhmax * 0.5; // this correponds to the mean thresholds of tree size to maturity, according to Visser et al. 2016 Functional Ecology (suited to both understory short-statured species, and top canopy large-statured species). NOTE that if we decide to keep it as a fixed species-specific value, this could be defined as a Species calss variable, and computed once in Species::Init. -- v230
        
        //*##############################*/
        //*## Calculate derived traits ##*/
        //*##############################*/
        // LAImax and fraction_filled, although constant throughout tree's life will be calculated further down
        
        t_Vcmax =  CalcVcmaxm(t_LMA, t_Nmass, t_Pmass) * t_LMA;
        t_Jmax = CalcJmaxm() * t_LMA;
        t_Rdark = CalcRdark(t_LMA, t_Nmass, t_Pmass, t_Vcmax);
#ifdef WATER
        t_wleaf=sqrt(S[t_sp_lab].s_leafarea*0.0001);
#endif
        
        // height allometry
        t_hmax = S[t_sp_lab].s_hmax;
        t_ah = S[t_sp_lab].s_ah;
        
        // leaf related traits
        
        //if(_LA_regulation > 0) CalcLAImax();
        
        parameter_name = "LAImax";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_LAImax, 0.0f, 10.0f, 2.0f, quiet);
        
        parameter_name = "leaflifespan";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_leaflifespan, 3.0f,1000.0f, 0.0f, quiet);
        
        parameter_name = "lambda_young";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_lambda_young, 0.0f,1.0f, 0.0f, quiet);
        
        parameter_name = "lambda_mature";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_lambda_mature, 0.0f,1.0f, 0.0f, quiet);
        
        parameter_name = "lambda_old";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_lambda_old, 0.0f,1.0f, 0.0f, quiet);
        
        if((t_leaflifespan == 0.0) | (t_lambda_young == 0.0) | (t_lambda_mature == 0.0) | (t_lambda_old == 0.0)) CalcLeafLifespan(); // if the Kikuzawa model is used, the leaflifespan will be modified by a random error term, which may considerably affect tree performance if the value is recomputed
        
        //*#######################################*/
        //*## Traits that vary during tree life ##*/
        //*#######################################*/
        // note that this rough division encodes approximations/choices in the TROLL model to not assume changes in certain functional traits during different ontogenetic stages
        
        parameter_name = "age";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_age, timestep, 10000.0f, 1.0f, quiet);
        
        parameter_name = "height";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_height, 0.0f, 150.0f, 0.0f, quiet);  // maximum value of 150m to leave a bit of room
        
        // !!!: assumption of random deviations from allometry, irrespective of neighborhood, is a problematic assumption for generic inventories
        if(t_height == 0.0){
            t_mult_height = d_intraspecific_height[dev_rand];
            UpdateHeight();
        } else {
            t_mult_height = t_height/CalcHeightBaseline(t_ah, t_hmax, t_dbh);
        }
        
        parameter_name = "CD";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_CD, 0.0f, t_height * 0.5f, 0.0f, quiet);
        
        // !!!: assumption of random deviations from allometry, irrespective of neighborhood, is a problematic assumption for generic inventories
        if(t_CD == 0.0){
            t_mult_CD = d_intraspecific_CD[dev_rand];
            UpdateCD();
        } else {
            t_mult_CD = t_CD/CalcCDBaseline(t_height);
        }
        
        parameter_name = "CR";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_CR, 0.0f, 25.0f, 0.0f, quiet);  // maximum value of 15m (Baobabs, Sequoias and Taxodium mucronatum can reach somewhere around 10m)
        
        // !!!: assumption of random deviations from allometry, irrespective of neighborhood, is a problematic assumption for generic inventories
        if(t_CR == 0.0){
            t_mult_CR = d_intraspecific_CR[dev_rand];
            UpdateCR();
        } else {
            t_mult_CR = t_CR/CalcCRBaseline(t_dbh);
        }
        
        //*##############################*/
        //*## Calculate derived traits ##*/
        //*##############################*/
        
        if(_BASICTREEFALL){
            parameter_name = "Ct";
            parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
            SetParameter(parameter_name, parameter_value, t_Ct, 0.0f,float(HEIGHT), 0.0f, quiet); // the default is a height fall threshold of maximum height (so sth the tree never reaches)
            if(t_Ct == 0.0) t_Ct = CalcCt();
        }
        
        float fraction_filled_general = 1.0 - crown_gap_fraction;
        t_fraction_filled = fminf(fraction_filled_general/(t_mult_CR * t_mult_CR),1.0);
        
        //*######################################*/
        //*## Further photosynthetic variables ##*/
        //*######################################*/
        parameter_name = "LAmax";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_LAmax, 0.0f, 100000.0f, -1.0f, quiet); // assuming LAI cannot exceed 10 m2/m2 and maximum crown area in TROLL is 25^2 * pi, then upper limit is around 20000, 100000 to be safe
        
        parameter_name = "LA";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_LA, 0.0f, 100000.0f, -1.0f, quiet); // assuming LAI cannot exceed 10 m2/m2 and maximum crown area in TROLL is 25^2 * pi, then upper limit is around 20000, 100000 to be safe - note that LA can be larger than LAmax when tree has been recently shaded and leaf turnover is slow
        
        parameter_name = "youngLA";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_youngLA, 0.0f, 100000.0f, -1.0f, quiet);
        
        parameter_name = "matureLA";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_matureLA, 0.0f, 100000.0f, -1.0f, quiet);
        
        parameter_name = "oldLA";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_oldLA, 0.0f, 100000.0f, -1.0f, quiet);
        
        parameter_name = "litter";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_litter, 0.0f, 100000.0f, -1.0f, quiet);
        
        float crown_area = PI*t_CR*t_CR;
        float crown_area_nogaps = GetCrownAreaFilled(crown_area);
        
        if(t_LAmax >= 0.0 && t_LA >= 0.0 && t_youngLA >= 0.0 && t_matureLA >= 0.0 && t_oldLA >= 0.0 && t_litter >= 0.0){
            // there could be more consistency checks here (e.g. t_youngLA + t_matureLA + t_oldLA should be approximately t_LA)
            t_LAI = t_LA/crown_area_nogaps;
        } else {
            if(_LA_regulation > 0){
                float LAIexperienced_eff;
                CalcLAmax(LAIexperienced_eff, t_LAmax);     // !!!: problematic calculation if only classic inventory data is provided (i.e. x/y/dbh/s_name): we calculate the maximum leaf area for an empty canopy, thus grossly overestimating it for trees in the understory --> first improvement would be implementing calculation of this variable from highest tree to smallest tree and successively allocating leaves. Cf. the currently not used CalcLAinitial(). Interestingly, when trying this, the effects were not very large, probably because they are overshadowed by the impact of random allometric deviations and crown overlap.
                t_LA = t_LAmax;   // assume half the maximum leaf area?
                t_LAI = t_LA/crown_area_nogaps;
            } else {
#ifdef CROWN_UMBRELLA
                t_LAI = dens * fminf(t_CD, 3.0);
#else
                t_LAI = dens * t_CD;
#endif
                t_LA = t_LAI * crown_area_nogaps;
            }
            InitialiseLeafPools();
        }
        
        parameter_name = "sapwood_area";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        float ba = t_dbh * t_dbh * 0.25 * PI;
        SetParameter(parameter_name, parameter_value, t_sapwood_area, 0.0f, ba, 0.0f, quiet);
        
        if(t_sapwood_area == 0.0){
            float ddbh = fminf(t_dbh, 0.04f);   // limit the approximate sapwood thickness to 5cm
            UpdateSapwoodArea(ddbh);
        }
        
        if(_LA_regulation > 0){
            float carbon_storage_max = CalcCarbonStorageMax();
            parameter_name = "carbon_storage";
            parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
            SetParameter(parameter_name, parameter_value, t_carbon_storage, 0.0f, carbon_storage_max, -1.0f, quiet);
            if(t_carbon_storage == -1.0) t_carbon_storage = carbon_storage_max * 0.5; // We here initialize trees with half their maximum storage
            
            parameter_name = "carbon_biometry";
            parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
            SetParameter(parameter_name, parameter_value, t_carbon_biometry, 0.0f, carbon_storage_max * 10.0f, 0.0f, quiet);
        }
        
        parameter_name = "hurt";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        unsigned short hurt_min = 0, hurt_max = USHRT_MAX, hurt_default = 0;
        SetParameter(parameter_name, parameter_value, t_hurt, hurt_min, hurt_max, hurt_default, quiet);
        
        parameter_name = "NPPneg";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_NPPneg, 0, INT_MAX, 0, quiet);
        
        parameter_name = "multiplier_seed";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_multiplier_seed, 1, INT_MAX, 1, quiet);
        
        //*##############################*/
        //*## "Convenience" parameters ##*/
        //*##############################*/
        // these parameters can be set to 0, as they are recomputed each timestep in the actual light environment
        // will be computed for initial configuration after allocation of leaves to voxel field
        
        // t_GPP = t_NPP = t_Rday = t_Rnight = t_Rstem = 0.0;

        parameter_name = "GPP";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_GPP, 0.0f, 1000.0f, 0.0f, quiet);

        parameter_name = "NPP";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_NPP, 0.0f, 1000.0f, 0.0f, quiet);

        parameter_name = "Rday";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_Rday, 0.0f, 1000.0f, 0.0f, quiet);

        parameter_name = "Rnight";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_Rnight, 0.0f, 1000.0f, 0.0f, quiet);

        parameter_name = "Rstem";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_Rstem, 0.0f, 1000.0f, 0.0f, quiet);
        
#ifdef WATER
        //*###########*/
        //*## water ##*/
        //*###########*/
        
        // t_transpiration = 0.0;

        parameter_name = "transpiration";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_transpiration, 0.0f, 0.1f, 0.0f, quiet);

        parameter_name = "root_depth";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_root_depth, 0.0f, 100.0f, 2.0f, quiet);
        
        parameter_name = "phi_root";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_phi_root, -15.0f, 0.0f, 0.0f, quiet);
        
        for(int l=0;l<nblayers_soil;l++){
            char par_name[30];
            sprintf(par_name, "root_biomass%d", l);
            string par_name_s = par_name;
            parameter_name = par_name_s;
            parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
            SetParameter(parameter_name, parameter_value, t_root_biomass[l], 0.0f, 10000.0f, 1.0f, quiet);
        }
        
        
        for(int l=0;l<nblayers_soil;l++){
            char par_name[30];
            sprintf(par_name, "soil_layer_weight%d", l); //
            string par_name_s = par_name;
            parameter_name = par_name_s;
            parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
            SetParameter(parameter_name, parameter_value, t_soil_layer_weight[l], 0.0f, 1.0f, 1.0f, quiet);
        }
        
        
        parameter_name = "g1_0"; // not necessarily needed currently, since computable from other traits, but this can be changed given the low confindence in these relationships (and should at least be recomputed here anyway)
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_g1_0, 0.0f, 1.0f, 1.0f, quiet);
        
        parameter_name = "WSF";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_WSF, 0.0f, 1.0f, 1.0f, quiet);
        
        t_g1=t_g1_0*t_WSF;
        
        parameter_name = "WSF_A";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_WSF_A, 0.0f, 1.0f, 1.0f, quiet);
        
        parameter_name = "Ndays_dry";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_Ndays_dry, 0, 1000, 0, quiet);
        
        parameter_name = "Ndays_wet";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_Ndays_wet, 0, 1000, 1, quiet);
        
        parameter_name = "pheno_factor";
        parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        SetParameter(parameter_name, parameter_value, t_pheno_factor, 0.0f, 1.0f, 1.0f, quiet);
        
#endif
        //*################*/
        //*## for output ##*/
        //*################*/
#ifdef Output_ABC
        //parameter_name = "dbh_previous";
        //parameter_value = GetParameter(parameter_name, parameter_names, parameter_values);
        //SetParameter(parameter_name, parameter_value, t_dbh_previous, 0.0f, t_dbh, t_dbh, quiet);
        t_dbh_previous = t_dbh; // check whether proper initialisation is needed
#endif
#ifdef TRACK_INDIVIDUALS
        StartTracking();
#endif
    }
    return(success);
}

// v.3.1: new function to initialize trees more realistically from inventories, not used yet
//void Tree::CalcLAinitial(){
//    if(t_LA < 0.0){
//        // 1. determine leaf area and related variables, based on current environment
//        float crown_area = PI*t_CR*t_CR;
//        float crown_area_nogaps = GetCrownAreaFilled(crown_area);
//
//        if(_LA_regulation > 0){
//            float LAIexperienced_eff;
//            CalcLAmax(LAIexperienced_eff, t_LAmax);
//            t_LA = t_LAmax;   // assume half the maximum leaf area?
//            t_LAI = t_LA/crown_area_nogaps;
//        } else {
//#ifdef CROWN_UMBRELLA
//            t_LAI = dens * fminf(t_CD, 3.0);
//#else
//            t_LAI = dens * t_CD;
//#endif
//            t_LA = t_LAI * crown_area_nogaps;
//        }
//        InitialiseLeafPools();
//
//        if(t_sapwood_area == 0.0){
//            float ddbh = fminf(t_dbh, 0.04f);   // initial sapwood thickness of ca. 2.5cm
//            UpdateSapwoodArea(ddbh);
//        }
//
//        // 2. allocate the leaves to the LAI3D field
//        // nota bene: we here use the same function as CalcLAI3D, with one exception: LAI2dens_cumulated instead of LAI2dens; this means that we allocate the cumulated LAI in each layer and do not require any summation afterwards
//        int site_crowncenter = t_site + t_CrownDisplacement;
//        int row_crowncenter = site_crowncenter/cols;
//        int col_crowncenter = site_crowncenter%cols;
//
//        float LA_cumulated = 0.0;   //Currently, an output variable is required by LoopLayerUpdateCrownStatistic_template, we here use LA_cumulated as control variable
//
//        int crown_top = int(t_height);
//        int crown_base = int(t_height - t_CD);
//        int max_shells = min(crown_top - crown_base + 1, 4);    //since the new crown shapes
//        float fraction_filled_target = t_fraction_filled;
//
//        for(int shell_fromtop = 0; shell_fromtop < max_shells; shell_fromtop++){
//            LoopLayerUpdateCrownStatistic_template(row_crowncenter, col_crowncenter, t_height, t_CR, t_CD, fraction_filled_target, shell_fromtop, GetRadiusSlope, t_LAI, LA_cumulated, LAI2dens_cumulated, UpdateLAI3D);
//        }
//    }
//}



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
        
        int crown_top = int(t_height);
        int crown_base = int(t_height - t_CD);
        int max_shells = min(crown_top - crown_base + 1, 4);    //since the new crown shapes
        float fraction_filled_target = t_fraction_filled;
        
        for(int shell_fromtop = 0; shell_fromtop < max_shells; shell_fromtop++){
            LoopLayerUpdateCrownStatistic_template(row_crowncenter, col_crowncenter, t_height, t_CR, t_CD, fraction_filled_target, shell_fromtop, GetRadiusSlope, t_LAI, LA_cumulated, LAI2dens, UpdateLAI3D);
        }
    }
}
#else
void Tree::CalcLAI() {
    if(t_age > 0){
        int crown_base = int(t_height-t_CD),
        crown_top = int(t_height);
        int site_crowncenter = t_site + t_CrownDisplacement;
        int row_crowncenter = site_crowncenter/cols;
        int col_crowncenter = site_crowncenter%cols;
        
        float fraction_abovetop = t_height - float(crown_top);
        float fraction_belowbase = float(crown_base + 1) - (t_height - t_CD);
        
        float crown_area = PI * t_CR * t_CR;     // floor of crown_area to bound area accumulation
        int crown_intarea = int(crown_area);   // floor of crown_area to bound area accumulation
        crown_intarea = max(crown_intarea,1);                                 // minimum area of crown (1)
        crown_intarea = min(crown_intarea,1963);                              // maximum area of crown (radius 25), int(3.14*25*25), int(3.14*25*25)
        
        float dens_avg = t_LAI/t_CD;
        float fraction_filled_target = t_fraction_filled;
        
        for(int h=crown_base;h<=crown_top;h++){
            float fraction_filled_actual = 0.0;
            float dens_layer;
            
            dens_layer = dens_avg;
            if(crown_top == crown_base) dens_layer *= t_CD;
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
    
    //if(t_LA > 0.0){       // TO BE CHECKED: what happens to a tree with leafarea=0 in the following timesteps ?
    //Tree root biomass
    float total_root_biomass=t_LA*t_LMA;
    //In the absence of a more explicit allocation scheme, we assume that the total root biomass=the total leaf biomass. This may be discussed and changed. Eg. in Schippers et al. 2015 Functional Plant Biology, they assume that root biomass is 1.4 times leaf biomass... More importantly, the tree-level carbon budget should be closed and balanced. Needs to be specified that this correponds to the total FINE root biomass. Note that similarly, in ED2 (Xu et al. 2016), a constant ratio of 1 is assumed between leaf biomass and fine root biomass (whereas a constant ratio of 0.25 is assumed between coarse root and aboveground stem biomass). But leaf and fine root phenology follow slightly different dynamics (see Xu et al. 2016 SI) so unclear how this is consistent...?
    
    //Tree root depth
    //t_root_depth=fminf(layer_depth[(nblayers_soil-1)],pow((0.001*total_root_biomass),0.8)*0.2173913);       //this is the tree root depth, computed according to Arora & Boer 2003 Earth interactions (see equ. 11 therein). Instead of assuming a fixed root exponential profile, that typically results in a fixed root depth from recruitment to death, hence overestimating root vertical growth and depth for small saplings, they derived a root distribution profile that depends on root biomass. The simulated root depth and distribution have to be checked. 0.2173913=3/b with b=13.8
    //t_root_depth=fminf(layer_depth[(nblayers_soil-1)],pow((0.001*total_root_biomass),0.1)*0.7);
    //t_root_depth=fmaxf(2.0, fminf(layer_depth[(nblayers_soil-1)],pow((0.001*total_root_biomass),0.1)*0.7));
    //t_root_depth=fminf(layer_depth[(nblayers_soil-1)], 0.06410256*t_height+1.435897);  // this dependency of tree root depth with tree height is highly questionable -- cf. eg. Stahl et al. 2013 Oecologia, but see also Brum et al. 2019 Journal of Ecology-- but parcimonious. Note that neither Stahl et al. 2013 nor Brum et al. 2019 reported actual root depth, but the "depth of water uptake". Should probably depends more on the total soil depth, and average soil water availabilty -- cf. distribution of maximal root depth per biomes in Canadell/Jackson et al.
    //t_root_depth=0.22*pow(t_dbh*100, 0.54); // this is the allometry used in ED2, Xu et al. 2016, for evergreen trees. Parameter values were fixed based on Kenzo et al. 2009 Journal of Tropical Biology, which reported on data from excavated trees in wet secondary forests in Malaysia, although Xu et al. 2016 applied it in a seasonally dry tropical forests in Costa Rica. This allometry was then compared against the one obtained from excavated trees in a seasonally dry tropical forests in Costa Rica by Smith-Martin et al. 2019 New Phytologist: although the shape and the differences among the allometries for deciduous and evergreen trees were in overall agreement with observations, the ED2 allometry generally underestimated the observed root depth. As it seems that dry forest species have deeper root than wet forest species (see e.g. Holbrook et al. 1995, cited in Smith-Martin et al. 2019, or Markesteijn & Poorter 2009), this should be better suited to our application to wet forest although this remains to be confronted with data. This allometry gives a root depth of 22cm and 2.65 m for trees with dbh=1cm and 1m respectively. Note that in Xu et al., the first parameter (here 0.22) varied among phenological types, so that root depth of evergreen trees is about twice the ones of deciduous. This link between plant phenology and rooting depth was found empirically in Smith-Martin et al. 2019 New Phytologist, Hasselquist et al. 2010 Oecologia, while Markesteijn & Poorter found a correlation between root depth and stem density on first year seedling in Panama. This link between phenology and rooting depth with higher rooting depth for evergreen species was needed to sustain leaf cover in simulations with ED2, despite their higher leaf and stem drought tolerance (P50 and TLP; Smith-Martin et al. 2019). In absence of data, we here assumed this allometry suits to all species but this has to be discussed !
    t_root_depth=fminf(0.35*pow(t_dbh*100, 0.54), layer_depth[(nblayers_soil-1)]); // this is the allometry used in ED2-hydro, Xu et al. 2016, for evergreen trees, with one parameter, b1=root_depth(dbh=1cm), changed to correct the overall underestimation of tree rootdepth it leads to (cf. Smith-Martin et al. 2019 New phytol.). In absence of data on root depth (in general, and even more at the species level, see email with V. Freycon & B. Ferry january 20th 2020), this should probably be calibrated/fine-tuned.
    float i_root_depth=1.0/t_root_depth;
    
    //Tree water potential in the root zone (or predawn water potential)
    float shallow_bound=1.0;
    float deep_bound=1.0;
    float sumG=0.0;
    t_phi_root=0.0;
    
    float layer_depth_previous = 0.0;
    float layer_thickness=0.0;
    float root_area = t_CR*t_CR*PI;

    for(int l=0; l<nblayers_soil; l++) {
        
        layer_thickness=layer_depth[l]- layer_depth_previous;
        layer_depth_previous =layer_depth[l];
        
        deep_bound=exp(-3.0*i_root_depth*layer_depth[l]);
        //deep_bound=exp(-4.02*i_root_depth*layer_depth[l]); // by consistency with the root depth allometry, we here used the parameter value used by Xu et al. 2016 for evergreen trees. This value (log(beta)) was defined based on Jackson et al. 1996 Oecologia.
        //t_soil_layer_weight[l]=shallow_bound-deep_bound;                        //the weight for each soil layer are the relative tree root biomass in this layer. This is the most classic approach, however probably not fully relevant, inasmuch as trees may equilibrate "preferentially" with the wettest part of the soil, where water is more available and more easily exctractable. Hence another possible weighting integrates the soil-to-root conductance into account (as in de Kauwe et al. 2015; Duursma & Medlyn 2011). See below.
        t_root_biomass[l]=total_root_biomass*(shallow_bound-deep_bound);            // this is the root biomass in layer l, computed from the integration of the root biomass exponential profile in this layer, following Arora & Boer 2003. This also corresponds to the shape of cumulative root fraction used by Jackson et al. 1996 Oecologia, who gathered a large global database of root distributions with this equation. It was thus used in many models, e.g. Duursma & Medlyn 2012; Xu et al. 2016.
        //t_soil_layer_weight[l]=t_root_biomass[l]*10.0/(-log(sqrt(PI*t_root_biomass[l]*10.0)*0.001)); // this soil layer weight integrates the soil-to-root conductance into account (as in de Kauwe et al. 2015; Duursma & Medlyn 2012)
        
        if(t_LA > 0.0){

if (_WATER_RETENTION_CURVE==1) {
            if (_SOIL_LAYER_WEIGHT==0) { // soil layer weights as a function of root biomass in each layer only (cf. M1 in de Kauwe et al 2015)
                t_soil_layer_weight[l]=t_root_biomass[l];
                t_phi_root+=t_soil_layer_weight[l]*soil_phi3D[l][site_DCELL[t_site]];
            } else if (_SOIL_LAYER_WEIGHT==1) {
                t_soil_layer_weight[l]=t_root_biomass[l]*10.0*Ks[l][site_DCELL[t_site]]/(abs(log(sqrt(PI*t_root_biomass[l]*10.0)*0.001))); // this soil layer weight integrates the soil-to-root conductance into account (as in de Kauwe et al. 2015; Duursma & Medlyn 2012)
                t_phi_root+=t_soil_layer_weight[l]*soil_phi3D[l][site_DCELL[t_site]];   //the water potential in the root zone is computed as the weighted mean of the soil water potential in each soil layer in the DCELL where the tree stands. Note that KsPhi was here not computed as Ks[l][site_DCELL[t_site]]*soil_phi3D[l][site_DCELL[t_site]], to avoid some potential divergence for very low water content, and due to the limit of float type, but directly as the exact power of SWC -- see in UpdateField.
                
            } else if (_SOIL_LAYER_WEIGHT==2) { // soil layer weight as in Duursma & Medlyn 2012 (cf M3 in de Kauwe et al. 2015)
                
                //if (soil_phi3D[l][site_DCELL[t_site]]>(-3.0)) t_soil_layer_weight[l]=t_root_biomass[l]*10.0*Ks[l][site_DCELL[t_site]]/(abs(log(sqrt(PI*t_root_biomass[l]*10.0)*0.001)))*(soil_phi3D[l][site_DCELL[t_site]]+3); // this soil layer weight integrates the soil-to-root conductance into account (as in de Kauwe et al. 2015; Duursma & Medlyn 2012). Note that in their implementation of MAESPA, Christina et al. used minimum root water potential = -1.6 MPa and not -3 MPa as here, and they also added a gravimetric component, since they explore the effect of very deep root. Sensibility to this value and addition to be tested.
                //if (soil_phi3D[l][site_DCELL[t_site]]>(-3.0)) t_soil_layer_weight[l]=t_root_biomass[l]*10.0*Ks[l][site_DCELL[t_site]]/(abs(log(sqrt(PI*t_root_biomass[l]*10.0/(length_dcell*length_dcell*layer_thickness))*0.001)))*(soil_phi3D[l][site_DCELL[t_site]]+3);
                if (soil_phi3D[l][site_DCELL[t_site]]>(-3.0)) t_soil_layer_weight[l]=(t_root_biomass[l]*10.0/root_area)*Ks[l][site_DCELL[t_site]]/(abs(log(sqrt(PI*t_root_biomass[l]*10.0/(root_area*layer_thickness))*0.001)))*(soil_phi3D[l][site_DCELL[t_site]]+3);
                else t_soil_layer_weight[l]=0.0;
                t_phi_root+=t_soil_layer_weight[l]*soil_phi3D[l][site_DCELL[t_site]];   //the water potential in the root zone is computed as the weighted mean of the soil water potential in each soil layer in the DCELL where the tree stands. Note that KsPhi was here not computed as Ks[l][site_DCELL[t_site]]*soil_phi3D[l][site_DCELL[t_site]], to avoid some potential divergence for very low water content, and due to the limit of float type, but directly as the exact power of SWC -- see in UpdateField.
               
                
            }
} else if (_WATER_RETENTION_CURVE==0) {
    
            if (_SOIL_LAYER_WEIGHT==0) { // soil layer weights as a function of root biomass in each layer only (cf. M1 in de Kauwe et al 2015)
                t_soil_layer_weight[l]=t_root_biomass[l];
                t_phi_root+=t_soil_layer_weight[l]*soil_phi3D[l][site_DCELL[t_site]];
            } else if (_SOIL_LAYER_WEIGHT==1) {
                t_soil_layer_weight[l]=t_root_biomass[l]*10.0/(abs(log(sqrt(PI*t_root_biomass[l]*10.0)*0.001))); // this soil layer weight integrates the soil-to-root conductance into account (as in de Kauwe et al. 2015; Duursma & Medlyn 2012)
                t_phi_root+=t_soil_layer_weight[l]*KsPhi[l][site_DCELL[t_site]];   //the water potential in the root zone is computed as the weighted mean of the soil water potential in each soil layer in the DCELL where the tree stands. Note that KsPhi was here not computed as Ks[l][site_DCELL[t_site]]*soil_phi3D[l][site_DCELL[t_site]], to avoid some potential divergence for very low water content, and due to the limit of float type, but directly as the exact power of SWC -- see in UpdateField.
                t_soil_layer_weight[l]*=Ks[l][site_DCELL[t_site]];
                
            } else if (_SOIL_LAYER_WEIGHT==2) { // soil layer weight as in Duursma & Medlyn 2012 (cf M3 in de Kauwe et al. 2015)

                if (soil_phi3D[l][site_DCELL[t_site]]>(-3.0)) t_soil_layer_weight[l]=t_root_biomass[l]*10.0/(abs(log(sqrt(PI*t_root_biomass[l]*10.0)*0.001)))*(soil_phi3D[l][site_DCELL[t_site]]+3); // this soil layer weight integrates the soil-to-root conductance into account (as in de Kauwe et al. 2015; Duursma & Medlyn 2012). Note that in their implementation of MAESPA, Christina et al. used minimum root water potential = -1.6 MPa and not -3 MPa as here, and they also added a gravimetric component, since they explore the effect of very deep root. Sensibility to this value and addition to be tested.
                else t_soil_layer_weight[l]=0.0;
                t_phi_root+=t_soil_layer_weight[l]*KsPhi[l][site_DCELL[t_site]];   //the water potential in the root zone is computed as the weighted mean of the soil water potential in each soil layer in the DCELL where the tree stands. Note that KsPhi was here not computed as Ks[l][site_DCELL[t_site]]*soil_phi3D[l][site_DCELL[t_site]], to avoid some potential divergence for very low water content, and due to the limit of float type, but directly as the exact power of SWC -- see in UpdateField.
                t_soil_layer_weight[l]*=Ks[l][site_DCELL[t_site]];
                
            }
            
}
            
            
            if(isnan(t_phi_root)) {
                Rcout << "nan t_phi_root: " << endl;
                Rcout << l << "\t" << t_soil_layer_weight[l] << "\t" << KsPhi[l][site_DCELL[t_site]] << soil_phi3D[l][site_DCELL[t_site]] << "\t" << endl;
            }
            
        } else {
            if (l==0) {
                t_soil_layer_weight[l]=1;
                t_phi_root+=soil_phi3D[l][site_DCELL[t_site]];
            }
            else t_soil_layer_weight[l]=0;
        }
        
        //if(t_soil_layer_weight[l]<=0) Rcout << "t_soil_layer_weight[l]=" << t_soil_layer_weight[l] << " t_root_biomass[l]=" << t_root_biomass[l] << " Ks=" << Ks[l][site_DCELL[t_site]] << " Ks*Phi soil =" << KsPhi[l][site_DCELL[t_site]] << " phi soil=" <<   soil_phi3D[l][site_DCELL[t_site]] << endl;
        //t_soil_layer_weight[l]=t_root_biomass[l]*10/(-log(sqrt(PI*t_root_biomass[l]*10)*0.001));
        //t_phi_root+=t_soil_layer_weight[l]*fmaxf(0.0,(KsPhi2[l][site_DCELL[t_site]]-KsPhi[l][site_DCELL[t_site]]*t_s->s_tlp));  //the water potential in the root zone is computed as the weighted mean of the soil water potential in each soil layer
        //t_soil_layer_weight[l]*=fmaxf(0.0,(KsPhi[l][site_DCELL[t_site]]-Ks[l][site_DCELL[t_site]]*t_s->s_tlp));
        sumG+=t_soil_layer_weight[l];
        shallow_bound=deep_bound;
        
        if(t_soil_layer_weight[l]<0.0 || isnan(t_soil_layer_weight[l])) {
            Rcout << "incorrect soil_layer_weight: " << endl;
            Rcout << l <<  "\t" << site_DCELL[t_site]<<  "\t" << t_soil_layer_weight[l] << "\t" << t_root_biomass[l] << "\t" << Ks[l][site_DCELL[t_site]] << "\t" << KsPhi[l][site_DCELL[t_site]] << "\t" << -log(sqrt(PI*t_root_biomass[l]*10)*0.001) << "\t" << deep_bound << "\t" << shallow_bound << "\t" << SWC3D[l][site_DCELL[t_site]] <<  "\t" <<(SWC3D[l][site_DCELL[t_site]]-Min_SWC[l])/(Max_SWC[l]-Min_SWC[l]) <<  "\t"  << Ksat[l]  <<  "\t" << soil_phi3D[l][site_DCELL[t_site]];
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
    } else {
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
    
    t_phi_root-=0.01*t_height; // gravitational effect on leaf water potential and stress
    
    if (isnan(t_phi_root)) {
        Rcout << "nan t_phi_root" << endl;
        Rcout << t_height << endl;
    }
    
    //! Tree water stress factors
    
    //t_WSF=fminf(1.0, fmaxf(0.0, ((SWC3D[0][site_DCELL[t_site]]-Min_SWC[0])/(Max_SWC[0]-Min_SWC[0])))); // this is the simple linear WSF, with SWC as independent varible, used in lots of model (see Powell et al. 2013 New Phytol, De Kauwe et al. 2015 Biogeosciences, Laio et al. 2001 Advances in Water resources, Egea et al. 2011 AFM....)
    //t_WSF=fminf(1.0, fmaxf(0.0, (t_phi_root-(t_s->s_tlp))*(t_s->s_dWSF))); // this is the linear WSF, with wtare potential as independent variable, used in CLM model (see Powell et al. 2013 New Phytologist, Verhoef & Egea 2011 AFM)
    //t_WSF=exp((log(0.05)/t_s->s_tlp)*t_phi_root); // this is the WSF, to simulate stomatal limitation (ie. hinder g1), drawn from Zhou et al. 2013 AFM, and de Kauwe et al. 2015 Biogeosciences. If this version of WSF is finally adopted, the parameter b=log(0.05)/t_s->s_tlp, should be declare as species variable (instead of s_dWSF).
    t_WSF=exp(S[t_sp_lab].s_b*t_phi_root); // this is the WSF shape used to simulate stomatal limitation (ie. hinder g1), drawn from Zhou et al. 2013 AFM, and de Kauwe et al. 2015 Biogeosciences, but with a different parameterization for the parameter b (using the relationship between phi_gs90 and tlp from Martin-StPaul et al. 2017 Ecology letters, and assuming the WSF=0.9 at phi_gs90).
    //float par=t_s->s_tlp+1;
    //t_WSF_A=(1.0+exp(6.0*par))/(1.0+exp(6.0*(par-t_phi_root))); // this is the WSF, to simulate non-stomatal limitation (ie. hinder Vcmax and Jmax), drawn from Zhou et al. 2013 AFM, and de Kauwe et al. 2015 Biogeosciences
    //t_WSF_A=(1.0+exp(3.0*par))/(1.0+exp(3.0*(par-t_phi_root))); // this is the WSF, to simulate non-stomatal limitation (ie. hinder Vcmax and Jmax), drawn from Zhou et al. 2013 AFM, and de Kauwe et al. 2015 Biogeosciences
#ifdef WATER
    //t_g1= (-3.97 * t_wsg + 6.53)*t_WSF; this is the relationship provided by Lin et al. 2015
    t_g1 = t_g1_0*t_WSF; // with the water stress factor added
#endif
    
    t_WSF_A=1/(1+pow(t_phi_root*S[t_sp_lab].s_itlp, 6)); // this is the WSF used to hinder Vcmax and Jmax in Xu et al. 2016 (equ. S5) (ED2-hydro).
    
    
    if (t_WSF < 0.0 || t_WSF_A < 0.0 || t_WSF>1.0 || t_WSF_A >1.0 ||t_phi_root >0.0 || isnan(t_WSF) || isnan(t_WSF_A) || isnan(t_phi_root)) {
        Rcout << "incorrect value in one of WSF, WSF_A, t_phi_root " << endl;
        Rcout <<t_WSF << "\t" << t_phi_root << "\t" << S[t_sp_lab].s_tlp  << "\t" << t_dbh << "\t" << t_height << "\t" << t_age << "\t" << S[t_sp_lab].s_phi_lethal << "\t" << sumG;
        Rcout <<endl;
    }
    //}
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
        float sum_weights=0.0;
        while (sum_weights<1.0 && l < nblayers_soil) {
            Transpiration[l][site_DCELL[t_site]] += t_soil_layer_weight[l] * t_transpiration;
            if (t_soil_layer_weight[l] < 0.0 || t_transpiration < 0.0 ) {
                Rcout << setprecision(10);
                Rcout << "Problem with soil_layer_weight and transpiration at site: " << t_site << " layer: " << l << " depth: " << depth << endl;
                Rcout << t_soil_layer_weight[l] << "\t" << t_transpiration << "\n";
            }
            sum_weights+=t_soil_layer_weight[l];
            depth = layer_depth[l];
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
#ifdef WATER
void Tree::Fluxh(int h,float &PPFD, float &VPD, float &Tmp, float &leafarea_layer, float &PPFD_incident, float &ExtinctLW) {
#else
    void Tree::Fluxh(int h,float &PPFD, float &VPD, float &Tmp, float &leafarea_layer) {
#endif
        
        int site_crowncenter = t_site + t_CrownDisplacement;
        int row_crowncenter = site_crowncenter/cols;
        int col_crowncenter = site_crowncenter%cols;
        int shell_fromtop = int(t_height)+1 - h;
        
#ifdef WATER
        float canopy_environment_cumulated[6] = {0.0,0.0,0.0,0.0,0.0, 0.0};
#else
        float canopy_environment_cumulated[4] = {0.0,0.0,0.0,0.0};
#endif
        float fraction_filled_target = t_fraction_filled;
        
        LoopLayerUpdateCrownStatistic_template(row_crowncenter, col_crowncenter, t_height, t_CR, t_CD, fraction_filled_target, shell_fromtop, GetRadiusSlope, t_LAI, canopy_environment_cumulated, LAI2dens, GetCanopyEnvironment);
        
        
        float LA_layer = canopy_environment_cumulated[0];
        float iLA_layer;
        if(LA_layer > 0.0) iLA_layer = 1.0/LA_layer;
        else iLA_layer = 0.0;
        
        leafarea_layer = LA_layer;
        PPFD = canopy_environment_cumulated[1] * iLA_layer;
        VPD = canopy_environment_cumulated[2] * iLA_layer;
        Tmp = canopy_environment_cumulated[3] * iLA_layer;
#ifdef WATER
        PPFD_incident = canopy_environment_cumulated[4] * iLA_layer;
        ExtinctLW = canopy_environment_cumulated[5] * iLA_layer;
#endif
        
        // if (PPFD <=0) {
        //  Rcout << "Warning PPFD <=0 in Fluxh !! PPFD=" << PPFD << "; iLA_layer=" << iLA_layer << "; canopy_environment_cumulated[1]=" << canopy_environment_cumulated[1] << "; VPD=" << VPD << "; Tmp=" << Tmp << "; h=" << h << "; Tree height=" << t_height << "; Tree_crowndepth=" << t_CD << "; Tree_leafarea=" << t_LA << endl;
        //}
        
        
    }
    
#else
    void Tree::Fluxh(int h, float &PPFD, float &VPD, float &Tmp, float &leafarea_layer) {
        
        int site_crowncenter = t_site + t_CrownDisplacement;
        int row_crowncenter = site_crowncenter/cols;
        int col_crowncenter = site_crowncenter%cols;
        
        float crown_area = fmaxf(PI * t_CR * t_CR,0.0);
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
        int crown_top = int(t_height);
        int crown_base = int(t_height - t_CD);
        float fraction_abovetop = t_height- float(crown_top);
        float fraction_belowbase = float(crown_base + 1) - (t_height - t_CD);
        float dens_avg = t_LAI/t_CD;
        
        float dens_layer = dens_avg;
        if(crown_top == crown_base) dens_layer *= t_CD;
        else if(h_index == crown_top) dens_layer *= fraction_abovetop;
        else if(h_index == crown_base) dens_layer *= fraction_belowbase;
        
        //given constant density within layer, we just need to multiply by area
        leafarea_layer = dens_layer * crown_area_nogaps;
    }
#endif
    
    //#############################
    // Empirical functions (traits)
    //#############################
    
    float Tree::CalcJmaxm(){
        float SLA=10000.0/t_LMA; // in cm2 g-1
        float Jmaxm = pow(10.0, fminf((-1.50+0.41*log10(t_Nmass*1000.0)+0.45*log10(SLA)), (-0.74+0.44*log10(t_Pmass*1000.0)+0.32*log10(SLA)))); // added as a Species member variable 14-04-2015; this is equ 2 in Domingues et al 2010 PCE (coefficients from fig7). s_Nmass and s_Pmass are given in g g-1, but should be in mg g-1 in equ 2 in Domingues et al. 2010, hence the mutiplication by 1000.
        return(Jmaxm); //  in micromol C m-2 s-1
    }
    
    
    
    
    
    //#######################
    // Death rate calculation
    //#######################
    float Tree::DeathRateNDD(float dbh, float carbon_starv, float ndd) {
        float dr=0;
        float basal=m*(1-t_wsg);
        float dd=deltaD*ndd*(1-2*dbh/t_dbhmax);
        
        dr=basal;
        if (_LA_regulation==0) {
            if(carbon_starv > t_leaflifespan) dr+=1.0/timestep;
        }
        else {
            if (carbon_starv <= 0.0 && t_NPP <= 0.0) dr+=1.0/timestep; // newIM 2021: carbon starvation occurs when the carbon stocks has been completly depleted, and carbon_starv represents t_carbon_storage (while it represents t_NPPneg when _LA_regulation==0)
        }
        if(dd > 0) dr+=dd;
        return dr*timestep;
    }
    
#ifdef WATER
    float Tree::DeathRate(float dbh, float carbon_starv, float phi_root) {
        float dr=0;
        float basal=fmaxf(m-m1*t_wsg,0.0);
        
        dr=basal;
        //if (carbon_starv > t_leaflifespan) dr+=1.0/timestep;
        if (_LA_regulation==0) {
            if (carbon_starv > t_leaflifespan) dr+=1.0/timestep;
        }
        else {
            if (carbon_starv <= 0.0 && t_NPP <= 0.0) dr+=1.0/timestep; // newIM 2021: carbon starvation occurs when the carbon stocks has been completly depleted, and carbon_starv represents t_carbon_storage (while it represents t_NPPneg when _LA_regulation==0)
        }
        // if the water availabiliy in the root zone is below the lethal level, the tree dies, !!!: not that deterministic, right?
        if (phi_root < (S[t_sp_lab].s_phi_lethal)) dr+=1.0/timestep;
        if (iter == int(nbiter-1)) output[26]<< t_wsg << "\t" << basal << "\t"  << dbh << "\t"  << dr   <<  "\n";
        
        /*if (iter>=622 && dr*timestep>=0.8) {
            Rcout<< "high deathrate: wsg=" << t_wsg << "; basal=" << basal << "; dbh="  << dbh << "; dr="  << dr*timestep   << "; carbon_starv="  << carbon_starv   << "; NPP="  << t_NPP   << "; phi_root="  << phi_root   << "; S[t_sp_lab].s_phi_lethal=" << S[t_sp_lab].s_phi_lethal << "; t_WSF=" << t_WSF << "; t_WSF_A=" << t_WSF_A << "; t_LA=" << t_LA << endl;
        }*/

        
        return dr*timestep;
    }
#else
    float Tree::DeathRate(float dbh, float carbon_starv) {
        float dr=0.0;
        float basal=fmaxf(m-m1*t_wsg,0.0);
        
        dr=basal;
        if (_LA_regulation==0) {
            if (carbon_starv > t_leaflifespan) dr+=1.0/timestep;
        }
        else {
            if (carbon_starv <= 0.0 && t_NPP <= 0.0) dr+=1.0/timestep; // newIM 2021: carbon starvation occurs when the carbon stocks has been completly depleted, and carbon_starv represents t_carbon_storage (while it represents t_NPPneg when _LA_regulation==0)
        }

    }
#endif
    
    
    //###############################################################
    // Farquhar von Caemmerer Berry model -- called by  Tree::GPPleaf
    //###############################################################
    
#ifdef WATER
    //! - Function Tree::GPPleaf when the WATER option is off, Tree::FluxesLeaf when WATER option is on
    //! - Includes the Farquhar model of photosynthesis and Medlyn et al. 2011 model of stomatal conductance -- see also Prentice et al 2014 Ecology Letters, Lin et al 2015 Nature Climate Change, Dewar et al. 2018 New phytologist; min added in order to prevent ci:ca bigger than 1 (even though Ehleringer et al 1986 reported some values above 1 (Fig3)
    //! - Tree::FluxesLeaf iterates on the leaf conditions (Tl, Cs and Dleaf) and calls another function Tree::Photosyn, which itself implements the models of photosynthsis and stomatal conductance.
    //! - When WATER option is on, the stomatal conductance parameter (g1) and photosynthetic capacities are reduced under water stressed conditions: stomatal and non-stomatal responses to water stress (see Egea et al. 2011 AFM, Zhou et al. 2013 AFM, Zhou et al. 2014 PCE, etc.). This is done by multipliying the values of g1 and of Vcmax and Jmax in absence of water stress by water stress factors (WSF and WSF_A respectively).
    //! - Returns the primary productivity (assimilation) per unit leaf area, i.e. in micromoles C m-2 s-1, and water fluxes (transpiration) in micromol H20 m-2 s-1.
    //! - It is converted into gC per m^2 of leaf per timestep by "nbhours_covered*15.7788*timestep" where 15.7788 = 3600*365.25*12/1000000 and nbhours_covered is the duration read in the daily variation file
    //! - NB1: 12 is the molar mass of carbon NB2: timestep is given as fraction of a year, so what is computed is actually the full assimilation per year which, in turn, is multiplied by the fraction per year that is under consideration.
    //! - BEWARE: slight inconsistency through use of 365.25 when daily timestep is likely to be given as 365, but not incorrect.
    //! - It uses lookup tables for acceleration of computation of T dependencies. cf. Bernacchi et al 2003 PCE; von Caemmerer 2000
    
    //!  the function Tree::FluxesLeaf implements an iterative algorithm to determine the leaf temperature, and CO2 concentration and VPD at the leaf surface, ie the ones that should be taken as arguments of the Farquhar and stomatal conductance models. The latters are themeselves implemented within the function Tree::Photosyn (analogy with the MAESTRA/MAESPA code from which this iterative scheme has been inspired, see Medlyn et al. 2007 Tree physiology). At each iteration, Tree::FluxesLeaf computes carbon and water fluxes at the leaf level using the Farqhuar model and the Penman-Monteith equation, and updates T, CO2 and VPD accordingly. Starting by assuming that leaf T, CO2 and VPD equal the ones of the surrounding air, the iterative scheme stops when the difference in leaf T between two consecutive iterations is negligble. The corresponding carbon assimilation and water transpiration are then retained and provided as outputs of Tree::FluxesLeaf.
    leafFluxes Tree::FluxesLeaf(float PPFD, float VPDa, float Ta, float WIND, float ExtinctLW, float PPFDtop, float PPFDinc, float Tatop, float VPDatop) {
        
        float TLEAF=0.0,CS=0.0,DS=0.0,// leaf temperature and CO2 concentration and vapour pressure deficit at the leaf surface.
        ALEAF=0.0,ET=0.0, // net assimilation rate and leaf-level evapotranspiration
        GH=0.0,GBV=0.0,GSV=0.0,GV=0.0, GSC=0.0, GBH=0.0, // conductance terms for Penman-Monteith calculation
        Rnetiso=0.0,lambdaET=0.0,HDIVT=0.0; // terms used for Penman-Monteth calculation
        
        int ITERMAX=30;
        //int itmax=ITERMAX;
        int convTA= int(iTaccuracy*Ta);  // discrete temperature to avoid repeated computation
        //int convVPDA=int(iVPDaccuracy*VPDa); // discrete VPD to avoid repeated computation
        int convTAtop= int(iTaccuracy*Tatop);  // discrete temperature to avoid repeated computation
        int convVPDAtop=int(iVPDaccuracy*VPDatop); // discrete VPD to avoid repeated computation
        //float ESAT=LookUp_ESAT[convTA]; //ESAT: Saturation vapour pressure (in kPa)
        //VPD = ESAT*(1.0 - RH); // Calculation of air VPD from relative humidity and air temperature, in kPa (!!), needed only if RH, and not VPD, is provided as an argument.
        
        // Computation of Penman-Monteith terms that do not depend on TLEAF, CS and DS :
        
        //float RSOL = PPFD*PPFDtoSW;   // Incoming solar energy in J m-2 s-1 -- PPFDtoSW is to account for the fact that PAR actually account for ca. 50% of the total solar energy at the Earth's surface (ie after its way through the atmosphere), and Penman-Monteith should include all the shortwave energy (ie PAR + NIR), and convert from micromol quanta to J (PPFDtoSW~ 2.0/ 4.57).
        float RSOL = 0.2188*PPFD + 0.0015*PPFDtop + 0.0174*PPFDinc; // this is based on the relationship between NIR/PAR ratio in the understory and PARtop/PAR or LAI, reported in Kume et al. 2011 Journal of Plant Research (Fig. 4 therein), to account for the absorbed NIR. 0.2188=1/4.47; 0.0015=0.1/(4.57*14.48) with 0.1~leaf absorptance in NIR and 14.48 fitted parameter in Kumme et al; 0.0174=0.1*11.52/(4.57*14.48), with 11.52 the other fitted parameter in Kumme et al., and 1/4.57 is to convert from microml m-2 s-1 to J.
        //if (PPFDinc/PPFDtop <0.8) {
        //Rcout << "PPFDtop=" << PPFDtop << " PPFD=" << PPFD <<" PPFDinc=" << PPFDinc << " RSOL=" << RSOL << " RSOL_NIR=" << RSOL_NIR << endl;
        //}
        float LHV = (H2OLV0 - 2.365e3*Ta)*H2OMW;// Latent heat of water vapour at air temperature (J mol-1) (this is the "landa" in PM equation)
        float SLOPE=LookUp_SLOPE[convTA]; /* Slope of the dependence of saturated vapor pressure with temperature (Jones 2013, Equation (5.15), p.102)
                                           This is constant s in Penman-Monteith equation  (Pa K-1) */
        float CMOLAR = 1000.0*PRESS/(RCONST*(Ta-ABSZERO)); //1000 because PRESS is in kPa
        float GAMMA = 1000.0*PRESS*CPAIR*AIRMA/LHV; // Psychrometric constant; 1000.0 to convert kPa into Pa. !! IM to be checked !! factor AIRMA not consistent with Jérôme's note and Appendix 3 of Jones, but similar to Duursma 's package
        float GRADN = LookUp_GRADN[convTA]; // Radiation conductance (mol m-2 s-1) at air temperature (Jones 2013, Eq 5.10 p. 101). // IM to be double-checked with Jones (not fully consistent with Medlyn et al. 2007)
        float GBHU = 0.003 * sqrt(WIND/t_wleaf) * CMOLAR; //Boundary layer conductance to heat transfer by forced convection (single sided) in mol m-2 s-1; Leuning et al (1995) PC&E 18:1183-1200 Eqn E1; Equation A2 in Medlyn et al. 2007
        //Rnetiso = absorptance_leaves*RSOL - LookUp_INLR[convTA][convVPDA]; // Calculation of isothermal net radiation (J m-2 s-1; Jones (2013) equation (5.4) p.100)
        Rnetiso = RSOL - LookUp_INLR[convTAtop][convVPDAtop]*ExtinctLW; // Calculation of isothermal net radiation (J m-2 s-1; Jones (2013) equation (5.4) p.100). No need of absorptance_leaves, since PPFD provided in argument is already the absorbed flux.
        
        //if (ExtinctLW<0.2) {
        //   Rcout << "Rnetiso=" << Rnetiso << " RSOL=" << RSOL << " INLR=" << LookUp_INLR[convTA][convVPDA]  << " INLR_top=" << LookUp_INLR[convTAtop][convVPDAtop] << " //ExtinctLW=" << ExtinctLW <<  endl;
        //}
        
        // Iterative scheme to determine TLEAF and CO2 concentration and VPD at leaf surface, and the corresponding Anet and ET:
        
        TLEAF = Ta; // Initialization of leaf T, CO2 & VPD at leaf surface equal to the ones of surrounding air
        DS=VPDa;
        CS=Cair; // IM: check whether we should not make Cair vary across the canopy.
        leafFluxes ps;
        
        for(int ITER=0;ITER<ITERMAX;ITER++) { // Iterative loop:
            
            if(CS<0 || DS<0 || CS>100000 ) {
                Rcout << "Warning in FluxesLeaf ! DS=" << DS << "; CS=" << CS << "; PPFD=" << PPFD << "; TLEAF=" << TLEAF << endl;
            }
            
            
            ps=Photosyn(PPFD,TLEAF,CS,DS);    // Compute CO2 assimilation rate and stomatal conductance at TLEAF, CS and DS
            ALEAF=ps.carbon_flux; // in micromolCO2 m-2 s-1
            GSC=ps.water_flux; // in mol CO2 m-2 s-1 by Photosyn function
            
            if(isnan(GSC) || isnan(ALEAF) ) {
                Rcout << "Warning in FluxesLeaf: " << " ALEAF= " << ALEAF << ", GSC= "<< GSC <<", CS= " << CS << ", PPFD=" << PPFD << ", DS= " << DS << ", TLEAF= " << TLEAF << ", ITER=" << ITER << endl ;
            }
            
            // Computation of the different conductance terms
            float GBHF; // Boundary layer conductance to heat transfer by free convection (single sided) in mol m-2 s-1. Leuning et al (1995) PC&E 18:1183-1200 Eqns E3 & E4. Equation A1 in Medlyn et al. 2007
            if(abs(TLEAF-Ta) > 0.01){ // this is to speed up: calculating GBHF involves a pow, so do only if difftemp is not zero.
                //float GRASHOF = 1.6e8 * abs(TLEAF-TAIR) * pow(WLEAF,3.0); // Grashof number
                //GBHF = 0.5 * DHEAT * pow(GRASHOF,0.25) / WLEAF * CMOLAR;
                GBHF = 56.2341 * DHEAT * pow(abs(TLEAF-Ta)/t_wleaf,0.25)* CMOLAR;
            }
            else GBHF = 0.0;
            GBH = GBHU + GBHF; // Total boundary layer conductance to heat (single sided), in mol m-2 s-1
            GH = 2*(GBH + GRADN); //GH heat and radiative conductance (free & forced & radiative components), mol m-2 s-1, to be used in the isothermal version of the Penman-Monteith equation (always two sided, hence the factor 2)
            //if (t_sp_lab==6) GBH*=2; // test to see if assuming that Cecropia obtusa is amphistomatous (and not hypostomatous) would solve the issue found when ALEAF is big, wleaf is big (and hence GBH small) that leads to negative CS.... IM 09/09/2021
            HDIVT = GH*CPAIR*AIRMA; // in J m-2 s-1 K-1, useful in several calculations below
            GBV = GBVGBH*GBH; // Boundary layer conductance for water vapour, for hypostomatous leaves (single-sided value). Note: we thus here assumed that all leaves are hypostomatous, this is a reasonable assumption as few species have been reported to have amphistomatous leaves in tropical forests (e.g. Drake et al. 2019 New phytologist; Muir 2015 Proc Roy Soc; email with Lawren Sack Oct. 29th 2020). Cf. comment in Leuning et al. 1995 after equ. E5.
            GSV = GSVGSC*GSC; // Stomatal conductance for water vapour, in mol H2O m-2 s-1
            if ((GSV/GBV) >100000) GV=GBV;
            else GV = (GBV*GSV)/(GBV+GSV); //Total conductance to water vapour (single-sided value: hypostomatous; stomatal & bdry layer components in series), mol H2O m-2 s-1 //
            
            if(isnan(GSV) || isnan(GV)|| (GSV < 0 )) {
                Rcout << "Warning in FluxesLeaf: " << " GV= " << GV << ", GSC= "<< GSC <<", GBV= " << GBV << ", GSV=" << GSV << ", GSC=" << GSC << ", ALEAF=" << ALEAF << ", GSVGSC= " << GSVGSC  <<", ALEAF=" << ALEAF << ", PPFD=" << PPFD << ", DS=" << DS << ", ITER=" << ITER << ", WIND=" << WIND << endl ;
            }
            
            
            // Computation of leaf-level evapotranspiration (ET) using the Penman-Monteith equation (mol H2O m-2 s-1).
            if(GV>0.0) lambdaET=(SLOPE*Rnetiso+1000.0*VPDa*HDIVT)/(SLOPE+GAMMA*GH/GV);
            else lambdaET = 0.0;
            
            //if (lambdaET<0) lambdaET = 0.0; // check whether this is ok. remy says keep negative value but force ET TO ZERO
            if (lambdaET<0) ET = 0.0;
            else ET = lambdaET/LHV; // in mol H2O m-2 s-1
            
            // Update of TLEAF, CS, and DS
            CS = Cair-ALEAF*GBHGBC/GBH; //CO2 concentration at the leaf surface, in micromol mol-1
            if (CS<0.0) CS=1.0;
            DS = ET * PRESS / GSV; // in kPa; IM important change in regards to Duursma R package or MAESTRA/MAESPA code: GV replaced by GSV (cf for eg Medvigy et al. 2009 equ B16, cf. doc comments on Physiol V6)
            //DS = ET * PRESS / GV; // test cluster 14/05/22
            
            if(CS <= 0 ) {
             Rcout << "Warning in FluxesLeaf: " << " CS= " << CS << ", Cair=" << Cair << ", ALEAF=" << ALEAF << ", GBH=" << GBH << ", GBHGBC=" << GBHGBC << endl ;
             }
            
            /*if(DS < 0 ) {
            
                Rcout << "Warning in FluxesLeaf: " << " DS= " << DS << ", VPDa= "<< VPDa << ", ITER=" << ITER << ", GSV=" << GSV << ", GSC=" << GSC << ", ALEAF=" << ALEAF << ", ET= " << ET << ", GV=" << GV << ", initial lambdaET=" << (SLOPE*Rnetiso+1000.0*VPDa*HDIVT)/(SLOPE+GAMMA*GH/GV) << ", Rnetiso=" << Rnetiso << "; PPFD=" << PPFD << "; Vcmax=" << t_Vcmax << "; Jmax=" << t_Jmax << "; t_Pmass=" << t_Pmass << "; S[spp].s_Pmass=" << S[t_sp_lab].s_Pmass << endl ;
            }*/
            
            //float TLEAF1 = Ta + (Rnetiso - lambdaET)/HDIVT; // Leaf temperature inferred from Jones (2013) equation (9.7), p.225. Note : in Vezy et al. 2018 they accounted for the rate of evaporation of water at the leaf sufarce when the leaf is wet in addition to the one of tranpiration. To do so they use Penman-Monteith with an "infinitly" large stomatal conductance to water vapor and use a weighted mean of both PM values...
            
            float TDIFF=(Rnetiso - lambdaET)/HDIVT;
            float TLEAF1= Ta + 0.25*TDIFF; // divide by 4 to slow down convergence and avoid big changes -- taken from code from Vezy et al. 2018 -- cf. discussion with Guerric Le Maire
            if(abs(TLEAF-TLEAF1)<0.01) {
                //itmax=ITER;
                ITER=ITERMAX;
                
            } // If Tleaf diff < 0.01°C, stop search // check whether this threshold value is optimal
            TLEAF = TLEAF1; // update TLEAF for next iteration // cf. Vezy et al. and discussion with Guerric Lemaire: suggest to make smaller steps at each iteration to ease convergence (TDIFF -> TDIFF/4)
            
            if(isnan(DS) || isnan(ET) || isnan(ALEAF) || ET < 0.0  || TLEAF <=0.0  || TLEAF >50.0 || CS <0 || DS<0 || CS>100000) {
                Rcout << "Warning in FluxesLeaf: " << " ALEAF= " << ALEAF << ", ET= "<< ET <<", CS= " << CS << ", PPFD=" << PPFD << ", DS= " << DS << ", TLEAF= " << TLEAF << ", GSV=" << GSV << ", GV= " << GV << ", Rnetsiso=" << Rnetiso << "; lambdaET=" << lambdaET << "; HDIVT=" << HDIVT << "; TDIFF=" << TDIFF << "; Ta=" << Ta << "; Cair=" << Cair << "; GBH=" << GBH << "; Cair=" << Cair << "; GBHGBC=" << GBHGBC << "; t_site=" << t_site << "; t_sp_lab=" << t_sp_lab << "; ITER=" << ITER << endl ;
            }
            
        }
        
        //Rcout << "itmax=" << itmax << endl;
        
        // At the end of loop, return water (ET, in umol H2O m-2 s-1) and carbon (assimilation) fluxes
        ET = ET*1e6;
        
        //if (iter > 4000 && t_height> 10.0) {
        //    Rcout << " species:" << S[t_sp_lab].s_name << " Tree height=" << t_height << " ET=" << ET << " SLOPE=" << SLOPE << " Rnetiso=" << Rnetiso << " SLOPE*Rnetiso=" << SLOPE*Rnetiso <<  " 1000*VPDa*HDIVT=" << 1000.0*VPDa*HDIVT << " VPDa=" << VPDa << " HDIVT=" << HDIVT << " GAMMA*GH/GV=" << GAMMA*GH/GV << endl;
        //}
        
        
        leafFluxes outFluxes;
        outFluxes.carbon_flux=ALEAF; // in micromol CO2 m-2 s-1
        outFluxes.water_flux=ET; // in micromol H20 m-2 s-1
        return outFluxes;
    }
    
    
    
    
#ifdef G0
    
    
    leafFluxes Tree::Photosyn(float PPFD, float TLEAF, float CS, float DS) {
        
        float Anet;
        
        //Parameters for the Farquhar model
        int convT= int(iTaccuracy*TLEAF);
        float KmT    =LookUp_KmT[convT]; // with temperature dependencies
        float GammaT =LookUp_GammaT[convT]; // with temperature dependencies
        float Rday= t_Rdark*LookUp_Rleaf[convT]*DAYRESP; // leaf respiration with temperature dependencies and inhibition due to light (DAYRESP).
        
        ////////////// Model of stomatal conductance for CO2 /////////////
        float GSDIVA;
        if (DS<0.0005)  GSDIVA = (1.0 + t_g1/sqrt(0.0005))/CS; // VPDMIN in MAESPA/Plantecophys R package
        else GSDIVA = (1.0 + t_g1/sqrt(DS))/CS; // Medlyn et al. 2011 Global Change Biology. Note that g1 is now a tree class variable that is initiated at tree birth (in Tree::Birth) and updated in Tree::Water_Availability depending on the tree water stress.
        //float GSDIVA = (1.0 + g1/sqrt(VPD))/(CS-GammaT); //Dewar et al. 2018 New Phytologist
        if(GSDIVA<=0 || isnan(GSDIVA)) {
            Rcout << "Warning in Photosyn ! GSDIVA=" << GSDIVA << "; DS=" << DS << "; CS=" << CS << endl;
            Rcout << "In Photosyn: PPFD=" << PPFD << "; TLEAF=" << TLEAF << "; CS=" << CS << "; DS=" << DS << endl;
        }
        
        //////////// Farquhar model of photosynthesis //////////
        
        float A, B, C, AC, AJ;
        
        // Solution when Rubisco activity is limiting
        float VcmaxTW =t_Vcmax*LookUp_VcmaxT[convT]*t_WSF_A;
        A = g0 + GSDIVA*(VcmaxTW - Rday);
        B = (1.0 - CS*GSDIVA)*(VcmaxTW - Rday) + g0*(KmT - CS)- GSDIVA*(VcmaxTW*GammaT + KmT*Rday);
        C = -(1.0 - CS*GSDIVA)*(VcmaxTW*GammaT + KmT*Rday) - g0*KmT*CS;
        float CIC = QUAD(A,B,C,1); //Finds larger root of quadratic function
        if ((CIC<=0.0)||(CIC-CS>0.01)) { //newIM: have replaced CIC>CS by (CIC-CS>0.01) to avoid error due to comparing floats (maybe there would be a best way to fix this...)
            AC = 0.0;
        }
        else AC=VcmaxTW*(CIC-GammaT)/(CIC+KmT);
        
        // Solution when electron transport rate is limiting
        float JmaxTW =t_Jmax*LookUp_JmaxT[convT]*t_WSF_A;
        float I = alpha * PPFD;
        float J=QUAD(theta,-(I+JmaxTW), JmaxTW*I, -1); // theta is the convexity term for electron transport rates (dimensionless, 0–1), here provided in input (default value =0.7, as in von Caemmerer 2000, but a value of 0.85 is used in Duursma' R package
        float VJ = J*0.25;
        A = g0 + GSDIVA*(VJ - Rday);
        B = (1.0 - CS*GSDIVA)*(VJ - Rday) + g0*(2.0*GammaT - CS) - GSDIVA*(VJ*GammaT +2.0*GammaT*Rday);
        C = -(1.0 - CS*GSDIVA)*GammaT*(VJ + 2.0*Rday) - g0*2.0*GammaT*CS;
        float CIJ = QUAD(A,B,C,1); //Finds larger root of quadratic function
        AJ=VJ*(CIJ-GammaT)/(CIC+2.0*GammaT);
        if ((AJ-Rday)<1.0e-6) {
            CIJ = CS; // Below light compensation point
            AJ = VJ*(CIJ-GammaT)/(CIJ+2.0*GammaT);
        }
        
        // Co-limitation
        Anet=fminf(AC, AJ)-Rday; // compute the *net* assimilation rate, in micromol C02 m-2 s-1
        
        if(isnan(Anet) || isnan(GSDIVA) ) {
            Rcout << "Warning in Photosyn: " << ", Anet= " << Anet << ", GSDIVA=" << GSDIVA << "; Anet*GSDIVA=" << Anet*GSDIVA << ", AC= "<< AC <<", AJ= " << AJ << ", Rday=" << Rday  << ", CS=" << CS <<"; CIC=" << CIC <<"; CIJ=" << CIJ <<", DS= " << DS << ", TLEAF= " << TLEAF << ", t_WSF_A=" << t_WSF_A << ", t_WSF= " << t_WSF <<  "; GammaT=" << GammaT << "; KmT=" << KmT << "; J=" << J << "; PPFD=" << PPFD << "; T_Rdark="<< t_Rdark << "; convT=" << convT << endl ;
        }
        
        if (isnan(Anet*GSDIVA) && Anet==0.0) { //to solve nan issue when Anet=0 and GSDIVA=inf
            Anet=1.0e-5;
        }
        
        leafFluxes outPhotosyn;
        outPhotosyn.carbon_flux=Anet; // in micromol C02 m-2 s-1
        outPhotosyn.water_flux=g0 + GSDIVA*Anet; // in mol CO2 m-2 s-1, the conversion to mol H20 is made in function FluxesLeaf.
        if (outPhotosyn.water_flux<g0) outPhotosyn.water_flux=g0;
        
        if(isnan(outPhotosyn.water_flux) || isnan(outPhotosyn.carbon_flux)) {
            Rcout << "Warning in Photosyn: " << ", Anet= " << Anet << ", AC= "<< AC <<", AJ= " << AJ << ", Rday=" << Rday << ", GSDIVA=" << GSDIVA << ", CS=" << CS <<"; CIC=" << CIC <<"; CIJ=" << CIJ  <<", DS= " << DS << ", TLEAF= " << TLEAF << ", t_WSF_A=" << t_WSF_A << ", t_WSF= " << t_WSF <<  "; GammaT=" << GammaT << "; KmT=" << KmT << "; J=" << J << "; PPFD=" << PPFD << "; t_Rdark="<< t_Rdark << "; convT=" << convT << endl ;
        }
        
        return outPhotosyn;
        
    }
    
  
    
#else
    
    leafFluxes Tree::Photosyn(float PPFD, float TLEAF, float CS, float DS) {
        
        float CI, Anet;
        
        //Parameters for the Farquhar model
        int convT= int(iTaccuracy*TLEAF);
        float KmT    =LookUp_KmT[convT]; // with temperature dependencies
        float GammaT =LookUp_GammaT[convT]; // with temperature dependencies
        float Rday= t_Rdark*LookUp_Rleaf[convT]*DAYRESP; // leaf respiration with temperature dependencies and inhibition due to light (DAYRESP).
        
        // Model of stomatal conductance for CO2
        float GSDIVA = (1.0 + t_g1/sqrt(DS))/CS; // Medlyn et al. 2011 Global Change Biology. Note that g1 is now a tree class variable that is initiated at tree birth (in Tree::Birth) and updated in Tree::Water_Availability depending on the tree water stress.
        //float GSDIVA = (1.0 + g1/sqrt(VPD))/(CS-GammaT); //Dewar et al. 2018 New Phytologist
        if(GSDIVA<=0 || isnan(GSDIVA)) {
            Rcout << "Warning in Photosyn ! GSDIVA=" << GSDIVA << "; DS=" << DS << "; CS=" << CS << endl;
            Rcout << "In Photosyn: PPFD=" << PPFD << "; TLEAF=" << TLEAF << "; CS=" << CS << "; DS=" << DS << endl;
        }
        
        // Internal CO2 concentration, assuming that minimal conductance (cf. Duursma et al. 2019 New Phytologist, ~cuticular conductance) is negligeable (g0~0) [Note that, if g0>0, then a quadratic solution must be computed, which complexifies and should lengthen the speed of the code ==> to try later]. Expression derived using Medlyn et al. 2011 GCB model (see equ. 7 in Marechaux & Chave 2017, and equ. 13 in Medlyn et al. 2011 corrigendum)
        CI=CS*t_g1/(t_g1+sqrt(DS));
        
        // Farquhar model of photosynthesis
        // Solution when Rubisco activity is limiting
        float AC, AJ;
        float VcmaxTW =t_Vcmax*LookUp_VcmaxT[convT]*t_WSF_A;
        if ((CI<=0.0)||(CI-CS>0.01)) { //newIM: have replaced CI>CS by (CI-CS>0.01) to avoid error due to comparing floats (maybe there would be a best way to fix this...)
            AC = 0.0;
        }
        else AC=VcmaxTW*(CI-GammaT)/(CI+KmT);
        // Solution when electron transport rate is limiting
        float JmaxTW =t_Jmax*LookUp_JmaxT[convT]*t_WSF_A;
        float I = alpha * PPFD;
        float J = (I+JmaxTW-sqrt((JmaxTW+I)*(JmaxTW+I)-4.0*theta*JmaxTW*I))*0.5/theta;// THETA is the convexity term for electron transport rates (dimensionless, 0–1), here provided in input (default value =0.7, as in von Caemmerer 2000, but a value of 0.85 is used in Duursma' R package
        AJ=0.25*J*(CI-GammaT)/(CI+2.0*GammaT);
        if ((AJ-Rday)<1.0e-6) {
            CI = CS; // Below light compensation point
            AJ = 0.25*J*(CI-GammaT)/(CI+2.0*GammaT);
        }
        Anet=fminf(AC, AJ)-Rday; // compute the *net* assimilation rate, in micromol C02 m-2 s-1
        
        if(isnan(Anet) || isnan(GSDIVA) ) {
            Rcout << "Warning in Photosyn: " << ", Anet= " << Anet << ", GSDIVA=" << GSDIVA << "; Anet*GSDIVA=" << Anet*GSDIVA << ", AC= "<< AC <<", AJ= " << AJ << ", Rday=" << Rday  << ", CS=" << CS <<"; CI=" << CI<<", DS= " << DS << ", TLEAF= " << TLEAF << ", t_WSF_A=" << t_WSF_A << ", t_WSF= " << t_WSF <<  "; GammaT=" << GammaT << "; KmT=" << KmT << "; J=" << J << "; PPFD=" << PPFD << "; T_Rdark="<< t_Rdark << "; convT=" << convT << endl ;
        }
        
        if (isnan(Anet*GSDIVA) && Anet==0.0) { //to solve nan issue when ANet=0 and GSDIVA=inf
            Anet=1.0e-5;
        }
        
        leafFluxes outPhotosyn;
        outPhotosyn.carbon_flux=Anet; // in micromol C02 m-2 s-1
        if (fabs(GSDIVA*Anet)<1.0e-6) outPhotosyn.water_flux=1.0e-5; // GSC should not have null values
        else   outPhotosyn.water_flux=fabs(GSDIVA*Anet); // in mol CO2 m-2 s-1, the conversion to mol H20 is made in function FluxesLeaf. Note that it is here assumed that the minimal stomatal conductance (g0) is negligeable.
        // if (outPhotosyn.water_flux<g0) outPhotosyn.water_flux=g0; // just as a reminder: in both Duursma's pacakage and Vezy's MAESPA code, such line is added (in Duursma, it is written: "# Extra step here; GS can be negative", in Vezy, it is written: "! Set nearly zero conductance (for numerical reasons)". this results in no possible negative values or zero values for GSC.
        
        if(isnan(outPhotosyn.water_flux) || isnan(outPhotosyn.carbon_flux)) {
            Rcout << "Warning in Photosyn: " << ", Anet= " << Anet << ", AC= "<< AC <<", AJ= " << AJ << ", Rday=" << Rday << ", GSDIVA=" << GSDIVA << ", CS=" << CS <<"; CI=" << CI<<", DS= " << DS << ", TLEAF= " << TLEAF << ", t_WSF_A=" << t_WSF_A << ", t_WSF= " << t_WSF <<  "; GammaT=" << GammaT << "; KmT=" << KmT << "; J=" << J << "; PPFD=" << PPFD << "; T_Rdark="<< t_Rdark << "; convT=" << convT << endl ;
        }
        
        return outPhotosyn;
        
    }
    
#endif
    
    //! - Function Tree::dailyGPPleaf when the WATER option is off, Tree::dailyFluxesLeaf when WATER option is on
    //! - Returns the *daily* primary productivity (assimilation; computed from Tree::GPPleaf, or Tree::FluxesLeaf) averaged across the daily fluctuations in climatic conditions, per unit leaf area, in micromoles C/m^2/s.
    leafFluxes Tree::dailyFluxesLeaf(float PPFD, float VPD, float T, float W, float ExtinctLW, float PPFDinc, float& wf_1016) {
        float dailyA=0.0;
        float dailylT=0.0;
        
        //Rcout << "In dailyFluxesLeaf PPFD=" << PPFD << "; T=" << T << "; VPD=" << VPD << "; W=" << W << endl;
        
        for(int i=0; i < nbsteps_varday; i++) {
            
#ifdef FULL_CLIMATE
            
            float ppfd_vardaytimestep = 0.0;
            float ppfd_top_vardaytimestep = 0.0;
            float t_top_vardaytimestep = 0.0;
            float vpd_top_vardaytimestep = 0.0;
            float ppfd_inc_vardaytimestep = 0.0;
            float vpd_vardaytimestep = 0.0;
            float t_vardaytimestep = 0.0;
            float ws_vardaytimestep = 0.0;
            
            if (iter==-1) {
                ppfd_vardaytimestep = PPFD * WDailyMean_all[i]*SWtoPPFD;
                ppfd_top_vardaytimestep = WDailyMean_all[i]*SWtoPPFD;
                t_top_vardaytimestep = tDailyMean_all[i];
                vpd_top_vardaytimestep = VPDDailyMean_all[i];
                ppfd_inc_vardaytimestep =  WDailyMean_all[i]*SWtoPPFD;
                vpd_vardaytimestep = VPD*VPDDailyMean_all[i];
                t_vardaytimestep = tDailyMean_all[i] - T;
                ws_vardaytimestep = W * windDailyMean_all[i];
            } else {
            int c=(iter%nbdays)*nbsteps_varday + i;
            ppfd_vardaytimestep = PPFD * varday_light[c]*SWtoPPFD;
            ppfd_top_vardaytimestep = varday_light[c]*SWtoPPFD;
            t_top_vardaytimestep = varday_T[c];
            vpd_top_vardaytimestep = varday_vpd[c];
            ppfd_inc_vardaytimestep = PPFDinc * varday_light[c]*SWtoPPFD;
            vpd_vardaytimestep = VPD * varday_vpd[c];
            t_vardaytimestep = varday_T[c] - T;
            ws_vardaytimestep = W * varday_WS[c];
                
            }
            
            if (vpd_vardaytimestep <=0 ) {
                Rcout << "Warning in dailyFluxesLeaf !!! vpd_wardaytimestep <=0" << endl;
            }
            if (ppfd_vardaytimestep <=0 ) {
                Rcout << "Warning ppfd_vardaytimestep  <=0 in dailyFluxesLeaf !!!"<< endl;
            }
            
#else
            
            if (iter==-1) {
                float ppfd_vardaytimestep = PPFD * varday_light[i];
                float ppfd_top_vardaytimestep = WDailyMean_year * varday_light[i];
                float t_top_vardaytimestep = tDailyMean_year * varday_T[i];
                float vpd_top_vardaytimestep = VPDDailyMean_year * varday_vpd[i];
                float ppfd_inc_vardaytimestep = PPFDinc * varday_light[i];
                float vpd_vardaytimestep = VPD * varday_vpd[i];
                float t_vardaytimestep = T * varday_T[i];
                float ws_vardaytimestep = W * varday_WS[i];
            } else {
                float ppfd_vardaytimestep = PPFD * varday_light[i];
                float ppfd_top_vardaytimestep = WDailyMean * varday_light[i];
                float t_top_vardaytimestep = tDailyMean * varday_T[i];
                float vpd_top_vardaytimestep = VPDDailyMean * varday_vpd[i];
                float ppfd_inc_vardaytimestep = PPFDinc * varday_light[i];
                float vpd_vardaytimestep = VPD * varday_vpd[i];
                float t_vardaytimestep = T * varday_T[i];
                float ws_vardaytimestep = W * varday_WS[i];
            }
            
            
            
            if (vpd_vardaytimestep <=0 ) {
                Rcout << "Warning in dailyFluxesLeaf !!! i=" << i << "; vpd_vardaytimestep=" << vpd_vardaytimestep << "; VPD=" << VPD << "; varday_vpd[i]=" << varday_vpd[i] << "; ppfd_vardaytimestep=" << ppfd_vardaytimestep << "; PPFD=" << PPFD << "; varday_light[i]=" << varday_light[i] <<endl;
            }
            if (ppfd_vardaytimestep <=0 ) {
                Rcout << "Warning ppfd_vardaytimestep  <=0 in dailyFluxesLeaf !!! ppfd_vardaytimestep=" << ppfd_vardaytimestep << "; PPFD=" << PPFD << "; varday_light[i]=" << varday_light[i] << endl;
            }
            
#endif
            
            leafFluxes fluxes_vardaytimestep= FluxesLeaf(ppfd_vardaytimestep, vpd_vardaytimestep, t_vardaytimestep, ws_vardaytimestep, ExtinctLW, ppfd_top_vardaytimestep, ppfd_inc_vardaytimestep, t_top_vardaytimestep, vpd_top_vardaytimestep);
            
            dailyA += fluxes_vardaytimestep.carbon_flux;
            dailylT += fluxes_vardaytimestep.water_flux;
            if (i>5 && i<18.5) {
                wf_1016 +=fluxes_vardaytimestep.water_flux;
            }
            
            if(isnan(fluxes_vardaytimestep.carbon_flux) || isnan(fluxes_vardaytimestep.water_flux) || isnan(dailyA) || isnan(dailylT)) {
                Rcout << "Warning in dailyFluxesLeaf: " << " fluxes_vardaytimestep.carbon_flux= " << fluxes_vardaytimestep.carbon_flux << ", fluxes_vardaytimestep.water_flux= "<< fluxes_vardaytimestep.water_flux <<", dailyA=" << dailyA << ", dailylT=" << dailylT << ", i=" << i << endl ;
            }
            
            //Rcout << dailyA << endl;
            // deprecated in v.2.4.1: compute GPP only if enough light is available threshold is arbitrary, but set to be low: in full sunlight ppfd is aroung 700 W/m2, and even at dawn, it is ca 3% of the max value, or 20 W/m2. The minimum threshold is set to 0.1 W/m2
            // Future update: compute slightly more efficiently, using 3-hourly values? This will have to be aligned with climate forcing layers (e.g. NCAR)
        }
        dailyA *= inv_nbsteps_varday;
        dailylT *= inv_nbsteps_varday;
        
        
        //Rcout << "In dailyFluxesLeaf, final daily average: dailyA=" << dailyA << "; dailylT=" << dailylT << endl;
        
        leafFluxes dailyF;
        dailyF.carbon_flux =dailyA; // in micromol C02 m-2 s-1
        dailyF.water_flux = dailylT; // in micromol H20 m-2 s-1
        
        if( isnan(dailyA) || isnan(dailylT)) {
            Rcout << "Warning in dailyFluxesLeaf:" <<" dailyA=" << dailyA << ", dailylT=" << dailylT << ", PPFD=" << PPFD << endl ;
        }
        
        return dailyF;
    }
    
    
#else
    
    //! - Function Tree::GPPleaf when the WATER option is off, Tree::FluxesLeaf when WATER option is on
    //! - Includes the Farquhar model of photosynthesis and Medlyn et al. 2011 model of stomatal conductance: formula for s_fci (ci/ca) -- see also Prentice et al 2014 Ecology Letters and Lin et al 2015 Nature Climate Change; min added in order to prevent ci:ca bigger than 1 (even though Ehleringer et al 1986 reported some values above 1 (Fig3)
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
    //Rdayleaf is actually not used anymore when WATER is activated
#endif
    
    float Tree::Rdayleaf(float T) {
        int convT= int(iTaccuracy*T);
        //if(T < 0 || isnan(T) || T > 50) Rcout << t_site << " species: " << t_s->s_name << " convT: " << T << endl;
        float Rday_leaf = t_Rdark*LookUp_Rleaf[convT]; //new IM: no redundancy anymore between LookUp_Rday and LookUp_Rnight
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
        //float agb = 0.0673*pow(t_wsg*t_height*LV*t_dbh*t_dbh*LH*LH*10000.0, 0.976);
        float agb = 0.0559*t_wsg*t_height*LV*t_dbh*t_dbh*LH*LH*10000.0; // simplified assumption of cylinder, 0.0559 accounts for stem taper cf. Chave et al. 2014)
        return(agb);
    }
    
    //Calculation of the increment of dbh from assimilated carbon/biomass (in m)
    float Tree::CalcIncrementDBH(float delta_agb){
        float ddbh = fmaxf((delta_agb/(0.559*t_wsg* 1.0e6*t_dbh*LH*t_height*LV*(3.0-t_dbh/(t_dbh+t_ah)))),0.0)* NH;
        return(ddbh);
    }
    
    //Calculation of the maximum amount of carbon stored in a tree
    float Tree::CalcCarbonStorageMax(){
        // we assume non-structural carbohydrates (NSC) is around 10% of the whole tree's carbon. We assume that only half of NSC (i.e. 5% of whole tree carbon) can be remobilized, since NSC has important metabolic functions or can be stored in tissues that are no longer accessible (heartwood). So NSC for remobilization should never exceed 0.05 * above ground biomass (in units of carbon). Cf. Martínez-Vilalta 2016, Ecological Monographs
        // to calculate the 5% value, we use CalcAGB(), and reconvert kg into gram (1000.0), then convert biomass into carbon by multiplying with 0.5, and take 5%
        //newIM 2021: trees actually store a lot of NSC in roots ! and the 10% estimate account for belowground stock. Add a factor 1.25 to account for belowground biomass, with 0.25 being an estimate of root:shott ration from Ledo et al. 2018 New Phytologist (although this ratio  varies with dbh, species and environment...).
        float carbon_storage_max = 1000.0 * CalcAGB() * 0.5 * 0.05 * 1.25;
        return(carbon_storage_max);
    }
    
    //Calculation of the treefall threshold, if _BASICTREEFALL is activated
    //Slightly updated in v.3.1
    float Tree::CalcCt(){
        float dbhrealmax = t_dbhmax * 1.5;
        float hrealmax = t_mult_height * CalcHeightBaseline(t_ah, t_hmax, dbhrealmax);   // realized maximum height
        float vC_intraspecific = vC/1.5 - 1.0/(2.3 * t_mult_height) + 1.0/2.3;  //! since v.2.5: adjusting vC to intraspecific height variation. If vC was not modified, tall trees would start falling at much larger heights than smaller trees of the same species and with the same dbh, despite a much worse height/dbh ratio. The default assumption is now that the minimum onset of treefalls should be around the same height threshold irrespective of the height multiplier, but stronger assumptions would be justified too (i.e. tall trees falling more easily). The formula is derived as follows: assuming that the onset of treefall can be described by the 99.5 percentile of the sqrt(-log(uniform)) distribution, which is 2.3, we calculate the corresponding Ct_min and impose the condition that it stays equal irrespective of t_mult_height. In this case, we can solve for vC_intraspecific. This is a conservative assumption, likely a stronger dependence on dbh/height ratio would be found, but probably superseded by E-Ping's module anyways
        float Ct = fminf(float(HEIGHT-1),hrealmax*fmaxf(1.0  - vC_intraspecific * sqrt(-log(gsl_rng_uniform_pos(gslrand))),0.0));
        return(Ct);
    }
    
    //Determines leaf life span, either from empirical function or from Kikuzawa model
    void Tree::CalcLeafLifespan(){
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
        
        //float time_young=fminf(t_leaflifespan/3.0,1.0);
        float time_young=fminf(t_leaflifespan/3.0,1.0*0.08333333*iterperyear); //modified IM jan23
        float time_mature=t_leaflifespan/3.0;
        float time_old=t_leaflifespan-time_mature-time_young;
        
        t_lambda_young = 1.0/time_young;
        t_lambda_mature = 1.0/time_mature;
        t_lambda_old = 1.0/time_old;
        
    }
    
    
    void Tree::InitialiseLeafPools(){
        t_youngLA = t_LA/(t_lambda_young * t_leaflifespan);
        t_matureLA  = t_LA/(t_lambda_mature * t_leaflifespan);
        t_oldLA = t_LA/(t_lambda_old * t_leaflifespan);
        t_litter = 0.0;
    }
    
    //Determine sapwood area, limited by increase in dbh (ddbh) (in m2)
    //! - Options: fixed percentage (option _sapwood == 0) or based on tree's leaf area (option _sapwood > 0). In the case _sapwood>0, a tree cannot retroactively convert heartwood into sapwood, just because it could allocate more leaves
    //! - TODO: should there be another limit on leaf area? (i.e. if sapwood cannot grow more than ddbh increment, then leaf area should probably not be allowed to grow beyond what is reasonable through the current sapwood area)
    void Tree::UpdateSapwoodArea(float ddbh){
        if(_sapwood > 0){
            float sapwood_area_new = PI * 0.5 * ddbh * (t_dbh - 0.5 * ddbh); // correction from previous equation in v.3.0, derived from (0.5 * t_dbh) * (0.5 * t_dbh) * PI - (0.5 * (t_dbh - ddbh)) * (0.5 * (t_dbh - ddbh)) * PI, this presupposes that t_dbh has already been updated (i.e. t_dbh += ddbh). If dbh has grown entirely from zero, then the equation reduces to PI * 0.5 * dbh * 0.5 * dbh, i.e. the whole stem area
            t_sapwood_area += sapwood_area_new;
            t_sapwood_area = fminf(t_sapwood_area, 0.0001 * 2.0 * t_LA / (0.066 + 0.017 * t_height - 0.18 + 1.6 * t_wsg)); // upper bound on sapwood area, either through previously existing sapwood area (sapwood cannot grow quicker than the rest of the tree) or through the amount of sapwood needed from from Fyllas et al. 2014, based on inversion of pipe model, multiplication with 0.0001 to convert cm2 to m2
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
    
    // Updates t_height, based on t_dbh
    void Tree::UpdateHeight(){
        float height_baseline = CalcHeightBaseline(t_ah, t_hmax, t_dbh);
        t_height = fminf(t_mult_height * height_baseline,HEIGHT-1);
    }
    
    // Updates t_CR, based on t_dbh
    void Tree::UpdateCR(){
        t_CR = CalcCRBaseline(t_dbh) * t_mult_CR;
        t_CR = fmaxf(CR_min, t_CR);
    }
    
    // Updates t_CD based on t_height
    void Tree::UpdateCD(){
        //Since v.2.5, simplification of the computation of the crown depth, in accordance with the Canopy Constructor algorithm
        t_CD = CalcCDBaseline(t_height) * t_mult_CD;
        t_CD = fminf(t_CD, 0.5*t_height);
    }
    
    
    // Determines the maximum LAI that the tree should reach, given the Farquhar model, at a theoretical average day, up to which point leaves can be allocated and until which the costs (self-shading) are lower than the benefits (additional assimilation)
    //! - LAImax (maximum LAI) is assumed to lie between 0 and 10, and is such that adding more leaves results in net loss of carbon
    //! - Range of LAImax is narrowed down by bisection (sequentially halving the possible range)
    //! - Uses actually absorbed PPFD instead of incident PPFD, since leaves are not perfectly illuminated, but often in the lower canopy layers
    void Tree::CalcLAImax(){
        float LAI_lowerbound = 0.0;
        float LAI_upperbound = 10.0;
        float LAImax_temp = 0.5 * (LAI_lowerbound + LAI_upperbound);
        
        //tree LAImax to not be greater 10. 10 is also the number of iterations to get the best value. Since we narrow down in half steps the precision should be ca. 0.5^10 ~ 0.001, which should be high enough for LAImax
        for(int i=0;i<10;i++){
            float absorb_prev = LAImax_temp;
            float absorb_delta = 0.5;                                           //calculate everything, assuming a medium leaf density in the lower canopy, TODO: if the crown has a non-cylindric shape, this should probably be taken as a third of LAImax, or if LAI_gradient is activated, as 25% of LAImax
            int intabsorb = CalcIntabsorb(absorb_prev, absorb_delta);
            
#ifdef FULL_CLIMATE // if FULL_CLIMATE is defined, then the terms just below should be only the relative attenuation due to the vertical structure of the canopy. Currently, CalcLAImax is defined depending on the climate conditions on the day of birth (see iter, in dailyFluxesLeaf), and not to the climate yearly mean as before. This needs to be rethought and changed.
            
            //get PPFD, VPD, and temperature at each discretisation step
            float PPFD_LAI = LookUp_flux_absorption[intabsorb];
            float VPD_LAI = LookUp_VPD[intabsorb];
            float Tmp_LAI = LookUp_T[intabsorb];
#ifdef WATER
            int intincident = CalcIntabsorb(absorb_prev);
            float Wind_LAI = exp(-0.5*absorb_prev) ; // to be thoroughly checked and computed using a look up table as well.
            float ExtinctLW_LAI = LookUp_ExtinctLW[intincident];
            float PPFD_LAI_inc = LookUp_flux[intincident];
#endif //WATER
            
#else //FULL_CLIMATE
            
            //get PPFD, VPD, and temperature at each discretisation step
            float PPFD_LAI = WDailyMean_year * LookUp_flux_absorption[intabsorb];
            float VPD_LAI = VPDDailyMean_year * LookUp_VPD[intabsorb];
            float Tmp_LAI = tDailyMean_year - LookUp_T[intabsorb];
#ifdef WATER
            int intincident = CalcIntabsorb(absorb_prev);
            float Wind_LAI = windDailyMean_year*exp(-0.5*absorb_prev) ; // to be thoroughly checked and computed using a look up table as well.
            float ExtinctLW_LAI = LookUp_ExtinctLW[intincident];
            float PPFD_LAI_inc = WDailyMean_year * LookUp_flux[intincident];
#endif //WATER
            
#endif //FULL_CLIMATE
            
            //calculate the GPP
#ifdef WATER
            float wf=0.0;
            float GPP_LAI = Tree::dailyFluxesLeaf(PPFD_LAI, VPD_LAI, Tmp_LAI, Wind_LAI, ExtinctLW_LAI, PPFD_LAI_inc, wf).carbon_flux;
#else
            float GPP_LAI = Tree::dailyGPPleaf(PPFD_LAI, VPD_LAI, Tmp_LAI);
            float Rday_LAI = Tree::dailyRdayleaf(Tmp_LAI);
#endif
            float effLA = 0.66 * nbhours_covered * 15.7788 * timestep; //convert  from micromoles C/m^2/s into gC per m^2 of leaf per timestep by "nbhours_covered*15.7788*timestep" where 15.7788 = 3600*365.25*12/1000000 (seconds, days, and mass of carbon) and nbhours_covered is the amount of time that is covered by the daily variation file. We also assume that one third of the leaves are mature and that the rest of the leaves have half the assimilation rates, so we derive a factor 0.66
            float effLA_night = 0.83 * (24.0 - nbhours_covered) * 15.7788 * timestep;  //same as during the day, but inverse of hours covered (we assume that non-covered hours are night values), assuming that respiration rate of yound and old leaves are 75% that of mature leaves.
            
            //float effLA = 0.5 * nbhours_covered * 15.7788 * timestep; //convert  from micromoles C/m^2/s into gC per m^2 of leaf per timestep by "nbhours_covered*15.7788*timestep" where 15.7788 = 3600*365.25*12/1000000 (seconds, days, and mass of carbon) and nbhours_covered is the amount of time that is covered by the daily variation file. here we assume that the leaves that will determine the LAImax are the one at the bottom of the crown and taht these are all old leaves, with hald the assimilation of mature leaves. Note that this is not the case for phenological strategies that exchange all their leaves. So when moving to a variable t_LAImax, we should account for the leaf area of the last layer and fill it with old leaves at maximum.
            //float effLA_night = 0.75 * (24.0 - nbhours_covered) * 15.7788 * timestep;  //same as during the day, but inverse of hours covered (we assume that non-covered hours are night values), assuming that respiration rate of yound and old leaves are 75% that of mature leaves.
            
            GPP_LAI *= effLA;
            
            //get the night respiration
#ifdef FULL_CLIMATE
            int convTnight = int(iTaccuracy*tnight);
#else
            int convTnight = int(iTaccuracy*Tnight_year);
#endif
            float Rnight_LAI = t_Rdark * effLA_night * LookUp_Rleaf[convTnight];
            
            //add up the two components of leaf respiration, and multiply the result by 1.5 (fine root respiration), since in TROLL, this cannot be separated from leaf respiration
#ifdef WATER
            float Rleaf_LAI = Rtot_by_Rabove * Rtotleaf_by_Rdark*Rnight_LAI + (Rtotleaf_by_Rdark-1)*Rnight_LAI;
#else
            Rday_LAI *= effLA * 0.4; //inhibition of respiration by ca. 40%, cf. Atkin et al. 2000
            float Rleaf_LAI = Rtot_by_Rabove * (Rday_LAI + Rnight_LAI);    //Rleaf=Rday+Rnight is multiplied by 1.5 to also account for fine root respiration (cf as in Fyllas et al 2014 and Malhi 2012).
#endif
            
            //calculate effective npp
            float NPP_leaf = 0.7 * (GPP_LAI - Rleaf_LAI);   // growth respiration, v. 2.4.1: 0.75 replaced by 0.7. According to Cannell and Thornley 2000, higher values should be used only if other sources of respiration (phloem loading etc.) are explicitly accounted for. For leaf construction, typically even a lower factor is a assumed (cf. Villar & Merino 2001, avg construction costs of 1.52 gC/gC)
            
            //update boundaries
            if(NPP_leaf > 0.0) LAI_lowerbound = LAImax_temp;
            else LAI_upperbound = LAImax_temp;
            
            //now update LAImax_temp
            LAImax_temp = 0.5 * (LAI_lowerbound + LAI_upperbound);
        }
        
        t_LAImax = LAImax_temp;
        
    }
    
    // Determines the maximum LAI that the tree should reach, similar to CalcLAImax, but it directly calculates the maximum leafarea for the current light environment the tree experiences rather than for theoretical day
    //! - the idea is to compute the LAI that the tree experiences at the crown top and compare it to its maximum LAI, then the tree adjusts its LAI or leaf area accordingly
    //! - importantly, since PPFD is an exponential function of leaf density (nonlinear), averaging LAI across the whole crown would not correspond to the LAI experienced by the tree. A heterogeneous environment with a mixture of light specks and dense spots above the crown will contribute considerably more PPFD (and have a lower effective LAI) than a homogeneously filled crown. We calculate the effective LAI instead of the real LAI above the crown top by first calculating average PPFD and then converting back to LAI
    void Tree::CalcLAmax(float &LAIexperienced_eff, float &LAmax){
        float crown_area = PI * t_CR * t_CR;
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
            
            LoopLayerUpdateCrownStatistic_template(row_crowncenter, col_crowncenter, t_height, t_CR, t_CD, fraction_filled_target, shell_fromtop, GetRadiusSlope, noinput, ppfd_CA, KeepFloatAsIs, GetPPFDabove);
            
            float ppfd_experienced = ppfd_CA[0];
            float crown_area_looped = ppfd_CA[1];
#else
            int height_abovetop = int(t_height) + 1;
            int crown_intarea = GetCrownIntarea(t_CR);
            
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
                // this should not happen, potentially introduce a break or assert() here
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
        
        //in the worst case (if the difference in LAI cannot be compensated for by the tree anymore, i.e. it would have to have negative leafarea), we will get zero LA_max, and the tree will allocate everything to storage
        float LAmaxphysiology = t_LAImax * crown_area_nogaps;
        
        LAmax = fminf(LAmaxphysiology,LAdiff);
        LAmax = fmaxf(0.0, LAmax);
        
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
        
#ifdef FULL_CLIMATE
        
        float PPFD =  LookUp_flux_absorption[intabsorb];
        float VPD =  LookUp_VPD[intabsorb];
        float T =  LookUp_T[intabsorb];
#ifdef WATER
        float W = exp(-0.5*absorb_prev); // to be thoroughly re-thought, and computed using a look-up table as well -- IM June 2021
        int intincident = CalcIntabsorb(absorb_prev);
        float PPFDinc = LookUp_flux_absorption[intincident];
        float ExtinctLW = LookUp_ExtinctLW[intincident];
        float wf=0.0;
        float GPP = Tree::dailyFluxesLeaf(PPFD, VPD, T, W, ExtinctLW, PPFDinc, wf).carbon_flux;
#else
        float GPP = Tree::dailyGPPleaf(PPFD, VPD, T);
        float Rday = Tree::dailyRdayleaf(T) * 0.4; // inhibition of respiration by ca. 40%, cf. Atkin et al. 2000
#endif // WATER
        
        int convTnight = int(iTaccuracy*tnight);
        
#else // FULL_CLIMATE
        
        float PPFD = WDailyMean_year * LookUp_flux_absorption[intabsorb];
        float VPD = VPDDailyMean_year * LookUp_VPD[intabsorb];
        float T = tDailyMean_year - LookUp_T[intabsorb];
#ifdef WATER
        float W = windDailyMean_year*exp(-0.5*absorb_prev); // to be thoroughly re-thought, and computed using a look-up table as well -- IM June 2021
        int intincident = CalcIntabsorb(absorb_prev);
        float PPFDinc = WDailyMean_year * LookUp_flux_absorption[intincident];
        float ExtinctLW = LookUp_ExtinctLW[intincident];
        float GPP = Tree::dailyFluxesLeaf(PPFD, VPD, T, W, ExtinctLW, PPFDinc).carbon_flux;
#else
        float GPP = Tree::dailyGPPleaf(PPFD, VPD, T);
        float Rday = Tree::dailyRdayleaf(T) * 0.4; // inhibition of respiration by ca. 40%, cf. Atkin et al. 2000
#endif // WATER
        
        int convTnight = int(iTaccuracy*Tnight_year);
        
#endif // FULL_CLIMATE
        float Rnight = t_Rdark * LookUp_Rleaf[convTnight];
        
#ifdef WATER
        float GPP_effective = (GPP - Rtot_by_Rabove * Rtotleaf_by_Rdark * Rnight + (Rtotleaf_by_Rdark-1) * Rnight);
#else
        float GPP_effective = (GPP - Rtot_by_Rabove * Rday - Rtot_by_Rabove * Rnight); // in gC m^-2 day-1  we assume that Rday/Rnight decreases linearly as well, so we will use a GPP_eff = GPP - Rday;
#endif
        
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
        CalcNPP();

        
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
            
            //if NPP is still negative, even after compensating for it through stored carbon, then carbon starvation sets in.
            if(t_NPP<0.0){
                //t_NPPneg++; //newIM 2021: if NPP still negative, this means that the tree has completly depleted its minimal carbon stock pool, ie. is not viable anymore (Martinez-Vilalta et al. 2016) ==> death at the next timestep (the previous version made a mix between the use of NPPneg (that was introduced in absence of a variable for carbon storage and a dynamic monitoring and allocation to this carbon storage pool: NPPneg is not needed anymore when _LA_regulation >0)
                t_NPP=0.0;
                //v.2.3.0 -- Line of code below was odd. If NPP <0.0, then to ensure C balance it should be simply reset to NPP=0 at this stage
                //t_NPP=t_GPP - 1.5*(t_Rday+t_Rnight+t_Rstem); REMOVED AS OF v.2.3.0.a4
                //NPP allocation to leaves
                UpdateLeafDynamics();
            } else {
                //t_NPPneg=0; //newIM 2021: no use of NPPneg anymore when _LA_regulation >0
                //in v.2.4.1: allocation to leaves is done before updating biometry
                //idea is: at least leaves need to be sustained, otherwise the tree cannot come back from carbon stress
                //NPP allocation to leaf, storage, wood and tree size increment
                UpdateLeafDynamics();
                UpdateTreeBiometry();
            }
        }
        
        UpdateVolumeDensity();
        
#ifdef WATER
        if (iter == (nbiter-90)) OutputTreeStandard(output[28]);
        if (iter == (nbiter-45)) OutputTreeStandard(output[29]);
        if (iter == (nbiter-1)) OutputTreeStandard(output[30]);
        
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
    //! - but this is not the case when WATER is activated: indeed Rday is now computed within Photosyn and FluxesLeaf functions and thus included in GPP computation.
    void Tree::CalcRespGPP(){
        
#ifdef WATER
        float tree_transpiration_1016=0.0;
#endif

        
        if(t_LA > 0.0){
#ifdef WATER
#else
            if(_GPPcrown){
                // v.2.3.1 -- fast GPP calculation option.
                float PPFD = 0.0, VPD = 0.0, Tmp = 0.0, leafarea_layer;
                Fluxh(int(t_height)+1, PPFD, VPD, Tmp, leafarea_layer);
                t_GPP = Tree::dailyGPPcrown(PPFD, VPD, Tmp, t_LAI);
                t_Rday = Tree::dailyRdayleaf(Tmp);
            } else {
#endif
                // Photosynthesis and respiration, set to zero
                t_GPP=0.0;
#ifdef WATER // the computation of leaf respiration during the day is accounted for in the Photosyn function
#else
                t_Rday=0.0;
#endif
                
                int crown_above_base=int(t_height-t_CD)+1; // for flux above crown base
                int crown_above_top=int(t_height)+1;                // for flux above crown top
                
#ifdef CROWN_UMBRELLA
                int h_stop = max(crown_above_base,crown_above_top-3);
#else
                int h_stop = crown_above_base;
#endif
                float leafarea_cumulated = 0.0;
                
#ifdef WATER
                float leafarea_toprounding=0.0; //newIM
                float MeanDCELLHeight=Canopy_height_DCELL[site_DCELL[t_site]];
#endif
                
                for(int h = crown_above_top; h >= h_stop ; h--) {
                    float PPFD = 0.0, VPD = 0.0, Tmp = 0.0, leafarea_layer = 0;
#ifdef WATER
                    float PPFD_incident = 0.0, ExtinctLW=0.0;
                    //float W=WS*exp(-0.5*LAI_DCELL[h][site_DCELL[t_site]]); // computation of wind speed, which is here assumed to declined exponentially with the cumulative LAI within a given neighborhood (here taken as the tree's dcell) (see equation B11 in Medvigy et al. 2009; see Leuning et al. 1995 PCE equ. E2). We could alternatively used the aerodynamic momentum transfer model (Monteith and Unsworth 2008) using Lorey'sheight (see E-Ping's second manuscript) -- to be discussed. // to be computed with a lookup table maybe, and within Fluxh probably.
                    int convHratio= int(iHaccuracy*h/MeanDCELLHeight);
                    float W=TopWindSpeed_DCELL[site_DCELL[t_site]]*LookUp_Wind[convHratio];
                    if (W<=0) {
                        Rcout << " Wind=" << W << " h=" << h << " MeanDCELLHeight=" << MeanDCELLHeight <<" convHratio=" << convHratio << " LookUp_Wind[convHratio]=" << LookUp_Wind[convHratio] << endl;
                    }
                    
                    //Rcout << " Wind=" << W << " h=" << h << " MeanDCELLHeight=" << MeanDCELLHeight <<" convHratio=" << convHratio << " LookUp_Wind[convHratio]=" << LookUp_Wind[convHratio] << " TopWindSpeed_DCELL[site_DCELL[t_site]]=" << TopWindSpeed_DCELL[site_DCELL[t_site]] << endl;
                    
                    Fluxh(h,PPFD, VPD, Tmp, leafarea_layer, PPFD_incident, ExtinctLW);
#else
                    
                    Fluxh(h,PPFD, VPD, Tmp, leafarea_layer);
#endif
                    
#ifdef WATER
                    if(leafarea_toprounding>0) {
                        //Rcout << "Rounding when h=" <<h << " with crown_above_top=" << crown_above_top << " and leafarea_toprounding=" << leafarea_toprounding << endl;
                        leafarea_layer+=leafarea_toprounding;
                        leafarea_toprounding=0.0;
                    }
                    
                    if(leafarea_layer < 0.0){
                        Rcout << "Warning leafarea <=0 in CalcRespGPP !! PPFD=" <<PPFD << "; leafarea_layer=" << leafarea_layer << " when h=" <<h << " with crown_above_top=" << crown_above_top << " and leafarea_toprounding=" << leafarea_toprounding << endl;
                    }
                    
                    if(leafarea_layer > 0.0 && PPFD>0){
                        if (PPFD <0) {
                            Rcout << "Warning PPFD <0 in CalcRespGPP !! PPFD=" <<PPFD << endl;
                        }
                        float water_flux_1016=0.0;
                        leafFluxes dailyF = Tree::dailyFluxesLeaf(PPFD, VPD, Tmp, W, ExtinctLW, PPFD_incident, water_flux_1016);
                        t_GPP += leafarea_layer * dailyF.carbon_flux;
                        t_transpiration += leafarea_layer * dailyF.water_flux;
                        tree_transpiration_1016 += leafarea_layer*water_flux_1016;
                        
                        if(isnan(t_GPP) || isnan(t_transpiration) || t_transpiration < 0.0) Rcout << "Problem at site: " <<  t_site << ", t_GPP= " << t_GPP << ", transpiration= "<< t_transpiration <<", leafarea_layer= " << leafarea_layer << ", PPFD=" << PPFD << ", VPD= " << VPD << ", T= " << Tmp << ", t_WSF_A=" << t_WSF_A << ", t_WSF= " << t_WSF << endl ;
                        
#else
                        t_GPP += leafarea_layer * Tree::dailyGPPleaf(PPFD, VPD, Tmp);
                        //if(isnan(t_GPP) || t_GPP < 0.0 || PPFD == 0.0) Rcout << "Problem at site: " <<  t_site << " leafarea_layer: " << leafarea_layer << " GPP: " << t_GPP << " PPFD: " << PPFD << " VPD: " << VPD << " T: " << Tmp << endl << endl;
#endif
                        
#ifdef WATER
#else
                        t_Rday += leafarea_layer * Tree::dailyRdayleaf(Tmp);
#endif
                        leafarea_cumulated += leafarea_layer;
                        //if(t_dbh > 0.2) Rcout << crown_above_top - h << "PPFD: " << PPFD << " VPD: " << VPD << " Tmp: " << Tmp << " leafarea_layer: " << endl;
                        
#ifdef WATER
                    } else {
                        if (leafarea_layer > 0.0) leafarea_toprounding=leafarea_layer;
                    }
#endif
                }
                
                // Averaging across layers. To check consistency, leafarea_layer can be added up and the sum compared to t_LA
                float ileafarea_cumulated;
                if(leafarea_cumulated > 0.0) ileafarea_cumulated= 1.0/leafarea_cumulated;
                else ileafarea_cumulated = 0.0;
                
                t_GPP   *= ileafarea_cumulated;
#ifdef WATER
                t_transpiration *= ileafarea_cumulated ;
                tree_transpiration_1016 *=ileafarea_cumulated ;
#else
                t_Rday  *= ileafarea_cumulated;
            }
#endif
        }
        
        float effLA = 0.5 * (t_LA+t_matureLA) * nbhours_covered * 15.7788 * timestep; // we convert  from micromoles C/m^2/s into gC per m^2 of leaf per timestep by "nbhours_covered*15.7788*timestep" where 15.7788 = 3600*365.25*12/1000000 (seconds, days, and mass of carbon) and nbhours_covered is the amount of time that is covered by the daily variation file.
        float effLA_night = (0.75 *t_LA+0.25*t_matureLA) * (24.0 - nbhours_covered) * 15.7788 * timestep; // same as during the day, but inverse of hours covered (we assume that non-covered hours are night values), and assuming that respiration of young and old leaves are 75% the one of mature leaves:  Kitajima et al. 2002 found no or small reductions in respiration with leaf age, Reich et al. 2009 did (Rdark/Amax constant), but often respiration declines are less steep than photosynthetic capacity, cf. Villar et al. 1995.
        
        t_GPP *=effLA;
        
#ifdef WATER
        float effLAT=(0.75*t_LA+0.25*t_matureLA)*nbhours_covered * 23.652*0.000001* timestep; // assuming that the decline in stomatal conductance of young and old leaves is less than for photosynthetic capacities (cf. ALbert et al. 2018)(75% the one of mature leaves), ie the water use efficiencies is lower of young and old leaves. Note that idally the Farquhar model should be applied to each leaf cohorts separately... 23.652=18.10^-6 * 3600*365, where 18.10^-6 is to convert micromol H20 into g, 3600*365 to convert s into year (as timestep is given in year); and 0.000001 is to convert g in m3.
        t_transpiration*=effLAT; // this is the amount of water transpired by the tree during the timestep in m3.  CHECK: from our WILT data of sapflow of several canopy mature trees, and assuming a constant sapwood thickness of 4cm, this can be 5-110 liter/day, ie. 0.005-0.11 m3/day. See also values in Granier et al. 1996 (0.250-0.3 m3/day for a big dominant canopy tree), or Andrade et al. 1998 Oecologia (46-379 kg/day, ie. 0.046-0.379 m3/day). Note, in this version, we never account for nighttime transpiration... to be discussed.
        tree_transpiration_1016 *=(0.75*t_LA+0.25*t_matureLA)*0.0324*0.000001; // where 0.0324= 18.10^-6*3600*0.5
        transpiration_1016 += tree_transpiration_1016;
#endif
        
        int convT= int(iTaccuracy*tDailyMean); // temperature data at a resolution of Taccuracy=0.1°C -- stored in lookup tables ranging from 0°C to 50°C ---
        int convTnight= int(iTaccuracy*tnight); // temperature data at a resolution of Taccuracy=0.1°C -- stored in lookup tables ranging from 0°C to 50°C ---
        
        t_Rstem = t_sapwood_area * (t_height-t_CD) * 0.5*(LookUp_Rstem[convT]+LookUp_Rstem[convTnight]);
        
        
        // the following two lines should maybe be revised, because the decline in respiration might not be the same as in photosynthetic activity. Kitajima et al. 2002 found no or small reductions in respiration with leaf age, Reich et al. 2009 did (Rdark/Amax constant), but often respiration declines are less steep than photosynthetic capacity, cf. Villar et al. 1995
        t_Rnight = t_Rdark * effLA_night * LookUp_Rleaf[convTnight];
#ifdef WATER
#else
        t_Rday *= effLA * 0.4; // inhibition of respiration by ca. 40%, cf. Atkin et al. 2000
#endif
    }
    
    
#ifdef WATER
    
    void Tree::CalcNPP(){
    t_NPP = 0.7 * (t_GPP - Rtot_by_Rabove * (Rtotleaf_by_Rdark*t_Rnight+t_Rstem)+(Rtotleaf_by_Rdark-1)*t_Rnight); // t_GPP already accounts for daytime leaf respiration. would be good to go back to total respiration computation whith this new scheme (here is a succession of approximation..)
    }
    
#else

    
    void Tree::CalcNPP(){
        t_NPP = 0.7 * (t_GPP - Rtot_by_Rabove * (t_Rday+t_Rnight+t_Rstem)); // growth respiration, v. 2.4.1: 0.75 replaced by 0.7. According to Cannell and Thornley 2000, higher values should be used only if other sources of respiration (phloem loading etc.) are explicitly accounted for. For leaf construction, typically even a lower factor is a assumed (cf. Villar & Merino 2001, avg construction costs of 1.52 gC/gC)
        // Rleaf=Rday+Rnight is multiplied by 1.5 to also account for fine root respiration (cf as in Fyllas et al 2014 and Malhi 2012); Rstem is multiplied by 1.5 to account for coarse root respiration (according to the shoot root biomass ratio of 0.2 - Jérôme's paper in prep- and also to branch respiration (Meir & Grace 2002, Cavaleri 2006, Asao 2015).
    }
    
#endif
    
    
    
    
    
    
    
    //####################################################
    // Leaf dynamics and C allocation, called by Tree::Growth
    //#####################################################
    //! - NPP allocation to leaves
    //! - In this current scheme of leaf demography and phenology in three leaf age classes: only the old leaves generate litterfall, and the dynamic of leaves cycle is generated by the dynamic of NPP, with a total leaf biomass varying - as opposed to De Weirdt et al 2012 in ORCHIDEE, but as in Wu et al 2016 but importantly without prescribing litterfall-
    void Tree::UpdateLeafDynamics() {
        float SLA=1.0/t_LMA; // newIM, but should be added as a tree variable to avoid multiple computation
        
        float ileafdem_resolution = 1.0/float(leafdem_resolution);
        float flush = 2.0 * fmaxf(t_NPP,0.0) * falloccanopy * 0.68 * SLA; // 0.68 is the fraction of NPP allocated to leaves (the other 32% being allocated to twigs and fruits), retrieved from Chave et al. 2008, 2010.
        // fine resolution of flush
        float flush_fine = ileafdem_resolution * flush;
        float lambda_young = ileafdem_resolution * t_lambda_young;
        float lambda_mature = ileafdem_resolution * t_lambda_mature ;
        float lambda_old = ileafdem_resolution * t_lambda_old ;
        
        if(_LA_regulation == 0){
            // leaf demography, without any dynamic leaf area regulation
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
            // the main idea is that the fixed fractions with which allocation to leaves and stem are modelled ("falloccanopy", "fallocwood", as fractions of newly assimilated carbon) are only regulative, i.e. they apply if the tree grows in sunlight without much constraint and still has space for new leaves. If a deviation from these conditions occurs, then the allocation patterns will change. 1) Reaching maximum leaf area: If a tree has filled its whole crown with leaves and any additional tree leaf would only result in more self-shading and a net loss of carbon, then the tree will only allocate leaves for keeping up the maximum leaf area and a) allocate excess carbon to a non-structural carbon (NSC) storage b) or allocate it to growth in diameter, if the storage is full already 2) Small or no potential for photosynthesis (maximum leaf area is smaller than the tree's actual leaf area, down to 0). Then, the tree will not allocate any carbon to leaf construction and reserve carbon mostly for respiration, storage or stem growth, until a gap appears. The module as a whole prioritizes leaf upkeep over growth, which is a conversative strategy. In reality trees or species may have more aggressive strategies (i.e. growing faster when shaded), but this would be worth a research project on its own
            // first, get LAmax, i.e. the maximum of leafarea that the tree could allocate before creating too much self-shading
            float LAIexperienced_eff;
            CalcLAmax(LAIexperienced_eff, t_LAmax);
            
            
#ifdef PHENO_DROUGHT
            
            /* version where the multiplier modifies leafarea_max, similarly to Xu et al. 2016: drawbacks: no possibilities for simultaneous increase of leaf production and old leaves litterfal.
             if (t_phi_root < S[t_sp_lab].s_tlp) t_Ndays_dry++;
             else t_Ndays_dry=0;
             if (t_phi_root > 0.5*S[t_sp_lab].s_tlp) t_Ndays_wet++;
             else t_Ndays_wet=0;
             if (t_Ndays_dry >=5) t_pheno_factor -=0.05;
             if (t_Ndays_wet >=5) t_pheno_factor +=0.05;
             t_pheno_factor=fmaxf(0.0, fminf(1.0, t_pheno_factor));
             t_LAmax = fmaxf(0.0,t_pheno_factor*t_LAmax);
             */
            
            /* version where the multiplier modifies lambda_old, ie residence time in old leaf pool, more similar to Chen et al. 2020, and could be supported by a less negative TLP of old leaves than younger leaves. */
            // but still using an approach similar to Xu et al. 2016, ie. counting number of "dry days" to trigger an increase of old leaf fall
            float Thres = fminf(pheno_thres*S[t_sp_lab].s_tlp, -0.01*t_height-0.02);
            if (t_phi_root < Thres) t_Ndays_dry++;
            else t_Ndays_dry=0;
            if (t_phi_root > Thres) t_Ndays_wet++;
            else t_Ndays_wet=0;
            if (t_Ndays_dry >=1) t_pheno_factor -=pheno_delta;
            if (t_Ndays_wet >=1) t_pheno_factor +=pheno_delta;
            t_pheno_factor=fmaxf(0.001, fminf(1.0, t_pheno_factor));
            lambda_old = fmin(1.0, fmaxf(0.0,lambda_old/t_pheno_factor));
            t_LAmax = fmaxf(0.0,t_LAmax);
            
#else
            t_LAmax = fmaxf(0.0,t_LAmax);
#endif
            
            // carbon intended for tree growth, mobilized as if it was used for flushing leaves (in case it is needed, if not it will be backconverted at the end of the procedure), in leaf area units
            float flush_biometry = 2.0 * fmaxf(t_NPP,0.0) * fallocwood * 0.6 * SLA;   // new in v. 2.4.0: only 60% of woody npp is actually used for construction, the rest is for branch fall repair (cf. Malhi et al. 2011)
            float flush_biometry_fine = ileafdem_resolution * flush_biometry;
            
            // storage carbon, mobilized as if it was used for flushing leaves (in case it is needed, if not, backconversion at end of procedure), in leaf area units
            float flush_storage = 2.0 * t_carbon_storage * SLA;
            float flush_storage_fine = ileafdem_resolution * flush_storage;
            
            // We have temporarily converted all carbon into carbon for leaf flushing, so we set the pools to zero, they will be filled up again later with all the carbon that was not required. Also overall leaf litter is set to zero
            t_carbon_biometry = 0.0;
            t_carbon_storage = 0.0;
            t_litter = 0.0;
            
            // get the maximum leafarea the tree can currently support
            // given that we assume that the leaf density environment only changes once every iteration, this can also be calculated only once every iteration
            
            //float new_young = 0.0;
            for (int i = 0; i < leafdem_resolution; i++){
                float new_litter = lambda_old * t_oldLA ;
                
                if (new_litter <0) {
                    Rcout << "Warning in UpdateLeafDynamics, newlitter <0, lambda_old=" << lambda_old << " t_oldLA=" << t_oldLA << " t_pheno_factor=" << t_pheno_factor << endl;
                }
                
                // first check whether tree can still support more leaves (does not have excess leafarea)
                // then calculate the flush needed, which is at least the amount of leaves to keep up the current leafarea
                
                ///* newIM 2021: modification
                
                float la_excess = t_LA - t_LAmax;
                float flush_needed = new_litter;                // flush_needed is set equal to litter, but will be reduced, if there is excess leaves on the tree
                
                float new_young;
                // if leafarea does not exceed the maximum leafarea, just allocate all the flush, otherwise adjust flush_needed
                if(la_excess < 0.0){
                    la_excess = 0.0;
                    new_young = flush_fine;  // incoming flux
                } else {
                    flush_needed = fmaxf(flush_needed-la_excess,0.0); //newIM 2021
                    new_young = fminf(flush_fine,flush_needed); // incoming flux
                }
                
                //Rcout << "flush_needed=" << flush_needed << " t_LAmax=" << t_LAmax << " t_LA=" << t_LA << " new_litter=" << new_litter << " new_young=" << new_young << endl;
                
                
                //End modification newIM 2021*/
                
                /*new version newIM 2021
                 
                 float flush_needed=t_LAmax - t_LA + new_litter;
                 float new_young= fminf( fmaxf(flush_needed, 0), flush_fine);
                 
                 //Rcout << "flush_needed=" << flush_needed << " t_LAmax=" << t_LAmax << " t_LA=" << t_LA << " new_litter=" << new_litter << " new_young=" << new_young << endl;
                 
                 end new version newIM 2021*/
                
                // subtract from flush whatever has been allocated
                flush -= new_young;
                
                // if flush needed is higher than incoming flush, then get more flush from carbohydrates usually reserved for diameter growth or storage
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
                
                if (t_litter <0 || t_oldLA <0) {
                    Rcout << "Warning in UpdateLeafDynamics, new_litter=" << new_litter << ", lambda_old=" << lambda_old << " t_oldLA=" << t_oldLA << " t_pheno_factor=" << t_pheno_factor << " t_LMA=" << t_LMA << " new_old=" << new_old << " t_Ndays_dry=" << t_Ndays_dry << " t_Ndays_wet=" << t_Ndays_wet << endl;
                }
                
                
                t_LA = t_youngLA + t_matureLA + t_oldLA ;
            }
            
            // This is to ensure that trees do not get infinitely small leaf areas (let's assume minimum size of 2.5cm * 2cm = 0.0005 m2)
            if(t_LA < 0.0005) t_LA = 0.0;
            
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
        //! t_ddbh = flor( volume* 4.0/( 3.0*PI*t_dbh*LH*t_height*LV ) )* NH;
        
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
        
        
#ifdef Output_ABC
        S[t_sp_lab].s_dbhmax_realized = fmaxf(S[t_sp_lab].s_dbhmax_realized,t_dbh);
#endif
    }
    
    void Tree::UpdateVolumeDensity(){
        float crown_area = PI*t_CR*t_CR;
        float crown_area_nogaps = GetCrownAreaFilled(crown_area);
        t_LAI = t_LA/crown_area_nogaps;
    }
    
    //####################################################
    // Tree death, called by Tree::Update
    //####################################################
    //! - This function records basic properties of a tree that has died and empties the variables at the tree site.
    //! - It does not, however, apply the destructor to the tree object
    void Tree::Death() {
        
#ifdef TRACK_INDIVIDUALS
        if(t_timeofyear_born >= 0){
            float agb = 1000.0 * CalcAGB();
            if(t_dbh >= 0.1){
                output_track[1] << t_site << "\t" << t_timeofyear_born << "\t" << iter << "\t" << t_age << "\t" << t_seedsproduced_sumyear << "\t" << t_seedsproduced << "\t" << t_time_carbonstarvation_year << "\t" << t_time_carbonstarvation << "\t" << t_dbh << "\t" << t_dbh - t_dbh_tracked << "\t" << t_height << "\t"  <<  t_height - t_height_tracked << "\t" << t_CR << "\t"  <<  t_CR - t_CR_tracked << "\t" << agb << "\t" << agb - t_agb_tracked << "\t" << t_GPP_sumyear << "\t" << t_GPPsquared_sumyear  << "\t" << t_NPP_sumyear << "\t" << t_NPPsquared_sumyear << "\t" << t_Rday_sumyear << "\t" << t_Rnight_sumyear << "\t" << t_Rstem_sumyear << "\t" << t_LAIabove_effavgyear<< "\t" << t_carbon_storage_avgyear << endl;
            }
            
            output_track[2] << t_site << "\t" << t_timeofyear_born << "\t" << iter << "\t" << t_age << "\t" << t_seedsproduced << "\t" << t_time_carbonstarvation << "\t" << t_dbh << "\t" << t_height << "\t" << t_CR << "\t" << agb << "\t" << t_GPPcum << "\t" << t_NPPcum << "\t" << t_LAIcum << "\t" << t_LAIeffcum << "\t" << t_GPPsquared_cum << "\t" << t_NPPsquared_cum << "\t" << t_LAIsquared_cum  << "\t" << t_LAIeffsquared_cum << endl;
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
        if(_OUTPUT_extended) {
            if(iter == 2) output[23] << "N\t" << t_sp_lab << "\t" << t_dbh << "\t" << t_age << "\t" << t_height <<  "\n";
            if(iter == int(nbiter/2)) output[24]<< "N\t" << t_sp_lab << "\t" << t_dbh << "\t" << t_age << "\t" << t_height <<  "\n";
            if(iter == int(nbiter-1)) output[25]<< "N\t" << t_sp_lab << "\t" << t_dbh << "\t" << t_age << "\t" << t_height <<  "\n";
        }
        
        t_sp_lab = 0;
        t_age = 0;
        t_hurt = 0;
        t_NPP=t_GPP=t_Rday=t_Rnight=t_Rstem=0.0; //! new v.2.3
        t_dbh = t_height = t_CR = t_CD= 0.0;
        t_CrownDisplacement = 0;
        
        if(_BASICTREEFALL) t_Ct = 0.0;
        
#ifdef Output_ABC
        t_dbh_previous = 0.0;
#endif
        
    }
    
    //################################
    // Seed dispersal, called by UpdateSeeds
    //#################################
    //! - This routine implements the reproduction stage (nbs seeds are produced per tree) and dispersal of the trees
    //! - Dispersal is equiprobable in all direction and normally distributed (for 2D normal distribution, the absolute distance follows a Rayleigh distribution)
    //! - Reproduction only occurs for mature trees
    //! - New v.2.1 threshold of maturity is defined as a size threshold (and not age as before), following Wright et al 2005 JTE
    void Tree::DisperseSeed(){
        if(t_dbh >= t_dbhmature){
            int nbs;
            if(_SEEDTRADEOFF) nbs=int(t_NPP*2.0*falloccanopy*0.08*0.5*(S[t_sp_lab].s_iseedmass)); //some multiplications could be avoided in this line.
            else nbs=nbs0*t_multiplier_seed;
            //else nbs=int(t_NPP*2*falloccanopy*0.08*0.5); // test 17/01/2017: use a factor to translate NPP into seeds produced, but not species specific, not linked to mass of grains
            for(int i=0;i<nbs;i++){
                // Loop over number of produced seeds
                //float rho = 2.0*((t_s->s_ds)+t_CR)*float(sqrt(fabs(log(genrand2()*iPi))));    //! s_ds is mean seed dispersal distance. Dispersal distance rho: P(rho) = rho*exp(-rho^2)
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
            //Fluxh(int(t_height)+1);            //strangly, t_PPFD was not updated at the beginning of Tree::Update (as I here suggest to do now),but only in Tree::Growth and CalcRespGPP, even though t_PPFD was already used in the first part of Tree::Update (death).
            //End new in v3.0 IM
            Water_availability();                   //here, t_phi_root and WSF are updated, which is needed to compute Deathrate, carbon assimilation and transpiration
#endif
            //v.2.4.0: outputs have been moved to Death() function
            if(_NDD)
                death = int(gsl_rng_uniform(gslrand)+DeathRateNDD(t_dbh, t_NPPneg, t_NDDfield[t_sp_lab]));
            else
#ifdef WATER  // note that I did not include a version with both drought-induced mortality/carbon starvation and NDD effect on mortality, to be done if needed.
                //death = int(gsl_rng_uniform(gslrand)+DeathRate(t_dbh, t_NPPneg, t_phi_root));
                if (_LA_regulation==0) death = int(gsl_rng_uniform(gslrand)+DeathRate(t_dbh, t_NPPneg, t_phi_root));
                else death = int(gsl_rng_uniform(gslrand)+DeathRate(t_dbh, t_carbon_storage, t_phi_root)); // newIM 2021: directly use the t_carbon_stoarge variable instead of NPPneg
#else
            if (_LA_regulation==0) death = int(gsl_rng_uniform(gslrand)+DeathRate(t_dbh, t_NPPneg));
            else death = int(gsl_rng_uniform(gslrand)+DeathRate(t_dbh, t_carbon_storage)); // newIM 2021: directly use the t_carbon_storage variable instead of NPPneg
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
        float h_true = t_height*LV;
        h_int = int(h_true*NH);
        row0=t_site/cols;
        col0=t_site%cols;
        
        //update of Thurt field at the site of the tree, for consistency
        //Thurt[0][t_site+sites] = max(int(t_height),Thurt[0][t_site+sites]);
        //fallen stem destructs other trees
        for(int h=1;h<h_int;h++) {                      // loop on the fallen stem (horizontally)
            xx=int(fmaxf(col0+h*cos(angle),0.0));          // get projection in col (= xx) direction, where xx is absolute location
            if(xx<cols){
                yy=   int(row0+h*sin(angle));         // get projection in row (= yy) direction, where yy is absolute location
                Thurt[0][xx+(yy+rows)*cols] = max(int(t_height),int(Thurt[0][xx+(yy+rows)*cols]));
                // Thurt[0] where the stem fell, calculation: xx+(yy+rows)*cols= xx + yy*cols + rows*cols = xx + yy*cols + sites / NEW in v.2.4: addition of damage instead of setting equal in order to account for cumulative damage (several treefalls hitting the same site)
            }
        }
        
        //fallen crown destructs other trees, less damaging than stem
        xx=col0+int((h_true*NH-t_CR)*cos(angle));
        yy=row0+int((h_true*NH-t_CR)*sin(angle));
        r_int = int(t_CR);
        for(int col=max(0,xx-r_int);col<min(cols,xx+r_int+1);col++) { // loop on the fallen crown (horizontally)
            for(int row=max(0,yy-r_int);row<min(rows,yy+r_int+1);row++) {
                if((col-xx)*(col-xx)+(row-yy)*(row-yy)<r_int*r_int) Thurt[0][col+(row+rows)*cols] = max(int((t_height-t_CR*NV*LH)*0.5),int(Thurt[0][col+(row+rows)*cols])) ; // less severe damage than stem / NEW in v.2.4: max() or addition of damage instead of setting equal in order to account for cumulative damage (several treefalls hitting the same site)
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
                (S[t_sp_lab].s_sum10)++;
                S[t_sp_lab].s_ba10 += t_dbh*LH*t_dbh*LH*3.1415*0.25;
            }
            if(t_dbh*LH >= 0.3) (S[t_sp_lab].s_sum30)++;
            S[t_sp_lab].s_ba += t_dbh*LH*t_dbh*LH*3.1415*0.25;
            S[t_sp_lab].s_npp += t_NPP*1.0e-6;
            S[t_sp_lab].s_gpp += t_GPP*1.0e-6;
            float agb = CalcAGB();
            S[t_sp_lab].s_agb += agb;
            S[t_sp_lab].s_rday += t_Rday*1.0e-6;
            S[t_sp_lab].s_rnight += t_Rnight*1.0e-6;
            S[t_sp_lab].s_rstem += t_Rstem*1.0e-6;
            S[t_sp_lab].s_litterfall += t_litter*1.0e-6;
            
#ifdef WATER
            int crown_top = int(t_height);
            int crown_base = int(t_height - t_CD);
            float grad= 1/float(crown_top - crown_base+1);
            for (int l=crown_base; l<(crown_top+1); l++) {
                LAI_young[l]+=t_youngLA*grad;
                LAI_mature[l]+=t_matureLA*grad;
                LAI_old[l]+=t_oldLA*grad;
            }
            
            abund_phi_root += t_phi_root;
            if (t_dbh*LH >= 0.1) abund10_phi_root += t_phi_root;
            agb_phi_root += agb*t_phi_root;
#endif
            
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
        output << iter << "\t" << t_site << "\t" << t_sp_lab << "\t" << t_height << "\t" << t_dbh << "\t" << t_litter << "\t" << t_age << "\t" << t_LA << "\t" << t_youngLA<< "\t" << t_matureLA << "\t" << t_oldLA << "\t" << t_CR << "\t" << t_CD <<"\t" << t_GPP  <<"\t" << t_NPP <<"\t" << t_Rstem <<"\t" << t_Rnight << "\t" << LAI3D[int(t_height)][t_site+SBORD] << "\t" << LAI3D[int(t_height-t_CD)+1][t_site+SBORD] << "\t" << t_root_depth << "\t" << t_phi_root << "\t" << t_WSF << "\t" << t_WSF_A << "\t" << t_transpiration << "\t" << t_LAImax << "\t" << t_LAmax ;
        for (int l=0; l<nblayers_soil; l++) output << "\t" << t_root_biomass[l];
        for (int l=0; l<nblayers_soil; l++) output << "\t" << t_soil_layer_weight[l];
        output << endl;
    }
    // Standard outputs during the simulation -- written to screen in real time
    void Tree::OutputTreeStandard(){
        Rcout << iter << "\t" << t_site << "\t" << t_sp_lab << "\t" << t_height << "\t" << t_dbh << "\t" << t_litter << "\t" << t_age << "\t" << t_LA << "\t" << t_youngLA<< "\t" << t_matureLA << "\t" << t_oldLA << "\t" << t_CR << "\t" << t_CD <<"\t" << t_GPP  <<"\t" << t_NPP <<"\t" << t_Rstem <<"\t" << t_Rday  <<"\t" << t_Rnight << "\t"  << LAI3D[int(t_height)][t_site+SBORD] << "\t" << LAI3D[int(t_height-t_CD)+1][t_site+SBORD] << "\t" << t_root_depth << "\t" << t_phi_root << "\t" << t_WSF;
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
        if(iter >= 6000 && iter < 6012) t_timeofyear_born = iter%iterperyear;
        else t_timeofyear_born = -1;
        
        if(t_timeofyear_born >= 0){
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
            t_height_tracked = t_height;
            t_CR_tracked = t_CR;
            t_agb_tracked = 1000.0 * CalcAGB();
            
            output_track[0] << t_site << "\t" << t_timeofyear_born << "\t" << t_site%cols << "\t" << t_site/cols << "\t" << t_s->s_name << "\t" << t_dbh << "\t" << t_CR << "\t" << t_height << "\t" << t_agb_tracked << "\t" << t_mult_CR << "\t" << t_mult_height << "\t" << t_wsg << "\t" << t_Nmass << "\t" << t_Pmass << "\t" << t_LMA << "\t" << t_dev_wsg << "\t" << t_mult_N << "\t" << t_mult_P << "\t" << t_mult_LMA << "\t" << t_Vcmax << "\t" << t_Jmax << "\t" << t_Rdark << "\t" << t_LAImax << "\t" << t_leaflifespan << endl;
        }
    }
#endif
    
    
    ////////////////////////////////////////////////
    /////// GLOBAL FUNCTIONS ///////////////////////
    ////////////////////////////////////////////////

#ifdef G0
    
    //Solves the quadratic equation. ROOT: finds smaller (-1), larger (1) root.
    float QUAD(float A, float B, float C, int ROOT){
        float quad=0.0;
        float discriminant=B*B-4.0*A*C;
        // ROOT=-1: smaller root; ROOT=1: larger root
        if(discriminant < 0){Rcerr << "IMAGINARY ROOTS IN QUADRATIC" << endl;quad=0.0;}
        if(A==0.0){if(B==0.0) quad=0.0; else quad=-C/B;}
        else quad=(- B + ROOT*sqrt(discriminant))/(2.0*A);
        return quad;
    }
    
#endif
    
    //! Global functions to compute Vcmax (on a mass basis) and leaf dark respiration (on a leaf area basis)
    // newIM : to avoid repeating same functions in Tree and Species class. Was needed for the Species class to correctly initialise Rdark and compute LCP
    
    float CalcVcmaxm(float LMA, float Nmass, float Pmass){
        float SLA=10000.0/LMA;  // in cm2 g-1
        float Vcmaxm = pow(10.0, fminf((-1.56+0.43*log10(Nmass*1000.0)+0.37*log10(SLA)), (-0.80+0.45*log10(Pmass*1000.0)+0.25*log10(SLA)))); // this is equation 2 in Domingues et al 2010 PCE (coefficients from fig7) which made better fits than equation 1 (without LMA). Nmass and Pmass are given in g g-1, but should be in mg g-1 in equ 2 in Domingues et al. 2010, hence the mutiplication by 1000.
        return(Vcmaxm); //  in micromol C g-1 s-1
    }
    float CalcRdark(float LMA, float Nmass, float Pmass, float Vcmax){
        float Parea = Pmass * LMA;  // in g m-2
        float Narea = Nmass * LMA;  // in g m-2
        float Rdark = (1.3893 + (0.0728 * Narea) + (0.0015 * Parea) + (0.0095 * Vcmax) - (0.0358 * 26.2)); // in micromolC m-2 s-1 //since v.2.5: correction of Atkin et al. 2015 New phytologist formula. The original formula was based on mean-centered values (cf. Atkin et al. 2015, and the clarification/correction published afterwards), unfortunately only one formula (PFT-specific formula) with absolute values is provided and this is the one used here, cf. corrigendum TableS4): https://nph.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fnph.13253&file=nph13253-sup-0001-SupInfo.pdf Warning: Vcmax should be provided on an area basis.
        return(Rdark);
    }
    
    
    
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
                //float radius_height = CR - crown_slope * (h_outer - height_baselayer);    // for the lowest layer, i.e. h == height_baselayer, radius = t_CR
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
    void KeepFloatAsIs(float input, float &output, float CD, float height, int layer_fromtop){
        output = input;
    }
    
    void KeepIntAsIs(int input, int &output, float CD, float height, int layer_fromtop){
        output = input;
    }
    
    
    
    // Global function: a modifying function that converts LAI to the density of a specific layer, using the GetDensity functions
    //! - modifier for GPP calculation where we need the leaves per layer to weight our results
    //! - LAI is the input, dens_layer the output
    void LAI2dens_cumulated(float LAI, float &dens_layer, float CD, float height, int layer_fromtop){
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
            dens_layer = LAI;         /* full LAI allocation */
        } else if(CD < 3.0 && (crown_top - layer_fromtop == crown_base)){
            dens_layer = LAI;
        } else {
            float fraction_layer = height - floor(height);    /* this is the fraction that each layer apart from the topmost layer will extend into the voxel above */
            if(layer_fromtop == 0) dens_layer = dens_top * fraction_layer;
            else if(layer_fromtop == 1) dens_layer = dens_top + dens_belowtop * fraction_layer;
            else if(layer_fromtop == 2) dens_layer = dens_top + dens_belowtop + dens * fraction_layer;
            else dens_layer = LAI;
        }
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
    
    
    void OutputCrownSliced(int height, int site, int row_slice, vector<float> &output_statistics){
        int row_current = site/cols;
        int col_current = site%cols;
        if(row_current == row_slice && col_current >= mincol_visual && col_current < maxcol_visual){
            output_visual[1] << iter << "\t" << row_current << "\t" << col_current << "\t" << height;
            for(int i = 0; i < output_statistics.size();i++){
                output_visual[1] << "\t" << output_statistics[i];
            } // we only output tree parts that fall in the current slice extent
            output_visual[1] << endl;
        }
    };
    
    
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
#ifdef WATER
    void GetCanopyEnvironment(int height, int site, float dens, float (&canopy_environment_cumulated)[6]){
#else
        void GetCanopyEnvironment(int height, int site, float dens, float (&canopy_environment_cumulated)[4]){
#endif
            // first get voxel field densities
            float absorb_prev = LAI3D[height+1][site+SBORD];
            float absorb_curr = LAI3D[height][site+SBORD];
            float absorb_delta = absorb_curr - absorb_prev;
            if(absorb_delta < 0.0) absorb_delta = 0.0;    // eliminate rounding errors
            int intabsorb = CalcIntabsorb(absorb_prev, absorb_delta);
#ifdef WATER
            int intincident = CalcIntabsorb(absorb_prev);
#endif
            
#ifdef FULL_CLIMATE
            // Obtain PPFD, VPD and T for the voxel
            float PPFD_voxel = LookUp_flux_absorption[intabsorb];
            float VPD_voxel = LookUp_VPD[intabsorb];
            float T_voxel = LookUp_T[intabsorb];
#ifdef WATER
            float PPFD_voxel_incident = LookUp_flux[intincident];
            float ExtinctLW_voxel = LookUp_ExtinctLW[intincident];
#endif //WATER
            
#else // FULL_CLIMATE
            // Obtain PPFD, VPD and T for the voxel
            float PPFD_voxel = WDailyMean * LookUp_flux_absorption[intabsorb];
            float VPD_voxel = VPDDailyMean * LookUp_VPD[intabsorb];
            float T_voxel = tDailyMean - LookUp_T[intabsorb];
#ifdef WATER
            float PPFD_voxel_incident = WDailyMean * LookUp_flux[intincident];
            float ExtinctLW_voxel = LookUp_ExtinctLW[intincident];
#endif //WATER
            
#endif //FULL_CLIMATE
            
            // Add the three variables up, weighted by leaf density inside voxel
            canopy_environment_cumulated[0] += dens;
            canopy_environment_cumulated[1] += PPFD_voxel * dens;
            canopy_environment_cumulated[2] += VPD_voxel * dens;
            canopy_environment_cumulated[3] += T_voxel * dens;
#ifdef WATER
            canopy_environment_cumulated[4] += PPFD_voxel_incident * dens;
            canopy_environment_cumulated[5] += ExtinctLW_voxel * dens;
#endif
            
            
            //if (canopy_environment_cumulated[1]<=0 || dens < 0.05 || absorb_delta < 0.05) {
            // Rcout << "Warning in GetCanopyEnvironment, PPFD <=0; PPFD_voxel=" << PPFD_voxel << "; dens=" << dens << endl;
            // Rcout << "WDailyMean=" << WDailyMean << "; LookUp_flux_absorption[intabsorb]=" << LookUp_flux_absorption[intabsorb] << "; intabsorb=" << intabsorb << "; absorb_prev=" << absorb_prev << "; absorb_delta=" << absorb_delta << endl;
            // }
            
            
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
                    //float radius_height = CR - crown_slope * (h_outer - height_baselayer);    // for the lowest layer, i.e. h == height_baselayer, radius = t_CR
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
 //' @title TROLL simulator
 //'
 //' @description
 //' Wrapper of the TROLL C++ simulator with Rcpp.
 //'
 //' @name trollCpp
 //'
 //' @param global_file char. Path to the global parameters file.
 //' @param climate_file char. Path to the climate file.
 //' @param species_file char. Path to the species file.
 //' @param day_file char. Path to the daytime file.
 //' @param pedology_file char. Path to the pedology file.
 //' @param lidar_file char. Path to the lidar file.
 //' @param forest_file char. Path to the forest file.
 //' @param soil_file char. Path to the soil file.
 //' @param output_file char. Path to the output folder.
 //'
 //' @return Void with outputs files written in the defined folder.
 //'
 //' @examples
 //' \dontrun{
 //' trollCpp(global_file = "./test/TROLLv4_input_0310.txt",
 //'          climate_file = "./test/dailymean_paracou2004_2005.txt",
 //'          species_file = "./test/TROLLv4_species.txt",
 //'          day_file = "./test/paracou2004_2005.txt",
 //'          pedology_file = "./test/TROLLv4_soil_MIP_vG.txt",
 //'          lidar_file = "",
 //'          forest_file = "",
 //'          soil_file = "",
 //'          output_file = "test/test/test")
 //' }
 //'
 //' @export
 // [[Rcpp::export]]
 void trollCpp(
     std::string global_file,
     std::string climate_file,
     std::string species_file,
     std::string day_file,
     std::string pedology_file,
     std::string lidar_file,
     std::string forest_file,
     std::string soil_file,
     std::string output_file
 ) {
            
        
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
            //     if(*argv[argn] == '-'){
            //         switch(*(argv[argn]+1)){
            //             case 'i':
            //                 bufi = argv[argn]+2;
            //                 break;
            //             case 'd':                       // new v.3.0; initialisation of daytime variation in a separate file ('d' for daytime variation)
            //                 bufi_daytimevar = argv[argn]+2;
            //                 break;
            //             case 'm':                       // new v.2.4; initialisation of climate parameters from separate file ('m' for meterology)
            //                 bufi_climate = argv[argn]+2;
            //                 break;
            //             case 'p':                       // new v.3.0; initialisation of soil parameters from separate file ('p' for pedology)
            //                 bufi_soil = argv[argn]+2;
            //                 break;
            //             case 's':                       // new v.3.0; initialisation of species parameters from separate file
            //                 bufi_species = argv[argn]+2;
            //                 break;
            //             case 'o':
            //                 buf = argv[argn]+2;
            //                 break;
            //             case 'f':                      // new v.2.3: initialisation from field, 'f' for "forest", "field data"
            //                 bufi_data = argv[argn]+2;
            //                 _FromInventory = 1;        // new v.3.1: automatic recognition of whether data sheet is provided or not
            //                 break;
            //             case 'w':                      // new v.2.3: initialisation from field, 'f' for "forest", "field data"
            //                 bufi_dataSWC = argv[argn]+2;
            //                 break;
            //             case 'l':
            //                 bufi_pointcloud = argv[argn]+2;  // new v.3.1.6: output of simulated point clouds for TROLL-created stands
            //                 _OUTPUT_pointcloud = 1;
            //                 break;
            //             case 'n':
            //                 easympi_rank=atoi(argv[argn]+2); // new v.2.2
                            
            //         }
            //     }
            // }
            bufi = &global_file[0] ;
      bufi_climate = &climate_file[0] ;
      bufi_species = &species_file[0] ;
      bufi_daytimevar = &day_file[0] ;
      bufi_soil = &pedology_file[0] ;
      bufi_data = &forest_file[0] ;
      bufi_dataSWC = &soil_file[0] ;
      bufi_pointcloud = &lidar_file[0] ;
      buf = &output_file[0] ;
      
      _FromInventory = 0; // added v.3.1.7, was previously undefined when no inputfile was provided
      _OUTPUT_pointcloud = 0;  // added v.3.1.7, was previously undefined when no inputfile was provided  
      
      if(strlen(bufi_data) != 0) _FromInventory = 1; // There is a more formal checking of the stream within ReadInputInventory, so this is only to check whether any kind of file/path has been provided, i.e. whether the attempt at initializing from data has been made. But maybe there is a better way of doing this? (and to check: What happens if the string provided in R is NA or NULL? Can we avoid this?)
      if(strlen(bufi_pointcloud) != 0) _OUTPUT_pointcloud = 1; // There is a more formal checking of the stream within ReadInputInventory, so this is only to check whether any kind of file/path has been provided, i.e. whether the attempt at initializing from data has been made. But maybe there is a better way of doing this? (and to check: What happens if the string provided in R is NA or NULL? Can we avoid this?)
      
            
            
            // input files
            sprintf(inputfile,"%s",bufi);
            sprintf(inputfile_daytimevar,"%s",bufi_daytimevar);
            sprintf(inputfile_climate,"%s",bufi_climate);
            sprintf(inputfile_soil,"%s",bufi_soil);
            sprintf(inputfile_species,"%s",bufi_species);
            
            if(_OUTPUT_pointcloud){
                sprintf(inputfile_pointcloud,"%s",bufi_pointcloud); // v.3.1.6
            }
            
            if(_FromInventory){
                sprintf(inputfile_inventory,"%s",bufi_data);
#ifdef WATER
                sprintf(inputfile_SWC,"%s",bufi_dataSWC);
#endif
            }
            
            // v.3.1: removed par output, because no single parameter sheet provided anymore (in future all separate parameter sheets could be provided as outputs as well
            ReadInputGeneral(); // v.3.1 has to be done before initialisation of random number generators (_NONRANDOM)
            
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
            
            Rcout << "On proc #" << easympi_rank << " seed: " << seed << endl;
            sprintf(outputinfo,"%s_%i_info.txt",buf, easympi_rank);
            output_info.open(outputinfo, ios::out);
            if(!output_info) Rcerr<< "ERROR with info file"<< endl;
            
            Initialise();           // Read global parameters
            InitialiseOutputStreams();  // Initialise Output streams, taken outside of Initialise() function in v.3.1 to mirror AllocMem()
            AllocMem();             // Memory allocation
            
#ifdef Output_ABC
            InitialiseABC();
#endif
            
            if(_OUTPUT_pointcloud){
                ReadInputPointcloud();  // parameters for point cloud generation, v.3.1.6
            }
            
            if(_FromInventory){
                ReadInputInventory();   // Initial configuration of the forest, read from data
            }
            
            Rcout << "klight is: " << klight << endl;
            Rcout << "CO2 concentration is: " << Cair << endl;
            Rcout << "Number of species: " << nbspp << endl << endl;
#ifdef WATER
            Rcout << "Atmospheric pressure is: " << PRESS << endl;
#endif
            
            if(_GPPcrown == 1) Rcout << "Activated Module: FastGPP" << endl;
            if(_BASICTREEFALL == 1) Rcout << "Activated Module: BASICTREEFALL" << endl;
            if(_NDD == 1) Rcout << "Activated Module: NDD" << endl;
            if(_SEEDTRADEOFF == 1) Rcout << "Activated Module: SEEDTRADEOFF" << endl;
            if(_FromInventory == 1) Rcout << "Activated Module: FromInventory" << endl;
            if(_OUTPUT_extended == 1) Rcout << "Activated Module: OUTPUT_extended" << endl;
            if(_OUTPUT_inventory == 1) Rcout << "Activated Module: OUTPUT_inventory" << endl;
            if(_OUTPUT_extended == 1 && extent_visual > 0) Rcout << "Activated visualization output." << endl;
            if(_OUTPUT_pointcloud == 1) Rcout << "Activated Module: Point cloud output (simplified ALS simulation)" << endl; // v.3.1.6
            
            //!*********************
            //!** Evolution loop  **
            //!*********************
            
            
            Rcout << "Simulation starts with " << nblivetrees << " trees." << endl;

            //** Information in file info **
            //******************************
            if(!mpi_rank){
                output_info << "\nTROLL simulator\n\n";
                output_info << "\n   2D discrete network: horizontal step = " << LH
                << " m, one tree per "<< LH*LH << " m^2 \n\n";
                output_info << "\n   Tree : (t_dbh,t_height,t_CR,t_CD) \n\n";
                output_info << "\n            + one species label \n\n";
                output_info << " Number of sites      : "<<rows<<"x"<<cols<<"\n";
                output_info << " Number of iterations : "<<nbiter<<"\n";
                output_info << " Duration of timestep : "<<timestep<<" years\n";
                output_info << " Number of Species    : "<<nbspp << "\n\n";
                output_info.flush();
            }
            
            // initial pattern, should be empty, unless an inventory has been provided
            if(_OUTPUT_extended) OutputSnapshot(output_basic[1], 1, 0.01);                  // Initial Pattern, for trees > 0.01m DBH
            else if (_OUTPUT_inventory) OutputSnapshot(output_basic[1], 1, 0.001);
            else OutputSnapshot(output_basic[1], 1, 0.1);                                   // Initial Pattern, for trees > 0.1m DBH

            
            double start_time,stop_time, duration=0.0;           // for simulation duration
            stop_time = clock();
            for(iter=0;iter<nbiter;iter++) {
                start_time = stop_time;
                
                Evolution();
                stop_time = clock();
                duration +=fmaxf(stop_time-start_time,0.0);
                
                if(_OUTPUT_extended == 1 && extent_visual > 0){
                    int timeofyear = GetTimeofyear();
                    if(timeofyear == 0) OutputVisual();
                }
                
                /*if(_OUTPUT_pointcloud > 0 && iter == iter_pointcloud_generation){
                    ExportPointcloud(mean_beam_pc, sd_beam_pc, klaser_pc, transmittance_laser, output_pointcloud); // v.3.1.6
                }*/
                
                
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
                
            }
            
            
            Rcout << "Simulation ends with " << nblivetrees << " trees." << endl;
            
            // final pattern (maybe use a switch later)
            if(_OUTPUT_extended){
                OutputSnapshot(output_basic[2], 1, 0.01);                 // Final Pattern, for trees > 0.01m DBH
            } else if (_OUTPUT_inventory){
                OutputSnapshot(output_basic[2], 1, 0.001);   
            } else {
                OutputSnapshot(output_basic[2], 1, 0.1);                  // Final Pattern, for trees > 0.1m DBH
            }
            if(_OUTPUT_extended){
                OutputLAI(output_extended[7]);
                OutputCHM(output_extended[8]);
            }
            if (_OUTPUT_inventory) {
                for (int d=0; d<nbdcells; d=d+1) {
                    output_basic[3] << d ;
                    for (int l=0; l<nblayers_soil; l=l+1) {
                         output_basic[3] << "\t" << SWC3D[l][d];
                    }
                    output_basic[3] << endl;
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
                output_info << "Number of processors : "<< mpi_size << "\n";
#endif
                output_info << "Average computation time : "<< durf/float(mpi_size) << " seconds.\n";
                output_info << "End of simulation.\n";
                output_info.flush();
                Rcout << "\nNumber of processors : "<< mpi_size << "\n";
                Rcout << "Average computation time : "<< durf/float(mpi_size) << " seconds.\n";
                Rcout << "End of simulation.\n";
            }
            
            CloseOutputs(); // new in v.3.1: Close and clear outputs, maybe not necessary as main function terminates shortly after, but maybe it ensures a cleaner communication with file system/within Rcpp
            FreeMem(); //Free dynamic memory  //! added in oct2013
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
                if(numeric >= parameter_min * 0.99 && numeric <= parameter_max * 1.01){   // built in precision check, quite tolerant (1%)
                    if(numeric < parameter_min) parameter = parameter_min;
                    else if(numeric > parameter_max) parameter = parameter_max;
                    else parameter = numeric;
                    if(!quiet) Rcout << parameter_name << ": " << parameter << endl;
                } else {
                    parameter = parameter_default;
                    if(!quiet) Rcout << "Warning. Value provided for '" << parameter_name << "' (" << numeric << ") is outside the allowed range (" << parameter_min << ", " << parameter_max << "). Set to default: " << parameter_default << endl;
                }
            } else {
                parameter = parameter_default;
                if(!quiet) Rcout << "Warning. Value provided for '" << parameter_name << "' (" << parameter_value << ") is not a " << typeid(numeric).name() << ". Set to default: " << parameter_default << endl;
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
                SetParameter(parameter_name, parameter_value, SWtoPPFD, 0.0f, 5.0f, 2.29f, quiet);
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
            }
#ifdef G0
              else if(parameter_name == "g0"){
                SetParameter(parameter_name, parameter_value, g0, 0.0f, 30.0f, 5.00f, quiet);
            }
#endif
#ifdef PHENO_DROUGHT
              else if(parameter_name == "pheno_thres"){
                  SetParameter(parameter_name, parameter_value, pheno_thres, 0.0f, 1.0f, 0.5f, quiet);
              } else if(parameter_name == "pheno_delta"){
                  SetParameter(parameter_name, parameter_value, pheno_delta, 0.0f, 1.0f, 0.1f, quiet);
              }
#endif
            else if(parameter_name == "vC"){
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
#ifdef WATER
            } else if(parameter_name == "PRESS"){
                SetParameter(parameter_name, parameter_value, PRESS, 10.0f, 110.0f, 101.0f, quiet);
#endif
            } else if(parameter_name == "_LL_parameterization"){
                SetParameter(parameter_name, parameter_value, _LL_parameterization, bool(0), bool(1), bool(1), quiet);
            } else if(parameter_name == "_LA_regulation"){
                SetParameter(parameter_name, parameter_value, _LA_regulation, 0, 2, 2, quiet);
            } else if(parameter_name == "_sapwood"){
                SetParameter(parameter_name, parameter_value, _sapwood, bool(0), bool(1), bool(1), quiet);
            } else if(parameter_name == "_seedsadditional"){
                SetParameter(parameter_name, parameter_value, _seedsadditional, bool(0), bool(1), bool(0), quiet);
            } else if(parameter_name == "_SOIL_LAYER_WEIGHT"){
                SetParameter(parameter_name, parameter_value, _SOIL_LAYER_WEIGHT, 0, 2, 2, quiet);
            } else if(parameter_name == "_WATER_RETENTION_CURVE"){
                SetParameter(parameter_name, parameter_value, _WATER_RETENTION_CURVE, 0, 1, 0, quiet);
            } else if(parameter_name == "_NONRANDOM"){
                SetParameter(parameter_name, parameter_value, _NONRANDOM, bool(0), bool(1), bool(1), quiet);
            } else if(parameter_name == "_GPPcrown"){
                SetParameter(parameter_name, parameter_value, _GPPcrown, bool(0), bool(1), bool(0), quiet);
            } else if(parameter_name == "_BASICTREEFALL"){
                SetParameter(parameter_name, parameter_value, _BASICTREEFALL, bool(0), bool(1), bool(1), quiet);
            } else if(parameter_name == "_SEEDTRADEOFF"){
                SetParameter(parameter_name, parameter_value, _SEEDTRADEOFF, bool(0), bool(1), bool(0), quiet);
            } else if(parameter_name == "_NDD"){
                SetParameter(parameter_name, parameter_value, _NDD, bool(0), bool(1), bool(0), quiet);
            } else if(parameter_name == "_CROWN_MM"){
                SetParameter(parameter_name, parameter_value, _CROWN_MM, bool(0), bool(1), bool(0), quiet);
            } else if(parameter_name == "_OUTPUT_extended"){
                SetParameter(parameter_name, parameter_value, _OUTPUT_extended, bool(0), bool(1), bool(0), quiet);
            } else if(parameter_name == "_OUTPUT_inventory"){
                SetParameter(parameter_name, parameter_value, _OUTPUT_inventory, bool(0), bool(1), bool(0), quiet);
            } else if(parameter_name == "extent_visual"){
                SetParameter(parameter_name, parameter_value, extent_visual, 0, INT_MAX, 0, quiet);
            }
            
            
            // !!!: TODO, implement NDD parameters
            /* if (_NDD) {
             In >> R; In.getline(buffer,128,'\n');
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
            } else if(parameter_name == "s_leafarea"){
                SetParameter(parameter_name, parameter_value, S.s_leafarea, 0.0f, 800.0f, 80.0f, quiet);
            }
        }
        
        
        // added v.3.1.6
        void AssignValuePointcloud(string parameter_name, string parameter_value){
            // we set parameters to values that have been read, or to their defaults, if outside of range or not the right type
            bool quiet = 1; //! only applies to successful initialization, warnings are always given
            
            if(parameter_name == "mean_beam_pc"){
                SetParameter(parameter_name, parameter_value, mean_beam_pc, 0.1f, 100.0f, 10.0f, quiet);
            } else if(parameter_name == "sd_beam_pc"){
                SetParameter(parameter_name, parameter_value, sd_beam_pc, 0.0f, 100.0f, 10.0f, quiet);
            } else if(parameter_name == "klaser_pc"){
                SetParameter(parameter_name, parameter_value, klaser_pc, 0.1f, 0.9f, 0.5f, quiet);
            } else if(parameter_name == "transmittance_laser"){
                SetParameter(parameter_name, parameter_value, transmittance_laser, 0.0f, 1.0f, 0.4f, quiet);
            } else if(parameter_name == "iter_pointcloud_generation"){
                SetParameter(parameter_name, parameter_value, iter_pointcloud_generation, 0, nbiter-1, nbiter-1, quiet);
            }
        }
        
        
        //! Global function: This function reads inputs from the general input file
        //! - Also sets up initial values for some parameters computed from input parameters
        //! - !!!: TO IMPLEMENT: automatic stop of program under error to avoid running empty simulations on cluster etc.
        //! - !!!: TO IMPLEMENT: separate error messages (i.e. no input file provided, empty input file, etc.)
        void ReadInputGeneral(){
            fstream In(inputfile, ios::in);
            if(In){
#ifdef WATER

                string parameter_names[68] = {"cols","rows","HEIGHT","length_dcell","nbiter","NV","NH","nbout","p_nonvert","SWtoPPFD","klight","absorptance_leaves","theta","phi","g1","g0", "pheno_thres", "pheno_delta", "vC","DBH0","H0","CR_min","CR_a","CR_b","CD_a","CD_b","CD0","shape_crown","dens","fallocwood","falloccanopy","Cseedrain","nbs0","sigma_height","sigma_CR","sigma_CD","sigma_P","sigma_N","sigma_LMA","sigma_wsg","sigma_dbhmax","corr_CR_height","corr_N_P","corr_N_LMA","corr_P_LMA","leafdem_resolution","p_tfsecondary","hurt_decay","crown_gap_fraction","m","m1","Cair","PRESS","_LL_parameterization","_LA_regulation","_sapwood","_seedsadditional","_SOIL_LAYER_WEIGHT","_WATER_RETENTION_CURVE","_NONRANDOM","_GPPcrown","_BASICTREEFALL","_SEEDTRADEOFF","_NDD","_CROWN_MM","_OUTPUT_extended","_OUTPUT_inventory", "extent_visual"};
                int nb_parameters = 68;

#else
                string parameter_names[61] = {"cols","rows","HEIGHT","length_dcell","nbiter","NV","NH","nbout","p_nonvert","SWtoPPFD","klight","absorptance_leaves","theta","phi","g1","vC","DBH0","H0","CR_min","CR_a","CR_b","CD_a","CD_b","CD0","shape_crown","dens","fallocwood","falloccanopy","Cseedrain","nbs0","sigma_height","sigma_CR","sigma_CD","sigma_P","sigma_N","sigma_LMA","sigma_wsg","sigma_dbhmax","corr_CR_height","corr_N_P","corr_N_LMA","corr_P_LMA","leafdem_resolution","p_tfsecondary","hurt_decay","crown_gap_fraction","m","m1","Cair","_LL_parameterization","_LA_regulation","_sapwood","_seedsadditional","_NONRANDOM","_GPPcrown","_BASICTREEFALL","_SEEDTRADEOFF","_NDD","_CROWN_MM","_OUTPUT_extended","extent_visual"};
                int nb_parameters = 61;
#endif
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
                Rcout << _WATER_RETENTION_CURVE << " " << _SOIL_LAYER_WEIGHT << " " << Cair << endl;
                
#ifdef WATER
                i_sites_per_dcell=1.0/float(sites_per_dcell);
                PPFDtoSW= 1/SWtoPPFD;
#ifdef G0
                g0*=0.001*GSVGSC; // g0 is provided in mmolH20 m-2 s-1, but will be used in Photosyn function for stomatal conductance to CO2 in molCO2 m-2 s-1 (and then converted back to flux of H20 in FluxesLeaf)
#endif
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
#ifdef WATER
                //alpha = absorptance_leaves*(1-LSQ)*0.5; // apparent quantum yield to electron transport. 0.5 because each photosystem absorbs half of the photons (von Caemmerer 2000, p 35). Note that, alpha was previously computed as 4*the apparent quantum yield for C fixation, phi in molC/mol photons, with phi=0.06 (in Marechaux & Chave 2017) or 0.075 (in FFischer's version), which provided alpha=0.3 and 0.24 respectively. This now gives alpha=0.37. Duursma et al. 2015 used alpha=0.24, while values reported or used by von Caemmerer 2000, Medlyn et al. 2002 are typically 0.3-0.37. Domingues et al. 2014 Plant Ecology & Diversity reported values for phi ranging from 0.041 to 0.098 (ie alpha ranging from 0.164 to 0.392, within a same tropical forest community (Tapajos, Brazil).
                alpha = (1-LSQ)*0.5; // no need of absorptance_leaves since PPFD provided in argument to Photosyn is already the absorbed flux
#else
                alpha = 4.0*phi; // apparent quantum yield to electron transport in mol e-/mol photons see Mercado et al 2009 , the conversion of the apparent quantum yield in micromolCO2/micromol quantum into micromol e-/micxromol quantum is done by multipliyng by 4, since four electrons are needed to regenerate RuBP. alpha is fixed at 0.3 mol e-/mol photons in Medlyn et al 2002, but see equ8 and Appendix 1 in Farquahr et al 1980: it seems that alpha should vary with leaf thickness: there is a fraction of incident light which is lost by absorption by other leaf parts than the chloroplast lamellae, and this fraction f may increase with leaf thickness. With the values of the paper: alpha= 0.5*(1-f)=0.5*(1-0.23)=0.385, but this is a theoretical value and observations often report lower values (see ex discussion in medlyn et al 2005 Tree phsyiology, Lore Veeryckt values, Mercado et al 2009 Table 10, Domingues et al. 2014)
#endif
            
                // new in v.3.1.2: visual extent
                int maxextent_visual = min(rows,cols);
                if(extent_visual > maxextent_visual) extent_visual = maxextent_visual; // make sure visualization does not exceed boundaries of simulation
                if(extent_visual > 0){
                    // define boundaries
                    int rowextent_slice = 10;
                    mincol_visual = cols/2 - extent_visual/2;
                    maxcol_visual = cols/2 + extent_visual/2;
                    minrow_visual = rows/2 - extent_visual/2;
                    maxrow_visual = rows/2 + extent_visual/2;
                    minrow_visual_slice = max(rows/2 - rowextent_slice/2, minrow_visual);
                    maxrow_visual_slice = min(rows/2 + rowextent_slice/2, maxrow_visual);
            
            }
        } else{
            Rcout << "ERROR. General input file could not be read." << endl;
        }
    }
        
        //! Global function: This function reads inputs from the species input file
        void ReadInputSpecies(){
            Rcout << endl << "Reading in file: " << inputfile_species << endl;
            fstream InSpecies(inputfile_species, ios::in);
            if(InSpecies){
                // possible parameters to initialise vector<string> parameter_names{"s_name","s_LMA","s_Nmass","s_Pmass","s_wsg","s_dbhmax","s_hmax","s_ah","s_seedmass","s_regionalfreq","s_tlp","s_leafarea"};
                //        int nb_parameters = int(parameter_names.size()); only works from C++11 onwards
                string parameter_names[12] = {"s_name","s_LMA","s_Nmass","s_Pmass","s_wsg","s_dbhmax","s_hmax","s_ah","s_seedmass","s_regionalfreq","s_tlp","s_leafarea"};
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
                    //Rcout << S[sp].s_name << " LCP: " << S[sp].s_LCP << endl;
                }
                Rcout << "Successfully read in species file." << endl;
            } else {
                Rcout<< "ERROR with the species file" << endl;
            }
            
            InSpecies.close();
        }
        
        
#ifdef FULL_CLIMATE
        
        
        //! Global function: This function reads inputs from the environmental daily variation input file
        void ReadInputDailyvar(){
            // currently very simple reading in, only basic error checking
            Rcout << endl << "Reading in file: " << inputfile_daytimevar << endl;
            
            fstream InDaily(inputfile_daytimevar, ios::in);
            
            if (InDaily) {
                InDaily.getline(buffer,256,'\n'); // read in header
                
                // we go through all lines in the input file
                nbsteps_varday = 0;
                nbhours_covered = 12.0; // quick fixed, but I should provide both endtime and startime for each half-hour iteration.
                
                float day_current, starttime_current,varday_light_current, varday_vpd_current, varday_T_current, varday_WS_current;
                
                string line;
                while(getline(InDaily, line)){
                    istringstream linestream(line);
                    
                    linestream >> day_current >> starttime_current >> varday_T_current >> varday_light_current >> varday_vpd_current >> varday_WS_current;
                    //float nbhours_current;
                    //if(endtime_current > starttime_current) nbhours_current = endtime_current - starttime_current;  // just calculate the time window
                    //else nbhours_current = 24.0 - (starttime_current - endtime_current); // i.e. if starttime is 23.5 and endtime is 0.5, etc. Calculate the inverse time window and then subtract from 24
                    // else nbhours_current =0.5;
                    
                    if (varday_vpd_current <=0.01) {
                        varday_vpd_current = 0.01;
                        Rcout << "Warning, VPD is <=0, on day " << day_current << " at " << starttime_current << "h." << endl;
                    }
                    
                    if (varday_light_current <=1) {
                        varday_light_current = 1;
                        Rcout << "Warning, light is <=0, on day " << day_current << " at " << starttime_current << "h." << endl;
                    }
                    
                    if (varday_WS_current <=0.1) {
                        varday_WS_current = 0.1;
                        Rcout << "Warning, wind is <=0, on day " << day_current << " at " << starttime_current << "h." << endl;
                    }
                    
                    //nbhours_covered += nbhours_current;
                    varday_light.push_back(varday_light_current);
                    varday_vpd.push_back(varday_vpd_current);
                    varday_T.push_back(varday_T_current);
                    varday_WS.push_back(varday_WS_current);
                    //Rcout << "at: " << starttime_current << " vardaytime_light: " << varday_light[nbsteps_varday] << " vardaytime_vpd: " << varday_vpd[nbsteps_varday] << " vardaytime_T: "<< varday_T[nbsteps_varday] << " vardaytime_WS: "<< varday_WS[nbsteps_varday] << endl;
                    nbsteps_varday++;
                }
                
                nbsteps_varday /= nbdays;
                
                Rcout << "Read in: " << nbsteps_varday << " timesteps per day, covering " << nbhours_covered << " hours of the day." << endl;
                Rcout << "Successfully read in daytime variation file" << endl;
                inv_nbsteps_varday = 1.0/float(nbsteps_varday);
            }
            else{
                Rcout<< "ERROR with the daily variation file" << endl;
            }
            
            if(NULL==(WDailyMean_all=new float[nbsteps_varday])) Rcerr<<"!!! WDailyMean_all" << endl;
            if(NULL==(tDailyMean_all=new float[nbsteps_varday])) Rcerr<<"!!! tDailyMean_all" << endl;
            if(NULL==(VPDDailyMean_all=new float[nbsteps_varday])) Rcerr<<"!!! VPDDailyMean_all" << endl;
            if(NULL==(windDailyMean_all=new float[nbsteps_varday])) Rcerr<<"!!! windDailyMean_all" << endl;

            for (int j=0; j<nbsteps_varday; j++) {
                WDailyMean_all[j] = tDailyMean_all[j] = VPDDailyMean_all[j] = windDailyMean_all[j] = 0.0;
            }
            
            WDailyMean_year = 0.0;

            
            for (int i=0; i<nbdays; i++) {
                
                float DailyMeanTemperature_current=0.0, DailyMeanWindSpeed_current=0.0,DailyMeanIrradiance_current=0.0,DailyMeanVapourPressureDeficit_current=0.0;
                
                for (int j=0; j<nbsteps_varday; j++) {
                    DailyMeanTemperature_current +=varday_T[i*nbsteps_varday +j];
                    DailyMeanWindSpeed_current +=varday_WS[i*nbsteps_varday +j];
                    DailyMeanIrradiance_current +=varday_light[i*nbsteps_varday +j];
                    DailyMeanVapourPressureDeficit_current +=varday_vpd[i*nbsteps_varday +j];
                    
                    WDailyMean_all[j] +=varday_light[i*nbsteps_varday +j];
                    VPDDailyMean_all[j] +=varday_vpd[i*nbsteps_varday +j];
                    tDailyMean_all[j] +=varday_T[i*nbsteps_varday +j];
                    windDailyMean_all[j] +=varday_WS[i*nbsteps_varday +j];
                }
                
                DailyMeanTemperature_current *=inv_nbsteps_varday;
                DailyMeanWindSpeed_current  *=inv_nbsteps_varday;
                DailyMeanIrradiance_current  *=inv_nbsteps_varday;
                DailyMeanVapourPressureDeficit_current  *=inv_nbsteps_varday;
                
                WDailyMean_year +=DailyMeanIrradiance_current;
                
                DailyMeanTemperature.push_back(DailyMeanTemperature_current);
                DailyMeanWindSpeed.push_back(DailyMeanWindSpeed_current);
                DailyMeanIrradiance.push_back(DailyMeanIrradiance_current);
                DailyMeanVapourPressureDeficit.push_back(DailyMeanVapourPressureDeficit_current);
                
            }
            
            for (int j=0; j<nbsteps_varday; j++) {
                WDailyMean_all[j] /=nbdays;
                VPDDailyMean_all[j] /=nbdays;
                tDailyMean_all[j] /=nbdays;
                windDailyMean_all[j] /=nbdays;
            }
            
            WDailyMean_year *=SWtoPPFD/nbdays;
            
            tnight=NightTemperature[0];
            precip=Rainfall[0];
            WSDailyMean=DailyMeanWindSpeed[0];
            WDailyMean=DailyMeanIrradiance[0]*SWtoPPFD;
            tDailyMean=DailyMeanTemperature[0];
            VPDDailyMean=DailyMeanVapourPressureDeficit[0];
            
            
        }
        
        
        
        //! Global function: This function reads inputs from the environmental variation input file
        //! - v.3.0 of the code suppose that environment is periodic (a period = a year), if one want to make climate vary, with interannual variation and climate change along the simulation, one just need to provide the full climate input of the whole simulation (ie number of columns=iter and not iterperyear) and change iterperyear by nbiter here.
        void ReadInputClimate(){
            Rcout << endl << "Reading in file: " << inputfile_climate << endl;
            
            fstream InClim(inputfile_climate, ios::in);
            
            if(InClim){
                // we go through all lines in the input file
                nbdays=0;
                InClim.getline(buffer,256,'\n');
                //Rcout << "Header line: " << buffer << endl;
                
                string line;
                while(getline(InClim, line)){
                    istringstream linestream(line);
                    
                    float NightTemperature_current, Rainfall_current;
                    linestream >>  NightTemperature_current >> Rainfall_current ;
                    
                    NightTemperature.push_back(NightTemperature_current);
                    Rainfall.push_back(Rainfall_current);
                    nbdays++;
                }
                
                iterperyear=365; // SS change because it will be always 365 iter per year 
                // if (nbdays%365==0) iterperyear=365; //check if this guarantees iterperyear is correctly infered
                // else iterperyear=12; 
                
                timestep=1.0/float(iterperyear);
                Rcout << "Read in climate data for " << nbdays << " days, with" << iterperyear << " iterations per year." << endl;
                // choose average conditions
                
                Tnight_year = 0.0;
                
                for (int i=0; i<nbdays; i++) {
                    Tnight_year += NightTemperature[i];
                }
                
                Tnight_year *= 1.0/float(nbdays);
                
                Rcout << "Successfully read the climate file" << endl;
            }
            else{
                Rcout<< "ERROR with the climate file" << endl;
            }
            InClim.close();
        }
        
        
#else //FULL_CLIMATE
        
        
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
                    
                    float starttime_current, endtime_current, varday_light_current, varday_vpd_current, varday_T_current, varday_WS_current;
                    linestream >> starttime_current >> endtime_current >> varday_light_current >> varday_vpd_current >> varday_T_current >> varday_WS_current;
                    float nbhours_current;
                    if(endtime_current > starttime_current) nbhours_current = endtime_current - starttime_current;  // just calculate the time window
                    else nbhours_current = 24.0 - (starttime_current - endtime_current); // i.e. if starttime is 23.5 and endtime is 0.5, etc. Calculate the inverse time window and then subtract from 24
                    
                    nbhours_covered += nbhours_current;
                    varday_light.push_back(varday_light_current);
                    varday_vpd.push_back(varday_vpd_current);
                    varday_T.push_back(varday_T_current);
                    varday_WS.push_back(varday_WS_current);
                    Rcout << "at: " << starttime_current << " vardaytime_light: " << varday_light[nbsteps_varday] << " vardaytime_vpd: " << varday_vpd[nbsteps_varday] << " vardaytime_T: "<< varday_T[nbsteps_varday] << " vardaytime_WS: "<< varday_WS[nbsteps_varday] << endl;
                    nbsteps_varday++;
                }
                
#ifdef WATER
                totday_light = 0.0;
                for (int i = 0; i < nbsteps_varday; i++) {
                    totday_light += varday_light[i];
                    Rcout << i << "\t" << varday_light[i] << endl;
                }
                Rcout << totday_light << endl;
                totday_light*=1.0/float(nbsteps_varday);
                Rcout << totday_light << endl;
#endif
                Rcout << "Read in: " << nbsteps_varday << " timesteps per day, covering " << nbhours_covered << " hours of the day." << endl;
                Rcout << "Successfully read in daytime variation file" << endl;
                inv_nbsteps_varday = 1.0/float(nbsteps_varday);
            }
            else{
                Rcout<< "ERROR with the daily variation file" << endl;
            }
        }
        
        
        
        //! Global function: This function reads inputs from the environmental variation input file
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
                    
                    float DailyMeanTemperature_current, NightTemperature_current, Rainfall_current, DailyMeanWindSpeed_current, DailyMeanIrradiance_current, DailyMeanVapourPressureDeficit_current;
                    linestream >> DailyMeanTemperature_current >> NightTemperature_current >> Rainfall_current >> DailyMeanWindSpeed_current >> DailyMeanIrradiance_current >> DailyMeanVapourPressureDeficit_current;
                    
                    DailyMeanTemperature.push_back(DailyMeanTemperature_current);
                    NightTemperature.push_back(NightTemperature_current);
                    Rainfall.push_back(Rainfall_current);
                    DailyMeanWindSpeed.push_back(DailyMeanWindSpeed_current);
                    DailyMeanIrradiance.push_back(DailyMeanIrradiance_current);
                    DailyMeanVapourPressureDeficit.push_back(DailyMeanVapourPressureDeficit_current);
                    //Rcout <<  DailyMeanTemperature_current << "\t" <<  NightTemperature_current << "\t" <<  Rainfall_current << "\t" <<  DailyMeanWindSpeed_current << "\t" <<  DailyMeanIrradiance_current << "\t" <<   DailyMeanVapourPressureDeficit_current << endl;
                    iterperyear++;
                }
                
                timestep=1.0/float(iterperyear);
                Rcout << "Read in climate data for " << iterperyear << " iterations per year." << endl;
                // choose average conditions
                WDailyMean_year = tDailyMean_year = VPDDailyMean_year = Tnight_year = 0.0;
#ifdef WATER
                windDailyMean_year=0.0;
#endif
                
                for (int i=0; i<iterperyear; i++) {
                    //DailyMeanVapourPressureDeficit[i]*=1.33; test main_1.33VPD on MESO@LR - 13/05/22
                    
                    WDailyMean_year += DailyMeanIrradiance[i]*SWtoPPFD;
                    tDailyMean_year += DailyMeanTemperature[i];
                    VPDDailyMean_year += DailyMeanVapourPressureDeficit[i];
#ifdef WATER
                    windDailyMean_year += DailyMeanWindSpeed[i];
#endif
                    Tnight_year += NightTemperature[i];
                }
                
                WDailyMean_year *= 1.0/float(iterperyear);
                tDailyMean_year *= 1.0/float(iterperyear);
                VPDDailyMean_year *= 1.0/float(iterperyear);
#ifdef WATER
                windDailyMean_year *=1.0/float(iterperyear);
#endif
                //Rcout << "WDailyMean: " << WDailyMean_year << endl;
                Tnight_year *= 1.0/float(iterperyear);
                
                tnight=NightTemperature[iter%iterperyear];
                precip=Rainfall[iter%iterperyear];
                WSDailyMean=DailyMeanWindSpeed[iter%iterperyear];
                WDailyMean=DailyMeanIrradiance[iter%iterperyear]*SWtoPPFD;
                tDailyMean=DailyMeanTemperature[iter%iterperyear];
                VPDDailyMean=DailyMeanVapourPressureDeficit[iter%iterperyear];
                
                Rcout << "Successfully read the climate file" << endl;
            }
            else{
                Rcout<< "ERROR with the climate file" << endl;
            }
            InClim.close();
        }
        
        
        
#endif //FULL_CLIMATE
        
        
        
        
#ifdef WATER
        //! Global function: This function reads inputs from the soil input file
        //! - in v.3.0, all soil parameters (Sat_SWC, Res_SWC) are computed from soil texture data (%clay, %silt, %sand) provided in input for each layer. If additional information is available from the field (soil pH, organic content, dry bulk density, cation exchange capacity), this could be also provided in input and used to refine the computation of these soil parameters (see Table 2 in Marthews et al. 2014 Geoscientific Model Development and Hodnett & Tomasella 2002 Geoderma -- for tropical soils). Alternatively, if no local field soil data is available, these soil parameters (Sat_SWC, Res_SWC) should be drawn from global maps and databases --see Marthews et al. 2014, and directly provided in input. ==> ccl: to standardize the input file, the soil parameters (Sat_SWC, Res_SWC) should probably be provided in input, and the computation of those properties from the available local data (here %clay, %silt, %sand) made using a new function of RconTROLL (and not here)
        //! - Sat_SWC and Res_SWC are here computed according Tomasella & Hodnett 1998 from soil texture information (see Table 2 in Marthews et al. 2014)
        void ReadInputSoil(){
            Rcout << endl << "Reading in file: " << inputfile_soil << endl;
            fstream InSoil(inputfile_soil, ios::in);
            
            if(InSoil){
                InSoil.getline(buffer,256,'\n');
                vector<float> layer_thickness, proportion_Silt, proportion_Clay, proportion_Sand; // in m, %, %,%
                vector<float> SOC, DBD, pH, CEC; //soil organic content, provided in %; dry bulk density, in g cm-3; pH; cation exchange capacity, in cmol kg-1
                SOC.reserve(20);
                DBD.reserve(20);
                pH.reserve(20);
                CEC.reserve(20);

                layer_thickness.reserve(20);
                proportion_Silt.reserve(20);
                proportion_Clay.reserve(20);
                proportion_Sand.reserve(20);
                nblayers_soil = 0;
                
                // we go through all lines in the input file
                string line;
                while(getline(InSoil, line)){
                    istringstream linestream(line);
                    
if (_WATER_RETENTION_CURVE==1) {
                    float thickness_current, proportion_Silt_current, proportion_Clay_current, proportion_Sand_current, SOC_current, DBD_current, pH_current, CEC_current;
                    linestream >> thickness_current >> proportion_Silt_current >> proportion_Clay_current >> proportion_Sand_current >> SOC_current >> DBD_current >> pH_current >> CEC_current;
                    layer_thickness.push_back(thickness_current);
                    proportion_Silt.push_back(proportion_Silt_current);
                    proportion_Clay.push_back(proportion_Clay_current);
                    proportion_Sand.push_back(proportion_Sand_current);
                    SOC_current*=10; // to convert SOC in % to gC kg-1 soil, to apply Hodnett & Tomasella 2002
                    SOC.push_back(SOC_current);
                    DBD.push_back(DBD_current);
                    pH.push_back(pH_current);
                    CEC.push_back(CEC_current);
} else if (_WATER_RETENTION_CURVE==0) {
                    float thickness_current, proportion_Silt_current, proportion_Clay_current, proportion_Sand_current;
                    linestream >> thickness_current >> proportion_Silt_current >> proportion_Clay_current >> proportion_Sand_current;
                    layer_thickness.push_back(thickness_current);
                    proportion_Silt.push_back(proportion_Silt_current);
                    proportion_Clay.push_back(proportion_Clay_current);
                    proportion_Sand.push_back(proportion_Sand_current);
}
                    
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
                if(NULL==(FC_SWC=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc FC_SW" << endl;

                
                for (int l=0; l<nblayers_soil; l++) {
                    
if (_WATER_RETENTION_CURVE==1) {
                    Sat_SWC[l] = 0.01*(81.799+(0.099*proportion_Clay[l])-(31.42*DBD[l])+(0.018*CEC[l])+(0.451*pH[l])-(0.0005*proportion_Sand[l]*proportion_Clay[l]));  // this is the Hodnett & Tomasella 2002 tropical pedotransfer function, as reported in Table 2 of Marthews et al. 2014. in m3.m-3
} else if (_WATER_RETENTION_CURVE==0) {
                    Sat_SWC[l]= 0.01*(40.61+(0.165*proportion_Silt[l])+(0.162*proportion_Clay[l])+(0.00137*proportion_Silt[l]*proportion_Silt[l])+(0.000018*proportion_Silt[l]*proportion_Silt[l]*proportion_Clay[l])); // this is the Tomasella & Hodnett 1998 tropical texture-based pedotransfer function, as reported in Table 2 of Marthews et al. 2014. in m3.m-3
}
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
                    
if (_WATER_RETENTION_CURVE==1) {
                    Res_SWC[l]= 0.01*(22.733-(0.164*proportion_Sand[l])+(0.235*CEC[l])-(0.831*pH[l])+(0.0018*proportion_Clay[l]*proportion_Clay[l])+(0.0026*proportion_Sand[l]*proportion_Clay[l])); // this is the Hodnett & Tomasella 2002 tropical pedotransfer function, as reported in Table 2 of Marthews et al. 2014. in m3.m-3
} if (_WATER_RETENTION_CURVE==0) {
                    Res_SWC[l]= 0.01*fmaxf(0.0,(-2.094+(0.047*proportion_Silt[l])+(0.431*proportion_Clay[l])-(0.00827*proportion_Silt[l]*proportion_Clay[l]))); // this is the Tomasella & Hodnett 1998 tropical texture-based pedotransfer function, as reported in Table 2 of Marthews et al. 2014.
}
                    Min_SWC[l]=Res_SWC[l]*sites_per_dcell*LH*LH*layer_thickness[l]; //in m3

                    Rcout << "layer " << l << " Vol=" << sites_per_dcell*LH*LH*layer_thickness[l] << "m3; Res=" << Res_SWC[l]<<  " MIN_SWC =" << Min_SWC[l] << " m3" << endl;
                }
                
if (_WATER_RETENTION_CURVE==1) {
                if(NULL==(a_vgm=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc a_vgm" << endl;
                if(NULL==(b_vgm=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc b_vgm" << endl;
                if(NULL==(c_vgm=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc c_vgm" << endl;
                if(NULL==(m_vgm=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc m_vgm" << endl;

                for (int l=0; l<nblayers_soil; l++) {

                    float alpha=1000*exp((-2.294-(3.526*proportion_Silt[l])+(2.440*(0.1*SOC[l]))-(0.076*CEC[l])-(11.331*pH[l])+(0.019*proportion_Silt[l]*proportion_Silt[l]))*0.01); // this is the alpha parameter of the van Genuchten-Mualem model, in MPa-1 (ie after already dividing by rho*g, following Hodnett & Tomasella 2002, see Table 2 in Marthews et al. 2014
                    a_vgm[l]=-1.0/alpha;
                    float n_vgm=exp((62.986-(0.833*proportion_Clay[l])-(0.529*(SOC[l]*0.1))+(0.593*pH[l])+(0.007*proportion_Clay[l]*proportion_Clay[l])-(0.014*proportion_Sand[l]*proportion_Silt[l]))*0.01);    // this is the n parameter of the van Genuchten-Mualem model, dimensionless, following Hodnett & Tomasella 2002, see Table 2 in Marthews et al. 2014
                    m_vgm[l]=1.0-1.0/n_vgm;
                    b_vgm[l]=1.0/m_vgm[l];
                    c_vgm[l]=1.0-m_vgm[l];
                    
                    FC_SWC[l]=(Res_SWC[l] + (Sat_SWC[l]-Res_SWC[l])*pow((pow(0.01*alpha, 1/c_vgm[l])+1),-(1/b_vgm[l])))*sites_per_dcell*LH*LH*layer_thickness[l]; // this is the layer water content at field capacity, in m3. As in Marthews et al. 2014 (cf. note in Table 2), we used Phi at FC=-10kPa and not -33kPa, following Marshall et al., 1996; Townend et al., 2001; Tomasella and Hodnett, 2004)
                    
                    Rcout << "layer " << l << " alpha=" << alpha << "\t" << "n_vgm=" << n_vgm << " FC_SWC=" << FC_SWC[l] << endl;
                }
} else if (_WATER_RETENTION_CURVE==0) {
                if(NULL==(phi_e=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc phi_e" << endl;
                if(NULL==(b=new float[nblayers_soil])) Rcerr<<"!!! Mem_Alloc b" << endl;
                for (int l=0; l<nblayers_soil; l++) {
                    phi_e[l]=-0.001*(0.285+(0.000733*proportion_Silt[l]*proportion_Silt[l])-(0.00013*proportion_Silt[l]*proportion_Clay[l])+(0.0000036*proportion_Silt[l]*proportion_Silt[l]*proportion_Clay[l])); // according to Tomasella & Hodnett 1998 tropical texture-based pedotransfer function, as reported in Table 2 of Marthews et al. 2014. phi_e is here in MPa.
                    b[l]=exp(1.197+(0.00417*proportion_Silt[l])-(0.0045*proportion_Clay[l])+(0.000894*proportion_Silt[l]*proportion_Clay[l])-(0.00001*proportion_Silt[l]*proportion_Silt[l]*proportion_Clay[l])); // according to Tomasella & Hodnett 1998 tropical texture-based pedotransfer function, as reported in Table 2 of Marthews et al. 2014.
                    //phi_e[l]=-0.00000001*pow(10.0,(2.17-(0.0063*proportion_Clay[l])-(0.0158*proportion_Sand[l])))*(1000*9.80665); // according to Cosby et al. 1984, non -tropical and texture-based but widely used, as reported in Table 2 in Marthews et al. 2014. In MPa.
                    //b[l]=3.10+0.157*proportion_Clay[l]-0.003*proportion_Sand[l]; // according to Cosby et al. 1984, non -tropical and texture-based but widely used, as reported in Table 2 in Marthews et al. 2014. Dimensionless.
                    
                    FC_SWC[l]=(Res_SWC[l] + (Sat_SWC[l]-Res_SWC[l])*pow(-0.01/phi_e[l],-(1/b[l])))*sites_per_dcell*LH*LH*layer_thickness[l]; // this is the layer water content at filed capacity, in m3. As in Marthews et al. 2014 (cf. note in Table 2), we used Phi at FC=-10kPa and not -33kPa, following Marshall et al., 1996; Townend et al., 2001; Tomasella and Hodnett, 2004)
                    
                    
                    Rcout << "layer " << l << " phi_e=" << phi_e[l] << "\t" << "b=" << b[l] << " FC_SWC=" << FC_SWC[l] << endl;
                }
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
            for(int i=0;i<10000;i++){ // modified FF v.3.1.5 (reduced from 100000 to 10000)
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
        
        
        
        
#ifdef LCP_alternative
        // v.3.1.5: create a LookUp table based on species identity and intraspecific deviation
        void InitialiseLookUpLAImax(){
            LookUpLAImax.reserve(10000 * nbspp); // 10000 is the possible number of combinations for intraspecific variation
            
            // since LAImax is defined at Tree level, we create pseudo trees (i.e. trees with only information on Pmass, Nmass and LMA) to calculate it from
            // for control purposes output min and max LAImax
            
            float minLAImax = 10.0; // fixed upper limit of LAImax is currently 10.0 m2/m2
            float maxLAImax = 0.0;
            float avgLAImax = 0.0;
            
            for(int spp = 1; spp < nbspp + 1; spp++){
                for(int dev = 0; dev < 10000; dev++){
                    Tree pseudotree;
                    pseudotree.t_sp_lab = spp;
                    
                    pseudotree.t_Pmass = S[spp].s_Pmass * d_intraspecific_P[dev];
                    pseudotree.t_Nmass = S[spp].s_Nmass * d_intraspecific_N[dev];
                    pseudotree.t_LMA = S[spp].s_LMA * d_intraspecific_LMA[dev];

#ifdef WATER
                    pseudotree.t_wsg = fmaxf(S[spp].s_wsg + d_intraspecific_wsg[dev], 0.05);
                    pseudotree.t_wleaf=sqrt(S[spp].s_leafarea*0.0001);
                    pseudotree.t_WSF=1;
                    pseudotree.t_WSF_A=1;
                    pseudotree.t_g1=(-3.97 * pseudotree.t_wsg + 6.53); //this is the relationship provided by Lin et al. 2015
#endif
                    
                    pseudotree.t_Vcmax = CalcVcmaxm(pseudotree.t_LMA, pseudotree.t_Nmass, pseudotree.t_Pmass) * pseudotree.t_LMA;
                    pseudotree.t_Jmax = pseudotree.CalcJmaxm() * pseudotree.t_LMA;
                    pseudotree.t_Rdark = CalcRdark(pseudotree.t_LMA, pseudotree.t_Nmass, pseudotree.t_Pmass, pseudotree.t_Vcmax);
                    
                    pseudotree.CalcLAImax();
                    
                    LookUpLAImax.push_back(pseudotree.t_LAImax);
                    if(pseudotree.t_LAImax < minLAImax) minLAImax = pseudotree.t_LAImax;
                    if(pseudotree.t_LAImax > maxLAImax) maxLAImax = pseudotree.t_LAImax;
                    avgLAImax += pseudotree.t_LAImax;
                }
            }
            
            avgLAImax *= 1.0/float(10000 * nbspp);
            Rcout << "Calculated LookUp table for LAImax. Min LAImax is: " << minLAImax << " | max LAImax is: " << maxLAImax << " avg LAImax is: " << avgLAImax << endl;
        }
#endif
        
        
        
        
        
        //! Global function: initialise lookup tables
        //! - this contains a number of empirical functions including:
        //! -# temperature dependence on Farquhar model parameters (following von Caemmerer 2000 and Bernacchi et al. 2003 PCE)
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
            //if(NULL==(LookUp_Rday=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_Rday" << endl; //newIM: no redundancy anymore between LookUp_Rday and LookUP_Rnight
            if(NULL==(LookUp_Rleaf=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_Rleaf" << endl; //newIM: no redundancy anymore between LookUp_Rday and LookUP_Rnight
            if(NULL==(LookUp_Rstem=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_Rstem" << endl;
            //if(NULL==(LookUp_Rnight=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_Rnight" << endl; //newIM: no redundancy anymore between LookUp_Rday and LookUP_Rnight
#ifdef WATER
            nbVPDbins=600;
            float VPDaccuracy=0.01;
            iVPDaccuracy=1.0/VPDaccuracy;
            Rcout << "Built-in maximal VPD: " << float(nbVPDbins)*VPDaccuracy <<endl;
            if(NULL==(LookUp_INLR=new float*[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_INLR" << endl;
            for (int i=1;i<nbTbins;i++) {
                if(NULL==(LookUp_INLR[i]=new float[nbVPDbins])) Rcerr<<"!!! Mem_Alloc LookUp_INLR" << endl;
            }
            //if(NULL==(LookUp_ESAT=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_ESAT" << endl;
            if(NULL==(LookUp_SLOPE=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_SLOPE" << endl;
            if(NULL==(LookUp_GRADN=new float[nbTbins])) Rcerr<<"!!! Mem_Alloc LookUp_GRADN" << endl;
            float ESAT0=0.61121;
#endif
            for(int i=1;i<nbTbins;i++) { // loop over "T" in GPPleaf()
                float temper=float(i)*Taccuracy;
                // !!!UPDATE provide references for these equations
                
#ifdef WATER
                LookUp_KmT[i] = 404.0*exp(((temper-25.0)/(298*0.00831*(273+temper)))*59.36)*
                (1+210*1.0/248.0*exp(-(temper-25.0)/(298*0.00831*(273+temper))*35.94)); // taken from von Caemmerer 2000, as in Domingues et al. 2010, for consistency
                LookUp_GammaT[i]=37.0*exp(((temper-25.0)/(298*0.00831*(273+temper)))*23.4); // taken from von Caemmerer 2000, as in Domingues et al. 2010, for consistency
#else
                LookUp_KmT[i] = 404.0*exp(((temper-25.0)/(298*0.00831*(273+temper)))*59.36)*
                (1+210*1.0/248.0*exp(-(temper-25.0)/(298*0.00831*(273+temper))*35.94))*iCair; // taken from von Caemmerer 2000, as in Domingues et al. 2010, for consistency
                LookUp_GammaT[i]=37.0*exp(((temper-25.0)/(298*0.00831*(273+temper)))*23.4)*iCair; // taken from von Caemmerer 2000, as in Domingues et al. 2010, for consistency
#endif
                LookUp_VcmaxT[i]=exp(26.35-65.33/(0.00831*(temper+273.15))); // taken from Bernacchi et al. 2003 PCE, as in Domingues et al. 2010 for consistency
                LookUp_JmaxT[i]=exp(17.57-43.54/(0.00831*(temper+273.15))); // taken from Bernacchi et al. 2003 PCE, as in Domingues et al. 2010 for consistency
                //LookUp_Rday[i]=exp((temper-25.0)*0.1*log(3.09-0.0215*(25.0+temper))); //newIM: no redundancy anymore between LookUp_Rday and LookUP_Rnight
                LookUp_Rleaf[i]=exp((temper-25.0)*0.1*log(3.09-0.0215*(25.0+temper))); //this is equ. 1 in Atkin et al. 2015 New phytologist //newIM: no redundancy anymore between LookUp_Rday and LookUP_Rnight
                LookUp_Rstem[i]=39.6*378.7*timestep*exp(((temper-25.0)/10.0)*log(2.0));
                //LookUp_Rnight[i]=exp((temper-25.0)*0.1*log(3.09-0.0215*(25.0+temper))); //newIM: no redundancy anymore between LookUp_Rday and LookUP_Rnight
                // exp((temp-25)/10*log(2)) is the temperature dependency of Rstem, supposing a constant Q10=2, according to Ryan et al 1994 and Meir & Grace 2002 exp((tnight-25)*0.1*log(3.09-0.0215*(25+tnight))) is the temperature dependencies used by Atkin 2015 (equ1)
#ifdef WATER
                // for computation of isothermal net long-range radiation
                float ESAT =0.61121 * exp((18.678 - temper/234.5) * temper/(257.14 + temper)); // Saturation partial pressure of water vapour in kPa From Jones 2013, Eq (5.15) page 102 (in agreement with Cochard 2019 equ 2 -- Buck equation)
                //float INLR=SIGMA*EMLEAF*pow(temper-ABSZERO,4.0); // Longwave radiation Jones (2014) Eq. 5.5, p101. // as in Jerome's script, but does not provide sound values (pb with EMLEAF ==> to be checked in Jones)
                for (int v=1; v<nbVPDbins; v++) {
                    float vpd=float(v)*VPDaccuracy;
                    //float ea=1000.0*(ESAT - vpd);
                    //float emaB0=0.642*pow(ea/(temper-ABSZERO),0.1428571); // cf. equ. D4 in Leuning et al. 1995 (and in Duursma's R package code)
                    // float emaB=1.24*pow(ea/(temper-ABSZERO),0.1428571); // cf. Brutsaert 1975 equ 11 (see also Marthews et al. 2012 Theor Appl Climatol, Table 2). Weird that this expression provides values >1 !
                    //float emaI=1-0.261*exp(-0.000777*temper*temper); // cf. Idso & Jackson 1969 (see also Marthews et al. 2012 Theor Appl Climatol, Table 2)
                    float ema=1/(SIGMA*pow(temper-ABSZERO,4.0))*(59.38 + (113.7*pow((ABSZERO-temper)/ABSZERO, 6)) + 96.96*sqrt(186*vpd/(temper-ABSZERO))); // cf. Dilley & O'Brien 1998 (see also Marthews et al. 2012 Theor Appl Climatol, Table 2); 186=4650/25
                    float INLR=(1-ema)*SIGMA*pow(temper-ABSZERO,4.0); // as in Duursma R package, following Leuning et al. 1995 PCE, equs D2-D5
                    LookUp_INLR[i][v]=INLR;
                    //Rcout << "T=" << temper << " vpd=" << vpd <<  " ema=" << ema <<  " INLR=" << INLR << endl;
                    
                }
                float SLOPE = 1000.0*(ESAT-ESAT0)/Taccuracy; // Slope of the ESAT function, but in Pa (not in kPa)
                ESAT0=ESAT;
                float GRADN = 4.*EMLEAF*SIGMA*pow(temper-ABSZERO,3.0)/(CPAIR*AIRMA);  // Equivalent conductance to radiative heat transfer Jones 2013 (page 101)
                //LookUp_ESAT[i]=ESAT;
                
                LookUp_SLOPE[i]=SLOPE;
                LookUp_GRADN[i]=GRADN;
#endif
            }
            
            
            
#ifdef WATER
            
            nbHbins=2000;
            float Haccuracy=0.01;
            iHaccuracy=1.0/Haccuracy;
            if(NULL==(LookUp_Wind=new float[nbHbins])) Rcerr<<"!!! Mem_Alloc LookUp_Wind" << endl;
            
            for(int i=1;i<nbHbins;i++) { // loop over "T" in GPPleaf()
                float h_ratio=float(i)*Haccuracy;
                
                if (h_ratio >= 1) {
                    LookUp_Wind[i]=0.831*log(16.67*((h_ratio)-0.8));       // drawn from Monteith & Unsworth 2008, with d = 0.8 H and z0 = 0.06 H (cf. Rau et al's TROLL wind manuscript).
                } else {
                    LookUp_Wind[i]=exp(-alphaInoue*(1-h_ratio));
                }
            }
            
            
#endif
            
            // look up table for flux averaging/integration
            // division into absorption prior to current voxel (absorb_prev) and absorption in current voxel (absorb_delta)
            // prior absorption has a maximum of 20 m2/m3, while absorption within one voxel can maximally reach 10 m2/m3
            if(NULL==(LookUp_flux_absorption=new float[80000])) Rcerr<<"!!! Mem_Alloc LookUp_flux" << endl;
            if(NULL==(LookUp_flux=new float[80000])) Rcerr<<"!!! Mem_Alloc LookUp_flux" << endl;
            if(NULL==(LookUp_ExtinctLW=new float[80000])) Rcerr<<"!!! Mem_Alloc LookUp_flux" << endl;
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
                        LookUp_ExtinctLW[i+400*j] = 0.8*exp(-0.8*absorb_prev); // extinction of thermal radiation, following Leuning et al. 1995 equ. D1
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
                        LookUp_ExtinctLW[i+400*j] = 0.8*exp(-0.8*absorb_prev); // extinction of thermal radiation, following Leuning et al. 1995 equ. D1
                        
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
                    if(!((xx == 0) && (yy == 0))){
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
            
            // new in v.3.1.6: reshuffle the allocation patterns slightly to avoid too regular-looking crowns (a cosmetic change)
            // in previous versions, a small minority of trees had fraction_filled values of very close to 0.75, 0.5, 0.25. Since we do not randomize allocation of filled/empty pixels per crown, but simply increment it step-wise (i.e. one empty gap in the crown every 4 pixels for a filled fraction of 0.75, or one empty gap every 2 pixels for 0.5, etc.), this meant that the determination of "holes" in the crown could accidentally align with the allocation of crown pixels in general (which usually have 4 or 8 pixels of the same distance following each other), and this could lead to weirdly regular shapes. This usually only occurred for a tiny fraction of trees and did not influence model behavior, but may create odd-looking outputs for people unfamiliar with models/TROLL, so it is desirable to remove it.
            // in the new version, we simply shuffle a few values that are close to each other - they have a similar or the same distance, so the overall outward-spiralling allocation remains the same, just with a slightly shifted order. To break the 4-based pattern, we use an increment of 7 and a distance of 3 for the exchanges. We start at the 6th pixel as the order will not matter for the innermost crown part.
            for(int i = 5; i < 2598; i += 7){
                int site_current = LookUp_Crown_site[i];
                int site_exchange = LookUp_Crown_site[i+3];
                LookUp_Crown_site[i] = site_exchange;
                LookUp_Crown_site[i+3] = site_current;
            }
            
#ifdef LCP_alternative
            InitialiseLookUpLAImax();
#endif
            
        }
            
            
            //! Global function: initialise output streams
            void InitialiseOutputStreams(){
                char nnn[200];
                if(!mpi_rank) {
                    sprintf(nnn,"%s_%i_sumstats.txt",buf, easympi_rank);
                    output_basic[0].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_initial_pattern.txt",buf, easympi_rank); // previously "state" output, but not used anymore, overwritten for initial pattern
                    output_basic[1].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_final_pattern.txt",buf, easympi_rank);
                    output_basic[2].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_final_SWC3D.txt",buf, easympi_rank);
                    output_basic[3].open(nnn, ios::out);
                    
                    // write headers for files
                    output_basic[0] << "iter\tsum1\tsum10\tsum30\tba\tba10\tagb\tgpp\tnpp\trday\trnight\trstem\tlitterfall" << endl;
                    // headers for initial and final patterns are written automatically
                    
                    sprintf(nnn,"%s_%i_sumstats_species.txt",buf, easympi_rank);
                    output_extended[0].open(nnn, ios::out);
                    output_extended[0] << "iter\tspecies\tsum1\tsum10\tsum30\tba\tba10\tagb\tgpp\tnpp\trday\trnight\trstem\tlitterfall" << endl;
                    
                    if(_OUTPUT_extended){
                        // sprintf(nnn,"%s_%i_sumstats_species.txt",buf, easympi_rank);
                        // output_extended[0].open(nnn, ios::out);
                        sprintf(nnn,"%s_%i_ppfd0.txt",buf, easympi_rank);
                        output_extended[1].open(nnn, ios::out);
                        sprintf(nnn,"%s_%i_death.txt",buf, easympi_rank);
                        output_extended[2].open(nnn, ios::out);
                        // sprintf(nnn,"%s_%i_death_snapshots.txt",buf, easympi_rank);
                        // output_extended[3].open(nnn, ios::out);
                        // sprintf(nnn,"%s_%i_deathrate.txt",buf, easympi_rank);
                        // output_extended[4].open(nnn, ios::out);
                        // sprintf(nnn,"%s_%i_sdd.txt",buf, easympi_rank);
                        // output_extended[5].open(nnn,ios::out);
                        // sprintf(nnn,"%s_%i_vertd.txt",buf, easympi_rank);
                        // output_extended[6].open(nnn,ios::out);
                        sprintf(nnn,"%s_%i_LAI.txt",buf, easympi_rank);
                        output_extended[7].open(nnn, ios::out);
                        sprintf(nnn,"%s_%i_CHM.txt",buf, easympi_rank);
                        output_extended[8].open(nnn, ios::out);
                        
                        // write headers
                        // output_extended[0] << "iter\tspecies\tsum1\tsum10\tsum30\tba\tba10\tagb\tgpp\tnpp\trday\trnight\trstem\tlitterfall" << endl;
                        output_extended[1] << "iter\tvariable\tvalue\tresidual" << endl;
                        if(_BASICTREEFALL) output_extended[2] << "iter\tnbdead_n1\tnbdead_n10\tnbTreefall1\tnbTreefall10" << endl;
                        else output_extended[2] << "iter\tnbdead_n1\tnbdead_n10" << endl;
                        // output_extended[3] << "iter\tspecies\tage\tdbh\theight" << endl;
                        // output_extended[4] << "iter\twsg\tdbh\tbasal\tdr" <<  endl;
                        // output_extended[5] << "iter\td\tfreq" << endl;
                        // output_extended[6] << "iter\th\tfreq" << endl;
                        
                        if(extent_visual > 0){
                            sprintf(nnn,"%s_%i_visual_field.txt",buf, easympi_rank);
                            output_visual[0].open(nnn, ios::out);
#ifdef CHM_SPIKEFREE
                            output_visual[0] << "iter" << "\t" << "row" << "\t" << "col" << "\t"  << "height" << "\t" << "height_spikefree" << "\t" << "LAI" << endl; // header
#else
                            output_visual[0] << "iter" << "\t" << "row" << "\t" << "col" << "\t"  << "height" << "\t" << "LAI" << endl; // header
#endif
                            
                            sprintf(nnn,"%s_%i_visual_slice.txt",buf, easympi_rank);
                            output_visual[1].open(nnn, ios::out);
                            output_visual[1] << "iter" << "\t" << "row" << "\t" << "col" << "\t"  << "height" << "\t" << "sp_lab" << "\t" << "ratio_height_Ct" << "\t" << "ratio_NPP_GPP" << endl; // header
                        }
                    }
                    
                    // v.3.1.6 output for point cloud
                    if(_OUTPUT_pointcloud){
                        sprintf(nnn,"%s_%i.las",buf, easympi_rank);
                        output_pointcloud.open(nnn, ios::out | ios::binary);
                        output_pointcloud.imbue(locale::classic()); // justification here: https://stackoverflow.com/questions/14750496/sending-integer-to-fstream-as-little-endian; locale regulates how streams print and read values (i.e. commas vs. points for decimals, etc.); setting it to classic to ensure portability, but not entirely sure how important this is in practice for binary files
                        
                    }
                    
#ifdef Output_ABC
                    sprintf(nnn,"%s_%i_abc_traitconservation.txt",buf, easympi_rank);
                    output_abc[0].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_abc_ground.txt",buf, easympi_rank);
                    output_abc[1].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_abc_chm.txt",buf, easympi_rank);
                    output_abc[2].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_abc_chmALS.txt",buf, easympi_rank);
                    output_abc[3].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_abc_transmittance.txt",buf, easympi_rank);
                    output_abc[4].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_abc_transmittanceALS.txt",buf, easympi_rank);
                    output_abc[5].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_abc_species.txt",buf, easympi_rank);
                    output_abc[6].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_abc_species10.txt",buf, easympi_rank);
                    output_abc[7].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_abc_traits.txt",buf, easympi_rank);
                    output_abc[8].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_abc_traits10.txt",buf, easympi_rank);
                    output_abc[9].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_abc_biomass.txt",buf, easympi_rank);
                    output_abc[10].open(nnn, ios::out);
#endif
                    
#ifdef WATER
                    //sprintf(nnn,"%s_%i_water_balance.txt",buf, easympi_rank);
                    //output_water[0].open(nnn, ios::out);
                    
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
                    sprintf(nnn,"%s_%i_SWC_begin.txt",buf, easympi_rank);
                    output[1].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_SWC_mid.txt",buf, easympi_rank);
                    output[2].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_SWC_end.txt",buf, easympi_rank);
                    output[3].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_SWP_begin.txt",buf, easympi_rank);
                    output[4].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_SWP_mid.txt",buf, easympi_rank);
                    output[5].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_SWP_end.txt",buf, easympi_rank);
                    output[6].open(nnn, ios::out);
                    // sprintf(nnn,"%s_%i_litterfall.txt",buf, easympi_rank);
                    // output[7].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_waterfluxes_begin.txt",buf, easympi_rank);
                    output[18].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_waterfluxes_mid.txt",buf, easympi_rank);
                    output[19].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_waterfluxes_end.txt",buf, easympi_rank);
                    output[20].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_LAIdynamics.txt",buf, easympi_rank);
                    output[21].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_LAIyoung.txt",buf, easympi_rank);
                    output[22].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_LAImature.txt",buf, easympi_rank);
                    output[23].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_LAIold.txt",buf, easympi_rank);
                    output[24].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_phi_root.txt",buf, easympi_rank);
                    output[31].open(nnn, ios::out);
                    sprintf(nnn,"%s_%i_LAIprofile.txt",buf, easympi_rank);
                    output[32].open(nnn,ios::out);
                    sprintf(nnn,"%s_%i_soilproperties.txt",buf, easympi_rank);
                    output[33].open(nnn,ios::out);

                    // write headers
                    output[21] << "iter\tLAI" << endl;
                    output[22] << "iter";
                    output[23] << "iter";
                    output[24] << "iter";
                    for (int l=0; l<HEIGHT+1; l++) {
                        output[22] << "\t" << "h_" << l;
                        output[23] << "\t" << "h_" << l;
                        output[24] << "\t" << "h_" << l;
                    }
                    output[22] << endl;
                    output[23] << endl;
                    output[24] << endl;
                    output[31] << "iter\tabundance_weighted_phi_root\tabundance_weighted_phi_root_n10\tagb_weighted_phi_root" << endl;
                    output[32] << "iter\theight\tLAI" << endl;  
                    output[1] << "layer";
                    output[2] << "layer";
                    output[3] << "layer";
                    output[4] << "layer";
                    output[5] << "layer";
                    output[6] << "layer";
                    output[18] << "variable";
                    output[19] << "variable";
                    output[20] << "variable";
                    for (int d=0; d<nbdcells;d++) {
                        output[1] << "\t" << "dcell_" << d;
                        output[2] << "\t" << "dcell_" << d;
                        output[3] << "\t" << "dcell_" << d;
                        output[4] << "\t" << "dcell_" << d;
                        output[5] << "\t" << "dcell_" << d;
                        output[6] << "\t" << "dcell_" << d;
                        output[18] << "\t" << "dcell_" << d;
                        output[19] << "\t" << "dcell_" << d;
                        output[20] << "\t" << "dcell_" << d;
                    }
                    output[1] << endl;
                    output[2] << endl;
                    output[3] << endl;
                    output[4] << endl;
                    output[5] << endl;
                    output[6] << endl;
                    output[18] << endl;
                    output[19] << endl;
                    output[20] << endl;
                    output[11] << "iter\tprecipitation\tinterception\tthroughfall\trunoff\tleak\tevaporation";
                    for(int l=0; l<nblayers_soil; l++)
                        output[11] << "\t" << "transpitation_" << l;
                    output[11] << "\t" << "transpiration1016";
                    for(int l=0; l<nblayers_soil; l++)
                        output[11]  << "\t" << "SWC_" << l;
                    for(int l=0; l<nblayers_soil; l++)
                        output[11]  << "\t" << "SWP_" << l;
                    output[11] << endl;
                    string sites_headers = "iter\tsite\tsp_lab\theight\tdbh\tlitter\tage\tLA\tyougLA\tmatureLA\toldLA\tCR\tCD\tGPP\tNPP\tRstem\tRnight\tLAI3D_canopy\tLAI3D_base_crown\troot_depth\tphi_root\tWSFs\tWSFns\ttranspiration\tLAImax\tLAmax";
                    output[12] << sites_headers;
                    output[13] << sites_headers;
                    output[14] << sites_headers;
                    output[15] << sites_headers;
                    output[16] << sites_headers;
                    output[17] << sites_headers;
                    output[28] << sites_headers;
                    output[29] << sites_headers;
                    output[30] << sites_headers;
                    for(int l=0; l<nblayers_soil; l++){
                        output[12] << "\t" << "root_biomass_" << l;
                        output[13] << "\t" << "root_biomass_" << l;
                        output[14] << "\t" << "root_biomass_" << l;
                        output[15] << "\t" << "root_biomass_" << l;
                        output[16] << "\t" << "root_biomass_" << l;
                        output[17] << "\t" << "root_biomass_" << l;
                        output[28] << "\t" << "root_biomass_" << l;
                        output[29] << "\t" << "root_biomass_" << l;
                        output[30] << "\t" << "root_biomass_" << l;
                    }
                    for(int l=0; l<nblayers_soil; l++){
                        output[12] << "\t" << "soil_layer_weight_" << l;
                        output[13] << "\t" << "soil_layer_weight_" << l;
                        output[14] << "\t" << "soil_layer_weight_" << l;
                        output[15] << "\t" << "soil_layer_weight_" << l;
                        output[16] << "\t" << "soil_layer_weight_" << l;
                        output[17] << "\t" << "soil_layer_weight_" << l;
                        output[28] << "\t" << "soil_layer_weight_" << l;
                        output[29] << "\t" << "soil_layer_weight_" << l;
                        output[30] << "\t" << "soil_layer_weight_" << l;
                    }
                    output[12] << endl;
                    output[13] << endl;
                    output[14] << endl;
                    output[15] << endl;
                    output[16] << endl;
                    output[17] << endl;
                    output[28] << endl;
                    output[29] << endl;
                    output[30] << endl;  

#endif
                    
                    
#ifdef WATER
                    
                    
if (_WATER_RETENTION_CURVE==1) {
                    output[33] << "layer" << "\t" << "depth" << "\t" << "sat" << "\t" << "max" << "\t" << "fc" << "\t" << "res" << "\t" << "min" << "\t" << "Ksat" << "\t" << "a_vgm" << "\t" << "m_vgm" << endl;
                    for (int l=0; l<nblayers_soil; l++) {
                    output[33] << l  << "\t" << layer_depth[l]  << "\t" << Sat_SWC[l] << "\t" << Max_SWC[l]  << "\t" << FC_SWC[l] << "\t" << Res_SWC[l] << "\t" << Min_SWC[l] << "\t" << Ksat[l] << "\t" << a_vgm[l] << "\t" << m_vgm[l] << endl;
                    }

} else if (_WATER_RETENTION_CURVE==0) {
                    
                    output[33] << "layer" << "\t" << "depth" << "\t" << "sat" << "\t" << "max" << "\t" << "fc" << "\t" << "res" << "\t" << "min" << "\t" << "Ksat" << "\t" << "phi_e" << "\t" << "b" << endl;
                    for (int l=0; l<nblayers_soil; l++) {
                    output[33] << l  << "\t" << layer_depth[l]  << "\t" << Sat_SWC[l] << "\t" << Max_SWC[l] << "\t" << FC_SWC[l] << "\t" << Res_SWC[l] << "\t" << Min_SWC[l] << "\t" << Ksat[l] << "\t" << phi_e[l] << "\t" << b[l] << endl;
                         }
}
                   
#endif
                    
#ifdef TRACK_INDIVIDUALS
                    if(_OUTPUT_extended){
                        // these are the "cases" of trees that are followed
                        sprintf(nnn,"%s_%i_trees_fortracking.txt",buf, easympi_rank);
                        output_track[0].open(nnn, ios::out);
                        output_track[0] << "site" << "\t" << "timeofyear_born" << "\t" << "col" << "\t" << "row" << "\t" << "species" << "\t" << "dbh" << "\t" << "cr" << "\t" << "height"  << "\t" << "agb" << "\t" << "multiplier_cr" << "\t" << "multiplier_height" << "\t" << "wsg" << "\t" << "Nmass" << "\t" << "Pmass" << "\t" << "LMA" << "\t" << "deviation_wsg" << "\t" << "multiplier_Nmass" << "\t" << "multiplier_Pmass" << "\t" << "multiplier_lma" << "\t" << "Vcmax" << "\t" << "Jmax" << "\t" << "Rdark" << "\t" << "LAImax" << "\t" << "leaflifespan" << endl;
                        
                        // these are the "activities" that are recorded for each tree
                        sprintf(nnn,"%s_%i_trees_tracked.txt",buf, easympi_rank);
                        output_track[1].open(nnn, ios::out);
                        
                        output_track[1] << "site" << "\t" << "timeofyear_born" << "\t" << "Iter" << "\t" << "age" << "\t" << "seeds" << "\t" << "seedstotal" << "\t" << "carbstarv" << "\t" << "carbstarvtotal" << "\t" << "dbh" << "\t" << "dbhgrowth" << "\t" << "height" << "\t" << "heightgrowth" << "\t" << "cr" << "\t" << "crgrowth" << "\t" << "agb" << "\t" << "agbgrowth" << "\t" << "GPP" << "\t" << "GPPsq" << "\t" << "NPP" << "\t" << "NPPsq" << "\t" << "Rday" << "\t" << "Rnight" << "\t" << "Rstem" << "\t" << "LAIabove_avg" << "\t" << "LAIabove_effavg" << "\t" << "carbstore_avg" << endl;
                        
                        // this is to get information on the dead trees > 10cm
                        sprintf(nnn,"%s_%i_trees_aftertracking.txt",buf, easympi_rank);
                        output_track[2].open(nnn, ios::out);
                        
                        output_track[2] << "site" << "\t" << "timeofyear_born" << "\t" << "Iter" << "\t" << "age" << "\t" << "seedstotal" << "\t" << "carbstarvtotal" << "\t" << "dbh" << "\t" << "height" << "\t" << "cr" << "\t" << "agb" << "\t" << "GPP" << "\t" << "NPP" << "\t" << "LAIabove_avg" << "\t" << "LAIabove_effavg" << "\t" << "GPPsquared" << "\t" << "NPPsquared" << "\t" << "LAIabovesquared_avg" << "\t" << "LAIabovesquared_effavg" << endl;
                    }
#endif
                }
            }

        
        //######################################
        // Global function: initialisation with bare ground conditions
        //######################################
        void Initialise() {
            //** Initialization of the simulation parameters **
            //*************************************************
            
            iter = -1;  // changed in v.3.0.1, previously undefined; new function GetTimeofyear accepts also negative iterations

            nblivetrees = 0;
            
            ReadInputSpecies();
#ifdef FULL_CLIMATE
            ReadInputClimate();
            ReadInputDailyvar();
#else
            ReadInputDailyvar();
            ReadInputClimate();
#endif
            
#ifdef WATER
            ReadInputSoil();
#endif
            
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
        // Global function: point cloud generation
        //######################################
        // added v.3.1.6
        
        void ReadInputPointcloud(){
            Rcout << endl << "Reading in file: " << inputfile_pointcloud << endl;
            
            fstream InPointcloud(inputfile_pointcloud, ios::in);
            
            if(InPointcloud){
                string parameter_names[5] = {"mean_beam_pc","sd_beam_pc","klaser_pc","transmittance_laser","iter_pointcloud_generation"};
                int nb_parameters = 5;
                vector<string> parameter_values(nb_parameters,"");
                
                Rcout << endl << "Reading in file: " << inputfile << endl;
                InPointcloud.getline(buffer,256,'\n');
                string parameter_name, parameter_value;
                
                while (InPointcloud >> parameter_name >> parameter_value){
                    InPointcloud.getline(buffer,256,'\n');
                    for(int i = 0; i < nb_parameters; i++){
                        if(parameter_name == parameter_names[i]) parameter_values[i] = parameter_value;
                    }
                }
                // now we assign values
                for(int i = 0; i < nb_parameters; i++){
                    AssignValuePointcloud(parameter_names[i], parameter_values[i]);
                }
            }
        }
        
        
        //######################################
        // Global function: initialisation from inventories
        //######################################
        
        // completely rewritten in v.3.1
        void ReadInputInventory(){
            
            Rcout << endl << "Reading in file: " << inputfile_inventory << endl;
            
            fstream InInventory(inputfile_inventory, ios::in);
            
            if(InInventory){
                // possible parameters to initialise
                //        vector<string> parameter_names{"col","row","s_name","CrownDisplacement","Pmass","Nmass","LMA","wsg","Rdark","Vcmax","Jmax","leaflifespan","lambda_young","lambda_mature","lambda_old","dbhmature","dbhmax","hmax","ah","Ct","LAImax","fraction_filled","mult_height","mult_CR","mult_CD","mult_P","mult_N","mult_LMA","mult_dbhmax","dev_wsg","age","dbh","sapwood_area","height","CD","CR","GPP","NPP","Rday","Rnight","Rstem","LAmax","LA","youngLA","matureLA","oldLA","LAI","litter","carbon_storage","carbon_biometry","multiplier_seed","hurt","NPPneg"};
                //        int nb_parameters = int(parameter_names.size()); // only works from C++11 onwards
                
#ifdef WATER
                string parameter_names_hardcoded[63] = { "col", "row", "s_name", "CrownDisplacement", 
                                                         "Pmass", "Nmass", "LMA", "wsg",
                                                         "Rdark", "Vcmax", "Jmax", "leaflifespan",
                                                         "lambda_young", "lambda_mature", "lambda_old",
                                                         "dbhmature", "dbhmax", "hmax", "ah", "Ct",
                                                         "LAImax", "fraction_filled",
                                                         "mult_height", "mult_CR", "mult_CD", "mult_P", "mult_N", "mult_LMA",
                                                         "mult_dbhmax", "dev_wsg", "age", "dbh", "sapwood_area", "height", "CD", "CR",
                                                         "GPP", "NPP", "Rday", "Rnight", "Rstem", 
                                                         "LAmax", "LA", "youngLA", "matureLA", "oldLA", "LAI", "litter",
                                                         "carbon_storage", "carbon_biometry", "multiplier_seed", "hurt", "NPPneg", 
                                                         "root_depth", "phi_root", "WSF", "WSF_A", "transpiration", "g1_0", "g1", 
                                                         "Ndays_dry", "Ndays_wet", "pheno_factor"};
                
                int nb_parameters = 63;
                
#else
                
                string parameter_names_hardcoded[53] = { "col","row","s_name","CrownDisplacement","Pmass","Nmass","LMA","wsg","Rdark","Vcmax","Jmax","leaflifespan","lambda_young","lambda_mature","lambda_old","dbhmature","dbhmax","hmax","ah","Ct","LAImax","fraction_filled","mult_height","mult_CR","mult_CD","mult_P","mult_N","mult_LMA","mult_dbhmax","dev_wsg","age","dbh","sapwood_area","height","CD","CR","GPP","NPP","Rday","Rnight","Rstem","LAmax","LA","youngLA","matureLA","oldLA","LAI","litter","carbon_storage","carbon_biometry","multiplier_seed","hurt","NPPneg"};
                
                int nb_parameters = 53;
                
                
#endif
            
                
                // conversion to vector for future use (e.g. water module needs flexible number of input layers)
                vector<string> parameter_names(parameter_names_hardcoded,  parameter_names_hardcoded + nb_parameters);
                
#ifdef WATER
                
                for(int l=0;l<nblayers_soil;l++){
                    char par_name[30];
                    sprintf(par_name, "root_biomass%d", l);
                    parameter_names.push_back(par_name);
                }
                nb_parameters += nblayers_soil;
                for(int l=0;l<nblayers_soil;l++){
                    char par_name[30];
                    sprintf(par_name, "soil_layer_weight%d", l);
                    parameter_names.push_back(par_name);
                }
                nb_parameters += nblayers_soil;
                
#endif
                
                // define position of col and row in string vector
                int pcol = 0, prow = 0;
                while(pcol < nb_parameters && parameter_names[pcol] != "col") pcol++;
                while(prow < nb_parameters && parameter_names[prow] != "row") prow++;
                
                // first get parameter names
                string line;
                getline(InInventory,line);
                istringstream firstlinestream(line);
                
                string parameter_name;
                vector<int> parameter_index;
                int nb_parameters_fromfile = 0;
                
                int flag_col = 1, flag_row = 1, flag_dbh = 1, flag_species = 1;
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
                    
                    if(parameter_name == "col") flag_col = 0;
                    if(parameter_name == "row") flag_row = 0;
                    if(parameter_name == "dbh") flag_dbh = 0;
                    if(parameter_name == "s_name") flag_species = 0;
                }
                
                // Warnings
                if(flag_dbh == 1){
                    Rcout << "WARNING! No diameter column provided, no initialization from file will be carried out." << endl;
                } else {
                    if((flag_col == 1) | (flag_row == 1)) Rcout << "WARNING! At least one coordinate column (col/row) is missing, random coordinates chosen" << endl;
                    if(flag_species == 1) Rcout << "WARNING! No species column provided, random species chosen" << endl;
                    
                    int nb_parameterlines = 0;
                    int nb_individuals = 0;
                    int nb_random = 0;
                    int nb_moved = 0;
                    int nb_speciesrandom = 0;
                    
                    // define a radius to look for alternative tree positions in case site is already initialized
                    int radius_max = 5;
                    int area_max = int(radius_max * radius_max * PI);
                    
                    // define an array of randomly permuted sites so that random number generator does not have to be called too often
                    vector<int> sites_shuffled;
                    sites_shuffled.reserve(sites);
                    for(int s = 0; s < sites; s++){
                        sites_shuffled.push_back(s);
                    }
                    random_shuffle ( sites_shuffled.begin(), sites_shuffled.end() );
                    
                    int sites_shuffled_index = 0;
                    
                    // we go through all lines in the input file */
                    while(getline(InInventory, line)){
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
                        
                        // additional checks on location of tree
                        int col = -1, row = -1;
                        bool quiet = 1;
                        if(flag_col == 0 && flag_row == 0){
                            SetParameter(parameter_names[pcol], parameter_values[pcol], col, 0, cols-1, -1, quiet);
                            SetParameter(parameter_names[prow], parameter_values[prow], row, 0, rows-1, -1, quiet);
                        }
                        
                        if(col >= 0 && row >= 0){
                            int site = col + row * cols;
                            int i = 0;
                            while(T[site].t_age != 0.0 && i < area_max){
                                i++;
                                int site_relative = LookUp_Crown_site[i];
                                int row_new = row + site_relative/51 - 25;
                                int col_new = col + site_relative%51 - 25;
                                if(row_new >= 0 && row_new < rows && col_new >= 0 && col_new < cols){
                                    site = col_new + row_new * cols;
                                }
                            }
                            
                            if(T[site].t_age == 0.0){
                                int success = T[site].BirthFromInventory(site, parameter_names, parameter_values, nb_speciesrandom);
                                if(success == 1){
                                    nb_individuals++;
                                    if(i > 0) nb_moved++;
                                }
                            }
                        } else {
                            // find a random free site
                            while(sites_shuffled_index < sites && T[sites_shuffled[sites_shuffled_index]].t_age != 0.0) sites_shuffled_index++;
                            
                            if(sites_shuffled_index < sites){
                                // as long as the search has stopped and the index has not run outside the range, the tree can be initialized
                                int site = sites_shuffled[sites_shuffled_index];
                                int success = T[site].BirthFromInventory(site, parameter_names, parameter_values, nb_speciesrandom);
                                if(success == 1){
                                    nb_individuals++;
                                    nb_random++;
                                }
                            }
                        }
                        
                        nb_parameterlines++;
                    }
                    
                    if(nb_parameterlines > 0){
                        Rcout << "Successfully initialised " << nb_individuals << " out of " << nb_parameterlines << " trees from file." << endl;
                        Rcout << "Coordinates: " << nb_moved << " trees were moved due to overlapping coordinates, and " << nb_random << " were placed randomly on the grid as coordinates were incomplete or could not be read in." << endl;
                        Rcout << "Species: " << nb_speciesrandom << " trees were assigned a random species." << endl;
                        if(sites_shuffled_index == sites) Rcout << "WARNING: shuffle index: " << sites_shuffled_index << " is equal site number. This means that random placement of trees has stopped early as the algorithm has run out of grid cells to place trees. This may indicate that there were more trees than grid cells." << endl;
                        
                    } else {
                        Rcout << "WARNING! Inventory file was empty. No trees were initialised." << endl;
                    }
                }
            } else {
                Rcout << "ERROR with the inventory file" << endl;
            }
            
            InInventory.close();
            
            Rcout << endl << "Reading in file: " << inputfile_SWC << endl;
            
            fstream InSWC(inputfile_SWC, ios::in);
            
            if(InSWC){
                
                string line;
                int l=0;
                int dcell=0;
                while(getline(InSWC, line)){
                    istringstream linestream(line);
                    linestream >> dcell;
                    float SWC_value;
                    while(linestream >> SWC_value){
                        SWC3D[l][dcell]=SWC_value;
                        l++;
                    }
                    if (l != nblayers_soil) Rcout << "Error in SWC input file (number of layers for dcell " << dcell << endl;
                    l=0;
                    dcell++;
                }
                
                if (dcell != nbdcells) Rcout << "Error in SWC input file (number of dcells) " << endl;
                
            } else {
                Rcout << "ERROR with the input SWC file" << endl;
            }
            
            InSWC.close();
            
            
            // added in v.3.1 to reduce transient behavior of forest on reinitialization when leaf parameters and tree dimensions are not known, but not used yet: does not massively improve the initial configuration of trees, as the main problem is in overlapping crowns and not how they allocate their leaf area; could be solved by a preceding Canopy Constructor reshuffle (Fischer et al. 2020, RSE), or by
            //*#################################################################*/
            //*### Update Leaf Area for more realistic initial configuration ###*/
            //*#################################################################*/
            // To do so, we order by height first and then allocate leaf area from top to bottom
            //     vector<float> heights_trees;
            //     vector<int> sites_trees;
            //
            //     heights_trees.reserve(sites);
            //     sites_trees.reserve(sites);
            //
            //     for(int site = 0; site < sites; site++){
            //        // Only consider non-initialized trees (i.e. t_LA < 0.0)
            //        if(T[site].t_age > 0.0 & T[site].t_LA < 0.0){
            //            float height = T[site].t_height;
            //            heights_trees.push_back(height);
            //            sites_trees.push_back(site);
            //
            //            int index_tree_current = int(sites_trees.size()) - 1;
            //
            //            while(index_tree_current > 0 && height > heights_trees[index_tree_current - 1]){
            //                heights_trees[index_tree_current] = heights_trees[index_tree_current - 1];
            //                sites_trees[index_tree_current] = sites_trees[index_tree_current - 1];
            //
            //                heights_trees[index_tree_current - 1] = height;
            //                sites_trees[index_tree_current - 1] = site;
            //                index_tree_current--;
            //            }
            //        }
            //     }
            //
            //     // clear voxel field
            //     for(int h=0;h<(HEIGHT+1);h++)
            //         for(int sbsite=0;sbsite<sites+2*SBORD;sbsite++)
            //             LAI3D[h][sbsite] = 0.0;
            //
            //     // allocate and compute leaf area
            //     for(int index_site = 0; index_site < sites_trees.size(); index_site++){
            //        int site = sites_trees[index_site];
            //        //Rcout << site << " Site of tree: " << T[site].t_site << " Height: " << T[site].t_height << " Height from index: " << heights_trees[index_site] << endl;
            //        T[site].CalcLAinitial();
            //     }
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
            
            for(int spp = 1; spp <= nbspp; spp++){
                double prob_species = double(S[spp].s_nbext);
                //Rcout << "prob_species: " << prob_species << endl;
                p_species[spp - 1] = prob_species;
            }
            
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
                    //SWC3D[l][dcell]=Max_SWC[l];
                    SWC3D[l][dcell]=FC_SWC[l];
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
            if(NULL==(LAI_DCELL=new float*[HEIGHT+1]))
                Rcerr<<"!!! Mem_Alloc\n";
            for(int h=0;h<(HEIGHT+1);h++) {
                if(NULL==(LAI_DCELL[h]=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            }
            if(NULL==(LAI_young=new float[HEIGHT+1]))
                Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(LAI_mature=new float[HEIGHT+1]))
                Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(LAI_old=new float[HEIGHT+1]))
                Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(Canopy_height_DCELL=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(TopWindSpeed_DCELL=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(HSum_DCELL=new int[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(Interception=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(Throughfall=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(Runoff=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(Leakage=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            if(NULL==(Evaporation=new float[nbdcells])) Rcerr<<"!!! Mem_Alloc\n";
            for(int dcell=0; dcell<nbdcells; dcell++) {
                for(int h=0;h<(HEIGHT+1);h++) LAI_DCELL[h][dcell]=0.0;
                Canopy_height_DCELL[dcell]=0.0;
                HSum_DCELL[dcell]=0;
                TopWindSpeed_DCELL[dcell]=0.0;
                Interception[dcell]=0.0;
                Throughfall[dcell]=0.0;
                Runoff[dcell]=0.0;
                Leakage[dcell]=0.0;
                Evaporation[dcell]=0.0;
            }
            for(int h=0;h<(HEIGHT+1);h++) {
                LAI_young[h]=0.0;
                LAI_mature[h]=0.0;
                LAI_old[h]=0.0;
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
        // Global function: Evolution at each timestep
        //######################################
        void Evolution() {
#ifdef CHECK_CARBON
            if(iter == 0){
                carbon_assimilated_total = 0.0;
                carbon_net_total = 0.0;
            }
#endif
            
#ifdef WATER
            transpiration_1016=0.0;
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
            
            for (int d=0; d<nbdcells; d++) {
                for (int l=0;l<nblayers_soil;l++) {
                    Transpiration[l][d]=0.0;
                }
            }
            for(int site=0;site<sites;site++) {
                T[site].Water_uptake();
            }
        
            // Update trees
            Average();                          //! Compute averages for outputs
            // if(_OUTPUT_extended) OutputField(); //! Output the statistics
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
            
#ifdef FULL_CLIMATE
            
            tnight=NightTemperature[iter%nbdays];
            precip=Rainfall[iter%nbdays];
            WSDailyMean=DailyMeanWindSpeed[iter%nbdays];
            WDailyMean=DailyMeanIrradiance[iter%nbdays]*SWtoPPFD;
            tDailyMean=DailyMeanTemperature[iter%nbdays];
            VPDDailyMean=DailyMeanVapourPressureDeficit[iter%nbdays];
            
#else
            tnight=NightTemperature[iter%iterperyear];
            precip=Rainfall[iter%iterperyear];
            WSDailyMean=DailyMeanWindSpeed[iter%iterperyear];
            WDailyMean=DailyMeanIrradiance[iter%iterperyear]*SWtoPPFD;
            tDailyMean=DailyMeanTemperature[iter%iterperyear];
            VPDDailyMean=DailyMeanVapourPressureDeficit[iter%iterperyear];
            
#endif // FULL_CLIMATE
            
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
            for (int d=0; d<nbdcells; d++) {
                Runoff[d]=0.0;
                Interception[d]=0.0;
                Throughfall[d]=0.0;
                Evaporation[d]=0.0;
                Leakage[d]=0.0;
                //for (int l=0;l<nblayers_soil;l++) {
                //    Transpiration[l][d]=0.0;
                //}
                for(int h=0;h<(HEIGHT+1);h++) {
                    LAI_DCELL[h][d]=0.0;
                }
                Canopy_height_DCELL[d]=0.0;
                HSum_DCELL[d]=0;
                TopWindSpeed_DCELL[d]=0.0;
            }
            
            // LAI_DCELL[h][dcell] provide the average LAI at height h in dcell, to estimate water interception and evaporation in each dcell at each timestep, as well as wind speed for a given height (this latter is new and was added simultaneously to BOUNDARY LAYER_ITERATIVE_SCHEME (IM June 2021).
            for(int site=0;site<sites;site++) {
                
                for(int h=0;h<(HEIGHT+1);h++) { LAI_DCELL[h][site_DCELL[site]]+=LAI3D[h][site+SBORD];}
                
                float Htop = 0.0;
                int H = HEIGHT;
                while (Htop == 0.0 && H > 0){
                    if (LAI3D[H][site+SBORD] > 0.0){
                        Htop = H;
                    }
                    H--;
                }
                
                if (Htop>0) {
                    Canopy_height_DCELL[site_DCELL[site]] += Htop;
                    HSum_DCELL[site_DCELL[site]]++;
                }
                
            }
            
            
            for (int d=0; d<nbdcells; d++) {
                
                for(int h=0;h<(HEIGHT+1);h++) LAI_DCELL[h][d]*=i_sites_per_dcell;
                if(HSum_DCELL[d] > 0) {
                    Canopy_height_DCELL[d] *=1.0/float(HSum_DCELL[d]);
                } else {
                    Canopy_height_DCELL[d] = 0.0;
                }
                
#ifdef FULL_CLIMATE
                if (Canopy_height_DCELL[d]<=MeteoStation_Height) {
                    TopWindSpeed_DCELL[d]=1.204/log(16.67*((MeteoStation_Height/Canopy_height_DCELL[d])-0.8)); // WS is the timestep windspeed at a height=MeteoStation_Height, and TopWindSpeed_DCELL is the wind speed computed at a height=Canopy_height_DCELL[d], according to the model of Monteith & Unsworth 2008 (see Rau et al's TROLL manuscript), with d=0.8H and z0=0.06H; 16.67~1/0.06, 1.204=log(0.2/0.06).
                } else TopWindSpeed_DCELL[d]=exp(alphaInoue*(1-MeteoStation_Height/Canopy_height_DCELL[d]));
                if (Canopy_height_DCELL[d]==0) {
                    Rcout << "in UpdateField: d=" << d << "; Canopyheight_DCELL[d]=" << Canopy_height_DCELL[d] << "; HSum_DCELL[d]=" << HSum_DCELL[d] << "; TopWindSpeed_DCELL[d]=" << TopWindSpeed_DCELL[d] << endl;
                }
#else
               if (Canopy_height_DCELL[d]<=MeteoStation_Height) {
                TopWindSpeed_DCELL[d]=WSDailyMean*1.204/log(16.67*((MeteoStation_Height/Canopy_height_DCELL[d])-0.8)); // WS is the timestep windspeed at a height=MeteoStation_Height, and TopWindSpeed_DCELL is the wind speed computed at a height=Canopy_height_DCELL[d], according to the model of Monteith & Unsworth 2008 (see Rau et al's TROLL manuscript), with d=0.8H and z0=0.06H; 16.67~1/0.06, 1.204=log(0.2/0.06).
               } else TopWindSpeed_DCELL[d]=WSDailyMean*exp(alphaInoue*(1-MeteoStation_Height/Canopy_height_DCELL[d]));
#endif
                
                // Rcout << "iter=" << iter << " WSDailyMean=" << WSDailyMean << " d=" << d <<  " canopy_height_DCELL[d]=" << Canopy_height_DCELL[d] << " HSum_DCELL[d]=" << HSum_DCELL[d] <<" TopWindSpeed_DCELL[d]=" << TopWindSpeed_DCELL[d];
                //if (d==225) Rcout << " d=" << d <<  " canopy_height_DCELL[d]=" << Canopy_height_DCELL[d] ;
                // Rcout << endl;
                
            }
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
            
            //for(int site=0;site<sites;site++) T[site].Water_uptake(); // Update of Transpiration: tree water uptake, each tree will deplete soil water content through its transpiration. Now made ate the end of the evolution loop so that the outputs for water uptake match the others (otherwise lag of one timestep)
            
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
                // However it is sometimes neglected and not represented in models, eg. Laio et al. 2001, Guterriez et al. 2014, Fischer et al. 2014.
                // note that, in this version, evaporation only depletes the most superficial soil layer. This could be changed, especially if the superficial soil layer is particularly thin and the energy reaching the soil high.
                
                
                // here, we use a phenomenological approach, following Granier et al. 1999 Ecological Modelling and Wagner et al. 2011 AFM, which assumed that evaporation is proportional to the energy reaching the soil.[this is an approximation as as the soil gets drier, more energy would be needed to remove the same amount of water from the soil as water molecules should be more tighly bound to soil particules and cavitation also occur in the soil...] ==> see if a model under which evaporation also depends on the soil water potential would not be better -- I guess so.
                // parameter values are not so clear, so TO BE CHECKED.
                // float e_factor=PPFDtoSW * 3600*0.000001*nbhours_covered* 0.1 * sites_per_dcell*LH*LH*0.001; // to be moved outside of the loop to avoid repeating calculation.
                //float e_Granier = e_factor* WDailyMean * exp(-klight*LAI_DCELL[0][d]);
                //3600*0.000001*nbhours_covered to convert Wmax in micromol of PAR /s /m2 into  Joule, and 10^-6 to MJoule as in Wagner et al. 2011 (however the value provided by Wagner et al. 2011 seems really weird -too high-, and the values we obtained here are in agreement with the ones reported in Marthews et al. 2014.
                //the value 0.1 is drawn from Wagner et al. 2011, but not really explained... to be checked!
                //sites_per_dcell*LH*LH*0.001 is to convert the amount of water in mm, ie. in 10-3 m3/m2, to the amount of water evaporated for the focal dcell in m3
                
                // in this newer version, we used the framework provided by Sellers et al. 1992, which is better mechanistically grounded: depends on the soil layer resistance, which varies with its water potential, and the aerodynamic resistance in series and the differences of vapour pressure between the top soil layer and air just above
                float absorb_prev = LAI_DCELL[1][d];
                float absorb_current = LAI3D[0][d];
                float absorb_delta = absorb_current - absorb_prev;
                if(absorb_delta < 0.0) absorb_delta = 0.0;    //! eliminate rounding errors
                int intabsorb = CalcIntabsorb(absorb_prev, absorb_delta);
                float VPDground = VPDDailyMean*LookUp_VPD[intabsorb]*1000; // in Pa
                float Tsoil = tDailyMean - LookUp_T[intabsorb];
                float esat_ground= 611.21*exp((18.678 - (Tsoil/234.5))*(Tsoil/(257.14+Tsoil))); // Buck equation; in Pa (see Jones p. 348)
                float esoil= esat_ground*exp(2.17*soil_phi3D[0][d]/(Tsoil-ABSZERO)); // esoil variation with the top soil layer water potential, following Duursma & Medlyn 2012 equ. 17, Cochard et al. 2021 equ. 36., see equ. 5.14 in Jones (p. 102), in Pa
                float eair= esat_ground-VPDground; // in Pa
                //float r_soil = exp(8.206 - 4.255*SWC3D[0][d]/Max_SWC[0]) ; // soil surface resistance in s m-1, following Sellers et al. 1992 equ. 19, see also equ 12 in Merlin et al. 2016 (also used in CLM, Oleson et al. 2007).
                float r_soil = exp(8.206 - 4.255*SWC3D[0][d]/FC_SWC[0]) ; // soil surface resistance in s m-1, following Sellers et al. 1992 equ. 19, see also equ 12 in Merlin et al. 2016 (also used in CLM, Oleson et al. 2007).
#ifdef FULL_CLIMATE
                float r_aero = 43.17347*exp(alphaInoue*(1-1/Canopy_height_DCELL[d]))/(WSDailyMean*TopWindSpeed_DCELL[d]); // aerodynamic resistance to hear transfer (boundary layer just above the soil surface), in s m-1 (see equ. 7 and 14 in Duursma & Medlyn 2012; and equ. B10 in Merlin et al. 2016). 43.17347= log(1/0.001)/(0.40*0.40), where 1= the reference height where the wind speed is measured, in m, 0.001=the momentum soil roughness in m (set to 0.001 following Yang et al. 2008 and Stefan et al 2015 in Merlin et al. 2016 equ B10), and 0.40=the von Karman constant.
#else
                float r_aero = 43.17347*exp(alphaInoue*(1-1/Canopy_height_DCELL[d]))/TopWindSpeed_DCELL[d]; // aerodynamic resistance to hear transfer (boundary layer just above the soil surface), in s m-1 (see equ. 7 and 14 in Duursma & Medlyn 2012; and equ. B10 in Merlin et al. 2016). 43.17347= log(1/0.001)/(0.40*0.40), where 1= the reference height where the wind speed is measured, in m, 0.001=the momentum soil roughness in m (set to 0.001 following Yang et al. 2008 and Stefan et al 2015 in Merlin et al. 2016 equ B10), and 0.40=the von Karman constant.
#endif
                float Rtot= r_soil+r_aero; // in s m-1
                float e = nbhours_covered*sites_per_dcell*LH*LH*0.0078*(esoil - eair)/((Tsoil-ABSZERO)*Rtot);  // 0.0078 = 0.001*3600*18e-3/8.31 with 18e-3 = the molar mass of water vapor in kg/mol and 8.31 the ideal gas constant in J/mol/K; 0.001*3600*nbhours_covered*sites_per_dcell*LH*LH is used to convert evaporation in kg m-2 s-1 to m3 per day per dcell.
                
                //if (soil_phi3D[0][d] < -1) {
                // Rcout << "r_soil=" << r_soil << " r_soil_sellers=" << r_soil_sellers <<" r_aero=" <<r_aero << " VPDground=" << VPDground << " esat_ground=" << esat_ground << " esoil=" << esoil << " eair=" << eair << " soil_phi3D[0][d]=" << soil_phi3D[0][d] << " Tsoil=" << Tsoil << " TopWindSpeed_DCELL[d]=" << TopWindSpeed_DCELL[d] << " Wind ground level=" << exp(-alphaInoue*(1-1/Canopy_height_DCELL[d]))*TopWindSpeed_DCELL[d] << " evaporation S92=" << e  << " evaporation S92_sellers=" << e_sellers << " e_granier=" << e_Granier << " SWC3D[0][d]-Min_SWC[0]=" << SWC3D[0][d]-Min_SWC[0] << endl;
                // }
                
                Evaporation[d]=fmaxf(0.0,fminf(e,(SWC3D[0][d]-Min_SWC[0]))); // the amount of water evaporated from the soil cannot result in a water content below the residual water content. A model depending on soil matric potential would not need this.
                if(Evaporation[d]<0 || isnan(Evaporation[d])  || (SWC3D[0][d]-Min_SWC[0])<0 ) {
                    Rcout << "evaporation=" << Evaporation[d] << " and e=" << e << " and SWC3D[0][d]=" << SWC3D[0][d] << " and Min_SWC[0]=" << Min_SWC[0] << "and SWC3D[0][d]-Min_SWC[0]=" << SWC3D[0][d]-Min_SWC[0] << "; raero=" << r_aero << "; Canopy_height_DCELL[d]=" << Canopy_height_DCELL[d]<< "; TopWindSpeed_DCELL[d]=" << TopWindSpeed_DCELL[d] << endl;
                }
                
                SWC3D[0][d]-=Evaporation[d];
                
                // Refilling by rainfall
                
                Interception[d]=fminf(precip, 0.2*LAI_DCELL[0][d]);      // This is the amount of rainfall - in mm, as rainfall -, intercepted by vegetation cover, following the approach used in Liang et al. 1994 Journal of Geophysical Reserach, and also used by Laio et al. 2001 Advances in Water Resources and Fischer et al. 2014 Environmental Modelling & Software (FORMIX3, Madagascar). More complex approach can be used however - see eg. Gutierrez et al. 2014 Plos One (FORMIND, Chili), or Wagner et al. 2011 AFM (Paracou)
                Throughfall[d]=precip-Interception[d];
                Throughfall[d]*=sites_per_dcell*LH*LH*0.001; // to convert in absolute amount of water entering the soil voxel in m3
                
                if(isnan(Throughfall[d]) || (Throughfall[d]) <0 ) {
                    Rcout << "Incorrect throughfall" << endl;
                    Rcout << precip << "\t" << Interception[d] << "\t" << LAI_DCELL[0][d] << endl;
                }
                
                float in=Throughfall[d];
                
                /*if(SWC3D[0][d]<Max_SWC[0]) {
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
                                Rcout << Throughfall[d] << "\t" <<in <<"\t" <<  precip << "\t" << Interception[d] << "\t" << LAI_DCELL[0][d] << endl;
                            }
                            in=0.0;
                        }
                        l++;
                    }
                }*/
                if(SWC3D[0][d]<Max_SWC[0]) {
                    int l=0;
                    while((l<nblayers_soil) && (in>0.0)) {
                        if(in>(FC_SWC[l]-SWC3D[l][d])) {
                            in-=(FC_SWC[l]-SWC3D[l][d]);
                            SWC3D[l][d]=FC_SWC[l];
                            if(isnan(SWC3D[l][d]) || (SWC3D[l][d]-Min_SWC[l])<=0) {
                                Rcout << "incorrect SWC3D, Min/Max_SWC" << endl;
                                Rcout <<Max_SWC[l] << endl;
                            }
                        }
                        else{
                            SWC3D[l][d]+=in;
                            if (isnan(SWC3D[l][d]) || (SWC3D[l][d]-Min_SWC[l])<0) {
                                Rcout << "incorrect SWC3D, Min/Max_SWC" << endl;
                                Rcout << Throughfall[d] << "\t" <<in <<"\t" <<  precip << "\t" << Interception[d] << "\t" << LAI_DCELL[0][d] << endl;
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
                    
                    float theta_w=(SWC3D[l][d]-Min_SWC[l])/(Max_SWC[l]-Min_SWC[l]);
                    
if (_WATER_RETENTION_CURVE==1) {
                    if(theta_w==0) {
                        theta_w=0.001; // SS addition for limit value
                        Rcout << "Warning theta_w = 0 " << endl ;
                    }
                    soil_phi3D[l][d]=a_vgm[l]*pow((pow(theta_w,-b_vgm[l])-1), c_vgm[l]); // this is the van Genuchten-Mualem model (as in Table 1 in Marthews et al. 2014)
                    float inter= 1-pow((1-pow(theta_w, b_vgm[l])),m_vgm[l]);
                    Ks[l][d]=Ksat[l]*pow(theta_w, 0.5)*inter*inter; // this is the van Genuchten-Mualem model (as in Table 1 in Marthews et al. 2014)
                    
                    if (isnan(soil_phi3D[l][d]) || isnan(Ks[l][d]) ||  (SWC3D[l][d]-Min_SWC[l])<0) //|| KsPhi[l][d]==0.0 || Ks[l][d]==0.0 || soil_phi3D[l][d]==0.0)
                        Rcout << "In bucket model, layer " << l << " dcell " << d << " theta_w=" << theta_w << " SWC3D[l][d]-Min_SWC[l]=" << (SWC3D[l][d]-Min_SWC[l]) << " soil_phi3D[l][d]=" << soil_phi3D[l][d] << " Ksat=" << Ksat[l] << " Ks[l][d]=" << Ks[l][d] << endl ;
                    

} else if (_WATER_RETENTION_CURVE==0) {
                    soil_phi3D[l][d]=phi_e[l]*pow(theta_w, -b[l]); // this is the soil water characteristic of Brooks & Corey-Mualem (as in Table 1 in Marthews et al. 2014)
                    Ks[l][d]=Ksat[l]*pow(theta_w, 2.5+2*b[l]); // this is the hydraulic conductivity curve of Brooks & Corey-Mualem (as in Table 1 in Marthews et al. 2014)
                    KsPhi[l][d]=Ksat[l]*phi_e[l]*pow(theta_w, 2.5+b[l]); //Ks times soil_phi3D, computed directly as the exact power of theta.
            
                    if (isnan(soil_phi3D[l][d]) || isnan(Ks[l][d]) ||  isnan(KsPhi[l][d]) || (SWC3D[l][d]-Min_SWC[l])<0) //|| KsPhi[l][d]==0.0 || Ks[l][d]==0.0 || soil_phi3D[l][d]==0.0)
                        Rcout << "In bucket model, layer " << l << " dcell " << d << " theta_w=" << theta_w << " SWC3D[l][d]-Min_SWC[l]=" << (SWC3D[l][d]-Min_SWC[l]) << " soil_phi3D[l][d]=" << soil_phi3D[l][d] << " Ksat=" << Ksat[l] << " phi_e=" << phi_e[l] <<" b[l]=" << b[l] << " KsPhi[l][d]=" << KsPhi[l][d] << " Ks[l][d]=" << Ks[l][d] << endl ;
                    //KsPhi2[l][d]=Ksat[l]*phi_e[l]*pow(theta_w, 2.5);
                    // we may want to shift to the van Genuchten-Mualem expressions of soil_phi3D and Ks, as the van genuchten-Mualem model is currently defacto the more standard soil hydraulic model (see ref in Table 1 in Marthews et al. 2014). To do so, see if we have data of soil pH, cation exchange capacity, organic carbon content, to explicitly compute the parameters with Hodnett & Tomasella 2002 (as recommended by Marthews et al. 2014 -- Table 2; or instead directly use the parameter provided by the map in Marthews et al. 2014.
                    
}
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
                    else SPECIES_SEEDS[site][spp] = 1;     // If s_Seed[site] = 0, site is not occupied, if s_Seed[site] > 1, s_Seed[site] is the presence of a seed
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
                        
#ifdef LCP_alternative
                        
#ifdef WATER
                        if(soil_phi3D[0][site_DCELL[site]]>0.5*S[spp].s_tlp) {
                            T[site].Birth(spp,site); // in this version, the light environment is checked within Birth() function
                        }
                        
#else
                        T[site].Birth(spp,site); // in this version, the light environment is checked within Birth() function
                        
#endif
                        
#else //LCP_alternative
                        
                        float flux = WDailyMean*exp(-fmaxf(LAI3D[0][site+SBORD],0.0)*kpar);
#ifdef WATER
                        if(flux>(S[spp].s_LCP) && soil_phi3D[0][site_DCELL[site]]>0.5*S[spp].s_tlp) {
                            T[site].Birth(spp,site);
                            SPECIES_SEEDS[site][spp]=0; // newIM nov2021, to adjust to the yearly update of Species_seeds
                        }
                        // in addition to a condition of light availability- hence light demanding species may not be able to grow in deep sahde conditions in understorey -, a condition on water availability is added - hence drought-intolerant species may not be recruited in water-stressd conditions
#else
                        // If enough light, germination, initialization of NPP (LCP is the species light compensation point
                        // here, light is the sole environmental resources tested as a limiting factor for germination, but we should think about adding nutrients (N,P) and water conditions...
                        if(flux>(S[spp].s_LCP))  {
                            T[site].Birth(spp,site);
                            SPECIES_SEEDS[site][spp]=0; // newIM nov2021, to adjust to the yearly update of Species_seeds
                        }
#endif //WATER
#endif //LCP_alternative
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
                        c_forceflex = gsl_rng_uniform(gslrand)*T[site].t_height;     // probability of treefall = 1-t_Ct/t_height, compare to genrand2(): genrand2() < 1 - t_Ct/t_height, or: genrand2() > t_Ct/t_height
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
                    float height_threshold = T[site].t_height/T[site].t_mult_height;  // since 2.5: a tree's stability is defined by its species' average height, i.e. we divide by the intraspecific height multiplier to account for lower stability in quickly growing trees; otherwise slender, faster growing trees would be treated preferentially and experience less secondary treefall than more heavily built trees
                    if(2.0*T[site].t_hurt*gsl_rng_uniform(gslrand) > height_threshold) {        // check whether tree dies: probability of death is 1.0-0.5*t_height/t_hurt, so gslrand <= 1.0 - 0.5 * t_height/t_hurt, or gslrand > 0.5 * t_height/t_hurt; modified in v.2.5: probability of death is 1.0 - 0.5*t_height/(t_mult_height * t_hurt), so the larger the height deviation (more slender), the higher the risk of being thrown by another tree
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
        int GetTimeofyear(){
            // new function to derive time of year, extended to negative iterations (-1 would be treated as last iteration of previous year)
            int timeofyear;
            if(iter < 0) timeofyear = iterperyear - abs(iter)%iterperyear;
            else timeofyear = iter%iterperyear;
            return(timeofyear);
        };
        float CalcHeightBaseline(float &ah, float &hmax, float &dbh){
            // height allometry
            float height = hmax * dbh/(dbh + ah);
            return(height);
        }
        
        float CalcCRBaseline(float &dbh){
            // crown radius allometry
            float CR;
            if(!_CROWN_MM) {
                CR = exp(CR_a + CR_b*log(dbh));            // power law, the default
            }
            else CR = CR_b * dbh/(dbh + CR_a);                       // Michaelis Menten type allometry !!!: requires CR_b to be the CR_max parameter and CR_a the initial increase */
            //for reference, two crown allometries that are reasonable in French Guiana
            //t_CR = t_mult_CR * exp(1.9472 + 0.5925*log(t_dbh)); // crown allometry deduced from Piste Saint-Elie */
            //t_CR = t_mult_CR * exp(1.8814 + 0.5869*log(t_dbh)); // this is crown allometry derived from data set compiled by Jucker et al. 2016 (Global Change Biology)
            return(CR)
            ;}
        
        float CalcCDBaseline(float &height){
            // crown depth allometry
            // since v.2.5, simplification of the computation of the crown depth, in accordance with the Canopy Constructor algorithm
            float CD = (CD_a + CD_b * height);
            return(CD);
        }
        
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
            
            int site,spp;
            float sum1=0.0,sum10=0.0,sum30=0.0,ba=0.0,npp=0.0,gpp=0.0, ba10=0.0, agb=0.0, rday=0.0, rnight=0.0, rstem=0.0, litterfall=0.0;
            
            float inbcells = 1.0/float(sites*mpi_size);
            float inbhectares = inbcells*NH*NH*10000.0;
            
#ifdef WATER
            abund_phi_root=0.0;
            abund10_phi_root=0.0;
            agb_phi_root=0.0;
#endif
            
            if(!mpi_rank) {
                for(spp=1;spp<=nbspp;spp++)
                    S[spp].s_sum10 = S[spp].s_sum30 = S[spp].s_ba = S[spp].s_ba10 = S[spp].s_agb = S[spp].s_gpp = S[spp].s_npp = S[spp].s_rday = S[spp].s_rnight = S[spp].s_rstem = S[spp].s_litterfall = 0;
                
                for(site=0;site<sites;site++)T[site].Average();
                
                for(spp=1;spp<=nbspp;spp++) {
                    float s_sum1 = float(S[spp].s_nbind)*inbhectares;
                    S[spp].s_sum10 *= inbhectares;
                    S[spp].s_sum30 *= inbhectares;
                    S[spp].s_ba *= inbhectares;
                    S[spp].s_ba10 *= inbhectares;
                    S[spp].s_agb *= inbhectares;
                    S[spp].s_gpp *= inbhectares;
                    S[spp].s_npp *= inbhectares;
                    S[spp].s_rday *= inbhectares;
                    S[spp].s_rnight *= inbhectares;
                    S[spp].s_rstem *= inbhectares;
                    S[spp].s_litterfall *= inbhectares;
                    
                    sum1 += float(S[spp].s_nbind)*inbhectares;
                    sum10 += S[spp].s_sum10;
                    sum30 += S[spp].s_sum30;
                    ba += S[spp].s_ba;
                    ba10 += S[spp].s_ba10;
                    agb += S[spp].s_agb;
                    gpp += S[spp].s_gpp;
                    npp += S[spp].s_npp;
                    rday += S[spp].s_rday;
                    rnight += S[spp].s_rnight;
                    rstem += S[spp].s_rstem;
                    litterfall += S[spp].s_litterfall;
                    
                    //if(_OUTPUT_extended){
                        output_extended[0] << iter << "\t" << S[spp].s_name << "\t" << s_sum1 << "\t" << S[spp].s_sum10 << "\t" << S[spp].s_sum30 << "\t" << S[spp].s_ba << "\t" << S[spp].s_ba10 << "\t" << S[spp].s_agb << "\t" << S[spp].s_gpp << "\t" << S[spp].s_npp << "\t" << S[spp].s_rday << "\t" << S[spp].s_rnight << "\t" << S[spp].s_rstem << "\t" << S[spp].s_litterfall << endl;
                    //}
                }
                
                
                output_basic[0] << iter << "\t" << sum1 << "\t" << sum10 << "\t" << sum30 << "\t" << ba << "\t" << ba10 << "\t" << agb << "\t" << gpp << "\t" << npp << "\t" << rday << "\t" << rnight << "\t" << rstem << "\t" << litterfall << endl;
                
                Rcout.setf(ios::fixed,ios::floatfield);
                Rcout.precision(2);
                
                
                
#ifdef WATER
                Rcout << iter << "\tTrees (1/ha): " << sum1 << " | " << sum10 << " | " << sum30 << " *** nbdead (%): " << 100.0*nbdead_n1 * inbhectares/sum1 << " | " << 100.0*nbdead_n10 * inbhectares/sum10 << " | " << 100.0*nbdead_n30 * inbhectares/sum30 << " *** AGB (t/ha): " << round(agb/1000.0) << " GPP (MgC/ha/yr) " << gpp*iterperyear << " NPP " << npp*iterperyear << " litterfall (Mg/ha/yr) " << litterfall*iterperyear << " *** Transpiration (mm): ";
#else
                
                Rcout << iter << "\tTrees (1/ha): " << sum1 << " | " << sum10 << " | " << sum30 << " *** nbdead (%): " << 100.0*nbdead_n1 * inbhectares/sum1 << " | " << 100.0*nbdead_n10 * inbhectares/sum10 << " | " << 100.0*nbdead_n30 * inbhectares/sum30 << " *** AGB (t/ha): " << round(agb/1000.0) << " GPP (MgC/ha/yr) " << gpp*iterperyear << " NPP " << npp*iterperyear << " litterfall (Mg/ha/yr) " << litterfall*iterperyear << endl;
#endif
                
                
                if(_OUTPUT_extended){
                    float tototest=0.0, tototest2=0.0, flux;
                    for(int site=0;site<sites;site++) {
                        flux = WDailyMean*exp(-fmaxf(LAI3D[0][site+SBORD],0.0)*kpar);
                        tototest += flux;
                        tototest2 += flux*flux;
                    }
                    tototest /=float(sites*LH*LH);                              // Average light flux (PPFD) on the ground
                    tototest2 /=float(sites*LH*LH);
                    if(iter) output_extended[1] << iter<< "\tMean PPFDground\t" << tototest << "\t" << sqrt(tototest2-tototest*tototest) << "\n";
                    
                    
                    if(_BASICTREEFALL) output_extended[2] << iter << "\t" << nbdead_n1*inbhectares << "\t" << nbdead_n10*inbhectares<< "\t" << nbTreefall1*inbhectares << "\t" << nbTreefall10*inbhectares << endl;
                    else output_extended[2] << iter << "\t" << nbdead_n1*inbhectares << "\t" << nbdead_n10*inbhectares << endl;
                    
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
                        
                        float carbon_leaves = T[s].t_LA * T[s].t_LMA * 0.5;
                        carbon_stored_leaves += carbon_leaves;
                    }
                }
                
                float factor_weight = 0.000001; // factor to convert carbon from g to tons
            
                Rcout.setf(ios::fixed,ios::floatfield);
                Rcout.precision(5);
                
                Rcout << iter << "\tTrunkC: " << carbon_stored_trunk *  factor_weight << " LeavesC: " << carbon_stored_leaves *  factor_weight  << " FreeC: " << carbon_stored_free *  factor_weight << " Total AssimC: " << carbon_assimilated_total *  factor_weight << " Total NetC: " << carbon_net_total *  factor_weight << endl;
                Rcout << iter << "\tTrunkC change: " << (carbon_stored_trunk - carbon_stored_trunk_previous) *  factor_weight << " LeavesC: " << (carbon_stored_leaves - carbon_stored_leaves_previous) *  factor_weight  << " FreeC: " << (carbon_stored_free - carbon_stored_free_previous) *  factor_weight << endl;
            
#endif
            
            Rcout.setf(ios::fixed,ios::floatfield);
            Rcout.precision(5);
            
#ifdef WATER
            
            float evapo=0.0, runoff=0.0, leak=0.0, interception=0.0, throughfall=0.0, lai=0.0;
            for (int d=0; d<nbdcells;d++) {
                evapo+=Evaporation[d]; // in m3
                interception+=Interception[d]; // in mm, as rainfall
                throughfall+=Throughfall[d]; // in m3
                runoff+=Runoff[d]; // in m3
                leak+=Leakage[d]; // in m3
                lai+=LAI_DCELL[0][d];
            }
            
            float isites=1.0/float(sites*LH*LH);
            float icells=1.0/float(nbdcells);
            evapo*=isites; // in m
            runoff*=isites; // in m
            leak*=isites; // in m
            throughfall*=isites; // in m
            interception*=isites*0.001; // in m
            lai*=icells;
            transpiration_1016*=isites;
            
            output[11] << iter << "\t" << precip << "\t" << interception << "\t" << throughfall << "\t" << runoff << "\t" << leak << "\t" << evapo << "\t";
            output[21] << iter << "\t" << lai << endl;
            
            for(int l=0; l<nblayers_soil; l++) {
                float transpi=0.0;
                for(int d=0; d<nbdcells;d++) {
                    transpi+=Transpiration[l][d];  // in m3
                }
                transpi*=isites; // in m
                output[11] << transpi << "\t";
                Rcout << transpi*1000 << " | "; // in mm
            }
            
            output[11] << transpiration_1016 << "\t" ;
            
            Rcout << transpiration_1016*1000 <<  " | "  << endl;
            
            float layer_depth_previous = 0.0;
            for(int l=0; l<nblayers_soil; l++) {
                float soilWC=0.0;
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
                float soilPhi=0.0;
                for(int d=0; d<nbdcells;d++) {
                    soilPhi+=soil_phi3D[l][d];  //in MPa
                }
                soilPhi*=icells; // in MPa
                output[11] << soilPhi << "\t";
            }
            
            output[11] <<"\n";
            
            if (iter==(nbiter-90) || iter==(nbiter-45) || iter==(nbiter-1)) {
                
                int o_swc, o_swp, o_wfluxes;
                if (iter==(nbiter-90)) {
                    o_swc=1;
                    o_swp=4;
                    o_wfluxes=18;
                }
                if (iter==(nbiter-45)) {
                    o_swc=2;
                    o_swp=5;
                    o_wfluxes=19;
                }
                if (iter==(nbiter-1)) {
                    o_swc=3;
                    o_swp=6;
                    o_wfluxes=20;
                }
                
                output[o_wfluxes] << "LAI" << "\t";
                for (int d=0; d<nbdcells;d++) {
                    output[o_wfluxes] << LAI_DCELL[0][d] << "\t";
                }
                output[o_wfluxes] << endl;
                
                output[o_wfluxes] << "Evaporation" << "\t";
                for (int d=0; d<nbdcells;d++) {
                    output[o_wfluxes] << Evaporation[d]*i_sites_per_dcell << "\t"; // in m
                }
                output[o_wfluxes] << endl;
                
                layer_depth_previous = 0.0;
                for(int l=0; l<nblayers_soil; l++) {
                    float layer_depth_current = layer_depth[l];
                    float layer_thickness = layer_depth_current - layer_depth_previous;
                    float norm=i_sites_per_dcell/layer_thickness;
                    output[o_swc] << l << "\t";
                    output[o_swp] << l << "\t";
                    output[o_wfluxes] << "Transpiration_" << l << "\t";
                    for (int d=0; d<nbdcells;d++) {
                        output[o_swc] << SWC3D[l][d]*norm  << "\t"; // in m3/m3
                        output[o_swp] << soil_phi3D[l][d] << "\t";
                        output[o_wfluxes] << Transpiration[l][d]*i_sites_per_dcell << "\t";
                    }
                    layer_depth_previous = layer_depth_current;
                    output[o_swc] << endl;
                    output[o_swp] << endl;
                    output[o_wfluxes] << endl;
                }
                
                
                
            }
            
            output[22] << iter  << "\t";
            output[23] << iter  << "\t";
            output[24] << iter  << "\t";
            for (int l=0; l<HEIGHT+1; l++) {
                LAI_young[l]*=isites;
                LAI_mature[l]*=isites;
                LAI_old[l]*=isites;
                output[22] << LAI_young[l]  << "\t";
                output[23] << LAI_mature[l]  << "\t";
                output[24] << LAI_old[l]  << "\t";
                LAI_young[l]=0.0;
                LAI_mature[l]=0.0;
                LAI_old[l]=0.0;
            }
            output[22] << endl;
            output[23] << endl;
            output[24] << endl;
            
            abund_phi_root*=inbhectares/sum1;
            abund10_phi_root*=inbhectares/sum10;
            agb_phi_root*=inbhectares/agb;
            
            output[31] << iter << "\t" << abund_phi_root << "\t" << abund10_phi_root << "\t" << agb_phi_root << endl;
            
            
#endif
            
#ifdef MPI
            // This section corresponds to the parallel version of the reporting of the global diagnostic variables. Since much work has been done on routine Average over the past years, this would need a full rewrite, !!!!Action 20/01/2016: rework the parallel version of function Average!!!!
            
            //     MPI_Reduce(&(S[spp].s_nbind),&sind,1,
            //     MPI_INT,MPI_SUM,0,MPI_COMM_WORLD);
            //     MPI_Reduce(S[spp].s_output_field,S[spp].s_output_field,5,
            //     MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD);
            //     MPI_Reduce(Mortality,Mortality,4,
            //     MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD);
            //     MPI_Reduce(&S[spp].s_output_field[6],&S[spp].s_output_field[6],5,
            //     MPI_FLOAT,MPI_MAX,0,MPI_COMM_WORLD);
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
                    for(h=0;h<(HEIGHT+1);h++) output[32] << iter << "\t" << h*LV << "\t" << layer[h]*norm << "\n";
                    output[32] <<  "\n";
                }
            }
        }
        
        //##############################################
        // Global function: output snapshots of the scene at one point in time
        //##############################################
        //! - This can be used to take snapshots of the forest in more detail and track its development over time.
        void OutputSnapshot(fstream& output, bool header, float dbh_limit){
            Rcout << "Writing snapshot of forest to file." << endl;
            if(header == 1){
                output << "iter\tcol\trow\tfrom_Data\tsp_lab\tsite\tCrownDisplacement\tPmass\tNmass\tLMA\twsg\tRdark\tVcmax\tJmax\tleaflifespan\tlambda_young\tlambda_mature\tlambda_old\tdbhmature\tdbhmax\thmax\tah\tCt\tLAImax\tfraction_filled\tmult_height\tmult_CR\tmult_CD\tmult_P\tmult_N\tmult_LMA\tmult_dbhmax\tdev_wsg\tage\tdbh\tsapwood_area\theight\tCD\tCR\tGPP\tNPP\tRday\tRnight\tRstem\tLAmax\tLA\tyoungLA\tmatureLA\toldLA\tLAI\tlitter\tcarbon_storage\tcarbon_biometry\tmultiplier_seed\thurt\tNPPneg";
                
                #ifdef WATER

                        output << "\troot_depth\tphi_root\tWSF\tWSF_A\ttranspiration\tg1_0\tg1";
                #ifdef PHENO_DROUGHT
                        output << "\tNdays_dry\tNdays_wet\tpheno_factor";
                #endif
                        for(int l=0;l<nblayers_soil;l++) {
                            output << "\troot_biomass" << l;
                        }
                        for(int l=0;l<nblayers_soil;l++) {
                            output << "\tsoil_layer_weight" << l;
                    }
                
                #endif
                
#ifdef Output_ABC
                output << "\tdbh_previous";
#endif
                output << "\tAGB\ts_name" << endl;
            }
            
            
            // reset the canopy to make sure that it is well-constructed
            for(int h=0;h<(HEIGHT+1);h++)
                for(int sbsite=0;sbsite<sites+2*SBORD;sbsite++)
                    LAI3D[h][sbsite] = 0.0;
            
            for(int site=0;site<sites;site++){                                    // Each tree contribues to LAI3D
                T[site].CalcLAI();
            }
            
            for(int h=HEIGHT;h>0;h--){                                 // LAI is computed by summing LAI from the canopy top to the ground
                for(int site=0;site<sites;site++){
                    int sbsite=site+SBORD;
                    LAI3D[h-1][sbsite] += LAI3D[h][sbsite];
                }
            }
            
            //output.setf(ios::fixed,ios::floatfield);
            output.precision(5);
            for(int row=0;row<rows;row++){
                for(int col=0;col<cols;col++){
                    int site = col + cols*row;
                    if(T[site].t_age > 0 && T[site].t_dbh >= dbh_limit){
                        // recalculate photosynthesis and respiration
                        //T[site].CalcRespGPP();
                        //T[site].CalcNPP();
                        // output all tree variables, this is potentially a very large file
                        // we currently do not output the t_NDDfield vector, as it is too large
                        output << iter << "\t" << col << "\t" << row << "\t" << T[site].t_from_Data << "\t" << T[site].t_sp_lab << "\t" << site << "\t" << T[site].t_CrownDisplacement << "\t" << T[site].t_Pmass << "\t" << T[site].t_Nmass << "\t" << T[site].t_LMA << "\t" << T[site].t_wsg << "\t" << T[site].t_Rdark << "\t" << T[site].t_Vcmax << "\t" << T[site].t_Jmax << "\t" << T[site].t_leaflifespan << "\t" << T[site].t_lambda_young << "\t" << T[site].t_lambda_mature << "\t" << T[site].t_lambda_old << "\t" << T[site].t_dbhmature << "\t" << T[site].t_dbhmax << "\t" << T[site].t_hmax << "\t" << T[site].t_ah << "\t" << T[site].t_Ct << "\t" << T[site].t_LAImax << "\t" << T[site].t_fraction_filled << "\t" << T[site].t_mult_height << "\t" << T[site].t_mult_CR << "\t" << T[site].t_mult_CD << "\t" << T[site].t_mult_P << "\t" << T[site].t_mult_N << "\t" << T[site].t_mult_LMA << "\t" << T[site].t_mult_dbhmax << "\t" << T[site].t_dev_wsg << "\t" << T[site].t_age << "\t" << T[site].t_dbh << "\t" << T[site].t_sapwood_area << "\t" << T[site].t_height << "\t" << T[site].t_CD << "\t" << T[site].t_CR << "\t" << T[site].t_GPP << "\t" << T[site].t_NPP << "\t" << T[site].t_Rday << "\t" << T[site].t_Rnight << "\t" << T[site].t_Rstem << "\t" << T[site].t_LAmax << "\t" << T[site].t_LA << "\t" << T[site].t_youngLA << "\t" << T[site].t_matureLA << "\t" << T[site].t_oldLA << "\t" << T[site].t_LAI << "\t" << T[site].t_litter << "\t" << T[site].t_carbon_storage << "\t" << T[site].t_carbon_biometry << "\t" << T[site].t_multiplier_seed << "\t" << T[site].t_hurt << "\t" << T[site].t_NPPneg;
                        
                        #ifdef WATER
                                        output << "\t" << T[site].t_root_depth << "\t" << T[site].t_phi_root << "\t" << T[site].t_WSF << "\t" << T[site].t_WSF_A << "\t" << T[site].t_transpiration << "\t" << T[site].t_g1 << "\t" << T[site].t_g1_0;
                        #ifdef PHENO_DROUGHT
                                        output << "\t" << T[site].t_Ndays_dry << "\t" << T[site].t_Ndays_wet << "\t" << T[site].t_pheno_factor;

                        
                        #endif
                                        for(int l=0;l<nblayers_soil;l++) {
                                            output << "\t" << T[site].t_root_biomass[l];
                                        }
                                        for(int l=0;l<nblayers_soil;l++) {
                                            output << "\t" << T[site].t_soil_layer_weight[l];
                                        }
                        
                        #endif
                        
#ifdef Output_ABC
                        output << "\t" << T[site].t_dbh_previous;
#endif
                        
                        // we add a few tree-based variables that are derived or environment-related, but not directly kept track of
                        float AGB = T[site].CalcAGB();
                        
                        output << "\t" << AGB << "\t" << S[T[site].t_sp_lab].s_name << endl;
                    }
                }
            }
        }
        

#ifdef CHM_SPIKEFREE
        //##########################
        //## Make a spikefree CHM ##
        //##########################
        void MakeCHMspikefree(vector<int>& chm_spikefree){
            chm_spikefree.clear();
            chm_spikefree.reserve(sites);
            for(int s = 0; s < sites; s++) chm_spikefree.push_back(0);
            
            
#ifdef Output_ABC // IM2023 added, otherwise row_start, row_end, col_start, col_end are not necessarily decalred, and this blocks code building.
            
            for(int r=row_start;r<row_end;r++){
                for(int c=col_start;c<col_end;c++){
                    int s = c + r*cols;
                    if(T[s].t_age > 0){
#ifdef CROWN_UMBRELLA
                        float height = T[s].t_height;
                        float CR = T[s].t_CR;
                        float CD = T[s].t_CD;
                        float fraction_filled_target = 1.0;         // to remove spikes, we assume a fully filled crown for all trees
                        int shell_fromtop = 0;                      // toplayer
                        float noinput = 0.0;
                        
                        LoopLayerUpdateCrownStatistic_template(r, c, height, CR, CD, fraction_filled_target, shell_fromtop, GetRadiusSlope, noinput, chm_spikefree, KeepFloatAsIs, UpdateCHMvector);
#else
                        int crown_top = int(T[s].t_height);
                        int crown_intarea = GetCrownIntarea(T[s].t_CR);
                        
                        int row_crowncenter = s/cols;
                        int col_crowncenter = s%cols;
                        
                        for(int i = 0; i < crown_intarea; i++){
                            int site_relative = LookUp_Crown_site[i];
                            int row = row_crowncenter + site_relative/51 - 25;
                            int col = col_crowncenter + site_relative%51 - 25;
                            if(row >= 0 && row < rows && col >= 0 && col < cols){
                                int site = col + row * cols;
                                if(chm_spikefree[site] < crown_top) chm_spikefree[site] = crown_top;
                            }
                        }
#endif
                    }
                }
            }
            
#endif
        }
#endif
        

        
        
        //################################
        //### Output for visualization ###
        //################################
        void OutputVisual(){
            // first simple chm output
#ifdef CHM_SPIKEFREE
            vector<int> chm_spikefree;
            MakeCHMspikefree(chm_spikefree);
            
            for(int col = mincol_visual; col < maxcol_visual; col++){
                for(int row = minrow_visual; row < maxrow_visual; row++){
                    int site = col + row * cols;
                    int height_canopy=0;
                    for(int h=0;h<(HEIGHT+1);h++){
                        if(LAI3D[h][site+SBORD] > 0.0) height_canopy = max(h,height_canopy);
                    }
                    output_visual[0] << iter << "\t" << row << "\t" << col << "\t" << height_canopy+1 << "\t" << chm_spikefree[site] << "\t" << LAI3D[0][site+SBORD] << endl;
                }
            }
#else
            for(int col = mincol_visual; col < maxcol_visual; col++){
                for(int row = minrow_visual; row < maxrow_visual; row++){
                    int site = col + row * cols;
                    int height_canopy=0;
                    for(int h=0;h<(HEIGHT+1);h++){
                        if(LAI3D[h][site+SBORD] > 0.0) height_canopy = max(h,height_canopy);
                    }
                    output_visual[0] << iter << "\t" << row << "\t" << col << "\t" << height_canopy+1 << "\t" << LAI3D[0][site+SBORD] << endl;
                }
            }
#endif
            
            // now the sliced output
            for(int row = minrow_visual_slice; row < maxrow_visual_slice; row++){
                for(int col = 0; col < cols; col++){
                    int s = col + row * cols;
                    if(T[s].t_age > 0){
                        int row_slice = row;
                        
                        float height = T[s].t_height;
                        float CD = T[s].t_CD;
                        int crown_top = int(height);
                        float CR = T[s].t_CR;
                        int crown_base = int(height - CD);
                        
                        vector<float> output_statistics;
                        output_statistics.reserve(4);
                        output_statistics.push_back(T[s].t_sp_lab);
                        
                        float ratio_height_Ct;
                        if(T[s].t_Ct > 0.0) ratio_height_Ct = T[s].t_height/T[s].t_Ct;
                        else ratio_height_Ct = 0.0;
                        output_statistics.push_back(ratio_height_Ct);
                        
                        float ratio_NPP_GPP;
                        if(T[s].t_GPP > 0.0) ratio_NPP_GPP = T[s].t_NPP/T[s].t_GPP;
                        else ratio_NPP_GPP = 0.0;
                        output_statistics.push_back(ratio_NPP_GPP);
                        
#ifdef CROWN_UMBRELLA
                        float fraction_filled_target = 1.0;        // we assume a fully filled crown for all trees
                        int max_shells = min(crown_top - crown_base + 1, 4);    //since the new crown shapes
                        
                        for(int h = 0; h <= crown_top - max_shells;h++) OutputCrownSliced(h, s, row_slice, output_statistics);
                        
                        for(int shell_fromtop = 0; shell_fromtop < max_shells; shell_fromtop++){
                            LoopLayerUpdateCrownStatistic_template(row, col, height, CR, CD, fraction_filled_target, shell_fromtop, GetRadiusSlope, row_slice, output_statistics, KeepIntAsIs, OutputCrownSliced);
                        }
#else
                        int crown_intarea = GetCrownIntarea(CR);
                        
                        for(int h = 0; h <= crown_base;h++) OutputCrownSliced(h, s, row_slice, output_statistics);
                        
                        for(int h = crown_base; h <= crown_top; h++){
                            for(int i = 0; i < crown_intarea; i++){
                                int site_relative = LookUp_Crown_site[i];
                                int row_crown = row + site_relative/51 - 25;
                                int col_crown = col + site_relative%51 - 25;
                                int site_crown = col_crown + row_crown * cols;
                                OutputCrownSliced(h, s, row_slice, output_statistics);
                            }
                        }
#endif
                        
                    }
                }
            }
        }
        
        //##################
        //### Output CHM ###
        //##################
        void OutputCHM(fstream& output_CHM){
#ifdef CHM_SPIKEFREE
            vector<int> chm_spikefree;
            MakeCHMspikefree(chm_spikefree);
            
            output_CHM  << "site" << "\t" << "row" << "\t" << "col" << "\t"  << "height" << "\t" << "height_spikefree" << "\t" << "LAI" << endl;
            for(int s=0;s<sites;s++){
                int height_canopy=0;
                for(int h=0;h<(HEIGHT+1);h++){
                    if(LAI3D[h][s+SBORD] > 0.0) height_canopy = max(h,height_canopy);
                }
                output_CHM << s << "\t" << int(s/cols) << "\t" << int(s%cols) << "\t" << height_canopy+1 << "\t" << chm_spikefree[s] << "\t" << LAI3D[0][s+SBORD] << endl;
            }
            
#else
            output_CHM << "site" << "\t" << "row" << "\t" << "col" << "\t"  << "height" << "\t" << "LAI" << endl;
            for(int s=0;s<sites;s++){
                int height_canopy=0;
                for(int h=0;h<(HEIGHT+1);h++)
                    if(LAI3D[h][s+SBORD] > 0.0) height_canopy = max(h,height_canopy);
                output_CHM << s << "\t" << int(s/cols) << "\t" << int(s%cols) << "\t"  << height_canopy+1 << "\t" << LAI3D[0][s+SBORD] << endl;
            }
#endif
        }
        
        //##############################################
        // Global function: writes the whole 3D LAI voxel field to file
        //##############################################
        void OutputLAI(fstream& output_transmLAI3D){
            output_transmLAI3D << "s\trow\tcol\th\tLAI3D" << endl;
            for(int s=0;s<sites;s++)
                for(int h=0; h<HEIGHT;h++)
                    output_transmLAI3D << s << "\t" << int(s/cols) << "\t" << int(s%cols) << "\t" << h << "\t" << LAI3D[h][s+SBORD] << endl;
        }
        
        //##############################################
        // Global function: transforming TROLL LAI3D field into point cloud and export as .las file
        //##############################################
        //! - Basic ALS simulation, same as for calculating the TROLL transmittance field, added in v.3.1.6; !!!: TODO: merge with UpdateTransmittanceCHM_ABC
        //! - First draws from a distribution to calculate the sampling density, i.e. the number of beams per voxel column
        //! - Then loops over beams per voxel column from top to bottom and calculates the number of hits given the leaf area density (LAD) of the respective voxel
        //! - If beams hit ground, all produce guaranteed returns
        //! - The parameters used is the k assumed for the laser, which is only based on leaf geometry and should thus be taken equal to klight
        //! - The difference to kpar is accounted for by parameterizing a probability of a further return after a hit, i.e. a transmittance of the leaves that are hit that is larger than for visible light and corresponds roughly to empirical fractions of second returns (~0.3-0.4). If taken together, a klight = 0.5 and a transmittance probability after a hit of 0.4 would yield an effective knir = 0.2, which is reasonable for most laser scanners.
        //! - This approach makes lots of simplifying assumptions, among which are: no flightline, no angles, no beam diameter/divergence, Lambert-Beer extinction, including assumptions about the conversion between transmittance probability and energy extinction. Importantly, a TROLL forest contains no woody material and does not represent topography at the moment, all of which may influence sampling densities as well.
        //! - One further question concerns the probability of obtaining a ground return: there is a long discussion in the literature about backscatter ratios between vegetation and ground. However, this seems to be neither a constant ratio nor seems there to be agreement on how it varies, likely due to dependence on footprint size and composition of reflecting surfaces. Cf., for example, Ni-Meister et al. 2001, IEEE Transactions: they use a ratio of mostly 1.0 (the implicit assumption in this simulation as well), but state that it should vary from site to site. Knapp et al. 2021, Remote Sensing, on the other hand: "The reflectance of the forest ground voxels was down-weighted by dividing by 2.5 in order to account for the lower reflectivity of the ground vs. vegetation." This specifically concerns GEDI waveform simulations, so large-footprint lasers, but the source publication does not seem to give a direct reference for the value. Furthermore, it would imply that the ratio of ground to vegetation reflectance is 0.4 for large-footprint full-waveform lidar, which seems to be in stark contrast to Chen et al. 2014, Remote Sensing of Environment, who looked at footprints of up to 8m and found a constant ratio ground/vegetation of 1.7 (they measured vegetation/ground ratio as ~0.57, so 1.7 = 1.0/0.57). However, their Figure 11 seems to indicate an inverse relationship for small footprints (~0.4m) with a ground to vegetation backscatter ratio of ca. 0.8. In the future, it would be worth running TROLL voxel output through an explicit raytracing simulator, with a much more detailed parameterization of surfaces (e.g. DART or Helios++, https://github.com/3dgeo-heidelberg/helios) and compare our simplified simulations to its outcomes/adjust accordingly.
        
        void GenerateVoxelreturnsALS(vector<int> &beams, vector<float> &beams_returns, float mean_beam, float sd_beam, float klaser, float transmittance_laser){
            
            int beams_expected = int(sites * mean_beam * 1.01); // reserve a bit more
            int returns_maximum = beams_expected * 5; // if every laser had 5 returns
            beams.reserve(beams_expected);
            beams_returns.reserve(returns_maximum);
            
            // loop through the whole array by site first, then by height, then by return number
            for(int r = 0; r < rows; r++){
                for(int c = 0; c < cols; c++){
                    int site = c + r * cols;
                    int nbbeams = int(mean_beam + gsl_ran_gaussian(gslrand, sd_beam));    // always rounding up
                    nbbeams = max(nbbeams,1);
                    
                    for(int beam = 0; beam < nbbeams; beam++){
                        //loop over the field from maximum height to 0 and iteratively update voxels from top to bottom, following the beam. An alternative version, also allowing for ground returns, can be activated to extending the loop to h >= -1. In this case, when a beam is not extinguished before it reaches the ground (h >= 0), then it is counted as a ground return
                        
                        beams.push_back(site); // store the location of the current beam
                        
                        int beam_continues = 1;
                        int beam_return = 0;
                        int h = HEIGHT-1; // starting point to sample from
                        
                        while(h >= -1 && beam_continues && beam_return < 5){
                            // there will only be a return, if the beam still exists
                            
                            if(h >= 0){
                                //returns due to vegetation
                                float LAI_above = LAI3D[h+1][site + SBORD];
                                float LAI_current = LAI3D[h][site + SBORD];
                                
                                float LAD = LAI_current - LAI_above;
                                
                                float prob_hit;
                                if(LAD > 0.0) prob_hit = 1.0 - exp(-klaser * LAD);
                                else prob_hit = 0.0;
                                
                                int is_hit = gsl_ran_bernoulli(gslrand, prob_hit);
                                
                                if(is_hit){
                                    beam_continues = gsl_ran_bernoulli(gslrand, transmittance_laser);
                                    beam_return++;
                                    float z_hit = float(h) + gsl_rng_uniform(gslrand);
                                    beams_returns.push_back(z_hit);
                                }
                            } else {
                                // ground returns
                                beams_returns.push_back(0.0);
                                beam_return++;
                                beam_continues = 0;
                            }
                            
                            h--;
                        }
                        
                        beams.push_back(beam_return); // add the return number
                    }
                }
            }
        }
        // !!!: At the moment, we simply assume a little-endian system for output (most personal computers, but not servers)
        // BELOW some hints on how we would check for endianness and implement conversion of big to little endian
        // the other way round (little to big) would be easy, as this is implemented in c++ via htol function
        
        // check endianness,cf. https://stackoverflow.com/questions/4181951/how-to-check-whether-a-system-is-big-endian-or-little-endian
        //int n = 1;
        //// little endian if true
        //if(*(char *)&n == 1) {...}
        
        // or with <bit> library, cf. https://en.cppreference.com/w/cpp/types/endian, but: C++20
        //#include <bit>
        //#include <iostream>
        //
        //int main() {
        //
        //    if constexpr (std::endian::native == std::endian::big)
        //        std::Rcout << "big-endian\n";
        //    else if constexpr (std::endian::native == std::endian::little)
        //        std::Rcout << "little-endian\n";
        //    else std::Rcout << "mixed-endian\n";
        //}
        
        // swapping template (then we just need to check the endianness of the system)
        // https://mklimenko.github.io/english/2018/08/22/robust-endian-swap/
        //template <typename T>
        //void SwapEndian(T &val) {
        //    union U {
        //        T val;
        //        std::array<std::uint8_t, sizeof(T)> raw;
        //    } src, dst;
        //
        //    src.val = val;
        //    std::reverse_copy(src.raw.begin(), src.raw.end(), dst.raw.begin());
        //    val = dst.val;
        //}
        
        /* void ExportPointcloudHeader(vector<int> &beams, fstream& output_pointcloud){
            // las files are defined as little endian
            // for the moment, we assume a little endian system and that chars actually have 8 bits (1 byte)
            // all names are just LAS definition names with underscores
            // future versions should upgrade format to 1.4, and include ways to parameterize coordinate reference system, etc.
            
            char file_signature[5] = "LASF";
            output_pointcloud.write(file_signature, sizeof(file_signature) - 1); // remove terminating NULL in char
            
            uint16_t file_source_id = 0;
            output_pointcloud.write(reinterpret_cast<const char *>(&file_source_id), sizeof(file_source_id));
            
            uint16_t global_encoding = 0;
            output_pointcloud.write(reinterpret_cast<const char *>(&global_encoding), sizeof(global_encoding));
            
            uint32_t project_id_guid_data_1 = 0;
            output_pointcloud.write(reinterpret_cast<const char *>(&project_id_guid_data_1), sizeof(project_id_guid_data_1));
            
            uint16_t project_id_guid_data_2 = 0;
            output_pointcloud.write(reinterpret_cast<const char *>(&project_id_guid_data_2), sizeof(project_id_guid_data_2));
            
            uint16_t project_id_guid_data_3 = 0;
            output_pointcloud.write(reinterpret_cast<const char *>(&project_id_guid_data_3), sizeof(project_id_guid_data_3));
            
            unsigned char project_id_guid_data_4[9] = "";
            output_pointcloud.write(reinterpret_cast<const char *>(&project_id_guid_data_4), sizeof(project_id_guid_data_4) - 1);
            
            unsigned char version_major = 1;    // this is slightly weird: why were these not simply defined as uint8_t in LAS specification? Or is "unsigned char" short for uint8_t? Because there is also an ascii char reserved for numbers (e.g. "1" corresponds to char = 49)
            output_pointcloud.write(reinterpret_cast<const char *>(&version_major), sizeof(version_major));
            
            unsigned char version_minor = 2;    // this is slightly weird: why were these not simply defined as uint8_t in LAS specification? Or is "unsigned char" short for uint8_t? Because there is also an ascii char reserved for numbers (e.g. "1" corresponds to char = 49)
            output_pointcloud.write(reinterpret_cast<const char *>(&version_minor), sizeof(version_minor));
            
            char system_identifier[33];
            sprintf(system_identifier,"ALS simulator");
            output_pointcloud.write(system_identifier, sizeof(system_identifier) - 1); // remove terminating NULL in char
            
            char generating_software[33] = "TROLL v.3.1.6+ forest simulator";
            output_pointcloud.write(generating_software, sizeof(generating_software) - 1); // remove terminating NULL in char
            
            // get current day and year, cf. https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm
            time_t now = time(0);
            tm *ltm = localtime(&now);
            
            uint16_t file_creation_day_of_year = ltm->tm_yday;
            uint16_t file_creation_year = 1900 + ltm->tm_year;
            
            output_pointcloud.write(reinterpret_cast<const char *>(&file_creation_day_of_year), sizeof(file_creation_day_of_year));
            output_pointcloud.write(reinterpret_cast<const char *>(&file_creation_year), sizeof(file_creation_year));
            
            uint16_t header_size = 227;
            output_pointcloud.write(reinterpret_cast<const char *>(&header_size), sizeof(header_size));
            
            uint32_t offset_to_point_data = 227;
            output_pointcloud.write(reinterpret_cast<const char *>(&offset_to_point_data), sizeof(offset_to_point_data));
            
            uint32_t number_of_variable_length_records = 0;
            output_pointcloud.write(reinterpret_cast<const char *>(&number_of_variable_length_records), sizeof(number_of_variable_length_records));
            
            unsigned char point_data_format_ID = 0; // no GPS time needed
            output_pointcloud.write(reinterpret_cast<const char *>(&point_data_format_ID), sizeof(point_data_format_ID));
            
            // each record in format 0 has 20 bytes (12 for coordinates, 2 for intensity, 6 for other information)
            uint16_t point_data_record_length = 20;
            output_pointcloud.write(reinterpret_cast<const char *>(&point_data_record_length), sizeof(point_data_record_length));
            
            // calculate the number of returns per return number
            int nb_perreturn[5] = {0};
            int nb_beams = int(beams.size()/2);
            int nb_returns = 0;
            
            for(int beam = 0; beam < nb_beams; beam++){
                int nb_returns_beam = beams[1 + beam * 2];
                for(int rtrn = 0; rtrn < nb_returns_beam; rtrn++){
                    nb_perreturn[rtrn]++;
                    nb_returns++;
                }
            }
            
            Rcout << "Sampled " << nb_beams << " pulses, creating " << nb_returns << " returns." << endl;
            
            uint32_t number_of_point_records = nb_returns;
            output_pointcloud.write(reinterpret_cast<const char *>(&number_of_point_records), sizeof(number_of_point_records));
            
            for(int i = 0; i < 5; i++){
                uint32_t number_of_points_by_return = nb_perreturn[i];
                output_pointcloud.write(reinterpret_cast<const char *>(&number_of_points_by_return), sizeof(number_of_points_by_return));
            }
            
            // there is no fixed-width type for floating-point numbers, so we assume that the 8 byte required by the las specification are fulfilled
            for(int i = 0; i < 3; i++){
                double xyz_scale_factor = 0.01;
                output_pointcloud.write(reinterpret_cast<const char *>(&xyz_scale_factor), 8); // hardcoded 8 bytes
            }
            
            for(int i = 0; i < 3; i++){
                double xyz_offset = 0.0;
                output_pointcloud.write(reinterpret_cast<const char *>(&xyz_offset), 8); // hardcoded 8 bytes
            }
            
            double max_x = cols;
            output_pointcloud.write(reinterpret_cast<const char *>(&max_x), 8); // hardcoded 8 bytes
            
            double min_x = 0;
            output_pointcloud.write(reinterpret_cast<const char *>(&min_x), 8); // hardcoded 8 bytes
            
            double max_y = rows;
            output_pointcloud.write(reinterpret_cast<const char *>(&max_y), 8); // hardcoded 8 bytes
            
            double min_y = 0;
            output_pointcloud.write(reinterpret_cast<const char *>(&min_y), 8); // hardcoded 8 bytes
            
            double max_z = HEIGHT;
            output_pointcloud.write(reinterpret_cast<const char *>(&max_z), 8); // hardcoded 8 bytes
            
            double min_z = 0;
            output_pointcloud.write(reinterpret_cast<const char *>(&min_z), 8); // hardcoded 8 bytes
        }
        
         void ExportPointcloud(float mean_beam, float sd_beam, float klaser, float transmittance_laser, fstream& output_pointcloud){
            Rcout << "Point cloud generation." << endl;
            
            vector<int> beams;
            vector<float> beams_returns;
            
            // three options
            GenerateVoxelreturnsALS(beams, beams_returns, mean_beam, sd_beam, klaser, transmittance_laser);
            
            ExportPointcloudHeader(beams, output_pointcloud);
            
            // now write point cloud records to file
            int nb_beams = int(beams.size()/2);
            int index_return = 0;
            
            for(int beam = 0; beam < nb_beams; beam++){
                int site_beam = beams[0 + beam * 2];
                int nb_returns_beam = beams[1 + beam * 2];
                
                int row = site_beam/cols;
                int col = site_beam%cols;
                
                // long format + 0.01 scaling (1cm precision)
                int32_t x_hit = round((float(col) + gsl_rng_uniform(gslrand)) * 100.0);
                int32_t y_hit = round((float(row) + gsl_rng_uniform(gslrand)) * 100.0);
                
                for(int rtrn = 0; rtrn < nb_returns_beam; rtrn++){
                    int32_t z_hit = round(beams_returns[index_return] * 100.0);
                    index_return++;
                    
                    output_pointcloud.write(reinterpret_cast<const char *>(&x_hit), sizeof(x_hit));
                    output_pointcloud.write(reinterpret_cast<const char *>(&y_hit), sizeof(y_hit));
                    output_pointcloud.write(reinterpret_cast<const char *>(&z_hit), sizeof(z_hit));
                    
                    uint16_t intensity = 0;
                    output_pointcloud.write(reinterpret_cast<const char *>(&intensity), sizeof(intensity));
                    
                    // the LAS-format makes use of sub-byte level information for return number / number of returns / scan direction / edge of flight line
                    // all are together in one single byte, stored in, respectively, 3 bits, 3 bits, 1 bit, 1 bit
                    // we need to reconstruct this here
                    // cf. comment in LAS defintion: "[A note on Bit Fields – The LAS storage format is “Little Endian.” This means that multi-byte data fields are stored in memory from least significant byte at the low address to most significant byte at the high address. Bit fields are always interpreted as bit 0 set to 1 equals 1, bit 1 set to 1 equals 2, bit 2 set to 1 equals 4 and so forth.]"; here bit 0 seems to mean the bit with the exponent 0, NOT the location/position 0 from left to right
                    // essentially (tested through trial and error), the system is:
                    // bit location 0 (exponent 7, "bit 7") = edge of flight line
                    // bit location 1 (exponent 6, "bit 6") = scan direction flag
                    // bit location 2-4 (exponents 5-3) = number of returns
                    // bit location 5-7 (exponents 2-0) = return number
                    
                    // we only have 5 cases for both return number and number of returns (both max 5), which are:
                    // 1: 001
                    // 2: 010
                    // 3: 011
                    // 4: 100
                    // 5: 101
                    
                    vector<int> information_bitlevel;
                    information_bitlevel.reserve(8);
                    
                    // we need to start with bits 7 and 6
                    information_bitlevel.push_back(0);  // edge of flight line (no such thing here)
                    information_bitlevel.push_back(0);  // scan direction (unimportant here)
                    
                    // the number of returns
                    if(nb_returns_beam <= 3){
                        information_bitlevel.push_back(0);
                        if(nb_returns_beam == 1){
                            information_bitlevel.push_back(0);
                            information_bitlevel.push_back(1);
                        } else {
                            information_bitlevel.push_back(1);
                            if(nb_returns_beam == 2){
                                information_bitlevel.push_back(0);
                            } else {
                                information_bitlevel.push_back(1);
                            }
                        }
                    } else {
                        information_bitlevel.push_back(1);
                        if(nb_returns_beam == 4){ // i.e. 4th return
                            information_bitlevel.push_back(0);
                            information_bitlevel.push_back(0);
                        } else {
                            information_bitlevel.push_back(0);
                            information_bitlevel.push_back(1);
                        }
                    }
                    
                    // and return number
                    int rtrn_actual = rtrn + 1;
                    if(rtrn_actual <= 3){
                        information_bitlevel.push_back(0);
                        if(rtrn_actual == 1){
                            information_bitlevel.push_back(0);
                            information_bitlevel.push_back(1);
                        } else {
                            information_bitlevel.push_back(1);
                            if(rtrn_actual == 2){
                                information_bitlevel.push_back(0);
                            } else {
                                information_bitlevel.push_back(1);
                            }
                        }
                    } else {
                        information_bitlevel.push_back(1);
                        if(rtrn_actual == 4){
                            information_bitlevel.push_back(0);
                            information_bitlevel.push_back(0);
                        } else {
                            information_bitlevel.push_back(0);
                            information_bitlevel.push_back(1);
                        }
                    }
                    
                    // now put combined binary information into a single decimal number
                    // e.g. first return out of one return total should be: 00 001 001 (which is 9 in decimal notation)
                    // e.g. fourth return out of five returns total should be: 00 101 100 (which is 44 in decimal notation)
                    int8_t return_info = 0;
                    for(int i = 0; i < 8; i++){
                        return_info += information_bitlevel[i] * pow(2,7-i);
                    }
                    output_pointcloud.write(reinterpret_cast<const char *>(&return_info), sizeof(return_info));
                    
                    unsigned char classification = 0;
                    output_pointcloud.write(reinterpret_cast<const char *>(&classification), sizeof(classification));
                    
                    char scan_angle_rank = 0;
                    output_pointcloud.write(reinterpret_cast<const char *>(&scan_angle_rank), sizeof(scan_angle_rank));
                    
                    unsigned char user_data = 0;
                    output_pointcloud.write(reinterpret_cast<const char *>(&user_data), sizeof(user_data));
                    
                    uint16_t point_source_id = 1;
                    output_pointcloud.write(reinterpret_cast<const char *>(&point_source_id), sizeof(point_source_id));
                    
                }
            }
        } */
         
         
         
        
#ifdef TRACK_INDIVIDUALS
        //##############################################
        // Global function: tree level tracking of key variables
        //##############################################
        void TrackingData_andOutput(){
            for(int site = 0; site < sites; site++){
                // we start the accounting the year after the trees have been born
                if(T[site].t_age > 0){
                    int timeofyear_born = T[site].t_timeofyear_born;
                    
                    if(timeofyear_born >= 0){
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
                        
                        float agb = 1000.0 * T[site].CalcAGB();
                        
                        // write to output every year and then reset to zero
                        int timeofyear = GetTimeofyear();
                        if(timeofyear == timeofyear_born){
                            // write to output
                            if(T[site].t_dbh >= 0.1){
                                output_track[1] << T[site].t_site << "\t" << timeofyear_born << "\t" << iter << "\t" << T[site].t_age << "\t" << T[site].t_seedsproduced_sumyear << "\t" << T[site].t_seedsproduced << "\t" << T[site].t_time_carbonstarvation_year << "\t" << T[site].t_time_carbonstarvation << "\t" << T[site].t_dbh << "\t" << T[site].t_dbh - T[site].t_dbh_tracked << "\t" << T[site].t_height << "\t"  <<  T[site].t_height - T[site].t_height_tracked << "\t" << T[site].t_CR << "\t"  <<  T[site].t_CR - T[site].t_CR_tracked << "\t" << agb << "\t" << agb - T[site].t_agb_tracked << "\t" << T[site].t_GPP_sumyear << "\t" << T[site].t_GPPsquared_sumyear  << "\t" << T[site].t_NPP_sumyear << "\t" << T[site].t_NPPsquared_sumyear << "\t" << T[site].t_Rday_sumyear << "\t" << T[site].t_Rnight_sumyear << "\t" << T[site].t_Rstem_sumyear << "\t" << T[site].t_LAIabove_effavgyear<< "\t" << T[site].t_carbon_storage_avgyear << endl;
                            }
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
                            T[site].t_height_tracked = T[site].t_height;
                            T[site].t_CR_tracked = T[site].t_CR;
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
        void UpdateTransmittanceCHM_ABC(float mean_beam, float sd_beam, float klaser, float transmittance_laser){
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
                        float height = T[s].t_height;
                        float CR = T[s].t_CR;
                        float CD = T[s].t_CD;
                        float fraction_filled_target = 1.0;        //to remove spikes, we assume a fully filled crown for all trees
                        int shell_fromtop = 0;              //toplayer
                        float noinput = 0.0;
                        
                        LoopLayerUpdateCrownStatistic_template(r, c, height, CR, CD, fraction_filled_target, shell_fromtop, GetRadiusSlope, noinput, chm_field_current, KeepFloatAsIs, UpdateCHM);
#else
                        int crown_top = int(T[s].t_height);
                        
                        float crown_area = PI * T[s].t_CR * T[s].t_CR;     //floor of crown_area to bound area accumulation
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
                    int dev_rand = int(gsl_rng_uniform_int(gslrng,10000)); // modified FF, v.3.1.5

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
                    
                    mu_height_varoutput += log(T[s].t_mult_height);
                    mu_CR_varoutput += log(T[s].t_mult_CR);
                    mu_CD_varoutput += log(T[s].t_mult_CD);
                    mu_P_varoutput += log(T[s].t_mult_P);
                    mu_N_varoutput += log(T[s].t_mult_N);
                    mu_LMA_varoutput += log(T[s].t_mult_LMA);
                    mu_dbhmax_varoutput += log(T[s].t_mult_dbhmax);
                    mu_wsg_varoutput += log(T[s].t_dev_wsg);
                    
                    moment2_random += dev_rand;
                    moment2_height_varinput += log(d_intraspecific_height[dev_rand]) * log(d_intraspecific_height[dev_rand]);
                    moment2_CR_varinput += log(d_intraspecific_CR[dev_rand]) * log(d_intraspecific_CR[dev_rand]);
                    moment2_CD_varinput += log(d_intraspecific_CD[dev_rand]) * log(d_intraspecific_CD[dev_rand]);
                    moment2_P_varinput += log(d_intraspecific_P[dev_rand]) * log(d_intraspecific_P[dev_rand]);
                    moment2_N_varinput += log(d_intraspecific_N[dev_rand]) * log(d_intraspecific_N[dev_rand]);
                    moment2_LMA_varinput += log(d_intraspecific_LMA[dev_rand]) * log(d_intraspecific_LMA[dev_rand]);
                    moment2_dbhmax_varinput += log(d_intraspecific_dbhmax[dev_rand]) * log(d_intraspecific_dbhmax[dev_rand]);
                    moment2_wsg_varinput += log(d_intraspecific_wsg[dev_rand]) * log(d_intraspecific_wsg[dev_rand]);
                    
                    moment2_height_varoutput += log(T[s].t_mult_height) * log(T[s].t_mult_height);
                    moment2_CR_varoutput += log(T[s].t_mult_CR) * log(T[s].t_mult_CR);
                    moment2_CD_varoutput += log(T[s].t_mult_CD) * log(T[s].t_mult_CD);
                    moment2_P_varoutput += log(T[s].t_mult_P) * log(T[s].t_mult_P);
                    moment2_N_varoutput += log(T[s].t_mult_N) * log(T[s].t_mult_N);
                    moment2_LMA_varoutput += log(T[s].t_mult_LMA) * log(T[s].t_mult_LMA);
                    moment2_dbhmax_varoutput += log(T[s].t_mult_dbhmax) * log(T[s].t_mult_dbhmax);
                    moment2_wsg_varoutput += log(T[s].t_dev_wsg) * log(T[s].t_dev_wsg);
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
                        float height_tree = T[s].t_height;
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
                        mean_CR += T[s].t_CR;
                        
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
                            mean_CR10 += T[s].t_CR;
                            
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
                        
                        float LMA = log(T[s].t_mult_LMA);
                        float LMA2 = LMA * LMA;
                        float Nmass = log(T[s].t_mult_N);
                        float Nmass2 = Nmass * Nmass;
                        float Pmass = log(T[s].t_mult_P);
                        float Pmass2 = Pmass * Pmass;
                        float wsg = T[s].t_dev_wsg;
                        float wsg2 = wsg * wsg;
                        float crown = log(T[s].t_mult_CR);
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
                    
                    AddCrownVolumeLayer(row_center, col_center, T[site].t_height, T[site].t_CR, T[site].t_CD, voxcrown);
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
        
        
        
        //! Close outputs
        void CloseOutputs(){
            output_info.close();
            output_info.clear();
            
            for(int i = 0; i < 4; i++){
                output_basic[i].close();
                output_basic[i].clear();
            }
            
            if(_OUTPUT_extended == 1){
                for(int i = 0; i < 9; i++){
                    output_extended[i].close();
                    output_extended[i].clear();
                }
                if(extent_visual > 0){
                    for(int i = 0; i < 2; i++){
                        output_visual[i].close();
                        output_visual[i].clear();
                    }
                }
            }
            
#ifdef Output_ABC
            for(int i = 0; i < 11; i++){
                output_abc[i].close();
                output_abc[i].clear();
            }
#endif
#ifdef WATER
            for(int i = 0; i < 10; i++){
                output[i].close();
                output[i].clear();
                //output_water[i].close();
                //output_water[i].clear();
            }
#endif
#ifdef TRACK_INDIVIDUALS
            for(int i = 0; i < 3; i++){
                output_track[i].close();
                output_track[i].clear();
            }
#endif
        }
        
        
        
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
            delete [] LookUp_ExtinctLW;
            //delete [] LookUp_Rday; //newIM: no redundancy anymore between LookUp_Rday and LookUp_Rleaf
            delete [] LookUp_JmaxT;
            delete [] LookUp_Rstem;
            delete [] LookUp_Rleaf; //newIM: no redundancy anymore between LookUp_Rday and LookUp_Rleaf
            delete [] LookUp_GammaT;
            //delete [] LookUp_Rnight; //newIM: no redundancy anymore between LookUp_Rday and LookUp_Rleaf
            delete [] LookUp_VcmaxT;
#ifdef WATER
            delete [] LookUp_INLR;
            delete [] LookUp_SLOPE;
            delete [] LookUp_GRADN;
#endif
        }
        
        

