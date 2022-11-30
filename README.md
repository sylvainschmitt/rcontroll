# *rcontroll*: individual-based forest growth simulator TROLL <img src='man/figures/logo.png' align="right" height="200" />

<!-- sticker("~/Téléchargements/TROLL.png", package="rcontroll", p_size=20, s_x=1, s_y = 0.85, s_width=.6, p_y = 1.6, filename="inst/figures/logo.png", h_color = "darkblue", h_fill = "white", p_color = "black") -->

[![R-CMD-check](https://github.com/sylvainschmitt/rcontroll/workflows/R-CMD-check/badge.svg)](https://github.com/sylvainschmitt/rcontroll/actions)

*rcontroll* integrates the individual-based  and spatially-explicit *TROLL* model to simulate forest ecosystem and species dynamics forward in time.
*rcontroll* provides user-friendly functions to set up and analyse simulations with varying community compositions, ecological parameters, and climate conditions.

## Description

`TROLL` is coded in C++ to efficiently simulate typically hundreds of thousands of individuals over hundreds of years. 
The `rcontroll` package is a wrapper of `TROLL` that includes functions in R that generate inputs and prepare the run,
run simulations, and analyse the outputs through tables, figures, and maps taking advantage of the R package ecosystem. 

### Pre-simulations

Pre-simulation functions include global parameters definition (`generate_parameters` function), optional definition of lidar parameters (`generate_lidar` function), and climate data generation ((`generate_climate` function). 
`rcontroll` also includes default data for species and climate inputs for a typical French Guiana rainforest. 
The purpose of the `generate_climate` function with the help of the corresponding vignette is to calculate `TROLL` climate inputs from ERA5-Land (Muñoz-Sabater et al. 2021), 
a global climatic reanalysis dataset.
Therefore, `rcontroll` users only need species-specific data to run `TROLL` simulations anywhere in the tropics.
The definition of the lidar parameters (`generate_lidar` function) is optional but allows the user to add an aerial lidar simulation for a time step defined in the TROLL simulation to obtain a corresponding point cloud.

### Simulations

Simulations are run one at a time using the `troll` function or as stacked runs, using the `stack` function.
The outputs of the two commands are stored in `trollsim` or `trollstack` classes, respectively;
they can be accessed using object attributes (with `@` in R) in the form of simple R objects. 
`trollsim` or `trollstack` objects consist of 9 attributes: (1) simulation name, (2) path to saved files, (3) simulation parameters,
(4) simulation inputs, (5) simulation log, (6) initial and final state of the forest (Fig. 1), 
(7) ecosystem metrics, (8) species metrics, and optionally (9) point clouds from aerial lidar simulations (Fig. 2).
The initial and final states of the simulated forest are represented by a table that records the spatial position, 
size and other relevant traits of all trees at the beginning and end of the simulation. 
The ecosystem and species metrics are summaries of ecosystem processes and states , such as  net primary productivity and aboveground biomass, 
and provided both at species-level and aggregated over the entire stand. 
Point clouds from aerial lidar simulations are stored as LAS using the R package `lidR` (Roussel et al., 2020).
Simulations can be saved using a user-defined path when run and later loaded as a simple simulation (`load_output` function) or a stack of simulations (`load_stack` function).

### Post-simulations

`rcontroll` also includes post-simulations functions. 
Simulation outputs can be retrieved simply from `trollsim` or `trollstack` objects and summarised or plotted in the R environnement with the `print`, `summary` and `autoplot` functions.
The miscellaneous `get_chm` function allows users to quickly retrieve canopy height models from aerial lidar point clouds (Fig. 2).
In addition, a function is available to visualise `TROLL` simulations as an animated figure (`autogif` function, Fig. 1) for research, demonstration or educational purposes.

![*Figure 1: Output from a TROLL simulation using the `autogif` function from rcontroll. The figures shows a latitudinal cut in the forest structure along the X-axis (in metre) with individual tree height (metre). The tree colours indicate the identity of the species and can be easily changed using the ggplot2 grammar. The figure shows the forest structure dynamically over 200 years of a successional trajectory starting from bare ground.*](https://raw.githubusercontent.com/sylvainschmitt/rcontroll/main/inst/figures/troll.gif)

![*Figure 2: Point cloud simulated from an airborne lidar in a TROLL simulation with default parameters after 600 years.  The horizontal axes represent the X-axis and Y-axis (in metres) and the vertical axis represents the height (in metres). The thermal colour scale indicates the height of the points in the cloud, from 0m in dark blue to 40m in red.*](https://raw.githubusercontent.com/sylvainschmitt/rcontroll/main/inst/figures/lidar.png)

## Installation

You can install the latest version of **rcontroll** from Github using the [`devtools`](https://github.com/hadley/devtools) package:

``` r
if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools")

devtools::install_github("sylvainschmitt/rcontroll")
```

## Usage

```r
library(rcontroll)
data("TROLLv3_species")
data("TROLLv3_climatedaytime12")
data("TROLLv3_daytimevar")
sim <- troll(name = "test",
             global = generate_parameters(iterperyear = 12, nbiter = 12*1),
             species = TROLLv3_species,
             climate = TROLLv3_climatedaytime12,
             daily = TROLLv3_daytimevar)
autoplot(sim, what = "species", 
         species = c("Cecropia_obtusa","Dicorynia_guianensis",
                     "Eperua_grandiflora","Vouacapoua_americana")) +
  theme(legend.position = "bottom")
```
