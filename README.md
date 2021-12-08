# *rcontroll*: individual-based forest growth simulator TROLL


[![R build status](https://github.com/sylvainschmitt/rcontroll/workflows/R-CMD-check/badge.svg)](https://github.com/sylvainschmitt/rcontroll/actions)
[![Codecov test coverage](https://codecov.io/gh/sylvainschmitt/rcontroll/branch/master/graph/badge.svg)](https://codecov.io/gh/sylvainschmitt/rcontroll?branch=master)

*rcontroll* integrates the individual-based  and spatially-explicit *TROLL* model to simulate forest ecosystem and species dynamics forward in time.
*rcontroll* provides user-friendly functions to set up and analyse simulations with varying community compositions, ecological parameters, and climate conditions.

## Description

The *rcontroll* package relies on a few functions in R to generate and provide inputs, prepare and run the simulations, 
and analyze the simulations through tables, figures, and maps that are easily connected to the rest of the R package ecosystem.
The whole workflow can even be run for one or several simulations using a unique function (`troll` and `stack`). 
Pre-simulations functions include global parameters definition (`generate_parameters`) and species and weather data input, with default values for French Guiana included in the package. 
Simulations are run alone (`troll`) or as a stack (`stack`) and stocked in three defined classes depending on the level of outputs wanted:  
`trollsimfull` for a full set of outputs, `trollsimreduced` for a reduced set of outputs, and `trollsimabc` for outputs later used in approximate bayesian computing or other calibration methods.
The post-simulation outputs are stored in the corresponding objects and can be accessed using object attributes (with `@` in R) in the form of simple R objects, mainly data frames, 
or summarized and plotted with the `print`, `summary` and `autoplot` methods. 
Simulations can be saved using a user-defined path when run and later loaded as a simple simulation (`load_output`) or a stack of simulations (`load_stack`).

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
data("TROLLv3_input")
data("TROLLv3_species")
data("TROLLv3_climatedaytime12")
data("TROLLv3_climatedaytime365")
data("TROLLv3_daytimevar")
TROLLv3_input$value[6] <- 12 # Nb iterations per Year
TROLLv3_input$value[5] <- Nyears*12 # iterations
sim <- troll(name = "test",
              # path = "./",
              full = TRUE,
              abc = FALSE, 
              random = TRUE,
              global = TROLLv3_input,
              species = TROLLv3_species,
              climate = TROLLv3_climatedaytime12,
              daily = TROLLv3_daytimevar)
autoplot(sim1, 
         what = "ecosystem", 
         variables = c("abund", "ba"), 
         selected_species = c("Cecropia_obtusa","Dicorynia_guianensis",
                              "Eperua_grandiflora","Vouacapoua_americana"))
```

## Contributing

This package is currently under intensive development on the [`dev` branch](https://github.com/sylvainschmitt/rcontroll/tree/dev).
Please check the following issue for contribution guidelines: https://github.com/sylvainschmitt/rcontroll/issues/14.

For a full list of changes see [NEWS](https://github.com/sylvainschmitt/rcontroll/blob/main/NEWS.md).
