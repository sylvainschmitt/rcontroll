# rcontroll 0.1.0.9063

-   removing `terra` reading NETcdf from climate vignette in macOS

# rcontroll 0.1.0.9062

-   clang support
-   REMINDER use valgrind for segfault : R -d "valgrind -s" -e 'rcontroll::troll(name = "test",global = rcontroll::generate_parameters(cols = 100, rows = 100,iterperyear = 12, nbiter = 12 \* 1),species = rcontroll::TROLLv3_species,climate = rcontroll::TROLLv3_climatedaytime12,daily = rcontroll::TROLLv3_daytimevar,verbose = TRUE)'

# rcontroll 0.1.0.9061

-   fixed NEWS after CRAN 1st submission
-   removing dubious URL after CRAN 1st submission

# rcontroll 0.1.0.9060

-   fixing win-build notes (wrong link & typo)
-   dontrun troll example because \>10s
-   reducing era5 data in climate vignette to reduce package size (\<100MB)
-   removing worflow spatial figure to reduce size

# rcontroll 0.1.0.9059

-   cover fix 1
-   removed RcppGSL
-   badges added
-   climate vignette typos

# rcontroll 0.1.0.9058

-   including PR #39
-   Guillaume's dev (configure, seed, lidar iter 0, CI)
-   Reject `inmemory` option
-   Cleaning code
-   Adding test (covr\>80%)
-   Fixing era5 new version
-   Adding era5 extdata
-   Compiling climate vignette

# rcontroll 0.1.0.9057

-   `dos2unix` whole files
-   add `inmemory` option to load simulation(s) results
-   check consistency between `Niter` (global parameter) & `iter_pointcloud_generation` (lidar parameters)
-   checked url in documents

# rcontroll 0.1.0.9056

-   Add compilation configure files with gsl-config and download on Wondows OS (configure/configure.win, makevars.in, cleanup/cleanup)
-   Add chmod +x to configure & cleanup ex: `git update-index --chmod=+x configure`
-   Add Rseed setup to connect set.seed with gsl seed for enhanced reproductibility
-   Set hack to simulate at t = 0 lidar
-   remove license from build
-   update GA check
-   remove GA covr
-   update description title
-   code cleaning with lintr::lint_package & styler::style_pkg

# rcontroll 0.1.0.9055

-   LICENSE.md

# rcontroll 0.1.0.9054

-   lidr figure, updated README & doc, updated version

# rcontroll 0.1.0.9053

-   Fabian TROLL v1.3.7 fixes las closing, no las automatic creation, leafflush bug
-   Fixing the NULL character to "" for TROLL cpp

# rcontroll 0.1.0.9052

-   lidar backend:
-   las attribute as list(`lidR::LAS`) for trollsim & trollstack
-   lidR in DESCRIPTION
-   get_chm methods
-   print methods
-   autoplot methods
-   load_output and load_stack

# rcontroll 0.1.0.9051

-   Guillaume cleaned frontend for lidar

# rcontroll 0.1.0.9050

-   Update TROLL to 3.1.6 & add lidar generation parameters

# rcontroll 0.1.0.9049

-   fixed and & or operands in cpp for windows-latest (release)

# rcontroll 0.1.0.9048

-   methods `get_forest` & `update_parameters`

# rcontroll 0.1.0.9047

-   `autogif` v1

# rcontroll 0.1.0.9046

-   TROLL 1.3.3, `TROLL.version()` & `autogif` begins

# rcontroll 0.1.0.9044

-   merge @gsalzet rcpp_gsl

# rcontroll 0.1.0.9043

-   Add import GSL and Add CI on GH Actions multi-arch

# rcontroll 0.1.0.9042

-   Add CI on GH Actions multi-arch

# rcontroll 0.1.0.9041

-   Add CI on GH Actions ubuntu alone

# rcontroll 0.1.0.9040

-   Add configure.win, cleanup.win and Makevars.win files

# rcontroll 0.1.0.9039

-   Change permission access of configure file

# rcontroll 0.1.0.9038

-   Update GH actions

# rcontroll 0.1.0.9037

-   add configure file to detect GSL install
-   reviewed english

# rcontroll 0.1.0.9036

-   `autoplot` v2

# rcontroll 0.1.0.9035

-   Fabian's cleaned CPP

# rcontroll 0.1.0.9034

-   using TROLL child for fake parralelisation
-   waiting for Fabian's cleaned CPP

# rcontroll 0.1.0.9033

-   fabain's neww cpp
-   pkgdown on gh-pages branch

# rcontroll 0.1.0.9032

-   pkgdown init

# rcontroll 0.1.0.9031

-   reviewed doc, vignettes, & README

# rcontroll 0.1.0.9030

-   fixed tests

# rcontroll 0.1.0.9029

-   reduced TROLLV3_output
-   fixed CRAN check
-   added CRAN comments

# rcontroll 0.1.0.9028

-   TROLLV3_output for exs and tests
-   autoplot upgraded
-   troll with fake parralelisation for R child runs of TROLL

# rcontroll 0.1.0.9027

-   test reprex, news to be filled

# rcontroll 0.1.0.9026

-   integration of TROLL main_v3.1_rcpp.cpp with corresponding new parameters
-   from data to be explored and values to be tested

# rcontroll 0.1.0.9025

-   doc & vignette

# rcontroll 0.1.0.9024

-   adding tests and fixing check

# rcontroll 0.1.0.9024

-   adding forest_path to work with FromData but is not correctly programmed in the cpp

# rcontroll 0.1.0.9023

-   cout and cerr to Rcout and Rcerr
-   adding a verbose to troll and stack

# rcontroll 0.1.0.9022

-   adding OUTPUT_reduced, FromData and NONRANDOM to general parameters with a lot of consequent changes
-   adding a thinning parameter
-   simplified vignette

# rcontroll 0.1.0.9021

-   simplifying trollsim and trollstack to one

# rcontroll 0.1.0.9020

-   seamless integration of trollCpp thanks to Rcpp !
-   updated troll.R & load_output
-   rm inst/troll & compile troll
-   rm climate365 & soil
-   vignette calibration & Schmitt2020
-   rm doc2word.sh

# rcontroll 0.1.0.9019

-   initiating all vignettes
-   doc2word.sh for sharing on gdrive
-   TROLL vignette

# rcontroll 0.1.0.9018

-   add progress bar in stack function with doSNOW package
-   R CMD check done: dev branch

# rcontroll 0.1.0.9017

-   corrected autoplot functions #11 #16
-   first attempt 1000 years vignette #13

# rcontroll 0.1.0.9016

-   generate_parameters #12

# rcontroll 0.1.0.9015

-   load_stack #9, print, show, summary stack #10, autoplot.stack #16, is.stack

# rcontroll 0.1.0.9014

-   stack #8

# rcontroll 0.1.0.9013

-   trollstack #6

# rcontroll 0.1.0.9012

-   autoplot.trollsim #11

# rcontroll 0.1.0.9011

-   #2 trollsim:
    -   parameters as a named vector
    -   forest and random as parameters
    -   final_pattern
    -   architecture for stacks
-   #3 troll:
    -   work with all binaries
    -   forest and random parameters
    -   final_pattern
-   #4 load_output:
    -   parameters as a named vector
    -   forest and random as parameters
    -   final_pattern
    -   consolidated reduced outputs

# rcontroll 0.1.0.9010

-   compile_troll and unix binaries #1

# rcontroll 0.1.0.9009

-   troll and load_output for all trollsimclasses #3 & #5

# rcontroll 0.1.0.9008

-   trollsimclasses #2

# rcontroll 0.1.0.9007

-   TROLL binaries for windows #1

# rcontroll 0.1.0.9006

-   load_output read all, consolidated output for TROLL_full #5
-   plot_ecosystem base plot for TROLL_full #11
-   print, summary \# 10
-   troll binary path, tmp, unlink for TROLL_full #3
-   trollsim consolidated outputs for TROLL_full #2
-   zzz.R tmp
-   test-troll working
-   update workflow.Rmd

# rcontroll 0.1.0.9005

-   tidyverse style guide #14

# rcontroll 0.1.0.9004

-   TROLL binaries for unix #1

# rcontroll 0.1.0.9003

-   opened dev branch
-   opened documentation for data #10

# rcontroll 0.1.0.9002

-   opened old branch and cleaned main branch

# rcontroll 0.1.0.9001

-   relaunch of the package

# Archived development

## RconTROLL 0.1.9011

-   Issue [#10](https://github.com/fischer-fjd/RconTroll/issues/6) about package structures
-   develompment guidelines (missing social coding with GitHub section)
-   RconTROLL renaming
-   README to md
-   AppVeyor
-   prepare extdata structure
-   test structure
-   package documentation with R/RconTROLL.R
-   options (zzz.R)
-   rm build and model functions
-   vignette bug fix

## RconTROLL 0.1.9012

-   getTROLL.R with examples to automatically add TROLL code and application into RconTROLL package installation
-   run.R with example
-   fake species data for examples #15
-   simplified plot.TROLLsim.R #14 done
-   update options in zzz.R

## RconTROLL 0.1.9011

-   developers guidelines #10 & README

## RconTROLL 0.1.9010

-   TROLL workflow vignette

## RconTROLL 0.1.9009

-   Issue [#6](https://github.com/fischer-fjd/RconTroll/issues/6) about species assignment in loadOutput
-   Issue [#8](https://github.com/fischer-fjd/RconTroll/issues/6) about full_final.txt integration

## RconTROLL 0.1.9008

-   `loadStack` and `loadOutput` recode

## RconTROLL 0.1.9007

-   New possibilities in plotting `what` argument : diversity and rank-abundance (disturbed and final)
-   New possibilities in plotting `what` argument : functional traits density plots (disturbed and final)

## RconTROLL 0.1.9006

-   `loadStack` function

-   Correct `plotly` and `ggplot2` namespaces issues

-   insert wip badge in `README` \# RconTROLL 0.1.9005

-   New possibilities in plotting `what` argument (agb, gpp, litterfall, all abund, all ba, all R, all final_pattern, all disturbance)

-   `inventoryFromOutput` fixed

-   Joining `plot` methods in one file with only one doc

-   Joining `print` methods in one file with only one doc

-   Joining `summary` methods in one file with only one doc

-   Corrected all WARNINGs and NOTEs in R CMD check

-   Corrected part of `goodpractice::gp()` recomendations

-   Add `cran-comment.md` to justify R CMD check NOTEs

## RconTROLL 0.1.9004

-   `TROLLsimstack`: rename compressed in aggregated and document all slot
-   `aggregate.TROLLsimstack` method
-   `stack.TROLLsim` in its own file with doc
-   `plot.TROLLsimstack` for (stack,any) with base, ggplot2 and plotly
-   `plot.TROLLsimstack` for (stack,stack) to compare with a control
-   `plot.TROLLsim` using `plot.TROLLsimstack`

## RconTROLL 0.1.9003

-   renaming objects and methods (e.g. `TROLLoutput` to `TROLLsim`, `virtualizeFromData` to `inventoryFromData`, etc.)
-   definition of `summary` for class TROLLsim
-   new class TROLLsimstack (with methods `print` and `summary`, the latter not yet fully implemented)

## RconTROLL 0.1.9002

-   Disturbance module integration in `TROLLoutput` class, `load_output` function and `plot.TROLLoutput`method
-   `virtualizeFromOutput` function
-   Renaming TROLL -\> RconTROLL
-   Travis-CI continuous integration
-   Codecover integration

## RconTROLL 0.1.9001

-   Initialise package and git repository (GitHub)
