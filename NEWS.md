# rcontroll 0.2.0.9013

- removing remaining exit

# rcontroll 0.2.0.9012

- adding a theta_w=0 case in cpp and force iterperyear=365 causing bug in simulations with incomplete years

# rcontroll 0.2.0.9011

- cpp init back from Isabelle version & TROLL debug, issue from using predLeafLifespanKikuzawa
- testing climates, guyaflux ok, era and cordex to test

# rcontroll 0.2.0.9010

- Canopy height dcell minimum to 10 cm

# rcontroll 0.2.0.9009

- Extended VPD max to 6 in TROLL for CORDEX simulations

# rcontroll 0.2.0.9008

- added cpp line for theta_w = 0

# rcontroll 0.2.0.9007

- full ERA5 data request to TROL data functional and documented

# rcontroll 0.2.0.9006

- request_era, request_era_batch, prepare_era, generate_climate, climate.Rmd updates

# rcontroll 0.2.0.9005

- download_era to request_era & request_era_batch

# rcontroll 0.2.0.9004

- download_era test

# rcontroll 0.2.0.9003

- TROLLv4 bug fix  & V3 news merge

# rcontroll 0.2.0.9002

- TROLLv4 bug fix pre-V3 news merge

# rcontroll 0.2.0.9001

- TROLLv4 integration & memory optimization, stack to check, suspect an error in generate_parameters

# rcontroll 0.1.1.9001

- extended documentation following review

# rcontroll 0.1.0.9065

- new stack in tmp fix & TROLLV3_output update

# rcontroll 0.1.0.9064

- improving memory usage

# rcontroll 0.1.0.9063

- fixed stack with batch foreach 

# rcontroll 0.1.0.9062

- fixes after CRAN 3rd submission (see cran-comments.md)

# rcontroll 0.1.0.9062

-   fixes after CRAN 2nd submission (see cran-comments.md)

# rcontroll 0.1.0.9064

- replace `sprintf` to `snprintf` with `sizeof()` argument

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
