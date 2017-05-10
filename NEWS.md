# TO DO

* define together use of global options for modelling purpose
* modification of input file
* merging of info and par file in output, potentially using info-file for module activation
* finish `summary` for TROLLsimstack
* Integrate virtual forest in modelling process (activate _from _FromData in `build` and integrate forest.txt in `run`)
* formal testin with test that
* use of `goodpractice`package
* integrate all methods to better manipulate TROLLsimstack (e.g. `[[`)

# RconTroll 0.1.9008
* `loadStack` and `loadOutput` recode

# RconTroll 0.1.9007

* New possibilities in plotting `what` argument : diversity and rank-abundance (disturbed and final)
* New possibilities in plotting `what` argument : functional traits density plots (disturbed and final)

# RconTroll 0.1.9006

* `loadStack` function
* Correct `plotly` and `ggplot2` namespaces issues
* insert wip badge in `README` ![stability-wip](https://img.shields.io/badge/stability-work_in_progress-lightgrey.svg)

# RconTroll 0.1.9005

* New possibilities in plotting `what` argument (agb, gpp, litterfall, all abund, all ba, all R, all final_pattern, all disturbance)
* `inventoryFromOutput` fixed
* Joining `plot` methods in one file with only one doc
* Joining `print` methods in one file with only one doc
* Joining `summary` methods in one file with only one doc
* Corrected all WARNINGs and NOTEs in R CMD check
* Corrected part of `goodpractice::gp()` recomendations
* Add `cran-comment.md` to justify R CMD check NOTEs

# RconTroll 0.1.9004

* `TROLLsimstack`: rename compressed in aggregated and document all slot
* `aggregate.TROLLsimstack` method
* `stack.TROLLsim` in its own file with doc
* `plot.TROLLsimstack` for (stack,any) with base, ggplot2 and plotly
* `plot.TROLLsimstack` for (stack,stack) to compare with a control
* `plot.TROLLsim` using `plot.TROLLsimstack`

# RconTroll 0.1.9003

* renaming objects and methods (e.g. `TROLLoutput` to `TROLLsim`, `virtualizeFromData` to `inventoryFromData`, etc.)
* definition of `summary` for class TROLLsim
* new class TROLLsimstack (with methods `print` and `summary`, the latter not yet fully implemented)

# RconTroll 0.1.9002

* Disturbance module integration in `TROLLoutput` class, `load_output` function and `plot.TROLLoutput`method
* `virtualizeFromOutput` function
* Renaming TROLL -> RconTroll
* Travis-CI continuous integration
* Codecover integration

# RconTroll 0.1.9001

* Initialise package and git repository (GitHub)
