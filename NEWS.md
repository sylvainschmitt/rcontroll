# RconTROLL 0.1.9011
* Issue [#10](https://github.com/fischer-fjd/RconTroll/issues/6) about package structures
* develompment guidelines (missing social coding with GitHub section)
* RconTROLL renaming
* README to md
* AppVeyor
* prepare extdata structure
* test structure
* package documentation with R/RconTROLL.R
* options (zzz.R)
* rm build and model functions
* vignette bug fix

# RconTROLL 0.1.9011
* developers guidelines #10 & README

# RconTROLL 0.1.9010
* TROLL workflow vignette

# RconTROLL 0.1.9009
* Issue [#6](https://github.com/fischer-fjd/RconTroll/issues/6) about species assignment in loadOutput
* Issue [#8](https://github.com/fischer-fjd/RconTroll/issues/6) about full_final.txt integration

# RconTROLL 0.1.9008
* `loadStack` and `loadOutput` recode

# RconTROLL 0.1.9007

* New possibilities in plotting `what` argument : diversity and rank-abundance (disturbed and final)
* New possibilities in plotting `what` argument : functional traits density plots (disturbed and final)

# RconTROLL 0.1.9006

* `loadStack` function
* Correct `plotly` and `ggplot2` namespaces issues
* insert wip badge in `README`
# RconTROLL 0.1.9005

* New possibilities in plotting `what` argument (agb, gpp, litterfall, all abund, all ba, all R, all final_pattern, all disturbance)
* `inventoryFromOutput` fixed
* Joining `plot` methods in one file with only one doc
* Joining `print` methods in one file with only one doc
* Joining `summary` methods in one file with only one doc
* Corrected all WARNINGs and NOTEs in R CMD check
* Corrected part of `goodpractice::gp()` recomendations
* Add `cran-comment.md` to justify R CMD check NOTEs

# RconTROLL 0.1.9004

* `TROLLsimstack`: rename compressed in aggregated and document all slot
* `aggregate.TROLLsimstack` method
* `stack.TROLLsim` in its own file with doc
* `plot.TROLLsimstack` for (stack,any) with base, ggplot2 and plotly
* `plot.TROLLsimstack` for (stack,stack) to compare with a control
* `plot.TROLLsim` using `plot.TROLLsimstack`

# RconTROLL 0.1.9003

* renaming objects and methods (e.g. `TROLLoutput` to `TROLLsim`, `virtualizeFromData` to `inventoryFromData`, etc.)
* definition of `summary` for class TROLLsim
* new class TROLLsimstack (with methods `print` and `summary`, the latter not yet fully implemented)

# RconTROLL 0.1.9002

* Disturbance module integration in `TROLLoutput` class, `load_output` function and `plot.TROLLoutput`method
* `virtualizeFromOutput` function
* Renaming TROLL -> RconTROLL
* Travis-CI continuous integration
* Codecover integration

# RconTROLL 0.1.9001

* Initialise package and git repository (GitHub)
