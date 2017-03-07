# TO DO

* define together use of global options for modelling purpose
* modification of input file
* merging of info and par file in output, potentially using info-file for module activation
* finish `summary` for TROLLsimstack
* create subclasses for TROLLsimstack
* define `plot` for TROLLsimstack
* Correct `plot.output` method with use of internal functions (see disturbance example)
* Integrate virtual forest in modelling process (activate _from _FromData in `build` and integrate forest.txt in `run`)
* formal testin with test that
* use of `goodpractice`package
* integrate all methods to better manipulate TROLLsimstack (e.g. `[[`)

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
