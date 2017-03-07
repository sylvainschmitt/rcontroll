# TO DO
* modification of input file
* merging of info and par file in output, potentially using info-file for module activation
* finish `summary` for TROLLsimstack
* create subclasses for TROLLsimstack
* define `plot` for TROLLsimstack
* Correct `plot.output` method with use of internal functions (see disturbance example)
* Integrate virtual forest in modelling process (activate _from _FromData in `build` and integrate forest.txt in `run`)

# RconTroll 0.1.9003

* renaming objects and methods (e.g. `TROLLoutput` to `TROLLsim`, `virtualizeFromData` to `inventoryFromData`, etc.)
* definition of `summary` for class TROLLsim
* new class TROLLsimstack (with methods `print` and `summary`, the latter not yet fully implemented)

# TO DO

* plot multiple `Output` better integration (https://github.com/fischer-fjd/RconTroll/issues/1)
* summary method for `Output` (https://github.com/fischer-fjd/RconTroll/issues/2)
* Correct `plot.output` method with use of internal functions (see disturbance example)
* Integrate virtual forest in modelling process (activate _from _FromData in `build` and integrate forest.txt in `run`)
* formal testin with test that
* use of `goodpractice`package
* Renaming load -> load_output
* Renaming TROLL.output -> Output

# RconTroll 0.1.9002

* Disturbance module integration in `TROLLoutput` class, `load_output` function and `plot.TROLLoutput`method
* `virtualizeFromOutput` function
* Renaming TROLL -> RconTroll
* Travis-CI continuous integration
* Codecover integration

# RconTroll 0.1.9001

* Initialise package and git repository (GitHub)
