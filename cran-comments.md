## Resubmission

This is a resubmission. In this version I have:

* fixed the compilation of C++ source code with clang for macOS and debian 
    * replaced the deprecated function `random_shuffle` by `shuffle` 
    * removed `bufi_soil` variable which was causing segfault

## Test environments 

* local, Ubuntu 20.04.5 LTS, R 3.6.3
* GitHub Actions, Ubuntu 20.04.5 LTS, R 3.6
* GitHub Actions, Ubuntu 20.04.5 LTS, R-release
* GitHub Actions, macOS latest, R-release
* R-hub builder, Windows, R-release

## R CMD check results

There were no ERRORs or WARNINGs. 

 * checking compilation flags used ... NOTE
Compilation used the following non-portable flag(s):
  ‘-Wdeprecated-declarations’
      
`-Wdeprecated-declarations` flags was added to pass check with macOS on GitHub Actions 
because the source code is using the deprecated `sprintf` function.
