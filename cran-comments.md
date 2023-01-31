## Resubmission

This is a resubmission. In this version I have:

* fixed the compilation of C++ source code with clang for macOS and debian 
    * replaced the deprecated function `sprintf` by `snprintf` 
    * replaced the deprecated function `random_shuffle` by `shuffle` 
    * removed `bufi_soil` variable which was causing segfault
    * fixed loop indentation to remove all clang warnings

## Test environments 

* local, Ubuntu 20.04.5 LTS, R 3.6.3
* GitHub Actions, Ubuntu 20.04.5 LTS, R 3.6
* GitHub Actions, Ubuntu 20.04.5 LTS, R-release
* GitHub Actions, macOS latest, R-release
* R-hub builder, Windows, R-release

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

Found the following (possibly) invalid URLs:
  URL: https://codecov.io/gh/sylvainschmitt/rcontroll (moved to https://app.codecov.io/gh/sylvainschmitt/rcontroll)
    From: README.md
    Status: 200
    Message: OK

This is the url given by the codecov website to get a working badge in the readme, 
if you use the second url the badge does not work.
