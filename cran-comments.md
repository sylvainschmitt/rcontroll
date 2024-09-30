## Resubmission

This is a resubmission. In this version I have avoided downloading gsl 
at installation time following Tomas Kalibera pull request.

## Test environments 

* local, Ubuntu 20.04.5 LTS, R 3.6.3
* GitHub Actions, Ubuntu 20.04.5 LTS, R 3.6
* GitHub Actions, Ubuntu 20.04.5 LTS, R-release
* GitHub Actions, macOS latest, R-release
* R-hub builder, Windows, R-release

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... [11s] NOTE
    Uses the superseded package: 'doSNOW (>= 1.0.10)'
    
We use doSNOW to pass the progress bar to foreach as described in their package:
"the snow options are passed to foreach using the .options.snow argument". 
We currently have no alternative as explained here:
https://stackoverflow.com/questions/66604588/showing-progress-bar-with-doparallel-foreach.
