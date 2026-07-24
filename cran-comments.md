## Submission

Following rlas and lidR archiving from CRAN, I've made lidR a suggest as it is
not core of rcontroll but only minor modules of the code.

## Test environments 

* local, Ubuntu 20.04.5 LTS, R 3.6.3
* GitHub Actions, Ubuntu 20.04.5 LTS, R 3.6
* GitHub Actions, Ubuntu 20.04.5 LTS, R-release
* GitHub Actions, macOS latest, R-release
* R-hub builder, Windows, R-release

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

* Package suggested but not available for checking: ‘lidR’

As described in the submission, after being archive lidR has been moved as a suggest.
Only a small portion of the code rely on it and its an optional module.
Thus I added a test that will throw an error of lidR is not available and invite to install from GitHub.
I prefer to keep as it this parts of the code instead of completely removing them.

* checking CRAN incoming feasibility ... [11s] NOTE
    Uses the superseded package: 'doSNOW (>= 1.0.10)'
    
We use doSNOW to pass the progress bar to foreach as described in their package:
"the snow options are passed to foreach using the .options.snow argument". 
We currently have no alternative as explained here:
https://stackoverflow.com/questions/66604588/showing-progress-bar-with-doparallel-foreach.
