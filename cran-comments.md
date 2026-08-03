## Submission

Following rlas and lidR archiving from CRAN, I've made lidR a suggest as it is
not core of rcontroll but only minor modules of the code.

## Test environments 

* local, Ubuntu 24.04.2 LTS, R 4.4.1
* GitHub Actions, Ubuntu latest, R-release
* GitHub Actions, macOS latest, R-release

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

* Package suggested but not available for checking: ‘lidR’

As described in the submission, after being archive lidR has been moved as a suggest.
Only a small portion of the code rely on it and its an optional module.
Thus I added a test that will throw an error of lidR is not available and invite to install from GitHub.
I prefer to keep as it this parts of the code instead of completely removing them.
