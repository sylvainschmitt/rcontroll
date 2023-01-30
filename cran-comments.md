## Test environments 

* local, Ubuntu 20.04.5 LTS, R 3.6.3
* GitHub Actions, Ubuntu 20.04.5 LTS, R 3.6
* GitHub Actions, Ubuntu 20.04.5 LTS, R-release
* R-hub builder, Windows, R-release
* win-builder

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking compiled code ... NOTE
  File ‘rcontroll/libs/rcontroll.so’:
    Found ‘rand’, possibly from ‘rand’ (C)
      Object: ‘troll_rcpp.o’
      
`gsl_rand` but not `rand` was used in the source.
