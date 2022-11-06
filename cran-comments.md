## Test environments 

* local Ubuntu 20.04.5 LTS, R 3.6.3
* GitHub Actions Ubuntu 20.04.5 LTS, R-release
* GitHub Actions Microsoft Windows Server 2022 10.0.20348, R-release

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 2 NOTEs:

* checking installed package size ... NOTE
    installed size is  6.2Mb
    sub-directories of 1Mb or more:
      doc    1.2Mb
      libs   3.9Mb

The rcontroll package includes the TROLL forest simulator in C++ with a compiled image of 3.9Mb. 
Therefore, we cannot have a size smaller than 5 MB, but we tried to minimize it and rcontroll is only 6.2 MB.

* checking compiled code ... NOTE
  File ‘rcontroll/libs/rcontroll.so’:
    Found ‘rand’, possibly from ‘rand’ (C)
      Object: ‘main_v3.1.3_rcpp.o’
      
gsl_rand but not rand was used in the source.
