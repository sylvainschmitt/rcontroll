## Test environments
* local Ubuntu 20.04 LTS, R 3.6.3
* GitHub actions Ubuntu 20.04 LTS, R 4.1.0

## R CMD check results
There were two NOTEs:
 
> checking installed package size ... NOTE
    installed size is  6.0Mb
    sub-directories of 1Mb or more:
      doc    1.2Mb
      libs   3.8Mb

The package includes the forest TROLL simulator coded in C++, which gives a compiled code of 3.8Mb. 
We tried to save as much space as possible from other parts of the package. 
This still resulted in 6Mb of installed package that we cannot reduce.

> checking compiled code ... NOTE
  File ‘rcontroll/libs/rcontroll.so’:
    Found ‘rand’, possibly from ‘rand’ (C)
      Object: ‘troll.o’

The forest TROLL simulator includes gslrand and not rand, still detected as a NOTE by automatic checks.