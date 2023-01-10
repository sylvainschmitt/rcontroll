if(getRversion() < "3.3.0") {
  stop("Your version of R is too old. This package requires R-3.3.0 or newer on Windows.")
}

# For details see: https://github.com/rwinlib/gsl
VERSION <- commandArgs(TRUE)

testfile <- sprintf("../windows/gsl-%s/include/gsl/gsl_blas.h", VERSION)
if(!file.exists(testfile)){  
  if(getRversion() < "3.3.0") setInternet2()
  download.file(sprintf("https://github.com/rwinlib/gsl/archive/v%s.zip", VERSION), "lib.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}