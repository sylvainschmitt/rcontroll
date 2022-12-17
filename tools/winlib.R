version <- commandArgs(TRUE)

# Download GSL Rtools build
if (!file.exists(sprintf("../windows/gsl-%s/include/gsl/gsl_blas.h", version))){
  download.file(sprintf("https://github.com/rwinlib/gsl/archive/v%s.zip", version), "lib.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}