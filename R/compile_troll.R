#' Compile troll
#'
#' @param full bool. TROLL with full outputs, if not reduced outputs (default
#'   TRUE)
#' @param abc bool. TROLL with abc outputs, forcing reduced outputs (default
#'   FALSE)
#' @param forest bool. TROLL with forest input, if not start from an empty grid
#'   (default FALSE)
#' @param random bool. TROLL with random outputs, if not the seed is fixed
#'   (default TRUE)
#'
#' @return simulation outputs in the path folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' compile_troll()
#' }
#' 
compile_troll <- function(
                  full = TRUE,
                  abc = FALSE,
                  forest = FALSE,
                  random = TRUE
) {
  # test
  # full = TRUE
  # abc = FALSE
  # forest = FALSE
  # random = TRUE
  # overwrite = TRUE
  
  # check all inputs
  if(!all(unlist(lapply(list(full, random, forest, abc), class)) == "logical"))
    stop("full, random, forest, and abc should be logical.")
  
  # get cpp
  cpp <- system.file("troll", "main.cpp", package = "rcontroll", mustWork = TRUE)
  troll <- readLines(cpp)
  
  # cpp2
  cpp2 <- file.path(
    system.file("troll", package = "rcontroll", mustWork = TRUE),
    paste0(
      "TROLL_",
      ifelse(full, "full", "reduced"), "_",
      ifelse(abc, "abc", "nonabc"), "_",
      ifelse(forest, "forest", "nonforest"), "_",
      ifelse(random, "random", "nonrandom"),
      ".cpp"
    )
  )  
  
  # full
  troll[100] <- ifelse(full, 
                       "bool _OUTPUT_reduced=0;//!< User control: reduced set of output files ",
                       "bool _OUTPUT_reduced=1;//!< User control: reduced set of output files "
                       )
  
  # abc
  troll[46] <- ifelse(abc, 
                      "#define Output_ABC       //!< new in v.2.4.1, refined in v.2.5: PARAMETERIZATION/OUTPUT TOOL, inclusion of ABC routines for comprehensive parameter inference with TROLL ",
                      "#undef Output_ABC       //!< new in v.2.4.1, refined in v.2.5: PARAMETERIZATION/OUTPUT TOOL, inclusion of ABC routines for comprehensive parameter inference with TROLL "
  )
  
  # forest
  troll[101] <- ifelse(forest, 
                       "bool _FromData=1;      //!< User control: if defined, an additional input file can be provided to start simulations from an existing data set or a simulated data set (5 parameters are needed: x and y coordinates, dbh, species_label, species ",
                       "bool _FromData=0;      //!< User control: if defined, an additional input file can be provided to start simulations from an existing data set or a simulated data set (5 parameters are needed: x and y coordinates, dbh, species_label, species "
  )
  
  # random
  troll[93] <- ifelse(random, 
                      "bool _NONRANDOM=1;     //!< User control: If _NONRANDOM == 1, the seeds for the random number generators will be kept fixed at 1, for bug fixing ",
                      "bool _NONRANDOM=0;     //!< User control: If _NONRANDOM == 1, the seeds for the random number generators will be kept fixed at 1, for bug fixing "
  )
  
  # write cpp
  writeLines(troll, con = cpp2)
  
  # exe
  os <- .Platform$OS.type
  out <- file.path(
    system.file("troll", os, package = "rcontroll", mustWork = TRUE),
    paste0(
      "TROLL_",
      ifelse(full, "full", "reduced"), "_",
      ifelse(abc, "abc", "nonabc"), "_",
      ifelse(forest, "forest", "nonforest"), "_",
      ifelse(random, "random", "nonrandom"),
      switch(os, 
             "unix" = ".out", 
             "windows" = ".exe -I C:\\rtools40\\mingw64\\include") # Need to use Rtools40 and install gsl library through Rtools bash shell (see : https://github.com/r-windows/rtools-packages)
    )
  )  
  
  # compiler
  
  
  # compile
  command <- paste0(
    switch(os, 
           "unix" = "g++ ", 
           "windows" = "C:\\rtools40\\mingw64\\bin\\g++.exe "), #TODO : Adapt path to architecture on OS == windows
    cpp2,
    " -O3 -o ",
    out,
    " -lgsl -lgslcblas -Wall"
  )
  message(command)
  system(command)
  
  # remove cpp
  unlink(cpp2)
}
