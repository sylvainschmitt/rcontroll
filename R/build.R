#' Build TROLL
#'
#' Function to build TROLL code
#'
#' @param src char. path to src file
#' @param app char. path to troll app (e.g. TROLL.out)
#' @param path char. working directory
#' @param optimisation char. optimisation level to build cpp with g++ (default NULL)
#' @param verbose logical. activate outputs in console
#' 
#' @return build the TROLL app
#'
#' @export
#'
#' @examples
#'
build <- function(
  src = getOption("RconTroll.src"),
  app = getOption("RconTroll.app"),
  path = getOption("RconTroll.path"),
  optimisation = NULL,
  verbose = TRUE
){
  output <- file.path(path, app)
  command <- paste('g++', src, '-o', output)
  if(!is.null(optimisation))
    command <- paste(command, optimisation)
  if(verbose)
    cat(command, '\n')
  system(command)
}
