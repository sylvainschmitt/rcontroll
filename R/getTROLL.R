#' get TROLL
#'
#' Locally copy TROLL C++ code into your package installation and eventually
#' complie it into an executable (linux/UNIX only).
#'
#' @param source char. link to the TROLL C++ source code online (default: GitHub
#'   TROLL-code official repository)
#' @param destination char. path to the TROLL C++ source code locally (default:
#'   package tools folder)
#' @param app char. (default: package tools folder)
#'
#' @return copy TROLL C++ code and compile it into your tools folder
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' getTROLL(source = 
#' "https://raw.githubusercontent.com/TROLL-code/TROLL/master/main_v2.3.2.cpp", 
#' destination = getOption("RconTROLL.src"), app = getOption("RconTroll.app"))
#' }
#' 
getTROLL <- function(source = "https://raw.githubusercontent.com/TROLL-code/TROLL/master/main_v2.3.2.cpp",
                     destination = getOption("RconTROLL.src"),
                     app = getOption("RconTroll.app")){
  
  # Download
  message("TROLL C++ source downloaded from", source, "to", destination)
  invisible(unlink(destination))
  writeLines(readLines(source), con = destination)
  
  # Compilation
  message("TROLL C++ source code compilation into", app, '\n')
  if (Sys.info()[['sysname']] == 'Windows')
    warning("TROLL automatic compilation is not availbale under windows os yet.")
  else {
    command <- paste0("g++ -Ofast -o ", app, " ", destination)
    system(command = command)
  }
}
