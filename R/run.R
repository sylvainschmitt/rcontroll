#' Run TROLL
#'
#' Function to run TROLL application
#'
#' @param name char. name of the model
#' @param path char. working directory
#' @param app char. path to troll app (e.g. TROLL.out)
#' @param input char. input file
#' @param overwrite logical. allow to overwrite existing outputs files
#' @param verbose logical. allow output in console
#'
#' @return model output files in the path folder
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' getTROLL()
#' init(path = system.file("tools", package = 'RconTROLL'),
#'      input = "init.txt")
#' run(path = system.file("tools", package = 'RconTROLL'),
#'     name = "test", app = getOption("RconTroll.app"),
#'     input = system.file("tools", "init.txt", package = 'RconTROLL'))
#' }
#' 
run <- function(path,
                name = getOption("RconTROLL.name"),
                app = getOption("RconTroll.app"),
                input = getOption("RconTroll.init"),
                overwrite = TRUE,
                verbose = TRUE){

  if(name %in% list.dirs(path, full.names = FALSE)[-1]){
    if(!overwrite)
      stop('Outputs already exist, use overwrite = T.')
    path_o <- file.path(path, name)
    unlink(path_o, recursive = TRUE)
  } else {
    path_o <- file.path(path, name)
  }
  dir.create(path_o)

  output <- file.path(path, name, name)
  command <- paste0(app,
                    ' -i', input, 
                    ' -o', output)
  if(verbose)
    cat(command, '\n')
  system(command)
}
