#' Create inventory from output
#' 
#' Create an inventory from a TROLL model output
#'
#' @param x TROLLsim
#' @param forest char. name of the file to write
#' @param path char. path to the working directory
#' @param overwrite logical. allow to overwrite existing input file
#'
#' @return virtual forest file for the TROLL program (forest.txt)
#' 
#' @export
#'
#' @examples
#' NA
#' 
inventoryFromOutput <- function(x,
                                 forest = getOption("RconTroll.forest"),
                                 path = getOption("RconTroll.path"),
                                 overwrite = TRUE){
  data <- as.data.frame(x@final_pattern)[c('x', 'y', 'dbh', 'sp_lab')]
  data$species <- row.names(x@sp_par[data$sp_lab,])
  data$dbh <- data$dbh*1000
  data <- data[!(data$dbh == 0),]
  if(!overwrite)
    if(forest %in% list.files(path))
      stop('The file already exist, use overwrite = T.')
  write.table(data, file = file.path(path, forest), row.names = FALSE,
              quote = FALSE, sep = '\t')
}