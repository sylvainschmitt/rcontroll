#' Virtualize from output
#' 
#' Virtualize  a forest from a TROLL model output
#'
#' @param x TROLLoutput
#' @param forest char. name of the file to write
#' @param path char. path to the working directory
#' @param overwrite logical. allow to overwrite existing input file
#'
#' @return virtual forest file for the TROLL program (forest.txt)
#' 
#' @export
#'
#' @examples
#' 
virtualizeFromOutput <- function(x,
                                 forest = getOption("RconTroll.forest"),
                                 path = getOption("RconTroll.path"),
                                 overwrite = TRUE){
  data <- as.data.frame(mature@final_pattern)[c('x', 'y', 'sp_lab', 'dbh')]
  data$sp_lab <- row.names(x@sp_par[data$sp_lab,])
  if(!overwrite)
    if(forest %in% list.files(path))
      stop('The file already exist, use overwrite = T.')
  write.table(data, file = file.path(path, forest), row.names = FALSE,
              col.names = FALSE, quote = FALSE, sep = '\t')
}