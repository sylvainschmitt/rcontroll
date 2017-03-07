#' @importFrom maptools elide
#' @importFrom sp coordinates<- 
#' @importFrom sp coordinates
NULL

#' Create inventory
#'
#' Virtualize a forest inventory in forest text file to initialize TROLL model
#'
#' @param forest int. path to the file to create
#' @param path char. working directory
#' @param overwrite logical. allow to overwrite existing input file
#' @param species df. species trait values (can be open from external file)
#' @param missing (logical, path). missing species should be save and where ?
#' @param spcorrect logical. should species will try to be corrected
#' @param data df. censuses data (can be open from external file)
#' @param plotcol char. name of the column containing plot information
#' @param xcol char. name of the column containing longitude information
#' @param ycol char. name of the column containing latitude information
#' @param censuscol char. name of the column containing census year information
#' @param spcol char. name of the column(s) containing secies information (can be a vector if species and genus are split)
#' @param dbhcol char. name of the column containing dbh information
#' @param alivecol char. name of the column containing alive information
#' @param NV int. vertical number of cells (nb per m)
#' @param NH int. horizontal number of cells (nb per m)
#' @param plot int/char. name or number of the plot to initialize on
#' @param year int. year of the census to intialize on
#' @param alive int/char/logical. code corresponding to alive tree
#'
#' @return virtual forest inventory for the TROLL program (forest.txt)
#'
#' @export
#'
#' @examples
#'
inventoryFromData <- function(
  # file
  forest = getOption("RconTroll.forest"),
  path = getOption("RconTroll.path"),
  overwrite = FALSE,
  # species data
  species = read.table(getOption("RconTroll.species"), header=TRUE, dec=".", sep="", row.names = 1),
  missing = getOption("RconTroll.missing"),
  spcorrect = TRUE,
  # data
  data = read.csv("~/Documents/BIOGET/Projet/Data/paracou data - corrected DBH/paracou_p1_15.csv"),
  plotcol = 'n_parcelle',
  xcol = 'Xutm',
  ycol = 'Yutm',
  censuscol = 'campagne',
  spcol = c('Genre', 'espece'),
  dbhcol = 'dbh_c',
  alivecol = 'status',
  # parameters
  NV = 1,
  NH = 1,
  plots = c(1),
  year = 1984,
  alive = 1
){
  # Reducing to the plot(s) of interest
  if(!(plotcol %in% names(data)))
    stop('plotcol is not one of the data columns !')
  data <- subset(data, data[which(names(data) == plotcol)] == plots)
  if(dim(data)[1] == 0)
    stop('No data for this plot(s) !')

  # Reducing to the year of interest
  if(!(censuscol %in% names(data)))
    stop('censuscol is not one of the data columns !')
  data <- subset(data, data[which(names(data) == censuscol)] == year)
  if(dim(data)[1] == 0)
    stop('No data for this census year !')

  # Keeping alive trees only
  if(!(alivecol %in% names(data)))
    stop('alivecol is not one of the data columns !')
  data <- subset(data, data[which(names(data) == alivecol)] == alive)
  if(dim(data)[1] == 0)
    stop('No data for this alive code !')

  # Formatting data
  if(!all(c(xcol, ycol, spcol, dbhcol) %in% names(data)))
    stop('xcol, ycol, spcol and/or dbhcol are not one of the data columns !')
  data <- data[c(xcol, ycol, dbhcol, spcol)]
  if(length(spcol) == 2)
    data$species <- paste0(data[,spcol[1]], '_', data[,spcol[2]])
  data <- data[-match(spcol, names(data))]
  names(data) <- c('x', 'y', 'dbh', 'sp')

  # Species check
  spref <- as.character(unique(row.names(species)))
  spdata <- as.character(unique(data$sp))
  spmatch <- as.character(unique(c(intersect(spref, spdata), intersect(spdata, spref))))
  spmis <- list(
    ref = data.frame(
      sp = as.character(setdiff(spref, spdata)),
      gen = as.character(unlist(lapply(strsplit(setdiff(spref, spdata), '_'), '[[', 1))),
      stringsAsFactors = FALSE),
    data = data.frame(
      sp = as.character(setdiff(spdata, spref)),
      gen = as.character(unlist(lapply(strsplit(setdiff(spdata, spref), '_'), '[[', 1))),
      stringsAsFactors = FALSE)
  )

  # Species corrections
  if(spcorrect){
    n <- length(spmis$ref$gen)
    cat(n, 'species mismatch to correct :\n')
    answer <- readline(prompt = 'Do you want to coorect theim (y/n) : ')
    if(answer == 'y'){
      jdata <- c()
      for(i in 1:n){
        genmatch <- match(spmis$ref$gen[i], spmis$data$gen)
        if(is.na(genmatch)){
          cat(i, '/', n, ': No suggestion for', spmis$ref$sp[i], '\n')
        } else {
          for(j in genmatch){
            answer <- readline(prompt = paste(i, '/', n, ': Is', spmis$data$sp[j], 'corresponding to', spmis$ref$sp[i], '(y/n) : '))
            switch (answer,
                    y = {
                      data$sp[data$sp == spmis$data$sp[j]] <- spmis$ref$sp[i]
                      spdata[spdata == spmis$data$sp[j]] <- spmis$ref$sp[i]
                      spmatch <- c(spmatch, spmis$ref$sp[i])
                      iref <- c(iref, i)
                      jdata <- c(jdata, j)
                    }
            )
          }
        }
      }
      if(length(iref) > 0){
        spmis$ref <- spmis$ref[-iref,]
        spmis$data <- spmis$data[-jdata,]
      }
    }
    iref <- c()
  }

  # For now removing missing data
  spmis <- sort(spmis$data$sp)
  spmisBA <- round(sum(data$dbh[match(spmis, data$sp)], na.rm = TRUE) / sum(data$dbh) * 100)
  spmisInd <- round(length(match(spmis, data$sp)) / length(data$sp) * 100)
  spmis <- c(paste(length(spmis), 'missing species representing', spmisInd, '% of individuals and', spmisBA, '% of basal area.'), spmis)
  warning(spmis[1], ' They will be removed from the data.')
  if(!is.null(missing))
    write.table(spmis, file.path(path, missing), col.names = FALSE, row.names = FALSE, quote = FALSE)
  data <- data[-match(spmis[-1], data$sp),]

  # Creating the grid
  datasp <- data
  coordinates(datasp) <- c('x', 'y') # spatializing data
  pxmin <- data[which(data$x == min(data$x)), c('x', 'y')] # getting main points for rotation
  pymin <- data[which(data$y == min(data$y)), c('x', 'y')]
  pobj <- cbind(pxmin['x'], pymin['y'])
  x <- as.numeric(pymin - pxmin) # getting vectors
  y <- as.numeric(pobj - pxmin)
  dot.prod <- x%*%y # angle computing
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- as.numeric(acos(dot.prod / (norm.x * norm.y)))
  theta <- (theta * 180) / (pi)
  data[c('x', 'y')] <- coordinates(elide(datasp, rotate = theta)) # rotation
  data$x <- round((data$x - min(data$x)) * NH)
  data$y <- round((data$y - min(data$y)) * NV)
  data <- data[order(data$dbh, decreasing = TRUE),]
  if(length(duplicated(data[1:2])) > 0){
    warning(length(duplicated(data[1:2])), ' trees occupe the same case in the grid. The biggest dbh will be kept.')
    data <- data[-duplicated(data[1:2]),]
  }
  # plot(y ~ x, data, col = as.factor(sp), pch = 16, cex = dbh / 50) # To see the result

  # Writting table
  data <- data[order(data$x, data$y),]
  if(!overwrite)
    if(forest %in% list.files(path))
      stop('The file already exist, use overwrite = T.')
  write.table(data, file = file.path(path, forest), row.names = FALSE, quote = FALSE, sep = '\t')
}
