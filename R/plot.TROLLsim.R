#' @import methods
#' @import ggplot2
#' @importFrom graphics legend matplot
#' @importFrom stats sd time
#' @importFrom utils read.csv read.table write.table
NULL

#' Plot TROLL simulations stack
#' 
#' @param x TROLLsim or TROLLsimstack. Troll simulation stacked or not object to
#'   plot
#' @param y Any, TROLLsim or TROLLsimstack. nothing, TROLL simulation or Troll
#'   aggregated simulation stack to compare with
#' @param what char. ecosystem output to plot, see details
#' @param ... other graphical parameters
#'   
#' @return Plot the simulations

#' @details Available plots:
#' \describe{
#' \item{agb}{above ground biomass}
#' \item{gpp}{gross primary productivity}
#' \item{abund, abu10, abu30}{abundances (total, above 10 and above 30 cm dbh)}
#' \item{ba, ba10}{basal area (total and above 10 cm dbh)}
#' \item{Rday, Rnight}{night and day respiration}
#' \item{diversity, distdiversity}{species diversity for different order of diversities
#'  (final and disturbed, require entropart)}
#' \item{rankabund, distrankabund}{species rank-abundance diagram(final and disturbed)}
#' \item{height, distheight}{tree height histogram (final and disturbed)}
#' \item{dbh, distdbh}{tree dbh histogram (final and disturbed)}
#' \item{age, distage}{tree age histogram (final and disturbed)}
#' \item{trait, disttrait}{functional trait density distribution, replace trait
#' by the name of the trait you want to plot (final and disturbed)}
#' }
#' 
#' @examples
#' NA
#' 
#' @name plot.TROLLsim
#'   
NULL

#' @rdname plot.TROLLsim
#' @export
setMethod('plot', signature(x="TROLLsim", y="missing"), function(x, y, what, ...) {
  plot(x = stack(x), what = what, ...)
})

#' @rdname plot.TROLLsim
#' @export
setMethod('plot', signature(x="TROLLsim", y="TROLLsim"), function(x, y, what, ...) {
  plot(x = stack(x, y), what = what, ...)
})

#### Simstack ####
#' @rdname plot.TROLLsim
#' @export
setMethod('plot', signature(x="TROLLsimstack", y="missing"), function(x, y, what, ...) {

  #### AGB, GPP, Litterfall ####
  if(what %in% c('agb', 'gpp', 'litterfall')){
    ylab <- switch(what,
                   'agb' = 'Aboveground biomass (kgC/ha)',
                   'litterfall' = 'Leaf litterfall per month (Mg dry mass/ha/year)',
                   'gpp' = "Total GPPLeaf (MgC/ha)")
    g <- .get_graph(x, 'Total', what, ylab = ylab, ...)
  }

  #### Abund, BA, R ####
  if(what %in% c('abund', 'abu10', 'abu30', 'ba', 'ba10', 'Rday', 'Rnight')){
    slot <- switch(what,
                   'abund' = 'abundances', 
                   'abu10' = 'abundances', 
                   'abu30' = 'abundances', 
                   'ba' = 'ba', 
                   'ba10' = 'ba', 
                   'Rday' = 'R', 
                   'Rnight' = 'R')
    ylab <- switch(what,
                   'abund' = "Total abundance (stems/ha)", 
                   'abu10' = "Number of trees with dbh > 10 cm (stems/ha)", 
                   'abu30' = "Number of trees with dbh > 30 cm (stems/ha)", 
                   'ba' = "Total basal area (m2/ha)", 
                   'ba10' = "Basal area of trees with dbh > 10 cm (stems/ha)", 
                   'Rday' = 'Total day respiration (MgC/ha)', 
                   'Rnight' = 'Total night respiration (MgC/ha)')
    g <- .get_graph(x, 'Total', slot, what, ylab = ylab, ...)
  }
  
  #### Species ####
  if(what %in% c('diversity', 'distdiversity', 'rankabund', 'distrankabund')){
    slot <- switch(what,
                   'diversity' = 'final_pattern',
                   'rankabund' = 'final_pattern',
                   'distdiversity' = 'disturbance',
                   'distrankabund' = 'disturbance')
    xlab <- switch(what,
                   'diversity' = 'Order of diversity',
                   'distdiversity' = 'Order of diversity',
                   'rankabund' = 'Rank',
                   'distrankabund' = 'Rank')
    ylab <- switch(what,
                   'diversity' = 'Diversity',
                   'distdiversity' = 'Diversity',
                   'rankabund' = 'log of abundance',
                   'distrankabund' = 'log of abundance')
    g <- .get_graph(x, what, slot, xlab = xlab, ylab = ylab, ...)
  }
  
  #### Height, dbh, age ####
  if(what %in% c('height', 'distheight', 'dbh', 'distdbh', 'age', 'distage')){
    slot <- switch(what,
                   'height' = 'final_pattern',
                   'dbh' = 'final_pattern',
                   'age' = 'final_pattern',
                   'species' = 'final_pattern',
                   'distheight' = 'disturbance',
                   'distdbh' = 'disturbance',
                   'distage' = 'disturbance',
                   'distspecies' = 'disturbance')
    col <- switch(what,
                  'height' = 'height',
                  'distheight' = 'height',
                  'dbh' = 'dbh',
                  'distdbh' = 'dbh',
                  'age' = 'age',
                  'distage' = 'age',
                  'species' = 'sp_lab',
                  'distspecies' = 'sp_lab')
    xlab <- switch(what,
                   'height' = 'height (m)',
                   'distheight' = 'height (m)',
                   'dbh' = 'diameter at breast height (m)',
                   'distdbh' = 'diameter at breast height (m)',
                   'age' = 'age (years)',
                   'distage' = 'age (years)',
                   'species' = 'species rank',
                   'distspecies' = 'species rank')
    ylab <- switch(what,
                   'height' = 'log10 stem number',
                   'distheight' = 'log10 stem number',
                   'dbh' = 'stem > 1 number',
                   'distdbh' = 'stem number',
                   'age' = 'log10 stem number',
                   'distage' = 'log10 stem number',
                   'species' = 'stem number',
                   'distspecies' = 'stem number')
    xmin <- switch(what,
                   'dbh' = 0.01,
                   'distdbh' = 0.01,
                   NA)
    ytrans <- switch(what,
                   'height' = 'log10',
                   'distheight' = 'log10',
                   'dbh' = 'identity',
                   'distdbh' = 'identity',
                   'age' = 'log10',
                   'distage' = 'log10',
                   'species' = 'identity',
                   'distspecies' = 'identity')
    g <- .get_hist(x, col, slot, xlab, ylab, xmin, ytrans, ...)
  }
  
  #### Trait ####
  if(what %in% c(names(x@layers[[1]]@sp_par))){
    g <- .get_density(x,what, 'final_pattern', ...)
  }
  if(what %in% paste0('dist',names(x@layers[[1]]@sp_par))){
    col <- unlist(strsplit(what, 'dist'))[2]
    g <- .get_density(x, col, 'disturbance', ...)
  }
  
  return(g)
})

#### Simstack, Simstack ####
#' @rdname plot.TROLLsim
#' @export
setMethod('plot', signature(x="TROLLsimstack", y="TROLLsimstack"), function(x, y, what, ...) {

  if(!y@aggregated)
    stop('You need to use aggregated data in second TROLL simulation stack !')
    
  #### AGB, GPP, Litterfall ####
  if(what %in% c('agb', 'gpp', 'litterfall')){
    g <- plot(x, what = what, ...)
    g <- .get_graph_control(g, y, 'Total', what)
  }
  
  #### Abund, BA, R ####
  if(what %in% c('abund', 'abu10', 'abu30', 'ba', 'ba10', 'Rday', 'Rngiht')){
    slot <- switch(what,
                   'abund' = 'abundances', 
                   'abu10' = 'abundances', 
                   'abu30' = 'abundances', 
                   'ba' = 'ba', 
                   'ba10' = 'ba', 
                   'Rday' = 'R', 
                   'Rngiht' = 'R')
    g <- plot(x, what = what, ...)
    g <- .get_graph_control(g, y, 'Total', slot, what)
  }
  
  #### Species ####
  if(what %in% c('diversity', 'distdiversity', 'rankabund', 'distrankabund')){
    slot <- switch(what,
                   'diversity' = 'final_pattern',
                   'rankabund' = 'final_pattern',
                   'distdiversity' = 'disturbance',
                   'distrankabund' = 'disturbance')
    g <-  plot(x, what = what, ...)
    g <- .get_graph_control(g, y, what, slot)
  }
  
  #### Height, dbh, age ####
  if(what %in% c('height', 'distheight', 'dbh', 'distdbh', 'age', 'distage')){
    stop('Comparisons of two simulations stack histograms is impossible !')
  }
  
  #### Trait ####
  if(what %in% c(names(x@layers[[1]]@sp_par), 
                 paste0('dist',names(x@layers[[1]]@sp_par)))){
    stop('Comparisons of two simulations stack density plot is impossible !')
  }
  
  return(g)
})

#### Internals ####

#### Data ####

.data_basic <- function(x, col, slot, list = NULL){
  if(is.null(list))
  data <- sapply(x@layers, function(y){
    slot(y, slot)[,col]
  })
  if(!is.null(list))
  data <- sapply(x@layers, function(y){
    slot(y, slot)[[list]][,col]
  })
  data <- data.frame(data)
  row.names(data) <- seq(1,x@layers[[1]]@par$general$nbiter,1)/x@layers[[1]]@par$general$iter
  return(data)
}

.data_species <- function(x, col, slot){
  Abd_list <- lapply(x@layers, function(y){
    trees <- row.names(y@sp_par)[slot(y, slot)$sp_lab]
    Abd <- as.vector(table(trees))
    names(Abd) <- names(table(trees))
    return(Abd)
  })
  Ps_list <- lapply(Abd_list, function(y){
    y / sum(y)
  })
  if(col %in% c('diversity', 'distdiversity')){
    if(!requireNamespace("entropart", quietly = TRUE))
      stop('You need to install entropart package to use plotDiversity methods !')
    x <- seq(0, 2, 0.1)
    data <- data.frame(row.names = x,
                       do.call('cbind',
                               lapply(Ps_list, function(y){
                                 CommunityProfile(
                                   entropart::Diversity, y, x)$y
                               })
                       )
    )
  }
  if(col %in% c('rankabund', 'distrankabund')){
    logAbd_list <- lapply(Abd_list, function(y){
      log(sort(y, decreasing = T))
    })   
    data <- data.frame(do.call('cbind', logAbd_list))
    row.names(data) <- seq_len(dim(data)[1])
  }
  return(data)
}

.data_ggplot <- function(x, col, slot, list = NULL){
  if(col %in% c('diversity', 'distdiversity', 'rankabund', 'distrankabund')){
    data <- .data_species(x, col, slot)
  } else {
    data <- .data_basic(x, col, slot, list)
  }
  n <- dim(data)[1]
  data <- data.frame(
    x = as.numeric(rep(row.names(data), length(names(data)))),
    y = unname(unlist(data)),
    layer = rep(names(data), each = n)
  ) 
  return(data)
}

.data_basic_final_pattern <- function(x, col, slot){
  data <- sapply(x@layers, function(y){
    slot(y, slot)[[col]]
  })
  data <- data.frame(data)
  return(data)
}

.data_basic_disturbance <- function(x, col, slot){
  data_list <- sapply(x@layers, function(y){
    slot(y, slot)[[col]]
  })
  if(is.list(data_list)){
    data <- data.frame(matrix(ncol = length(names(data_list)),
                              nrow = max(unlist(lapply(data_list, length)))))
    names(data) <- names(data_list)
    data <- sapply(names(data), function(name){
      data[seq_len(length(data_list[[name]])),name] <- data_list[[name]]
      return(data[,name])
    })
    data[is.na(data)] <- 0
    data <- data.frame(data)
  } else {
    data <- data.frame(data_list)
  }
  return(data)
}

.data_hist_ggplot <- function(x, col, slot){
  if(slot == 'final_pattern')
    data <- .data_basic_final_pattern(x, col, slot)
  if(slot == 'disturbance')
    data <- .data_basic_disturbance(x, col, slot)
  n <- dim(data)[1]
  data <- data.frame(
    values = unname(unlist(data)),
    layer = rep(names(data), each = n)
  ) 
  return(data)
}

.data_trait <- function(x, col, slot){
  data_list <- lapply(x@layers, function(y){
    y@sp_par[slot(y,slot)$sp_lab,col]
  })
  data <- data.frame(matrix(ncol = length(names(data_list)),
                            nrow = max(unlist(lapply(data_list, length)))))
  names(data) <- names(data_list)
  data <- sapply(names(data), function(name){
    data[seq_len(length(data_list[[name]])),name] <- data_list[[name]]
    return(data[,name])
  })
  data <- data.frame(data)
  return(data)
}

.data_trait_ggplot <- function(x, col, slot){
  data <- .data_trait(x, col, slot)
  n <- dim(data)[1]
  data <- data.frame(
    y = unname(unlist(data)),
    layer = rep(names(data), each = n)
  ) 
  return(data)
}

#### Graphs ####

.get_graph <- function(x, col, slot, list = NULL, xlab = 'Time (year)', ylab, ...){
  data <- .data_ggplot(x, col, slot, list)
  g <- ggplot(data, ggplot2::aes_string(x = 'x', y = 'y', colour = 'layer')) +
    geom_point(size=0.5) +
    geom_line() +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw()
  return(g)
}

.get_graph_control <- function(g, y, col, slot, list = NULL){
  if(col %in% c('diversity', 'distdiversity', 'rankabund', 'distrankabund')){
    data <- .data_species(y, col, slot)
  } else {
    data <- .data_basic(y, col, slot, list)
  }
  data <- cbind(g$data, data)
  c <- ggplot(data, ggplot2::aes_string(x = 'x', y = 'y', 
                                                 colour = 'layer')) +
    geom_linerange(ggplot2::aes(ymin = min, ymax = max), 
                            colour = 'grey') +
    geom_line() +
    geom_line(ggplot2::aes_string(x = 'x', y = 'mean'), colour = 'black') +
    xlab(g$labels$x) +
    ylab(g$labels$y) +
    theme_bw()
  return(c)
}

#### Hist ####

.get_hist <- function(x, col, slot, xlab, ylab, xmin, ytrans, ...){
  data <- .data_hist_ggplot(x, col, slot)
  ggplot(data, ggplot2::aes_string(x = 'values', 
                                                 colour = 'layer')) +
    geom_histogram() +
    scale_x_continuous(limits = c(xmin,NA)) +
    scale_y_continuous(trans = ytrans) +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw()
}

#### Density ####

.get_density <- function(x, col, slot, ...){
  data <- .data_trait_ggplot(x, col, slot)
  ggplot(data, ggplot2::aes_string('y', colour = 'layer', fill = 'layer')) +
    geom_density(alpha = 0.1) +
    xlab(col) +
    theme_bw()
}
