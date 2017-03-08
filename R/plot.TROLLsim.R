#' @import methods
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
#' @param ggplot2 logical. creates ggplot graph
#' @param plotly logical. use plotly library for interactive plots with ggplot
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
#' \item{height, distheight}{tree height histogram (final and disturbed)}
#' \item{dbh, distdbh}{tree dbh histogram (final and disturbed)}
#' \item{age, distage}{tree age histogram (final and disturbed)}
#' \item{species, distspecies}{tree species histogram (final and disturbed)}
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
setMethod('plot', signature(x="TROLLsim", y="missing"), function(x, y, what, ggplot2 = FALSE, plotly = FALSE, ...) {
  plot(x = stack(x), what = what, ggplot2 = ggplot2, plotly = plotly, ...)
})

#' @rdname plot.TROLLsim
#' @export
setMethod('plot', signature(x="TROLLsim", y="TROLLsim"), function(x, y, what, ggplot2 = FALSE, plotly = FALSE, ...) {
  plot(x = stack(x, y), what = what, ggplot2 = ggplot2, plotly = plotly, ...)
})

#### Simstack ####
#' @rdname plot.TROLLsim
#' @export
setMethod('plot', signature(x="TROLLsimstack", y="missing"), function(x, y, what, ggplot2 = FALSE, plotly = FALSE, ...) {
  ggplot2 <- .library_plots(ggplot2, plotly)
  
  #### AGB, GPP, Litterfall ####
  if(what %in% c('agb', 'gpp', 'litterfall')){
    ylab <- switch(what,
                   'agb' = 'Aboveground biomass (kgC/ha)',
                   'litterfall' = 'Leaf litterfall per month (Mg dry mass/ha/year)',
                   'gpp' = "Total GPPLeaf (MgC/ha)")
    g <- .get_graph(x, ggplot2, 'Total', what, ylab = ylab, ...)
  }

  #### Abundances, BA, R ####
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
    g <- .get_graph(x, ggplot2, 'Total', slot, what, ylab = ylab, ...)
  }
  
  #### Height, dbh, age, species ####
  if(what %in% c('height', 'distheight', 'dbh', 'distdbh', 'age', 'distage', 'species', 'distspecies')){
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
                   'distdbh' = 0,
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
    g <- .get_hist(x, ggplot2, col, slot, xlab, ylab, xmin, ytrans, ...)
  }
  
  if(plotly)
    return(ggplotly(g))
  if(ggplot2)
    return(g)
})

#### Simstack, Simstack ####
#' @rdname plot.TROLLsim
#' @export
setMethod('plot', signature(x="TROLLsimstack", y="TROLLsimstack"), function(x, y, what, ggplot2 = FALSE, plotly = FALSE, ...) {
  ggplot2 <- .library_plots(ggplot2, plotly)
  if(!y@aggregated)
    stop('You need to use aggregated data in second TROLL simulation stack !')
    
  #### AGB, GPP, Litterfall ####
  if(what %in% c('agb', 'gpp', 'litterfall')){
    g <- plot(x, what = what, ggplot2 = ggplot2, plotly = plotly, ...)
    if(!ggplot2)
      stop('No method available to compare TROLLsimstack without ggplot2 yet !')
    if(plotly)
      stop('Plotly not compatible with methods to compare TROLLsimstack !')
    g <- .get_control(g, y, 'Total', what)
  }
  
  #### Abundances, BA, R ####
  if(what %in% c('abund', 'abu10', 'abu30', 'ba', 'ba10', 'Rday', 'Rngiht')){
    slot <- switch(what,
                   'abund' = 'abundances', 
                   'abu10' = 'abundances', 
                   'abu30' = 'abundances', 
                   'ba' = 'ba', 
                   'ba10' = 'ba', 
                   'Rday' = 'R', 
                   'Rngiht' = 'R')
    g <- plot(x, what = what, ggplot2 = ggplot2, plotly = plotly, ...)
    if(!ggplot2)
      stop('No method available to compare TROLLsimstack without ggplot2 yet !')
    if(plotly)
      stop('Plotly not compatible with methods to compare TROLLsimstack !')
    g <- .get_control(g, y, 'Total', slot, what)
  }
  
  #### Height, dbh, age, species ####
  if(what %in% c('height', 'distheight', 'dbh', 'distdbh', 'age', 'distage', 'species', 'distspecies')){
    stop('Comparisons of two simulations stack histograms is imposible !')
  }
  
  if(ggplot2)
    return(g)
})

#### Internals ####

.library_plots <- function(ggplot2, plotly){
  if(plotly)
    ggplot2 <- TRUE
  if(plotly && !requireNamespace("plotly", quietly = TRUE))
    stop('You need to install plotly package to use plotly option !')
  if(ggplot2 && !requireNamespace("ggplot2", quietly = TRUE))
    stop('You need to install ggplot2 package to use ggplot2 option !')
  return(ggplot2)
}

.basic_data <- function(x, col, slot, list = NULL){
  if(is.null(list))
    data <- sapply(x@layers, function(y){
      slot(y, slot)[,col]
    })
  if(!is.null(list))
    data <- sapply(x@layers, function(y){
      slot(y, slot)[[list]][,col]
    })
  data <- data.frame(data)
  return(data)
}

.basic_graph <- function(x, col, slot, list = NULL, xlab = 'Time (year)', ylab, ...){
  data <- .basic_data(x, 'Total', 'agb')
  matplot(data, xlab = xlab, ylab = ylab, ...)
  legend('bottomright', names(data), fill = 1:6)
}

.ggplot_table <- function(x, col, slot, list = NULL){
  data <- .basic_data(x, col, slot, list)
  n <- dim(data)[1]
  data <- data.frame(
    time = rep(seq(1,x@layers[[1]]@par$general$nbiter,1)/x@layers[[1]]@par$general$iter, length(names(data))),
    values = unname(unlist(data)),
    layer = rep(names(data), each = n)
    ) 
  return(data)
}

.ggplot_graph <- function(x, col, slot, list = NULL, xlab = 'Time (year)', ylab, ...){
  data <- .ggplot_table(x, col, slot, list)
  g <- ggplot(data, aes(x = time, y = values, colour = layer)) +
    geom_point(size=0.5) +
    geom_line() +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw()
  return(g)
}

.get_graph <- function(x, ggplot2 = FALSE, col, slot, list = NULL, xlab = 'Time (year)', ylab, ...){
  if(!ggplot2)
    .basic_graph(x, col, slot, list, xlab , ylab, ...)
  if(ggplot2)
    .ggplot_graph(x, col, slot, list, xlab, ylab, ...)
}

.get_control <- function(g, y, col, slot, list = NULL){
  data <- .basic_data(y, col, slot, list)
  time <- seq(1,y@layers[[1]]@par$general$nbiter,1)/y@layers[[1]]@par$general$iter
  data <- cbind(time, data)
  data$control <- 1
  data <- cbind(g$data, data)
  c <- ggplot(data, aes(x = time, y = values, colour = layer)) +
    geom_linerange(aes(ymin = min, ymax = max), colour = 'grey') +
    geom_line() +
    geom_line(aes(x = time, y = mean), colour = 'black') +
    xlab(g$labels$x) +
    ylab(g$labels$y) +
    theme_bw()
  return(c)
}

.basic_data_final_pattern <- function(x, col, slot){
  data <- sapply(x@layers, function(y){
    slot(y, slot)[[col]]
  })
  data <- data.frame(data)
  return(data)
}

.basic_data_disturbance <- function(x, col, slot){
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

.basic_hist <- function(x, col, slot, xlab = 'Time (year)', ylab, xmin, ytrans, ...){
  stop('Histogram not available yet without ggplot2 option !')
}

.ggplot_table_hist <- function(x, col, slot){
  if(slot == 'final_pattern')
    data <- .basic_data_final_pattern(x, col, slot)
  if(slot == 'disturbance')
    data <- .basic_data_disturbance(x, col, slot)
  n <- dim(data)[1]
  data <- data.frame(
    values = unname(unlist(data)),
    layer = rep(names(data), each = n)
  ) 
  return(data)
}

.ggplot_hist <- function(x, col, slot, xlab, ylab, xmin, ytrans, ...){
  data <- .ggplot_table_hist(x, col, slot)
  g <- ggplot(data, aes(x = values, colour = layer)) +
    geom_histogram() +
    scale_x_continuous(limits = c(xmin,NA)) +
    scale_y_continuous(trans = ytrans) +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw()
  return(g)
}

.get_hist <- function(x, ggplot2 = FALSE, col, slot, xlab, ylab, xmin, ytrans, ...){
  if(!ggplot2)
    .basic_hist(x, col, slot, xlab , ylab, xmin, ytrans, ...)
  if(ggplot2)
    .ggplot_hist(x, col, slot, xlab, ylab, xmin, ytrans, ...)
}
