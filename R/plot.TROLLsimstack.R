#' @import methods
NULL

#' Plot TROLL simulations stack
#' 
#' @param x TROLLsimstack. Troll simulation stack object to plot
#' @param y TROLLsimstack. nothing or Troll aggregated simulation stack to
#'   compare with
#' @param what char. ecosystem output to plot, see details
#' @param ggplot2 logical. creates ggplot graph
#' @param plotly logical. use plotly library for interactive plots with ggplot
#' @param ... other graphical parameters
#'   
#' @return Plot the simulations

#' @details Available plots:
#' \describe{
#' \item{agb}{above ground biomass}
#' }
#' 
#' @examples
#' 
#' @name plot.TROLLsimstack
#'   
NULL

#' @rdname plot.TROLLsimstack
#' @export
setMethod('plot', signature(x="TROLLsimstack", y="missing"), function(x, y, what, ggplot2 = FALSE, plotly = FALSE, ...) {
  ggplot2 <- .library_plots(ggplot2, plotly)
  
  if(what %in% c('agb', 'gpp', 'litterfall')){
    ylab <- switch(what,
                   'agb' = 'Aboveground biomass (kgC/ha)',
                   'litterfall' = 'Leaf litterfall per month (in Mg dry mass/ha/year)',
                   'gpp' = "Total GPPLeaf (in MgC/ha)"
                   
    )
    g <- .get_graph(x, ggplot2, 'Total', what, ylab = ylab, ...)
  }

  
  if(plotly)
    return(ggplotly(g))
  if(ggplot2)
    return(g)
})

#' @rdname plot.TROLLsimstack
#' @export
setMethod('plot', signature(x="TROLLsimstack", y="TROLLsimstack"), function(x, y, what, ggplot2 = FALSE, plotly = FALSE, ...) {
  ggplot2 <- .library_plots(ggplot2, plotly)
  if(!y@aggregated)
    stop('You need to use aggregated data in second TROLL simulation stack !')

    
  if(what %in% c('agb', 'gpp', 'litterfall')){
    g <- plot(x, what = what, ggplot2 = ggplot2, plotly = plotly, ...)
    g <- .get_control(g, y, 'Total', what)
  }
  
  if(plotly)
    return(plotly::ggplotly(g))
  if(ggplot2)
    return(g)
})

#### Internals ####

.library_plots <- function(ggplot2, plotly){
  if(plotly)
    ggplot2 <- TRUE
  if(plotly && !require(plotly))
    stop('You need to install plotly package to use plotly option !')
  if(ggplot2 && !require(ggplot2))
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
  data <- .basic_data(y, col, slot)
  time <- seq(1,y@layers[[1]]@par$general$nbiter,1)/y@layers[[1]]@par$general$iter
  data <- cbind(time, data)
  data$control <- 1
  data <- cbind(g$data, data)
  c <- ggplot(data, aes(x = time, y = values, colour = layer)) +
    geom_linerange(aes(ymin = min, ymax = max), colour = 'grey') +
    # geom_point(size=0.2) +
    geom_line() +
    geom_line(aes(x = time, y = mean), colour = 'black') +
    xlab(g$labels$x) +
    ylab(g$labels$y) +
    theme_bw()
  return(c)
}
