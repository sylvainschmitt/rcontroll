#' @import methods
#' @importFrom graphics image
NULL

#' Function to plot TROLL outputs.
#'
#' @param x TROLLsim
#' @param y char. Output to plot, see details.
#'
#' @return Plot the output
#'
#' @details Available plots:
#' \describe{
#' \item{defaults}{abund + agb + ba +height hist}
#' \item{abund}{
#'  \describe{
#'  \item{abund}{Number of stems/ha ~ Time (in years)}
#'  \item{abu10}{Number of trees with dbh > 10 cm (in stems/ha) ~ Time (in years)}
#'  \item{abu30}{Number of trees with dbh > 30 cm (in stems/ha) ~ Time (in years)}
#'  \item{allabund}{abund + abu10 + abu30}
#'  }
#' }
#' \item{agb}{Aboveground biomass (in tonnes/ha) ~ Time (in years)}
#' \item{ba}{
#'  \describe{
#'  \item{ba}{Basal area (in m2/ha) ~ Time (in years)}
#'  \item{ba10}{Basal area of trees with dbh > 10 cm (in stems/ha) ~ Time (in years)}
#'  \item{allba}{ba + ba10}
#'  }
#' }
#' \item{death}{Not implemented}
#' \item{disturbance}{
#'  \describe{
#'  \item{dist height}{disturbed tree height histogram}
#'  \item{dist dbh}{disturbed dbh histogram}
#'  \item{dist rank-abundance}{disturbed species rank-abundance histogram}
#'  }
#' }
#' \item{final pattern}{
#'  \describe{
#'  \item{heigh}{final tree height histogram}
#'  \item{dbh}{final dbh histogram}
#'  \item{rank-abundance}{final species rank-abundance histogram}
#'  \item{age}{final tree age distribution}
#'  \item{species}{final species spatial distribution map}
#'  }
#' }
#' \item{flux}{GPP, Autotrphic respiration, NPP ~ Time (in years)}
#' \item{gpp}{Total GPPLeaf (in MgC/ha) ~ Time (in years)}
#' \item{LAI}{Not implemented}
#' \item{litterfall}{Leaf litterfall per month (in Mg/ha/year dry mass) ~ Time (in years)}
#' \item{npp}{Total NPPLeaf (in MgC/ha) ~ Time (in years)}
#' \item{R}{Not implemented}
#' \item{Ra}{Autotrophic respiration (in MgC/ha) ~ Time (in years)}
#' \item{relabund}{Not implemented}
#' \item{vertd}{PAD (m2 per m3) ~ Height (in m)}
#' \item{wd}{
#'  \describe{
#'  \item{wd}{Total wood density (in g/cm3) ~ Time (in years)}
#'  \item{wd10}{Wood density of trees with dbh > 10 cm (in g/cm3) ~ Time (in years)}
#'  \item{wd30}{Wood density of trees with dbh > 30 cm (in g/cm3) ~ Time (in years)}
#'  \item{allwd}{wd + wd10 + wd30}
#'  }
#' }
#' }
#'
#' @export
#'
#' @examples
#'
#' @name plot.TROLLsim
#' 
setMethod('plot', 'TROLLsim', function(x, y = NULL, ...) {
  
  ##### data ####
  nbiter <- x@par$general$nbiter
  iter <- x@par$general$iter
  distiter <- x@par$general$disturb_iter
  age <- round(nbiter / iter, 1) # in years
  distage <- round(distiter / iter, 1)
  surf <- prod(x@info$step * x@info$SitesNb) / 10000 # in ha
  
  if(missing(y)) {
    y <- 'default'
  }
  switch (y,
          
          ##### default ####
          'default' = {
            par(mfrow=c(2,2))
            plot(x, 'abund')
            plot(x, 'agb')
            plot(x, 'ba')
            plot(x, 'height')
            par(mfrow=c(1,1))
          },
          
          ##### abund ####
          'abund' = {
            plot(seq(1,nbiter,1)/iter,
                 x@abundances$abund$Total, pch=20,
                 main="Total abundance (in stems/ha)",
                 xlab="Times (in years)",
                 ylab="Number of stems/ha",
                 xlim=c(0,nbiter/iter))
          },
          
          'abu10' = {
            plot(seq(1,nbiter,1)/iter,
                 x@abundances$abu10$Total, pch=20,
                 main="Number of trees with dbh > 10 cm (in stems/ha)",
                 xlab="Times (in years)",
                 ylab="Number of stems/ha",
                 xlim=c(0,nbiter/iter))
          },
          
          'abu30' = {
            plot(seq(1,nbiter,1)/iter,
                 x@abundances$abu30$Total, pch=20,
                 main="Number of trees with dbh > 30 cm (in stems/ha)",
                 xlab="Times (in years)",
                 ylab="Number of stems/ha",
                 xlim=c(0,nbiter/iter))
          },
          
          'allabund' = {
            par(mfrow=c(3,1))
            plot(x, 'abund')
            plot(x, 'abu10')
            plot(x, 'abu30')
            par(mfrow=c(1,1))
          },
          
          ##### agb ####
          'agb' = {
            plot(seq(1,nbiter,1)/iter,
                 x@agb$Total, pch=20,
                 main="Aboveground biomass (in tonnes/ha)",
                 xlab="Times (in years)",
                 ylab="Aboveground biomass (in tonnes/ha)",
                 xlim=c(0,nbiter/iter))
          },
          
          ##### ba ####
          'ba' = {
            plot(seq(1,nbiter,1)/iter,
                 x@ba$ba$Total, pch=20,
                 main="Total basal area (in m2/ha)",
                 xlab="Times (in years)",
                 ylab="Basal area (in m2/ha)",
                 xlim=c(0,nbiter/iter))
          },
          
          'ba10' = {
            plot(seq(1,nbiter,1)/iter,
                 x@ba$ba10$Total, pch=20,
                 main="Basal area of trees with dbh > 10 cm (in stems/ha)",
                 xlab="Times (in years)",
                 ylab="Basal area (in m2/ha)",
                 xlim=c(0,nbiter/iter))
          },
          
          'allba' = {
            par(mfrow=c(2,1))
            plot(x, 'ba')
            plot(x, 'ba10')
            par(mfrow=c(1,1))
          },
          
          ##### death ####
          'death' = {
            warning('death plots not implemented yet !')
          },
          
          ##### disturbance ####
          'dist height' = .plot_dist_height(x),
          'dist dbh' = .plot_dist_dbh(x),
          'dist rank-abundance' = .plot_dist_rank_abundance(x),
          
          ##### final pattern ####
          
          'height' = {
            hist <- hist(x@final_pattern$height[x@final_pattern$height != 0], breaks=seq(0,50,by=1), plot = FALSE)
            plot(hist,
                 main = paste("Tree height histogram after", age, "years (", surf, "ha)"),
                 col="green",
                 xlim=c(0,30),
                 ylim=c(0,40000),
                 xlab = "Height class")
          },
          
          'dbh' = {
            hist(x@final_pattern$dbh[x@final_pattern$dbh != 0],
                 main = paste("Tree dbh histogram after", age, "years (", surf, "ha)"),
                 col="green",
                 xlab = "Dbh class")
          },
          
          'rank-abundance' = {
            barplot(table(x@final_pattern$sp_lab)[order(table(x@final_pattern$sp_lab), decreasing = T)],
                    col = 'green',
                    xlab = 'rank',
                    ylab = 'abundance',
                    main = paste("Tree rank-abundance histogram after", age, "years (", surf, "ha)"))
          },
          
          'age' = {
            image(x@final_pattern['age'],
                 col=rev(heat.colors(10)),
                 main="TROLL age distribution",
                 xlab="x (m)",
                 ylab="y (m)"
            )
          },
          
          'species' = {
            image(x@final_pattern['sp_lab'],
                 col = rainbow(length(unique(x@final_pattern$sp_lab))),
                 main="TROLL species distribution",
                 xlab="x (m)",
                 ylab="y (m)"
            )
          },
          
          ##### flux ####
          'flux' = {
            plot(seq(1,nbiter,1)/iter,
                 x@gpp$Total, pch=20,
                 xlab="Times (in years)",
                 ylab="Total flux (in MgC/ha)",
                 main="Flux (in MgC/ha)")
            points(seq(1,nbiter,1)/iter,
                   x@npp$Total, pch=20, col = 'red')
            points(seq(1,nbiter,1)/iter,
                   x@gpp$Total - x@npp$Total,
                   pch=20, col = 'green')
            legend("topright", col=c("black", "green",  "red"), pch=20,
                   legend=c("GPP", "Autotrophic respiration", "NPP"))
          },
          
          ##### gpp ####
          'gpp' = {
            plot(seq(1,nbiter,1)/iter,
                 x@gpp$Total, pch=20,
                 xlab="Times (in years)",
                 ylab="Total GPPLeaf (in MgC/ha)",
                 main="Gross primary productivity (in MgC/ha)")
          },
          
          ##### LAI ####
          'LAI' = {
            warning('LAI plots not implemented yet !')
          },
          
          ##### litterfall ####
          'litterfall' = {
            plot(seq(1,nbiter,1)/iter,
                 x@litterfall$Total * 12, pch=20,
                 xlab="Times (in years)",
                 ylab="Leaf litterfall per month (in Mg/ha/year dry mass)",
                 main="Leaf litterfall per month (in Mg dry mass/ha/year)")
          },
          
          ##### npp ####
          'npp' = {
            plot(seq(1,nbiter,1)/iter,
                 x@npp$Total, pch=20,
                 xlab="Times (in years)",
                 ylab="Total NPPLeaf (in MgC/ha)",
                 main="Net primary productivity (in MgC/ha)")
          },
          
          ##### R ####
          'R' = {
            warning('R plots not implemented yet !')
          },
          
          
          ##### Ra ####
          'Ra' = {
            plot(seq(1,nbiter,1)/iter,
                 x@gpp$Total - x@npp$Total, pch=20,
                 xlab="Times (in years)",
                 ylab="Autotrophic respiration (in MgC/ha)",
                 main="Autotrophic respiration (in MgC/ha)")
          },
          
          ##### relabund ####
          'relabund' = {
            warning('relabund plots not implemented yet !')
          },
          
          ##### vertd ####
          
          'vertd' = {
            max_height <- max(x@vertd$height)
            plot(x =c(-diff(tail(x@vertd$vertd, max_height)),0),
                 y = seq(1, max_height),
                 type = "l",
                 main = paste("PAD distribution after", age, "years"),
                 xlab = "PAD (m2 per m3)",
                 ylab = "Height",
                 xlim = c(0,1),
                 ylim = c(0,60))
          },
          
          ##### wd ####
          'wd' = {
            mean_wood_dens <- rep(0,nbiter)
            for (i in 1:nbiter) {
              mean_wood_dens[i] <- sum(x@abundances$relabdund[i,] / 100 * x@sp_par$wsg)
            }
            plot(seq(1,nbiter,1)/iter,
                 mean_wood_dens,
                 ylim=c(0, 0.7), pch=20,
                 main="Total wood density (in g/cm3)",
                 xlab="Time (in year)",
                 ylab="Average plot wood density",
                 xlim=c(0,nbiter/iter))
          },
          
          'wd10' = {
            mean_wood_dens <- rep(0,nbiter)
            for (i in 1:nbiter) {
              mean_wood_dens[i] <- sum(x@abundances$relabu10[i,] / 100 * x@sp_par$wsg)
            }
            plot(seq(1,nbiter,1)/iter,
                 mean_wood_dens,
                 ylim=c(0, 0.7), pch=20,
                 main="Wood density of trees with dbh > 10 cm (in g/cm3)",
                 xlab="Time (in year)",
                 ylab="Average plot wood density",
                 xlim=c(0,nbiter/iter))
          },
          
          'wd30' = {
            mean_wood_dens <- rep(0,nbiter)
            for (i in 1:nbiter) {
              mean_wood_dens[i] <- sum(x@abundances$relabu30[i,] / 100 * x@sp_par$wsg)
            }
            plot(seq(1,nbiter,1)/iter,
                 mean_wood_dens,
                 ylim=c(0, 0.7), pch=20,
                 main="Wood density of trees with dbh > 30 cm (in g/cm3)",
                 xlab="Time (in year)",
                 ylab="Average plot wood density",
                 xlim=c(0,nbiter/iter))
          },
          
          'allwd' = {
            par(mfrow=c(3,1))
            plot(x, 'wd')
            plot(x, 'wd10')
            plot(x, 'wd30')
            par(mfrow=c(1,1))
          },
          
          stop('Option not available')
          
  )
})

#### Disturbance ####

.plot_dist_height <- function(x){
  hist <- hist(x@disturbance$height, breaks=seq(0,50,by=1), plot = FALSE)
  plot(hist,
       main = paste("Height histogram of tree killed in disturbance \n after", 
                    distage, "years (", surf, "ha)"),
       col="green",
       xlim=c(0,30),
       ylim=c(0,40000),
       xlab = "Height class")
}

.plot_dist_dbh <- function(x){
  hist(x@disturbance$dbh,
       main = paste("Dbh histogram of tree killed in disturbance \n after", 
                    distage, "years (", surf, "ha)"),
       col="green",
       xlab = "Dbh class")
}

.plot_dist_rank_abundance <- function(x){
  barplot(table(x@disturbance$sp_lab)[order(table(x@disturbance$sp_lab), 
                                            decreasing = T)],
          col = 'green',
          xlab = 'rank',
          ylab = 'abundance',
          main = paste(
            "Rank-abundance histogram of tree killed in disturbance \n after", 
            distage, "years (", surf, "ha)"))
}
