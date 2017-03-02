#' @include TROLLoutput.R
#' @importFrom sp coordinates<- gridded<-
NULL


#' Function to load TROLL output
#'
#' @param name char. Name given to the model output
#' @param path char. Path where the model is saved
#'
#' @return an S4 \linkS4class{TROLLoutput} class object
#'
#' @export
#'
#' @examples
#'
load <- function(name = getOption("TROLL.name"), 
                 path = file.path(getOption("TROLL.path"), getOption("TROLL.name"))){

  # Final pattern
  final_pattern <- read.table(file.path(path, paste0(name, '_0_final_pattern.txt')))
  names(final_pattern) <- c("x","y","age","dbh","height","crown_radius","crown_depth","sp_lab")
  coordinates(final_pattern) <- c('x', 'y')
  gridded(final_pattern) <- TRUE

  # Opening files
  x <- TROLLoutput(
    name = name,
    path = path,
    abundances = list(
      abund = read.table(file.path(path, paste0(name, '_0_abund.txt')), row.names = 1),
      abu10 = read.table(file.path(path, paste0(name, '_0_abu10.txt')), row.names = 1),
      abu30 = read.table(file.path(path, paste0(name, '_0_abu30.txt')), row.names = 1)
    ),
    agb = read.table(file.path(path, paste0(name, '_0_agb.txt'))),
    ba = list(
      ba = read.table(file.path(path, paste0(name, '_0_ba.txt')), row.names = 1),
      ba10 = read.table(file.path(path, paste0(name, '_0_ba10.txt')), row.names = 1)
    ),
    dbh = data.frame(), # Don't get it must ask
    death = list(
      death = read.table(file.path(path, paste0(name, '_0_death.txt')), row.names = 1),
      death1 = read.table(file.path(path, paste0(name, '_0_death1.txt'))),
      death2 = read.table(file.path(path, paste0(name, '_0_death2.txt'))),
      death3 = read.table(file.path(path, paste0(name, '_0_death3.txt'))),
      deathrate = read.table(file.path(path, paste0(name, '_0_deathrate.txt')))
    ),
    final_pattern = final_pattern,
    gpp = read.table(file.path(path, paste0(name, '_0_gpp.txt')), row.names = 1),
    info = list(
      step = as.numeric(scan(file.path(path, paste0(name, '_0_info.txt')), character(), skip = 4, n = 7, quiet = T)[7]),
      SitesNb = as.numeric(unlist(strsplit(scan(file.path(path, paste0(name, '_0_info.txt')),
                                     character(), skip = 12, n = 5, quiet = T)[5], 'x'))),
      IterationsNb = scan(file.path(path, paste0(name, '_0_info.txt')), character(), skip = 13, n = 5, quiet = T)[5],
      timestep = scan(file.path(path, paste0(name, '_0_info.txt')), character(), skip = 14, n = 5, quiet = T)[5],
      SpeciesNb = scan(file.path(path, paste0(name, '_0_info.txt')), character(), skip = 15, n = 5, quiet = T)[5],
      ComputationTime = scan(file.path(path, paste0(name, '_0_info.txt')), character(), skip = 17, n = 5, quiet = T)[5]
    ),
    litterfall = read.table(file.path(path, paste0(name, '_0_litterfall.txt'))),
    npp = read.table(file.path(path, paste0(name, '_0_npp.txt')), row.names = 1),
    par = list(
      general = list(
        nbcol = scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 4, n = 1, quiet = TRUE),
        nbrows = scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 5, n = 1, quiet = TRUE),
        nbiter= scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 6, n = 1, quiet = TRUE),
        iter = scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 7, n = 1, quiet = TRUE),
        NV = scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 8, n = 1, quiet = TRUE),
        NH = scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 9, n = 1, quiet = TRUE),
        nbout = scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 10, n = 1, quiet = TRUE),
        numesp = scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE),
        p = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 12, n = 1, quiet = TRUE),
        disturb_iter = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 13, n = 1, quiet = TRUE),
        disturb_intensity = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 14, n = 1, quiet = TRUE),
        daylight = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 15, n = 24, quiet = TRUE),
        dayT = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 16, n = 24, quiet = TRUE),
        dayVPD = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 17, n = 24, quiet = TRUE)
      ),
      species_par = list(
        klight = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 19, n = 1, quiet = TRUE),
        phi = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 20, n = 1, quiet = TRUE),
        g1 = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 21, n = 1, quiet = TRUE), 
        vC = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 22, n = 1, quiet = TRUE),
        DBH0 = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 23, n = 1, quiet = TRUE),
        H0 = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 24, n = 1, quiet = TRUE),
        ra0 = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 25, n = 1, quiet = TRUE),
        ra1 = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 26, n = 1, quiet = TRUE),
        de0 = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 27, n = 1, quiet = TRUE),
        de1 = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 28, n = 1, quiet = TRUE),
        dens = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 29, n = 1, quiet = TRUE),
        fbranchstem = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 30, n = 1, quiet = TRUE),
        fcanopy = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 31, n = 1, quiet = TRUE),
        seedrain = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 32, n = 1, quiet = TRUE),
        nbseeds = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 33, n = 1, quiet = TRUE),
        mindeathrate = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 34, n = 1, quiet = TRUE),
        m1 = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 35, n = 1, quiet = TRUE),
        CO2 = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), skip = 36, n = 1, quiet = TRUE)
      ),
      climate = list(
        Tyear = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), n = 12, quiet = TRUE,
             skip = 38 + scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE) + 4),
        maxT = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), n = 12, quiet = TRUE,
                    skip = 38 + scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE) + 5),
        nightmeanT = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), n = 12, quiet = TRUE,
                          skip = 38 + scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE) + 6),
        rainfall = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), n = 12, quiet = TRUE,
                        skip = 38 + scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE) + 7),
        wind = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), n = 12, quiet = TRUE,
                    skip = 38 + scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE) + 8),
        maxIrradiance = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), n = 12, quiet = TRUE,
                             skip = 38 + scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE) + 9),
        irradiance = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), n = 12, quiet = TRUE,
                             skip = 38 + scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE) + 10),
        e_s = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), n = 12, quiet = TRUE,
                   skip = 38 + scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE) + 11),
        e_a = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), n = 12, quiet = TRUE,
                   skip = 38 + scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE) + 12),
        VPD = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), n = 12, quiet = TRUE,
                   skip = 38 + scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE) + 13),
        dailymeanVPD = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), n = 12, quiet = TRUE,
                            skip = 38 + scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE) + 14),
        dailymaxVPD = scan(file.path(path, paste0(name, '_0_par.txt')), numeric(), n = 12, quiet = TRUE,
                           skip = 38 + scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE) + 15)
      )
    ),
    paramspace = list(
      proc	= as.integer(scan(file.path(path, paste0(name, '_0_paramspace.txt')), character(), 2, quiet = TRUE)[2]),
      phi = as.numeric(scan(file.path(path, paste0(name, '_0_paramspace.txt')), character(), 2, skip = 1, quiet = TRUE)[2]),
      k = as.numeric(scan(file.path(path, paste0(name, '_0_paramspace.txt')), character(), 2, skip = 2, quiet = TRUE)[2]),
      fallocwood	= as.numeric(scan(file.path(path, paste0(name, '_0_paramspace.txt')), character(), 2, skip = 3, quiet = TRUE)[2]),
      falloccanopy	= as.numeric(scan(file.path(path, paste0(name, '_0_paramspace.txt')), character(), 2, skip = 4, quiet = TRUE)[2]),
      m	= as.numeric(scan(file.path(path, paste0(name, '_0_paramspace.txt')), character(), 2, skip = 5, quiet = TRUE)[2]),
      m1	= as.numeric(scan(file.path(path, paste0(name, '_0_paramspace.txt')), character(), 2, skip = 6, quiet = TRUE)[2])
    ),
    ppfd0 = read.table(file.path(path, paste0(name, '_0_ppfd0.txt')), row.names = 1),
    R = list(
      Rday = read.table(file.path(path, paste0(name, '_0_Rday.txt')), row.names = 1),
      Rnight = read.table(file.path(path, paste0(name, '_0_Rnight.txt')), row.names = 1)
    ),
    # site = list(
    #   site1 = read.table(file.path(path, paste0(name, '_0_site1.txt')), row.names = 1),
    #   site2 = read.table(file.path(path, paste0(name, '_0_site2.txt')), row.names = 1),
    #   site3 = read.table(file.path(path, paste0(name, '_0_site3.txt')), row.names = 1),
    #   site4 = read.table(file.path(path, paste0(name, '_0_site4.txt')), row.names = 1),
    #   site5 = read.table(file.path(path, paste0(name, '_0_site5.txt')), row.names = 1),
    #   site6 = read.table(file.path(path, paste0(name, '_0_site6.txt')), row.names = 1)
    # ),
    sp_par = read.table(file.path(path, paste0(name, '_0_par.txt')),
                        header=TRUE, dec=".", sep="",
                        skip=38, row.names=1,
                        nrows=scan(file.path(path, paste0(name, '_0_par.txt')), integer(), skip = 11, n = 1, quiet = TRUE)),
    vertd = read.table(file.path(path, paste0(name, '_0_vertd.txt')))
  )

  # Naming
  names(x@abundances$abund) <- c(rownames(x@sp_par), "Total")
  names(x@abundances$abu10) <- c(rownames(x@sp_par), "Total")
  names(x@abundances$abu30) <- c(rownames(x@sp_par), "Total")
  colnames(x@agb) <- c(rownames(x@sp_par), "Total")
  colnames(x@ba$ba) <- c(rownames(x@sp_par), "Total")
  colnames(x@ba$ba10) <- c(rownames(x@sp_par), "Total")
  colnames(x@gpp) <- c(rownames(x@sp_par), "Total")
  colnames(x@litterfall) <- c(rownames(x@sp_par), "Total")
  colnames(x@npp) <- c(rownames(x@sp_par), "Total")
  colnames(x@R$Rday) <- c(rownames(x@sp_par), "Total")
  colnames(x@R$Rnight) <- c(rownames(x@sp_par), "Total")
  names(x@vertd) <- c('height', 'vertd')

  # Transforming
  x@abundances$relabdund <- x@abundances$abund[,-ncol(x@abundances$abund)]*100/x@abundances$abund$Total
  x@abundances$relabu10 <- x@abundances$abu10[,-ncol(x@abundances$abu10)]*100/x@abundances$abu10$Total
  x@abundances$relabu30 <- x@abundances$abu30[,-ncol(x@abundances$abu30)]*100/x@abundances$abu30$Total

  return(x)
}
