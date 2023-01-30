#' @include trollsim.R
#' @include trollstack.R
#' @include get_chm.trollsim.R
#' @import methods
#' @import ggplot2
#' @importFrom dplyr filter mutate select
#' @importFrom reshape2 melt
#' @importFrom terra as.data.frame
NULL

#' Plot TROLL simulation or stack
#'
#' This is a method to plot TROLL simulations, including either temporal
#' trajectories of whole ecosystem or species metrics, the initial or final
#' pattern observed in the forest community, or lidar outputs. Metrics includes
#' abundances of individuals above 1cm (N), above 10cm (N10), and above 30cm
#' (N30), aboveground biomass (AGB), basal area of individuals above 1cm (BA),
#' and above 10cm (BA10), gross primary production (GPP), net primary production
#' (NPP), respiration of day (Rday), night (Rnight) and stem (Rstem), and
#' litterfall.
#'
#' @param object TROLL simulation or stack.
#' @param what char. What to plot: "temporal", "spatial" "distribution", or
#'   "lidar". "temporal" is for temporal trajectories of the whole ecosystem or
#'   defined species. "spatial" is for spatial patterns in the initial or final
#'   forest. "distribution" is for metrics distribution in the initial or final
#'   forest. "lidar" is for canopy height model plot.
#' @param variables char. Which variable(s) to plot. Only one variable is
#'   accepted when plotting "spatial".
#' @param species char. Which species to plot. NULL indicates the whole
#'   ecosystem level. "all" can be used to use all species.
#' @param iter char. Which iteration(s) to plot, for temporal thinning or to
#'   specify which forest to plot. "initial" or "final" can be used. NULL is
#'   converted to "final".
#'
#' @return A ggplot2 object.
#'
#' @examples
#'
#' data("TROLLv3_output")
#' autoplot(TROLLv3_output)
#'
#' @export
setMethod("autoplot", "trollsim", function(object,
                                           what = "temporal",
                                           variables = NULL,
                                           species = NULL,
                                           iter = NULL) {
  # dplyr
  spnames <- n_iter <- value <- variable <- dbh <- s_name <- NULL
  x <- y <- canopy_height <- NULL

  # check parameters
  if (!(what %in% c("temporal", "spatial", "distribution", "lidar"))) {
    stop("what should be temporal, spatial, or distribution")
  }
  if (!is.null(iter)) {
    iter <- switch(iter,
      "final" = max(object@forest$iter),
      "initial" = min(object@forest$iter)
    )
  }

  # temporal
  if (what == "temporal") {
    # species
    if (!is.null(species) & nrow(object@species) == 0) {
      stop("The species table is empty, please use extended
           outputs with global parameter _OUTPUT_extended.")
    }
    if (!is.null(species)) {
      if ("all" %in% species) {
        species <- unique(object@species$species)
      }
    }
    spnames <- species
    # variables
    if (is.null(variables)) {
      if (is.null(species)) {
        variables <- names(object@ecosystem)
      } else {
        variables <- names(object@species)
      }
    }
    if (is.null(species)) {
      absent <- variables[!(variables %in% names(object@ecosystem))]
    } else {
      absent <- variables[!(variables %in% names(object@species))]
    }
    if (length(absent) > 0) {
      warning(paste("The following variables are not 
                    present in the selected outputs: ",
                    paste(absent, sep = ", ")))
    }
    # table
    if (is.null(species)) {
      tab <- object@ecosystem
    } else {
      tab <- object@species
    }
    # iter
    if (is.null(species)) {
      iters <- unique(object@ecosystem$iter)
    } else {
      iters <- unique(object@species$iter)
    }
    if (!is.null(iter)) {
      if (!(iter %in% iters)) {
        stop("iteration not available in the ecosystem table, please check.")
      }
      n_iter <- iter
      tab <- filter(tab, iter %in% n_iter)
    }
    # prep table
    tab <- mutate(tab,
                  iter = as.numeric(iter /
                                      object@parameters["iterperyear"]))
    if (!("simulation" %in% names(tab))) {
      tab$simulation <- "sim"
    }
    if (is.null(species)) {
      tab <- melt(tab, c("iter", "simulation"))
    } else {
      tab <- melt(tab, c("iter", "species", "simulation")) %>%
        filter(species %in% spnames) %>%
        mutate(species = gsub("_", " ", species))
    }
    tab <- filter(tab, variable %in% variables) %>%
      mutate(variable = .get_units(as.character(variable)))
    # prep graph
    if (is.null(species)) {
      g <- ggplot(tab, aes(x = iter, y = value))
    } else {
      g <- ggplot(tab, aes(x = iter, y = value, color = species))
    }
    g <- g +
      geom_line() +
      theme_bw() +
      xlab("Time (year)") +
      theme(legend.text = element_text(face = "italic"))
    if (length(unique(tab$simulation)) == 1) {
      g <- g + facet_wrap(~variable, scales = "free_y", labeller = label_parsed)
    } else {
      g <- g + facet_grid(variable ~ simulation, scales = "free_y",
                          labeller = label_parsed)
    }
  }

  # spatial
  if (what == "spatial") {
    # variables
    if (is.null(variables)) {
      variables <- "s_name"
    }
    if ("species" %in% variables) {
      variables <- "s_name"
    }
    if (!(variables %in% names(object@forest))) {
      stop("The variable is unavailable for the spatial patterns.")
    }
    if (length(variable) > 1) {
      stop("Spatial patterns plot use only one variable.")
    }
    # species
    if (is.null(species)) {
      species <- unique(object@forest$s_name)
    }
    if ("all" %in% species) {
      species <- unique(object@forest$s_name)
    }
    # iter
    if (is.null(iter)) {
      iter <- max(object@forest$iter)
    }
    if (all(!(iter %in% unique(object@forest$iter)))) {
      stop("iteration not available in the forest table, please check.")
    }
    n_iter <- iter
    # prep table
    forest <- object@forest %>%
      filter(iter %in% n_iter) %>%
      filter(s_name %in% species) %>%
      mutate(s_name = gsub("_", " ", s_name))
    forest$variable <- unlist(forest[, variables])
    # prep graph
    g <- ggplot(
      forest,
      aes(col / object@parameters["NH"],
        row / object@parameters["NV"],
        size = dbh, col = variable
      )
    ) +
      geom_point() +
      theme_bw() +
      scale_size_continuous("DBH (m)", range = c(0.01, 1)) +
      coord_equal() +
      xlab("X (m)") +
      ylab("Y (m)") +
      guides(colour = guide_legend(title = parse(text = .get_units(variables))))
    if (variables == "s_name") {
      g <- g + theme(legend.text = element_text(face = "italic"))
    }
    if (variables == "s_name" & length(species) > 10) {
      g <- g + guides(colour = "none")
    }
    if ("simulation" %in% names(forest)) {
      g <- g + facet_wrap(~simulation)
    }
  }

  # distribution
  if (what == "distribution") {
    # variables
    n_vars <- names(object@forest)
    n_vars <- n_vars[!(n_vars %in% c(
      "iter", "row", "col", "s_name", "from_Data", "sp_lab",
      "site", "CrownDisplacement",
      "lambda_young", "lambda_mature", "lambda_old",
      "fraction_filled", "mult_height", "mult_CR", "mult_CD",
      "mult_P", "mult_N",
      "mult_LMA", "mult_dbhmax", "dev_wsg",
      "carbon_storage", "carbon_biometry",
      "multiplier_seed", "hurt", "NPPneg"
    ))]
    if (is.null(variables)) {
      variables <- n_vars
    }
    absent <- variables[!(variables %in% n_vars)]
    if (length(absent) > 0) {
      warning(paste("The following variables are not
                    present in the selected outputs: ",
                    paste(absent, sep = ", ")))
    }
    # species
    if ("all" %in% species) {
      species <- unique(object@forest$s_name)
    }
    # iter
    if (is.null(iter)) {
      iter <- max(object@forest$iter)
    }
    if (all(!(iter %in% unique(object@forest$iter)))) {
      stop("iteration not available in the forest table, please check.")
    }
    n_iter <- iter
    # prep table
    forest <- object@forest %>%
      filter(iter %in% n_iter)
    if (!("simulation" %in% names(forest))) {
      forest$simulation <- "sim"
    }
    forest <- forest %>%
      melt(c("simulation", "iter", "s_name")) %>%
      filter(variable %in% variables) %>%
      mutate(variable = .get_units(as.character(variable)))
    if (!is.null(species)) {
      forest <- filter(forest, s_name %in% species) %>%
        mutate(s_name = gsub("_", " ", s_name))
    }
    # prep graph
    if (is.null(species)) {
      g <- ggplot(forest, aes(value)) +
        geom_histogram() +
        theme_bw()
    }
    if (!is.null(species)) {
      g <- ggplot(forest, aes(value, col = s_name)) +
        geom_density(fill = NA) +
        scale_color_discrete("Species") +
        theme_bw() +
        theme(legend.text = element_text(face = "italic"))
    }
    if (length(unique(forest$simulation)) == 1) {
      g <- g + facet_wrap(~variable, scales = "free", labeller = label_parsed)
    } else {
      g <- g + facet_grid(variable ~ simulation, scales = "free",
                          labeller = label_parsed)
    }
  }

  # lidar
  if (what == "lidar") {
    # check las existence
    if (length(object@las) == 0) {
      stop("The TROLL outputs does not contain a las from lidar simulation.")
    }
    # variables
    if (!is.null(variables)) {
      message("variables should be null with lidar, the value will be ignored.")
    }
    # species
    if (!is.null(species)) {
      message("species should be null with lidar, the value will be ignored.")
    }
    # iter
    if (!is.null(iter)) {
      message("iter should be null with lidar, the value will be ignored.")
    }

    if (is.null(iter)) {
      iter <- max(object@forest$iter)
    }
    if (all(!(iter %in% unique(object@forest$iter)))) {
      stop("iteration not available in the forest table, please check.")
    }
    n_iter <- iter
    # prep graph
    g <- get_chm(object) %>%
      lapply(as.data.frame, xy = TRUE) %>%
      bind_rows(.id = "simulation") %>%
      ggplot(aes(x, y, fill = canopy_height)) +
      geom_raster() +
      coord_equal() +
      xlab("X (m)") +
      ylab("Y (m)") +
      viridis::scale_fill_viridis("Canpoy\nheight", option = "H") +
      theme_bw()
    if (inherits(object, "trollstack")) {
      g <- g + facet_wrap(~simulation)
    }
  }

  return(g)
})

.get_units <- function(vars) {
  lapply(vars, function(var) {
    switch(var,
      # temporal
      "sum1" = "N~(stems)",
      "sum10" = "N[10]~(stems)",
      "sum30" = "N[30]~(stems)",
      "ba" = "BA~(m^{2}~ha^{-1})",
      "ba10" = "BA[10]~(m^{2}~ha^{-1})",
      "agb" = "AGB~(Kg~ha^{-1})",
      "gpp" = "GPP~(MgC~ha^{-1})",
      "npp" = "NPP~(MgC~ha^{-1})",
      "rday" = "R[day]~(MgC~ha^{-1})",
      "rnight" = "R[night]~(MgC~ha^{-1})",
      "rstem" = "R[stem]~(MgC~ha^{-1})",
      "litterfall" = "Litterfall~(Mg~ha^{-1})",
      # distribution
      "Pmass" = "P[m]~(mg~g^{-1})",
      "Nmass" = "N[m]~(mg~g^{-1})",
      "LMA" = "LMA~(g~m^{-2})",
      "wsg" = "wsg~(g~cm^{-3})",
      "Rdark" = "R[dark]~(KgC)",
      "Vcmax" = "V[cmax]~(mu~mol[CO2]~g^{-1}~s^{-1})",
      "Jmax" = "J[max]~(mu~mol~g^{-1}~s^{-1})",
      "leaflifespan" = "Leaflifespan~(months)",
      "dbhmature" = "DBH[mature]~(m)",
      "dbhmax" = "DBH[maximum]#(m)",
      "hmax" = "h[max]~(m)",
      "ah" = "a[h]~(m)",
      "Ct" = "C[t]~(m)",
      "LAImax" = "LAI[max]~(m^{2}~m^{-2})",
      "age" = "Age~(years)",
      "dbh" = "DBH~(m)",
      "sapwood_area" = "Sapwood~area~(m^{2})",
      "height" = "Height~(m)",
      "CD" = "CD~(m)",
      "CR" = "CR~(m)",
      "GPP" = "GPP~(KgC)",
      "NPP" = "NPP~(KgC)",
      "Rday" = "R[day]~(KgC)",
      "Rnight" = "R[night]~(KgC)",
      "Rstem" = "R[stem]~(KgC)",
      "LAmax" = "LA[max]~(m^{2})",
      "LA" = "LA~(m^{2})",
      "youngLA" = "LA[young]~(m^{2})",
      "matureLA" = "LA[mature]~(m^{2})",
      "oldLA" = "LA[old]~(m^{2})",
      "LAI" = "LAI~(m^{2}~m^{-2})",
      "litter" = "Litter~(Kg)",
      "dbh_previous" = "DBH[previous]~(m)",
      "AGB" = "AGB~(Kg)",
      # spatial
      "s_name" = "Species",
      # undefined
      var
    )
  }) %>%
    unlist()
}
