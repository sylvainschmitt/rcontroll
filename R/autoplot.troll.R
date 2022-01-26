#' @include trollsim.R
#' @include trollstack.R
#' @import methods
#' @import ggplot2
#' @importFrom dplyr filter mutate select
#' @importFrom reshape2 melt
NULL

#' Plot TROLL simulation
#'
#' This is a method to plot TROLL simulations, including either temporal
#' trajectories of whole ecosystem or species metrics, or the initial or final
#' pattern observed in the forest community. Metrics includes abundances of
#' individuals above 1cm (N), above 10cm (N10), and above 30cm (N30),
#' aboveground biomass (AGB), basal area of individuals above 1cm (BA), and
#' above 10cm (BA10), gross primary production (GPP), net primary production
#' (NPP), respiration of day (Rday), night (Rnight) and stem (Rstem), and
#' litterfall.
#'
#' @param object TROLL simulation.
#' @param what char. What to plot: "forest", "ecosystem", or "species".
#' @param variables char. Wich variable(s) to plot (ecosystem or species).
#' @param iter char. Which iteration(s) to plot.
#' @param species char. Which species to plot when using the species table.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' 
#' data("TROLLv3_output")
#' autoplot(TROLLv3_output)
#'
#' @export
setMethod("autoplot", "trollsim", function(
  object, 
  what = "ecosystem",
  variables = NULL, 
  iter = NULL,
  species = NULL
) {
  # dplyr
  spnames <- Niter <- value <- variable <- dbh <- s_name <- NULL
  
  # check parameters
  if(!(what %in% c("forest", "ecosystem", "species")))
    stop("what should be forest, ecosystem, or species")
  
  # forest
  if(what == "forest"){
    if(!is.null(variables))
      warning("variables parameter is unused when plotting forest.")
    if(!is.null(species))
      warning("species parameter is unused when plotting forest.")
    if(length(unique(object@forest$iter)) == 0)
      stop("The forest table is empty, please use extended outputs with global parameter _OUTPUT_extended.")
    if(is.null(iter))
      iter <- max(object@forest$iter)
    if(all(!(iter %in% unique(object@forest$iter))))
      stop("iteration not available in the forest table, please check.")
    Niter <- iter
    g <- ggplot(filter(object@forest, iter %in% Niter),
           aes(col/object@parameters["NH"],
               row/object@parameters["NV"],
               size = dbh, col = s_name)) +
      geom_point() +
      theme_bw() +
      scale_size_continuous("DBH (m)", range = c(0.01, 1)) +
      scale_color_discrete(guide = "none") +
      coord_equal() +
      xlab("X (m)") + ylab("Y (m)")
  }
  
  # ecosystem
  if(what == "ecosystem"){
    if(is.null(variables))
    variables <- names(object@ecosystem)
    absent <- variables[!(variables %in% names(object@ecosystem))]
    if(length(absent) > 0)
      warning(paste("The following variables are not present in the ecosystem table: ", paste(absent, sep =", ")))
    if(!is.null(species))
      warning("species parameter is unused when plotting ecosystem.")
    ecosystem <- object@ecosystem
    if(!is.null(iter)){
      if(!(iter %in% unique(object@ecosystem$iter)))
        stop("iteration not available in the ecosystem table, please check.")
      Niter <- iter
      ecosystem <- filter(ecosystem, iter %in% Niter)
    }
    g <- ecosystem %>%
      mutate(iter = as.numeric(iter / object@parameters["iterperyear"])) %>% 
      melt("iter") %>%
      filter(variable %in% variables) %>% 
      mutate(variable = .get_units(as.character(variable))) %>% 
      ggplot(aes(x =iter, y = value)) +
      geom_line() +
      facet_wrap(~variable, scales = "free_y", labeller = label_parsed) +
      theme_bw() +
      xlab("Time (year)")
  }
  
  # species
  if(what == "species"){
    if(nrow(object@species) == 0)
      stop("The species table is empty, please use extended outputs with global parameter _OUTPUT_extended.")
    if(is.null(variables))
      variables <- names(object@ecosystem)
    absent <- variables[!(variables %in% names(object@ecosystem))]
    if(length(absent) > 0)
      warning(paste("The following variables are not present in the ecosystem table: ", paste(absent, sep =", ")))
    if(is.null(species))
      species <- object@species$species
    absent <- species[!(species %in% object@species$species)]
    if(length(absent) > 0)
      warning(paste("The following species are not present in the species table: ", paste(absent, sep =", ")))
    sptab <- object@species
    if(!is.null(iter)){
      if(!(iter %in% unique(object@ecosystem$iter)))
        stop("iteration not available in the ecosystem table, please check.")
      Niter <- iter
      sptab <- filter(sptab, iter %in% Niter)
    }
    spnames <- species
    g <- sptab %>%
      mutate(iter = as.numeric(iter / object@parameters["iterperyear"])) %>% 
      melt(c("iter", "species")) %>%
      filter(variable %in% variables) %>% 
      mutate(variable = .get_units(as.character(variable))) %>% 
      filter(species %in% spnames) %>% 
      mutate(species = gsub("_", " ", species)) %>% 
      ggplot(aes(x =iter, y = value,color = species)) +
      geom_line() +
      facet_wrap(~variable, scales = "free_y", labeller = label_parsed) +
      theme_bw() +
      xlab("Time (year)") +
      theme(legend.text = element_text(face = "italic"))
  }
  
  return(g)
})


#' Plot TROLL stack
#'
#' This is a method to plot TROLL stacks, including either temporal trajectories
#' of whole ecosystem or species metrics, or the initial or final pattern
#' observed in the forest community. Metrics includes abundances of individuals
#' above 1cm (N), above 10cm (N10), and above 30cm (N30), aboveground biomass
#' (AGB), basal area of individuals above 1cm (BA), and above 10cm (BA10), gross
#' primary production (GPP), net primary production (NPP), respiration of day
#' (Rday), night (Rnight) and stem (Rstem), and litterfall.
#'
#' @param object TROLL stack.
#' @param what char. What to plot: "forest", "ecosystem", or "species".
#' @param variables char. Wich variable(s) to plot (ecosystem or species).
#' @param iter char. Which iteration(s) to plot.
#' @param species char. Which species to plot when using the species table.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' autoplot(stack)
#' }
#'
#' @export
setMethod("autoplot", "trollstack", function(
  object, 
  what = "ecosystem",
  variables = NULL, 
  iter = NULL,
  species = NULL
) {
  # dplyr
  spnames <- Niter <- value <- variable <- dbh <- s_name <- simulation <- NULL
  
  # check parameters
  if(!(what %in% c("forest", "ecosystem", "species")))
    stop("what should be forest, ecosystem, or species")
  
  # forest
  if(what == "forest"){
    if(!is.null(variables))
      warning("variables parameter is unused when plotting forest.")
    if(!is.null(species))
      warning("species parameter is unused when plotting forest.")
    if(length(unique(object@forest$iter)) == 0)
      stop("The forest table is empty, please use extended outputs with global parameter _OUTPUT_extended.")
    if(is.null(iter))
      iter <- max(object@forest$iter)
    if(all(!(iter %in% unique(object@forest$iter))))
      stop("iteration not available in the forest table, please check.")
    Niter <- iter
    g <- ggplot(filter(object@forest, iter %in% Niter),
                aes(col/object@parameters["NH"],
                    row/object@parameters["NV"],
                    size = dbh, 
                    col = s_name)) +
      geom_point() +
      theme_bw() +
      scale_size_continuous("DBH (m)", range = c(0.01, 1)) +
      scale_color_discrete(guide = "none") +
      coord_equal() +
      xlab("X (m)") + ylab("Y (m)") +
      facet_wrap(~ simulation)
  }
  
  # ecosystem
  if(what == "ecosystem"){
    if(is.null(variables))
      variables <- names(object@ecosystem)
    absent <- variables[!(variables %in% names(object@ecosystem))]
    if(length(absent) > 0)
      warning(paste("The following variables are not present in the ecosystem table: ", paste(absent, sep =", ")))
    if(!is.null(species))
      warning("species parameter is unused when plotting ecosystem.")
    ecosystem <- object@ecosystem
    if(!is.null(iter)){
      if(!(iter %in% unique(object@ecosystem$iter)))
        stop("iteration not available in the ecosystem table, please check.")
      Niter <- iter
      ecosystem <- filter(ecosystem, iter %in% Niter)
    }
    g <- ecosystem %>%
      mutate(iter = as.numeric(iter / object@parameters["iterperyear"])) %>% 
      melt(c("iter", "simulation")) %>%
      filter(variable %in% variables) %>%
      mutate(variable = .get_units(as.character(variable))) %>% 
      ggplot(aes(x =iter, y = value, col = simulation)) +
      geom_line() +
      facet_wrap(~variable, scales = "free_y", labeller = label_parsed) +
      theme_bw() +
      xlab("Time (year)")
  }
  
  # species
  if(what == "species"){
    if(nrow(object@species) == 0)
      stop("The species table is empty, please use extended outputs with global parameter _OUTPUT_extended.")
    if(is.null(variables))
      variables <- names(object@ecosystem)
    absent <- variables[!(variables %in% names(object@ecosystem))]
    if(length(absent) > 0)
      warning(paste("The following variables are not present in the ecosystem table: ", paste(absent, sep =", ")))
    if(is.null(species))
      species <- object@species$species
    absent <- species[!(species %in% object@species$species)]
    if(length(absent) > 0)
      warning(paste("The following species are not present in the species table: ", paste(absent, sep =", ")))
    sptab <- object@species
    if(!is.null(iter)){
      if(!(iter %in% unique(object@ecosystem$iter)))
        stop("iteration not available in the ecosystem table, please check.")
      Niter <- iter
      sptab <- filter(sptab, iter %in% Niter)
    }
    spnames <- species
    if(inherits(object, "trollstack"))
      melt_vars <- c("iter", "species", "simulation")
    else
      melt_vars <- c("iter", "species")
    g <- sptab %>%
      mutate(iter = as.numeric(iter / object@parameters["iterperyear"])) %>% 
      melt(melt_vars) %>%
      filter(variable %in% variables) %>%
      mutate(variable = .get_units(as.character(variable))) %>% 
      filter(species %in% spnames) %>% 
      mutate(species = gsub("_", " ", species)) %>% 
      ggplot(aes(x =iter, y = value, color = species, linetype = simulation)) +
      geom_line() +
      facet_wrap(~variable, scales = "free_y", labeller = label_parsed) +
      theme_bw() +
      xlab("Time (year)") +
      theme(legend.text = element_text(face = "italic"))
  }
  
  return(g)
})

.get_units <- function(vars){
  lapply(vars, function(var)
    switch (var,
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
            "litterfall" = "Litterfall~(MgC~ha^{-1})",
            var
    )
  ) %>% 
    unlist()
}

