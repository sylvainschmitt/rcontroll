#' @include trollsim.R
#' @include trollstack.R
#' @import methods
#' @import ggplot2
#' @importFrom dplyr filter mutate select
#' @importFrom reshape2 melt
#' @importFrom viridis scale_color_viridis
NULL

#' Plot TROLL outputs
#'
#' This is a method to plot TROLL simulations or stacks, including either
#' temporal trajectories of whole ecosystem or species metrics, or the final
#' pattern observed in the forest community. Metrics includes abundances of
#' individuals above 1cm (abund & N), above 10cm (abu10 & N10), and above 30cm
#' (abu30 & N30), aboveground biomass (agb & AGB), basal area of individuals
#' above 1cm (ba, BA), and above 10cm (ba10, BA10), gross primary production
#' (gpp, GPP), net primary production (npp, NPP), and respiration of day (rday),
#' night (rnight) and stem (rstem).
#'
#' @param object TROLL simulation or stack.
#' @param what char. What to plot: "final pattern" or "ecosystem".
#' @param variables char. Wich variable(s) to plot: full simulation: "species",
#'   "abu10", "abu30", "abund", "agb", "ba", "ba10", "gpp", "npp", "rday",
#'   "rnight", or "rstem"; reduced simulation: "N", "N10", "N30", "BA10", "NPP",
#'   "GPP", "AGB"
#' @param selected_species char. Which species to plot: "species name" or "total".
#'
#' @return ggplot2 object
#'
#' @examples
#' \dontrun{
#' autoplot(sim)
#' }
#' 
#' @name autoplot.troll
#'   
NULL

#' @export
#' @rdname autoplot.troll
setMethod("autoplot", "trollsim", function(
  object, 
  what = "ecosystem",
  variables = NULL, 
  selected_species = "total"
) {
  # dplyr
  species <- iter <- value <- variable <- dbh <- s_name <- NULL
  
  # check parameters
  if(!(what %in% c("final pattern", "ecosystem")))
    stop("what should be final pattern or ecosystem")
  if(object@parameters["_OUTPUT_reduced"] == 0 & !is.null(variables) &
     !all(variables %in% c("abu10", "abu30", "abund", "agb", "ba", "ba10", "gpp",
                           "npp", "rday", "rnight", "rstem")))
    stop('variables should be "abu10", "abu30", "abund", "agb", "ba", "ba10", "gpp", "npp", "rday", "rnight", or "rstem"')
  if(object@parameters["_OUTPUT_reduced"] == 1 & !is.null(variables) &
     !all(variables %in% c("N", "N10", "N30", "BA10", "NPP", "GPP", "AGB")))
    stop('variables should be "N", "N10", "N30", "BA10", "NPP", "GPP", or "AGB"')
  if(object@parameters["_OUTPUT_reduced"] == 0 &
     !all(selected_species %in% c(object@inputs$species$s_name, "total")))
    stop(paste('selected_species should be', paste(object@inputs$species$s_name, collapse = ", "), "or total."))
  if(object@parameters["_OUTPUT_reduced"] == 1 &
     !all(selected_species %in% c("total")))
    stop(paste('selected_species should be total when using reduced outputs'))
  
  # final pattern
  if(object@parameters["_OUTPUT_reduced"] == 0 & what == "final pattern")
    g <- ggplot(object@final_pattern, aes(col, row, size = dbh, col = s_name)) +
      geom_point() +
      theme_bw() +
      scale_size_continuous("DBH (m)", range = c(0.1, 3)) +
      scale_color_viridis(guide = "none", discrete = T) +
      coord_equal() +
      xlab("X") + ylab("Y")
  if(object@parameters["_OUTPUT_reduced"] == 1 & what == "final pattern")
    stop(paste('no final pattern available with reduced outputs'))
  
  # ecosystem
  if(what == "ecosystem")
    g <- object@outputs %>%
      filter(species %in% selected_species) %>%
      mutate(iter = as.numeric(iter / object@parameters["iterperyear"])) %>% 
      melt(c("iter","species")) %>%
      filter(variable %in% variables) %>% 
      ggplot(aes(x =iter, y = value,color = species)) +
      geom_line() +
      facet_wrap(~variable, scales = "free_y") +
      theme_bw() +
      xlab("Time (year)")    
    
  return(g)
})


#' @export
#' @name autoplot.troll
setMethod("autoplot", "trollstack", function(
  object, 
  what = "ecosystem",
  variables = NULL, 
  selected_species = "total"
) {
  # dplyr
  species <- iter <- value <- variable <- dbh <- s_name <- simulation <- NULL
  
  # check parameters
  if(!(what %in% c("final pattern", "ecosystem")))
    stop("what should be final pattern or ecosystem")
  if(object@parameters["_OUTPUT_reduced"] == 0 & !is.null(variables) &
     !all(variables %in% c("abu10", "abu30", "abund", "agb", "ba", "ba10", "gpp",
                           "npp", "rday", "rnight", "rstem")))
    stop('variables should be "abu10", "abu30", "abund", "agb", "ba", "ba10", "gpp", "npp", "rday", "rnight", or "rstem"')
  if(object@parameters["_OUTPUT_reduced"] == 1 & !is.null(variables) &
     !all(variables %in% c("N", "N10", "N30", "BA10", "NPP", "GPP", "AGB")))
    stop('variables should be "N", "N10", "N30", "BA10", "NPP", "GPP", or "AGB"')
  if(object@parameters["_OUTPUT_reduced"] == 0 &
     !all(selected_species %in% c(object@inputs$species$s_name, "total")))
    stop(paste('selected_species should be', paste(object@inputs$species$s_name, collapse = ", "), "or total."))
  if(object@parameters["_OUTPUT_reduced"] == 1 &
     !all(selected_species %in% c("total")))
    stop(paste('selected_species should be total when using reduced outputs'))
  
  # final pattern
  if(object@parameters["_OUTPUT_reduced"] == 0 & what == "final pattern")
    g <- ggplot(object@final_pattern, aes(col, row, size = dbh, col = s_name)) +
      geom_point() +
      theme_bw() +
      scale_size_continuous("DBH (m)", range = c(0.1, 5)) +
      scale_color_viridis(guide = "none", discrete = T) +
      coord_equal() +
      xlab("X") + ylab("Y") +
      facet_wrap(~ simulation)
  if(object@parameters["_OUTPUT_reduced"] == 1 & what == "final pattern")
    stop(paste('no final pattern available with reduced outputs'))
  
  # ecosystem
  if(what == "ecosystem")
    g <- object@outputs %>%
      filter(species %in% selected_species) %>%
      mutate(iter = as.numeric(iter / object@parameters["iterperyear"])) %>% 
      melt(c("iter", "simulation","species")) %>%
      filter(variable %in% variables) %>% 
      ggplot(aes(iter, value, col = species, linetype = simulation)) +
      geom_line() +
      facet_wrap(~ variable , scales = "free_y") +
      theme_bw() +
      xlab("Time (year)")
  
  return(g)
})
