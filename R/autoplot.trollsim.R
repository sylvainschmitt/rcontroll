#' @include trollsim.R
#' @include trollsimfull.R
#' @include trollsimreduced.R
#' @include trollsimabc.R
#' @import methods
#' @import ggplot2
#' @importFrom dplyr filter mutate select
#' @importFrom reshape2 melt
#' @importFrom viridis scale_color_viridis
NULL

#' Function to plot TROLL outputs
#'
#' @param object trollsim
#' @param what char. "final pattern" or "ecosystem"
#' @param variables char. full outputs: "species", "abu10", "abu30", "abund",
#'   "agb", "ba", "ba10", "gpp", "litterfall", "npp", "rday", "rnight", or
#'   "rstem"; reduced outputs: "N", "N10", "N30", "BA10", "NPP", "GPP", "AGB"
#' @param selected_species char. species name or total
#' @param ... unused argument
#'
#' @return ggplot2 object
#'
#' @examples
#' NA
#'
#' @name autoplot.trollsim
#'   
NULL

#' @export
#' @rdname autoplot.trollsim
setMethod("autoplot", "trollsim", function(object, what, ...) {
  stop("trollsim is a prototype and does not have an autoplot method")
})

#' @export
#' @rdname autoplot.trollsim
setMethod("autoplot", "trollsimfull", 
          function(
            object, 
            what = "ecosystem",
            variables = c("abu10", "abu30", "abund", "agb", "ba", "ba10", "gpp",
                          "litterfall", "npp", "rday", "rnight", "rstem"), 
            selected_species = "total"
          ) {
            # dplyr
            species <- iter <- value <- variable <- dbh <- s_name <- NULL
            
            # check parameters
            if(!(what %in% c("final pattern", "ecosystem")))
              stop("what should be final pattern or ecosystem")
            if(!all(variables %in% c("abu10", "abu30", "abund", "agb", "ba", "ba10", "gpp",
                                     "litterfall", "npp", "rday", "rnight", "rstem")))
              stop('variables should be "abu10", "abu30", "abund", "agb", "ba", "ba10", "gpp", "litterfall", "npp", "rday", "rnight", or "rstem"')
            if(!all(selected_species %in% c(object@inputs$species$s_name, "total")))
              stop(paste('selected_species should be', paste(object@inputs$species$s_name, collapse = ", "), "or total."))
            
            # final pattern
            if(what == "final pattern")
              g <- ggplot(object@final_pattern, aes(col, row, size = dbh, col = s_name)) +
                geom_point() +
                theme_bw() +
                scale_size_continuous("DBH (m)", range = c(0.1, 3)) +
                scale_color_viridis(guide = "none", discrete = T) +
                coord_equal() +
                xlab("X") + ylab("Y")
            
            # ecosystem
            if(what == "ecosystem")
              g <- object@species_outputs %>%
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
#' @rdname autoplot.trollsim
setMethod("autoplot", "trollsimreduced", 
          function(
            object, 
            what = "ecosystem",
            variables = c("N", "N10", "N30", "BA10", "NPP", "GPP", "AGB"), 
            selected_species = "total"
          ) {
            # dplyr
            species <- iter <- value <- variable <- dbh <- s_name <- NULL
            
            # check parameters
            if(!(what %in% c("final pattern", "ecosystem")))
              stop("what should be final pattern or ecosystem")
            if(!all(variables %in% c("N", "N10", "N30", "BA10", "NPP", "GPP", "AGB")))
              stop('variables should be "N", "N10", "N30", "BA10", "NPP", "GPP", or "AGB"')
            
            # final pattern
            if(what == "final pattern")
              g <- ggplot(object@final_pattern, aes(col, row, size = dbh, col = s_name)) +
                geom_point() +
                theme_bw() +
                scale_size_continuous("DBH (m)", range = c(0.1, 3)) +
                scale_color_viridis(guide = "none", discrete = T) +
                coord_equal() +
                xlab("X") + ylab("Y")
            
            # ecosystem
            if(what == "ecosystem")
              g <- object@reduced_outputs %>%
                mutate(iter = as.numeric(iter / object@parameters["iterperyear"])) %>%
                select(-species) %>%
                melt("iter") %>%
                filter(variable %in% variables) %>% 
                ggplot(aes(iter, value)) +
                geom_line() +
                facet_wrap(~variable, scales = "free_y") +
                theme_bw() +
                xlab("Time (year)")    
            
            return(g)
          })

#' @export
#' @rdname autoplot.trollsim
setMethod("autoplot", "trollsimabc", 
          function(
            object, 
            what
          ) {
            stop("No autoplot methods implemented for trollsimabc yet.")
          })



