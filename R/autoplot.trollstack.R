#' @include trollstackfull.R
#' @include trollstackreduced.R
#' @include trollstackabc.R
#' @import methods
#' @import ggplot2
#' @importFrom dplyr filter mutate select
#' @importFrom reshape2 melt
#' @importFrom viridis scale_color_viridis
NULL

#' Function to plot TROLL stack
#'
#' @param object trollsim
#' @param what char. "final pattern" or "ecosystem"
#' @param variables char. full outputs: "species", "abu10", "abu30", "abund",
#'   "agb", "ba", "ba10", "gpp", "litterfall", "npp", "rday", "rnight", or
#'   "rstem"; reduced outputs: "N", "N10", "N30", "BA10", "NPP", "GPP", "AGB"
#' @param species char. species name or total
#' @param ... unused argument
#'
#' @return ggplot2 object
#'
#' @examples
#' NA
#'
#' @name autoplot.trollstack
#'   
NULL

#' @export
#' @rdname autoplot.trollstack
setMethod("autoplot", "trollstackfull", 
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
                scale_size_continuous("DBH (m)", range = c(0.1, 5)) +
                scale_color_viridis(guide = "none", discrete = T) +
                coord_equal() +
                xlab("X") + ylab("Y") +
                facet_wrap(~ simulation)
            
            # ecosystem
            if(what == "ecosystem")
              g <- object@species_outputs %>%
                filter(species %in% selected_species) %>%
                mutate(iter = as.numeric(iter / object@parameters["iterperyear"])) %>%
                select(-species) %>%
                melt(c("iter", "simulation")) %>%
                filter(variable %in% variables) %>% 
                ggplot(aes(iter, value, col = simulation)) +
                geom_line() +
                facet_wrap(~ variable, scales = "free_y") +
                theme_bw() +
                xlab("Time (year)")
            
            return(g)
          })

#' @export
#' @rdname autoplot.trollstack
setMethod("autoplot", "trollstackreduced", 
          function(
            object, 
            what = "ecosystem",
            variables = c("N", "N10", "N30", "BA10", "NPP", "GPP", "AGB"), 
            species = "total"
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
                scale_size_continuous("DBH (m)", range = c(0.1, 5)) +
                scale_color_viridis(guide = "none", discrete = T) +
                coord_equal() +
                xlab("X") + ylab("Y") +
                facet_wrap(~ simulation)
            
            # ecosystem
            if(what == "ecosystem")
              g <- object@reduced_outputs %>%
                mutate(iter = as.numeric(iter / object@parameters["iterperyear"])) %>%
                select(-species) %>%
                melt(c("iter", "simulation")) %>%
                filter(variable %in% variables) %>% 
                ggplot(aes(iter, value, col = simulation)) +
                geom_line() +
                facet_wrap(~variable, scales = "free_y") +
                theme_bw() +
                xlab("Time (year)")
            
            return(g)
          })

#' @export
#' @rdname autoplot.trollstack
setMethod("autoplot", "trollstackabc", 
          function(
            object, 
            what
          ) {
            stop("No autoplot methods implemented for trollstackabc yet.")
          })



