#' @include trollsim.R
#' @importFrom dplyr filter mutate select
#' @importFrom reshape2 melt
#' @import ggplot2
NULL

#' Function to plot ecosystem metrics from trollsim
#'
#' @param sim trollsim. trollsim to be plotted
#'
#' @return a ggplot object
#'
#' @export
#'
#' @examples
#'
#' NA
plot_ecosystem <- function(sim) {
  species <- iter <- value <- NULL
  if (!inherits(sim, "trollsim")) {
    stop("sim is not a trollsim.")
  }
  pars <- sim@inputs$global$value
  names(pars) <- sim@inputs$global$param
  g <- sim@species_outputs %>%
    filter(species == "total") %>%
    mutate(iter = as.numeric(iter / pars["iterperyear"])) %>%
    select(-species) %>%
    melt("iter") %>%
    ggplot(aes(iter, value)) +
    geom_line() +
    facet_wrap(~variable, scales = "free_y") +
    theme_bw() +
    xlab("Time (year)")
  return(g)
}
