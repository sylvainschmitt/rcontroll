#' @include troll.R
#' @import ggplot2
#' @import viridis
#' @importFrom gganimate transition_time
#' @importFrom vroom vroom
NULL

#' Create an animation from a TROLL simulation
#'
#' `autogif()` uses `ggplot2` and `gganimate` to render an animation from a
#' TROLL simulation. The animation can include a vertical cut in the forest
#' structure along the X-axis highlighting either tree species (`variables =
#' 'species'`), either tree height relative to their maximum height (`variables
#' = 'height_ct'`), or tree carbon acquisition with net over growth primary
#' productivity (`variables = 'npp_gpp'`). The animation can also include a top
#' view of the canopy representing either canopy trees height (`variables =
#' 'height'`) or total leaf area index per pixel (`variables = 'lai'`).
#'
#' @param name char. Model name (if NULL timestamp).
#' @param path char. Path to save the simulation outputs, the default is null
#'   corresponding to a simulation in memory without saved intermediary files
#'   (based on temporary files from [option.rcontroll]).
#' @param variables char. Variables to build as a gif among 'species',
#'   'height_ct', 'npp_gpp', 'height', or 'lai'.
#' @param global df. Global parameters (e.g. [TROLLv3_input] or using
#'   [generate_parameters()]).
#' @param species df. Species parameters (e.g. [TROLLv3_species]).
#' @param climate df. Climate parameters (e.g. [TROLLv3_climatedaytime12]).
#' @param daily df. Daily variation parameters (e.g. [TROLLv3_daytimevar]).
#' @param forest df. TROLL with forest input, if null starts from an empty grid
#'   (default NULL) (e.g. using [TROLLv3_output] with [get_forest()]).
#' @param verbose bool. Show TROLL outputs in the console.
#' @param overwrite bool. Overwrite previous outputs.
#' @param thin int. Vector of integers corresponding to the iterations to be
#'   kept to reduce output size, default is NULL and corresponds to no thinning.
#'
#' @return A list of `gganimate` objects corresponding to chosen outputs.
#'
#' @seealso [autoplot,trollsim-method]
#'
#' @examples
#' \dontrun{
#' data("TROLLv3_species")
#' data("TROLLv3_climatedaytime12")
#' data("TROLLv3_daytimevar")
#' autogif(
#'   name = "test", global = generate_parameters(
#'     cols = 100, rows = 100,
#'     iterperyear = 12, nbiter = 12 * 100,
#'     extent_visual = 10
#'   ),
#'   species = TROLLv3_species,
#'   climate = TROLLv3_climatedaytime12,
#'   daily = TROLLv3_daytimevar,
#'   verbose = FALSE
#' )
#' }
#'
#' @export
autogif <- function(name = NULL,
                    path = NULL,
                    variables = c(
                      "species", "height_ct", "npp_gpp",
                      "height", "lai"
                    ),
                    global,
                    species,
                    climate,
                    daily,
                    forest = NULL,
                    verbose = TRUE,
                    overwrite = TRUE,
                    thin = NULL) {
  if (!all(variables %in% c(
    "species", "height_ct",
    "npp_gpp", "height", "lai"
  ))) {
    stop(
      "No valid autogif available for: ", variables,
      ". Please use either 'species', 'height_ct',
         'npp_gpp', 'height', or 'lai'."
    )
  }
  if (global[which(global$param == "_OUTPUT_extended"), 2] == 0) {
    stop("_OUTPUT_extended option should be activated in global
         parameters to produce gif from TROLL
         (generate_parameters(_OUTPUT_extended=1)).")
  }
  if (global[which(global$param == "extent_visual"), 2] == 0) {
    stop("extent_visual option should be non null in global
         parameters to produce gif from TROLL
         (e.g. generate_parameters(extent_visual=100)).")
  }


  tmp <- FALSE
  if (is.null(path)) {
    path <- getOption("rcontroll.tmp")
    tmp <- TRUE
  }

  sim <- troll(
    name = name,
    path = path,
    global = global,
    species = species,
    climate = climate,
    daily = daily,
    forest = forest,
    verbose = verbose,
    overwrite = overwrite,
    thin = thin
  )

  results <- .troll_to_gif(name, path, variables)

  if (tmp) {
    unlink(sim@path, recursive = TRUE, force = TRUE)
    sim@path <- character()
  }

  return(results)
}

.troll_to_gif <- function(name,
                          path,
                          variables) {
  LAI <- height <- height_spikefree <- iter <- NULL # nolint
  ratio_NPP_GPP <- ratio_height_Ct <- sp_lab <- NULL # nolint
  results <- list()
  slice <- vroom(
    file.path(
      path, name,
      paste0(name, "_0_visual_slice.txt")
    ),
    col_types = cols()
  ) %>%
    mutate(sp_lab = as.factor(round(sp_lab))) %>%
    mutate(height = height + 0.5)
  field <- vroom(
    file.path(
      path, name,
      paste0(name, "_0_visual_field.txt")
    ),
    col_types = cols()
  ) %>%
    mutate(col = col + 0.5, row = row + 0.5)

  if ("species" %in% variables) {
    results$species <- ggplot(
      slice,
      aes(
        x = col, y = height,
        fill = sp_lab, alpha = row
      )
    ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_viridis_d(option = "magma") +
      scale_alpha(range = c(0.5, 1.0), guide = "none") +
      xlab("X (m)") +
      ylab("Height (m)") +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      coord_fixed() +
      geom_raster() +
      theme(legend.position = "none") +
      labs(title = "Year: {frame_time/12}") +
      transition_time(iter)
  }


  if ("npp_gpp" %in% variables) {
    results$npp_gpp <- ggplot(
      data = slice,
      aes(
        x = col, y = height,
        fill = ratio_NPP_GPP, alpha = row
      )
    ) +
      scale_fill_viridis_c(expression(frac(NPP, GPP)), direction = -1) +
      scale_alpha(range = c(0.5, 1.0), guide = "none") +
      xlab("X (m)") +
      ylab("Height (m)") +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      coord_fixed() +
      geom_raster() +
      theme(legend.position = "right") +
      labs(title = "Year: {frame_time/12}") +
      transition_time(iter)
  }


  if ("height_ct" %in% variables) {
    results$height_ct <- ggplot(
      data = slice,
      aes(
        x = col, y = height,
        fill = ratio_height_Ct,
        alpha = row
      )
    ) +
      scale_fill_viridis_c(expression(frac(height, Ct))) +
      scale_alpha(range = c(0.5, 1.0), guide = "none") +
      xlab("X (m)") +
      ylab("Height (m)") +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      coord_fixed() +
      geom_raster() +
      theme(legend.position = "right") +
      labs(title = "Year: {frame_time/12}") +
      transition_time(iter)
  }


  if ("height" %in% variables) {
    results$height <- ggplot(
      data = field,
      aes(
        x = col, y = row,
        fill = height_spikefree
      )
    ) +
      scale_fill_viridis_c("height (m)") +
      xlab("X (m)") +
      ylab("Y (m)") +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      coord_fixed() +
      geom_raster() +
      theme(legend.position = "right") +
      labs(title = "Year: {frame_time/12}") +
      transition_time(iter)
  }


  if ("lai" %in% variables) {
    results$lai <- ggplot(data = field, aes(x = col, y = row, fill = LAI)) +
      scale_fill_viridis_c("height (m)") +
      xlab("X (m)") +
      ylab("Y (m)") +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      coord_fixed() +
      geom_raster() +
      theme(legend.position = "right") +
      labs(title = "Year: {frame_time/12}") +
      transition_time(iter)
  }

  return(results)
}
