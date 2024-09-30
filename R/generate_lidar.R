#' Generate lidar parameters
#'
#' `generate_lidar` generate the lidar parameters used in `TROLL` lidar
#' simulation. All parameters have a default value from literature.
#'
#' @param mean_beam_pc num. Mean pulse density (pulses per m2).
#' @param sd_beam_pc num. Standard deviation of pulse density (per m2).
#' @param klaser_pc num. laser attenuation factor.
#' @param transmittance_laser num. Percentage of pulses that continue through
#'   the canopy after a hit.
#' @param iter_pointcloud_generation num. Number of iteration for point cloud
#'   generation.
#'
#' @return A [data.frame)] of lidar simulation parameters.
#'
#' @seealso [troll()], [stack()]
#'
#' @examples
#' generate_lidar(iter_pointcloud_generation = 3600)
#'
#' @export
generate_lidar <- function(mean_beam_pc = 10,
                           sd_beam_pc = 5,
                           klaser_pc = 0.63,
                           transmittance_laser = 0.4,
                           iter_pointcloud_generation = NULL) {
  # check args
  if (!all(unlist(lapply(
    list(
      mean_beam_pc,
      sd_beam_pc,
      klaser_pc,
      transmittance_laser,
      iter_pointcloud_generation
    ),
    class
  )) == "numeric")) {
    stop("parameters should be numeric.")
  }

  data.frame(
    param = c(
      "mean_beam_pc",
      "sd_beam_pc",
      "klaser_pc",
      "transmittance_laser",
      "iter_pointcloud_generation"
    ),
    value = c(
      mean_beam_pc,
      sd_beam_pc,
      klaser_pc,
      transmittance_laser,
      iter_pointcloud_generation
    ),
    description = c(
      "/* mean pulse density (pulses per m2) */",
      "/* sd of pulse density (per m2) */",
      "/* the k constant (reflecting geometric properties of the canopy) */",
      paste(
        "/* percentage of pulses that continue through the canopy",
        "after a hit, multiply with klaser_pc",
        "to obtain an effective klaser */"
      ),
      "/* iteration for point cloud generation */"
    )
  )
}
