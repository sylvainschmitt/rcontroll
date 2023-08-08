suppressMessages(library(rcontroll))
suppressMessages(library(lidR))
data("TROLLv3_output")
sim <- troll(name = "lidar", 
             path = "inst/figures/",
             global = update_parameters(TROLLv3_output, nbiter = 12 * 1),
             species = TROLLv3_output@inputs$species,
             climate = TROLLv3_output@inputs$climate,
             daily = TROLLv3_output@inputs$daily,
             forest = get_forest(TROLLv3_output),
             lidar = generate_lidar(mean_beam_pc = 100,
                                    iter_pointcloud_generation = 12),
             verbose = FALSE, 
             load = FALSE)
fake <- readLAS("inst/figures/lidar/lidar_0.las")
plot(fake, bg = "white", axis = TRUE, size = 1, legend = TRUE)
nouragues <- readLAS('inst/figures/nouragues_clipped.las')
plot(nouragues, bg = "white", axis = TRUE, size = 2, legend = TRUE, backend = "rgl")
# currently manual capture
