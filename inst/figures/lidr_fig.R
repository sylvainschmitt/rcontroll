suppressMessages(library(rcontroll))
data("TROLLv3_output")
sim <- troll(name = "test",
             global = update_parameters(TROLLv3_output, nbiter = 12*1),
             species = TROLLv3_output@inputs$species,
             climate = TROLLv3_output@inputs$climate,
             daily = TROLLv3_output@inputs$daily,
             forest = get_forest(TROLLv3_output),
             lidar = generate_lidar(mean_beam_pc = 100, iter_pointcloud_generation = 12),
             verbose = FALSE)
lidR::plot(sim@las[[1]], bg = "white", axis = TRUE, size = 1)
# currently manual capture
