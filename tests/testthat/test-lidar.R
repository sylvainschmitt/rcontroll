test_that("lidar", {
  data("TROLLv3_output")
  sim <- troll(
    name = "test",
    global = update_parameters(TROLLv3_output, nbiter = 12 * 1),
    species = TROLLv3_output@inputs$species,
    climate = TROLLv3_output@inputs$climate,
    daily = TROLLv3_output@inputs$daily,
    forest = get_forest(TROLLv3_output),
    lidar = generate_lidar(mean_beam_pc = 10, iter_pointcloud_generation = 11),
    verbose = FALSE
  )
  expect_true(inherits(sim@las[[1]], "LAS"))
  expect_s3_class(autoplot(sim, what = "lidar"), "ggplot")
})
