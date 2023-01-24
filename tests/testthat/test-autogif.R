test_that("autogif", {
  data("TROLLv3_output")
  gifs <- autogif(
    name = "dynamic",
    global = update_parameters(TROLLv3_output,
                               nbiter = 12 * 10,
                               extent_visual = 10),
    species = TROLLv3_output@inputs$species,
    climate = TROLLv3_output@inputs$climate,
    daily = TROLLv3_output@inputs$daily,
    forest = get_forest(TROLLv3_output),
    verbose = FALSE
  )
  expect_true(lapply(gifs, inherits, "gganim")[[1]])
})
