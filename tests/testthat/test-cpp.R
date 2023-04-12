test_that("cpp", {
  # avoid fake parrallelisation for vscode debugger
  data("TROLLv3_species")
  data("TROLLv3_climatedaytime12")
  data("TROLLv3_daytimevar")
  sim1 <- rcontroll:::.troll_child(
    name = "test",
    global = generate_parameters(
      rows = 100, cols = 100,
      iterperyear = 12, nbiter = 4
    ),
    species = TROLLv3_species,
    climate = TROLLv3_climatedaytime12,
    daily = TROLLv3_daytimevar
  )
  sim2 <- rcontroll:::.troll_child(
    name = "test",
    global = generate_parameters(
      rows = 100, cols = 100,
      iterperyear = 12, nbiter = 4
    ),
    species = TROLLv3_species,
    climate = TROLLv3_climatedaytime12,
    daily = TROLLv3_daytimevar
  )
  expect_s4_class(sim, "trollsim")
})
