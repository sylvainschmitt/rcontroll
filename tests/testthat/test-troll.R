test_that("troll", {
  data("TROLLv3_input")
  data("TROLLv3_species")
  data("TROLLv3_climatedaytime365")
  data("TROLLv3_daytimevar")
  TROLLv3_input$value[5] <- 10 # iterations
  sim <- troll(
    name = "test",
    global = TROLLv3_input,
    species = TROLLv3_species,
    climate = TROLLv3_climatedaytime365,
    daily = TROLLv3_daytimevar
  )
  expect_s4_class(sim, "trollsim")
})
