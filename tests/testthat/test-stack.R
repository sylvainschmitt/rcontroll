test_that("stack", {
  data("TROLLv3_species")
  data("TROLLv3_climatedaytime12")
  data("TROLLv3_daytimevar")
  TROLLv3_input_stack <- generate_parameters(rows = 100, cols = 100,
                                             iterperyear = 12, nbiter = 4) %>% 
    mutate(simulation = list(c("seed50000", "seed500"))) %>% 
    tidyr::unnest(simulation)
  TROLLv3_input_stack[62,2] <- 500 
  sims <- stack(name = "test3", 
               simulations = c("seed50000", "seed500"),
               global = TROLLv3_input_stack,
               species = TROLLv3_species,
               climate = TROLLv3_climatedaytime12,
               daily = TROLLv3_daytimevar,
               verbose = F,
               cores = 2)
  expect_true(is.character(capture.output(show(sims))))
  expect_true(is.character(capture.output(print(sims))))
  expect_true(is.character(capture.output(summary(sims))))
  expect_s4_class(sims, "trollstack")
  expect_s3_class(autoplot(sims, what = "forest"), "ggplot")
  expect_s3_class(autoplot(sims, what = "ecosystem"), "ggplot")
  expect_s3_class(autoplot(sims, what = "species"), "ggplot")
})
