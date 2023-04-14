test_that("stack", {
  data("TROLLv4_species")
  data("TROLLv4_climate")
  data("TROLLv4_dailyvar")
  data("TROLLv4_pedology")
  stack_parameters <- generate_parameters(nbiter = 10) %>%
    mutate(simulation = list(c("seed50000", "seed500"))) %>%
    tidyr::unnest(simulation)
  # stack_parameters[which(stack_parameters$param == "Cseedrain")[1],2] <- 500 # nolint
  sims <- stack(
    name = "teststack",
    # path = getwd(), # nolint
    simulations = c("seed50000", "seed500"),
    global = stack_parameters,
    species = TROLLv4_species,
    climate = TROLLv4_climate,
    daily = TROLLv4_dailyvar,
    pedology = TROLLv4_pedology,
    cores = 2,
    load = TRUE,
    verbose = TRUE,
    date = "2004/01/01"
  )
  expect_true(is.character(capture.output(show(sims))))
  expect_true(is.character(capture.output(print(sims))))
  expect_true(is.character(capture.output(summary(sims))))
  expect_s4_class(sims, "trollstack")
  expect_s3_class(autoplot(sims, what = "spatial"), "ggplot")
  expect_s3_class(autoplot(sims, what = "temporal"), "ggplot")
  expect_s3_class(autoplot(sims, what = "distribution"), "ggplot")
  unlink(file.path(getwd(), "teststack"), recursive = TRUE)
})
