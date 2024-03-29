test_that("sim", {
  data("TROLLv3_species")
  data("TROLLv3_climatedaytime12")
  data("TROLLv3_daytimevar")
  sim <- troll(
    name = "test",
    global = generate_parameters(
      rows = 100, cols = 100,
      iterperyear = 12, nbiter = 4
    ),
    species = TROLLv3_species,
    climate = TROLLv3_climatedaytime12,
    daily = TROLLv3_daytimevar,
    verbose = FALSE
  )
  expect_true(is.character(capture.output(show(sim))))
  expect_true(is.character(capture.output(print(sim))))
  expect_true(is.character(capture.output(summary(sim))))
  expect_s4_class(sim, "trollsim")
  expect_s3_class(autoplot(sim, what = "spatial"), "ggplot")
  expect_s3_class(autoplot(sim, what = "temporal"), "ggplot")
  expect_s3_class(autoplot(sim, what = "distribution"), "ggplot")
  expect_true(is.character(capture.output(get_log(sim))))
})
