test_that("sim", {
  data("TROLLv4_species")
  data("TROLLv4_climate")
  data("TROLLv4_dailyvar")
  data("TROLLv4_pedology")
  sim <- troll(
    name = "test",
    # path = getwd(), # nolint
    global = generate_parameters(nbiter = 10),
    species = TROLLv4_species,
    climate = TROLLv4_climate,
    daily = TROLLv4_dailyvar,
    pedology = TROLLv4_pedology,
    load = TRUE,
    verbose = TRUE,
    date = "2004/01/01"
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
