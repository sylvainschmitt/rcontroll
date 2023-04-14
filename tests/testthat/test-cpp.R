test_that("cpp", {
  # avoid fake parrallelisation for vscode debugger
  data("TROLLv4_species")
  data("TROLLv4_climate")
  data("TROLLv4_dailyvar")
  data("TROLLv4_pedology")
  sim <- rcontroll:::.troll_child(
    name = "testcpp",
    path = getwd(),
    global = generate_parameters(nbiter = 10),
    species = TROLLv4_species,
    climate = TROLLv4_climate,
    daily = TROLLv4_dailyvar,
    pedology = TROLLv4_pedology,
    load = TRUE,
    date = "2004/01/01"
  )
  expect_s4_class(sim, "trollsim")
  unlink(file.path(getwd(), "testcpp"), recursive = TRUE)
})