test_that("cpp", {
  skip_on_cran()
  
  library(rcontroll)
  library(tidyverse)
  setwd("~/Documents/rcontroll_v4/tests/testthat/")
  filein <- "sim/guyaflux_sampled.tsv"
  data("TROLLv4_species")
  data("TROLLv4_pedology")
  data <- vroom::vroom(filein,
                col_types = list(rainfall = "numeric"))
  clim <- generate_climate(data)
  day <- generate_dailyvar(data) %>% 
    mutate(Snet = ifelse(Snet < 0, 0, Snet)) %>% # weird
    mutate(VPD = ifelse(VPD <= 0, 0.0005, VPD)) %>% 
    mutate(WS = ifelse(WS <= 0, 0.0005, WS))
  n <- round(0.1*365)
  sim <- rcontroll:::.troll_child(
    name = "testcpp",
    path = getwd(),
    global = generate_parameters(nbiter = n),
    species = TROLLv4_species,
    climate = clim,
    daily = day,
    pedology = TROLLv4_pedology,
    load = FALSE,
    verbose = TRUE,
    overwrite = TRUE
  )
  # test <- load_output(name = "testcpp", path = "testcpp")
  # autoplot(test)
  
  
  # avoid fake parrallelisation for vscode debugger
  # global_pars <- generate_parameters(nbiter = 10)
  # data("TROLLv4_species")
  # data("TROLLv4_climate")
  data("TROLLv4_dailyvar")
  # data("TROLLv4_pedology")
  # data("TROLLv4_output")
  # sim <- rcontroll:::.troll_child(
  #   name = "testcpp",
  #   path = getwd(), # nolint
  #   global = global_pars,
  #   species = species_pars,
  #   climate = climate_pars,
  #   daily = daily_pars,
  #   pedology = pedology_pars,
    # forest = get_forest(TROLLv4_output), # nolint
    # soil = get_soil(TROLLv4_output), # nolint
    # load = TRUE,
    # verbose = TRUE
    # date = "2004/01/01"
  # )
  # expect_s4_class(sim, "trollsim")
  # unlink(file.path(getwd(), "testcpp"), recursive = TRUE)
})
