test_that("cpp", {
  skip_on_cran()
  
  library(rcontroll)
  library(tidyverse)
  setwd("~/Documents/rcontroll_v4/tests/testthat/")
  filein <- "sim/era_sampled.tsv"
  n_years <- 2
  data("TROLLv4_species")
  data("TROLLv4_pedology")
  data <- vroom::vroom(filein,
                col_types = list(rainfall = "numeric"))
  data <- data %>%
    filter(year(time) %in% (1:n_years + 1000))
  clim <- generate_climate(data)
  day <- generate_dailyvar(data)
  
  sim <- rcontroll:::.troll_child(
    name = "testcpp",
    path = getwd(),
    global = generate_parameters(nbiter = 1095, NONRANDOM = 0),
    species = TROLLv4_species,
    climate = clim,
    daily = day,
    pedology = TROLLv4_pedology,
    load = FALSE,
    verbose = TRUE,
    overwrite = TRUE
  )
  
  # sim <- rcontroll:::.troll_child(
  #   name = "testcpp",
  #   path = getwd(),
  #   global = generate_parameters(nbiter = 1095, NONRANDOM = 0),
  #   species = TROLLv4_species,
  #   climate = TROLLv4_climate,
  #   daily = TROLLv4_dailyvar,
  #   pedology = TROLLv4_pedology,
  #   load = FALSE,
  #   verbose = TRUE,
  #   overwrite = TRUE
  # )
  # Working > testcppref
  
  library(patchwork)
  n <- 2
  list(bnew = vroom::vroom("testcpp/testcpp_0_sumstats.txt"),
       aold = vroom::vroom("~/Documents/alt/troll/test/test_0_sumstats.txt") %>%
         filter(iter < 365*n)) %>%
    bind_rows(.id = "sim") %>%
    ggplot(aes(iter, agb, col = sim)) +
    geom_line() +
    theme_bw() +
    xlim(0, 365*n) +
    list(bnew = vroom::vroom("testcpp/testcpp_0_sumstats.txt"),
         aold = vroom::vroom("~/Documents/alt/troll/test/test_0_sumstats.txt") %>%
           filter(iter < 365*n)) %>%
    bind_rows(.id = "sim") %>%
    ggplot(aes(iter, sum1, col = sim)) +
    geom_line() +
    theme_bw() +
    xlim(0, 365*n)
  
  # avoid fake parrallelisation for vscode debugger
  # global_pars <- generate_parameters(nbiter = 10)
  # data("TROLLv4_species")
  # data("TROLLv4_climate")
  # data("TROLLv4_dailyvar")
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
  # )
  
  # test <- load_output(name = "testcpp", path = "testcpp")
  # rcontroll::autoplot(test, variables = "agb")
  
  expect_s4_class(sim, "trollsim")
  # unlink(file.path(getwd(), "testcpp"), recursive = TRUE)
})
