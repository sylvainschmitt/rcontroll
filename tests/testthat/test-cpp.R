test_that("cpp", {
  skip_on_cran()
  
  library(rcontroll)
  library(tidyverse)
  setwd("~/Documents/rcontroll_v4/tests/testthat/")
  filein <- "sim/guyaflux_sampled.tsv"
  n_years <- 2
  data("TROLLv4_species")
  data("TROLLv4_pedology")
  data <- vroom::vroom(filein,
                col_types = list(rainfall = "numeric"))
  # data <- data %>%
  # filter(year(time) %in% (1:n_years + 1000)) # could be added in troll()

  sim <- rcontroll:::.troll_child(
    name = "testcpp",
    path = getwd(),
    global = generate_parameters(nbiter = 365*n_years, NONRANDOM = 0),
    species = TROLLv4_species,
    climate = generate_climate(data),
    daily =  generate_dailyvar(data),
    pedology = TROLLv4_pedology,
    load = FALSE,
    verbose = TRUE,
    overwrite = TRUE
  )
  
  # sim <- rcontroll:::.troll_child(
  #   name = "testcpp",
  #   path = getwd(),
  #   global = vroom::vroom("debug/R1_input_global.txt"),
  #   species = vroom::vroom("debug/R1_input_species.txt"),
  #   climate = vroom::vroom("debug/R1_input_climate.txt"),
  #   daily =  vroom::vroom("debug/R1_input_daily.txt"),
  #   pedology = vroom::vroom("debug/R1_input_pedology.txt"),
  #   load = FALSE,
  #   verbose = TRUE,
  #   overwrite = TRUE
  # )
  
  library(patchwork)
  comp_sims <- function(sims, years) {
    g <- sims %>%
      bind_rows(.id = "sim") %>%
      filter(iter < 365*years) %>%
      ggplot(aes(iter, agb, col = sim)) +
      geom_line() +
      theme_bw() +
      xlim(0, 365*years) +
      sims %>%
      bind_rows(.id = "sim") %>%
      filter(iter < 365*years) %>%
      ggplot(aes(iter, sum1, col = sim)) +
      geom_line() +
      theme_bw() +
      xlim(0, 365*years)
    return(g)
  }
  list(
    bguyaflux = vroom::vroom("testcpp/testcpp_0_sumstats.txt"),
    aref = vroom::vroom("testcppref/testcpp_0_sumstats.txt")
  ) %>% comp_sims(2)
  
  # vroom::vroom("testcpp/testcpp_input_daily.txt") %>% 
  #   filter(DayJulian < 365*2) %>% 
  #   gather(variable, value_0, -DayJulian, -time_numeric) %>% 
  #   left_join(vroom::vroom("testcppbug2/testcpp_input_daily.txt") %>% 
  #               filter(DayJulian < 365*2) %>% 
  #               gather(variable, value_1, -DayJulian, -time_numeric)) %>% 
  #   filter(value_0 != value_1)
  
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
  # verbose = TRUE,
  # date = "2004/01/01"
  # )
  
  # test <- load_output(name = "testcpp", path = "testcpp")
  # rcontroll::autoplot(test, variables = "agb")
  
  expect_s4_class(sim, "trollsim")
  # unlink(file.path(getwd(), "testcpp"), recursive = TRUE)
})
