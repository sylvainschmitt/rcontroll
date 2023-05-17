test_that("cpp", {
  skip_on_cran()
  
  library(rcontroll)
  library(tidyverse)
  setwd("~/Documents/rcontroll_v4/tests/testthat/")
  filein <- "sim/soil.tsv"
  n_years <- 1
  data("TROLLv4_species")
  data("TROLLv4_climate")
  data("TROLLv4_dailyvar")
  data <- vroom::vroom(filein)
  pedo <- data %>% 
    filter(address == "Piste de Paracou") %>% 
    filter(depth == "0-5cm") %>% 
    select(-address, -depth) %>% 
    pivot_wider(names_from = variable, values_from = value) %>% 
    mutate(layer_thickness = list(c(0.1, 0.23, 0.4, 0.8, 0.97))) %>% 
    unnest() %>% 
    rename(proportion_Silt = silt, 
           proportion_Clay = clay, 
           proportion_Sand = sand, 
           SOC = soc, 
           DBD = bdod,
           pH =phh2o,
           CEC = cec) %>% 
    select(layer_thickness, proportion_Silt, 
           proportion_Clay, proportion_Sand, SOC, DBD, pH, CEC)
  
  sim <- rcontroll:::.troll_child(
    name = "testcpp",
    path = getwd(),
    global = generate_parameters(nbiter = 365*n_years, NONRANDOM = 0),
    species = TROLLv4_species,
    climate = TROLLv4_climate,
    daily = TROLLv4_dailyvar,
    pedology = pedo,
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
  
  # library(rcontroll)
  # library(tidyverse)
  # library(patchwork)
  # setwd("~/Documents/rcontroll_v4/tests/testthat/")
  # comp_sims <- function(sims, years, vars = c("agb", "sum1")) {
  #   sims %>%
  #     bind_rows(.id = "sim") %>%
  #     filter(iter < 365*years) %>%
  #     mutate(date = as_date("0000-01-01") + iter) %>%
  #     gather(variable, value, -sim, -iter, -date) %>%
  #     filter(variable %in% vars) %>%
  #     ggplot(aes(date, value, col = sim)) +
  #     geom_line(alpha = 0.3) +
  #     geom_smooth() +
  #     theme_bw() +
  #     facet_wrap(~ variable, scales = "free_y")
  # }
  # # vroom::vroom("sftp://genologin1.toulouse.inrae.fr/home/sschmitt/work/trollExp/results/simulations/current/10-years/guyaflux/warmup/R1/R1_input_global.txt")
  # list(
  #   cordex = vroom::vroom("sim/cordex.txt"),
  #   cordex_full  = vroom::vroom("sim/cordex_full.txt"),
  #   era = vroom::vroom("sim/era.txt"),
  #   guyaflux = vroom::vroom("sim/guyaflux.txt"),
  #   # ref = vroom::vroom("testcppref/testcpp_0_sumstats.txt")
  #   ref = vroom::vroom("~/Documents/alt/troll/historical_700y/test_0_sumstats.txt")
  # ) %>% comp_sims(600)
  
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
