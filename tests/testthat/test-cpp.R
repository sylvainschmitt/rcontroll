test_that("cpp", {
  skip_on_cran()
  library(rcontroll)
  library(tidyverse)
  
  # avoid fake parrallelisation for vscode debugger
  global_pars <- generate_parameters(nbiter = 10)
  data("TROLLv4_species")
  data("TROLLv4_climate")
  data("TROLLv4_dailyvar")
  data("TROLLv4_pedology")
  data("TROLLv4_output")
  
  # forest <- vroom::vroom("~/Documents/runs/warmup/R1/R1_0_final_pattern_save.txt") %>% 
  #   mutate(g1_save = g1_0) %>% 
  #   mutate(g1_0 = g1) %>% 
  #   mutate(g1 = g1_save) %>% 
  #   select(-g1_save)
  # vroom::vroom_write(forest, "~/Documents/runs/warmup/R1/R1_0_final_pattern.txt")
  
  # spinup <-  load_output(name = "R1",
  #                        path = "~/Documents/runs/warmup/R1/")
  spinup <-  load_output(name = "testcpp",
                         path = "~/Documents/rcontroll_v4/tests/testthat/spinup")
  guyaflux <- vroom::vroom("~/Documents/data/guyaflux/guyaflux_filled.tsv") %>% 
    filter(year(time) == 2014)
  
  sim <- rcontroll:::.troll_child(
    name = "testcpp",
    path = getwd(), # nolint
    global = update_parameters(spinup, nbiter = 31), 
    species = spinup@inputs$species, 
    climate = generate_climate(guyaflux), 
    daily = generate_dailyvar(guyaflux), 
    pedology = spinup@inputs$pedology, 
    forest = get_forest(spinup),
    soil = get_soil(spinup), 
    load = TRUE,
    verbose = TRUE
  )
  # sim <- rcontroll:::.troll_child(
  #   name = "testcpp",
  #   path = getwd(), # nolint
  #   global = global_pars,
  #   species = TROLLv4_species,
  #   climate = TROLLv4_climate,
  #   daily = TROLLv4_dailyvar,
  #   pedology = TROLLv4_pedology,
  #   load = TRUE,
  #   verbose = TRUE,
  #   date = "2004/01/01"
  # )
  expect_s4_class(sim, "trollsim")
  # unlink(file.path(getwd(), "testcpp"), recursive = TRUE)
  
  # spinup <- load_output(name = "testcpp",
  #                       path = "~/Documents/rcontroll_v4/tests/testthat/spinup")
  # spinup2 <- load_output(name = "testcpp",
  #                       path = "~/Documents/rcontroll_v4/tests/testthat/spinup2")
  # sim0 <- load_output(name = "testcpp",
  #                    path = "~/Documents/rcontroll_v4/tests/testthat/testcpp0")
  # sim <- load_output(name = "testcpp",
  #                     path = "~/Documents/rcontroll_v4/tests/testthat/testcpp")
  # 
  # spinup2@ecosystem %>%
  #   mutate(date = as_date("1601-01-01")+iter) %>%
  #   select(-iter) %>%
  #   mutate(sim = "spinup2") %>%
  #   bind_rows(sim0@ecosystem %>%
  #               mutate(date = as_date("1602-01-01")+iter) %>%
  #               select(-iter) %>%
  #               mutate(sim = "test")) %>%
  #   bind_rows(sim@ecosystem %>%
  #               mutate(date = as_date("1602-01-01")+iter) %>%
  #               select(-iter) %>%
  #               mutate(sim = "test2")) %>%
  #   gather(variable, value, -date, -sim) %>%
  #   filter(date > as_date("1601-11-30"), date < as_date("1602-02-01")) %>%
  #   ggplot(aes(date, value)) +
  #   geom_line(aes(col = sim), alpha = 0.5) +
  #   theme_bw() +
  #   facet_wrap(~ variable, scales = "free")
  
  # filter(spinup@forest, iter != -1) %>%
  #   select(-iter, -from_Data, -col, -row, -s_name) %>%
  #   gather(variable, value_pre, -site) %>%
  #   left_join(
  #     filter(sim@forest, iter == -1) %>%
  #       select(-iter, -from_Data, -col, -row, -s_name) %>%
  #       gather(variable, value_post, -site)
  #   ) %>%
  #   mutate(diff = value_post - value_pre) %>%
  #   filter(diff != 0) %>%
  #   group_by(variable) %>%
  #   summarise(N = n())
})
