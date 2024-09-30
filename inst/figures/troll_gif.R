library(rcontroll)
data("TROLLv3_output")
gifs <- autogif(
  name = "dynamic",
  variables = "species",
  global = update_parameters(TROLLv3_output,
    nbiter = 12 * 200,
    extent_visual = 200
  ),
  species = TROLLv3_output@inputs$species,
  climate = TROLLv3_output@inputs$climate,
  daily = TROLLv3_output@inputs$daily,
  forest = get_forest(TROLLv3_output),
  verbose = FALSE
)
gganimate::anim_save(
  animation = gifs$species,
  filename = "inst/figures/troll.gif",
  nframes = 200, fps = 30
)
