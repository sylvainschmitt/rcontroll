library(rcontroll)

data("TROLLv3_species")
data("TROLLv3_climatedaytime12")
data("TROLLv3_daytimevar")
data("TROLLv3_output")

sim <- troll(
  name = "test",
  global = generate_parameters(
    cols = 100, rows = 100,
    iterperyear = 12, nbiter = 12 * 1
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE
)
print("Done !")

rcontroll::troll(name = "test",global = rcontroll::generate_parameters(cols = 100, rows = 100,iterperyear = 12, nbiter = 12 * 1),species = rcontroll::TROLLv3_species,climate = rcontroll::TROLLv3_climatedaytime12,daily = rcontroll::TROLLv3_daytimevar,verbose = TRUE)