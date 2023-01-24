test_that("climate", {
  climate <- generate_climate(
    x = -52.75468, 
    y = 4.060414,
    tz = "America/Cayenne",
    era5land_hour = system.file("extdata", "ERA5land_hr_Nouragues_2022.nc", package = "rcontroll"), 
    era5land_month = system.file("extdata", "ERA5land_mth_FG_2021_2022.nc", package = "rcontroll")
  )
  expect_s3_class(climate$daytimevar, "data.frame")
  expect_s3_class(climate$climatedaytime12, "data.frame")
})
