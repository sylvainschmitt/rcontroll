test_that("climate", {
  era_file <- system.file("extdata",
                          "ERA5land_Paracou_2004.tsv",
                          package = "rcontroll"
  )
  era <- vroom::vroom(era_file)
  expect_s3_class(generate_climate(hourly_data = era), 
                  "data.frame")
  expect_s3_class(generate_dailyvar(hourly_data = era),
                  "data.frame")
})
