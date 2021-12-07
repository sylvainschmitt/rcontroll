test_that("trollsim", {
  expect_s4_class(trollsim(), "trollsim")
  expect_s4_class(trollsimfull(), "trollsimfull")
  expect_s4_class(trollsimreduced(), "trollsimreduced")
  expect_s4_class(trollsimabc(), "trollsimabc")
})
