context("LCZ")

test_that("validation", {
  expect_silent(validate_lcz(lcz_default))

  lcz <- lcz_default

  lcz[[3]]$fr_build <- 200
  lcz[[2]]$h2w <- 20000

  expect_warning(validate_lcz(lcz),
                 regexp = "h2w in class \"Compact midrise\" is out of range.")
  expect_warning(validate_lcz(lcz),
                 regexp = "fr_build in class \"Compact low-rise\" is out of range.")


  expect_silent(set_lcz_parameter(lcz_default, "Compact high-rise", h2w = 3))
  lcz2 <- set_lcz_parameter(lcz_default, "Compact high-rise", h2w = 3)
  expect_s3_class(lcz2, "lcz")

})
