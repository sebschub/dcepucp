context("NetCDF output")


ucp <- upar(berlin_grid, fr_urb = berlin_fr_urb,
            fr_uclass = berlin_fr_uclass, fr_udir = berlin_fr_udir,
            fr_roof = berlin_fr_roof, w_street = berlin_w_street,
            w_build = berlin_w_build)
upar2nc("test.nc", ucp)

test_that("upar2nc", {
  expect_true(file.exists("test.nc"))
})
