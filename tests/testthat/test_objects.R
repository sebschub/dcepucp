context("Objects")

bgrid <- ugrid(pollat = 37.483, pollon = -166.6,
               startlat_tot = -0.882, startlon_tot = -0.882,
               ie_tot = 195, je_tot = 195,
               dlat = 0.009, dlon = 0.009,
               ke_uhl = 13,
               hhl_uhl = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 80, 100, 140),
               n_uclass = 1,
               n_udir = 4, angle_udir = c(-45., 0., 45., 90.))


test_that("ugrid", {
  expect_equal(bgrid, berlin_grid)
  expect_is(bgrid, "ugrid")
})


blat <- latitude(bgrid)
blon <- longitude(bgrid)

test_that("geographical longitude and latitude", {
  expect_lt(max(blat), 53.4)
  expect_gt(min(blat), 51.6)
  expect_lt(max(blon), 14.9)
  expect_gt(min(blon), 11.9)
})

ucp <- upar(bgrid, fr_urb = berlin_fr_urb,
            fr_uclass = berlin_fr_uclass, fr_udir = berlin_fr_udir,
            fr_roof = berlin_fr_roof, w_street = berlin_w_street,
            w_build = berlin_w_build)

test_that("upar", {
  expect_length(ucp, 7)
  expect_is(ucp, "upar")
})
