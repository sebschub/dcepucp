#' Define an urban grid
#'
#' Function to define an urban grid including COSMO-CLM grid parameters. Some
#' consistency checks are done.
#'
#' @param pollat Geographical latitude of the rotated north pole (in degrees,
#'   north >0); for a non-rotated lat-lon grid set \code{pollat = 90}.
#' @param pollon Geographical longitude of the rotated north pole (in degrees,
#'   east >0); for a non-rotated lat-lon grid set \code{pollon = -180}.
#' @param dlat 'Meridional' (rotated lat-direction) grid spacing (in degrees).
#' @param dlon 'Zonal' (rotated lon-direction) grid spacing (in degrees).
#' @param startlat_tot Latitude of the lower left scalar grid point of the total
#'   domain (in degrees, north >0, rotated coordinates).
#' @param startlon_tot  Longitude of the lower left scalar grid point of the
#'   total domain (in degrees, east >0, rotated coordinates).
#' @param ie_tot Number of gridpoints of the total domain in 'west-east'
#'   direction of the rotated coordinates.
#' @param je_tot Number of gridpoints of the total domain in 'south-north'
#'   direction of the rotated coordinates.
#' @param ke_uhl Number of main urban height levels corresponding to the number
#'   of wall elements.
#' @param hhl_uhl Vector of the height of urban half levels. The \code{ke_uhl+1}
#'   values are the heights of the roof levels.
#' @param n_uclass Number of urban classes within a grid cell.
#' @param n_udir Number considered street orientations for each urban class.
#' @param angle_udir Vector of street orientations relative to the south-north
#'   axis.
#'
#' @return An object of class \code{ugrid}.
#' @export
#'
#' @examples
#' grid <- ugrid(pollat = 40., pollon = -70.6,
#'               startlat_tot = -0.882, startlon_tot = -0.882,
#'               ie_tot = 195, je_tot = 195,
#'               dlat = 0.009, dlon = 0.009,
#'               ke_uhl = 8, hhl_uhl = c(0, 5, 10, 15, 20, 25, 30, 35,40),
#'               n_uclass = 1,
#'               n_udir = 2, angle_udir = c(0., 90.))
ugrid <- function(pollat, pollon,
                  dlat, dlon, startlat_tot, startlon_tot, ie_tot, je_tot,
                  ke_uhl, hhl_uhl,
                  n_uclass = 1, n_udir = 2, angle_udir = c(0., 90.)) {

  stopifnot(pollat <=  90., pollat >=  -90.)
  stopifnot(pollon <= 180. && pollon >= -180.)

  stopifnot(dlat > 0.)
  stopifnot(dlon > 0.)

  stopifnot(ie_tot > 0.)
  stopifnot(je_tot > 0.)

  stopifnot(ke_uhl >= 0.)
  stopifnot(length(hhl_uhl) == ke_uhl + 1, hhl_uhl >= 0., diff(hhl_uhl) > 0.)

  stopifnot(n_uclass >= 0.)
  stopifnot(n_udir >= 0.)
  stopifnot(length(angle_udir) == n_udir, diff(angle_udir) > 0.)

  structure(list(pollat = pollat, pollon = pollon,
                 dlat = dlat, dlon = dlon,
                 startlat_tot = startlat_tot, startlon_tot = startlon_tot,
                 ie_tot = ie_tot, je_tot = je_tot,
                 ke_uhl = ke_uhl, hhl_uhl = hhl_uhl,
                 n_uclass = n_uclass, n_udir = n_udir, angle_udir = angle_udir),
            class = "ugrid")

}

#' Print urban grid information
#'
#' Prints parameters of an urban grid.
#'
#' @param grid Object of class \code{ugrid}.
#'
#' @export
#'
#' @examples
#' grid <- ugrid(pollat = 40., pollon = -70.6,
#'               startlat_tot = -0.882, startlon_tot = -0.882,
#'               ie_tot = 195, je_tot = 195,
#'               dlat = 0.009, dlon = 0.009,
#'               ke_uhl = 8, hhl_uhl = c(0, 5, 10, 15, 20, 25, 30, 35,40),
#'               n_uclass = 1,
#'               n_udir = 2, angle_udir = c(0., 90.))
#' print.ugrid(grid)
#' grid                       # also calls print.ugrid
print.ugrid <- function(grid) {
  stopifnot(class(grid) == "ugrid")

  out1 <- format(c(grid$pollat, grid$pollon))
  cat("   # Rotated pole coordinates\n")
  cat(paste0("   Latitude:  ", out1[2], "\n"))
  cat(paste0("   Longitude: ", out1[1], "\n"))

  out2 <- format(c(grid$dlat, grid$dlon))
  out3 <- format(c(grid$startlat_tot, grid$startlon_tot))
  out4 <- format(c(grid$ie_tot, grid$je_tot))
  cat("   # Horizontal grid\n")
  cat(paste0("   Grid spacing latitude : ", out2[1], "\n"))
  cat(paste0("   Grid spacing longitude: ", out2[2], "\n"))
  cat(paste0("   Start latitude : ", out3[1], "\n"))
  cat(paste0("   Start longitude: ", out3[2], "\n"))
  cat(paste0("   Grid cells latitude : ", out4[1], "\n"))
  cat(paste0("   Grid cells longitude: ", out4[2], "\n"))

  cat("   # Urban height levels\n")
  cat(paste0("   Number of urban main levels: ", grid$ke_uhl, "\n"))
  cat(paste0("   Height of half levels      : ", paste(grid$hhl_uhl, collapse = " "), "\n"))

  out5 <- format(c(grid$n_uclass, grid$n_udir))
  cat("   # Other urban dimensions\n")
  cat(paste0("   Number of urban classes    : ", out5[1], "\n"))
  cat(paste0("   Number of street directions: ", out5[2], "\n"))
  cat(paste0("   Angles of street direction : ", paste(grid$angle_udir, collapse = " "), "\n"))
}

#' Rotated latitude
#'
#' Calculate rotated latitude values from the \code{startlat_tot}, \code{dlat}
#' and \code{je_tot} of the input.
#'
#' @param grid Object of class \code{ugrid}.
#'
#' @return Vector of rotated latitude values.
#' @export
#'
#' @examples
#' grid <- ugrid(pollat = 40., pollon = -70.6,
#'               startlat_tot = -0.882, startlon_tot = -0.882,
#'               ie_tot = 195, je_tot = 195,
#'               dlat = 0.009, dlon = 0.009,
#'               ke_uhl = 8, hhl_uhl = c(0, 5, 10, 15, 20, 25, 30, 35, 40),
#'               n_uclass = 1,
#'               n_udir = 2, angle_udir = c(0., 90.))
#' rlat <- rotated_latitude(grid)
rotated_latitude <- function(grid) {
  stopifnot(class(grid) == "ugrid")
  grid$startlat_tot + grid$dlat * seq(0, grid$je_tot - 1)
}

#' Rotated longitude
#'
#' Calculate rotated longitude values from the \code{startlon_tot}, \code{dlon}
#' and \code{ie_tot} of the input.
#'
#' @param Object of class \code{ugrid}.
#'
#' @return Vector of rotated longitude values.
#' @export
#'
#' @examples
#' grid <- ugrid(pollat = 40., pollon = -70.6,
#'               startlat_tot = -0.882, startlon_tot = -0.882,
#'               ie_tot = 195, je_tot = 195,
#'               dlat = 0.009, dlon = 0.009,
#'               ke_uhl = 8, hhl_uhl = c(0, 5, 10, 15, 20, 25, 30, 35, 40),
#'               n_uclass = 1,
#'               n_udir = 2, angle_udir = c(0., 90.))
#' rlon <- rotated_longitude(grid)
rotated_longitude <- function(grid) {
  stopifnot(class(grid) == "ugrid")
  grid$startlon_tot + grid$dlon * seq(0, grid$ie_tot - 1)
}

sind <- function(x) sinpi(x/180)
cosd <- function(x) cospi(x/180)
tand <- function(x) tanpi(x/180)
asind <- function(x) asin(x) * 180/pi
atan2d <- function(x1, x2) atan2(x1, x2) * 180/pi

#' Geographical latitude
#'
#' Calculate the geographical latitude values for the input grid.
#'
#' @param grid Object of class \code{ugrid}.
#'
#' @return Two-dimension array of the geographical latitude.
#' @export
#'
#' @examples
#' grid <- ugrid(pollat = 40., pollon = -70.6,
#'               startlat_tot = -0.882, startlon_tot = -0.882,
#'               ie_tot = 195, je_tot = 195,
#'               dlat = 0.009, dlon = 0.009,
#'               ke_uhl = 8, hhl_uhl = c(0, 5, 10, 15, 20, 25, 30, 35,40),
#'               n_uclass = 1,
#'               n_udir = 2, angle_udir = c(0., 90.))
#' lat <- latitude(grid)
latitude <- function(grid) {
  stopifnot(class(grid) == "ugrid")

  rlon <- rotated_longitude(grid)
  rlat <- rotated_latitude(grid)

  rlon <- ifelse(rlon > 180., rlon - 360., rlon)

  asind(
    cosd(grid$pollat) * cosd(rlon)           %o% cosd(rlat) +
    sind(grid$pollat) * rep(1., grid$ie_tot) %o% sind(rlat)
  )

}

#' Geographical longitude
#'
#' Calculate the geographical longitude values for the input grid.
#'
#' @param grid Object of class \code{ugrid}.
#'
#' @return Two-dimension array of the geographical longitude.
#' @export
#'
#' @examples
#' grid <- ugrid(pollat = 40., pollon = -70.6,
#'               startlat_tot = -0.882, startlon_tot = -0.882,
#'               ie_tot = 195, je_tot = 195,
#'               dlat = 0.009, dlon = 0.009,
#'               ke_uhl = 8, hhl_uhl = c(0, 5, 10, 15, 20, 25, 30, 35,40),
#'               n_uclass = 1,
#'               n_udir = 2, angle_udir = c(0., 90.))
#' lon <- longitude(grid)
longitude <- function(grid) {
  stopifnot(class(grid) == "ugrid")

  rlon <- rotated_longitude(grid)
  rlat <- rotated_latitude(grid)

  rlon <- ifelse(rlon > 180., rlon - 360., rlon)

  arg1 <- sind(grid$pollon) *
    (-sind(grid$pollat) * cosd(rlon)           %o% cosd(rlat) +
      cosd(grid$pollat) * rep(1., grid$ie_tot) %o% sind(rlat)
     ) -
    cosd(grid$pollon)   * sind(rlon)           %o% cosd(rlat)
  arg2 <- cosd(grid$pollon) *
    (-sind(grid$pollat) * cosd(rlon)           %o% cosd(rlat) +
      cosd(grid$pollat) * rep(1., grid$ie_tot) %o% sind(rlat)
     ) +
    sind(grid$pollon)   * sind(rlon) %o% cosd(rlat)

  ifelse(arg2 == 0.0, 1.e-20, arg2)

  atan2d(arg1, arg2)

}

stopifnot_fraction_or_na <- function(x) {
  stopifnot(is.na(x) | (x >= 0. & x <= 1.))
}

stopifnot_nonnegative_or_na <- function(x) {
  stopifnot(is.na(x) | x >= 0)
}

stopifnot_small_or_na <- function(x) {
  stopifnot(is.na(x) | abs(x) < 1.e13)
}

#' Define a set of urban parameters
#'
#' Function to define a set of urban parameters. This includes building and
#' street properties as well as the urban grid. Some consistency checks are
#' done.
#'
#' @param grid Object of class \code{ugrid}.
#' @param fr_urb Fraction of urban surfaces in a grid cell and, thus, defines
#'   urban grid cells.
#' @param fr_uclass Fraction of each urban class in each urban grid cell. The
#'   sum for each urban grid cell is 1.
#' @param fr_udir Fraction of each urban class and street direction in each grid
#'   cell. The sum for each urban grid cell is 1.
#' @param w_street Street width of each urban class and street direction in each
#'   grid cell.
#' @param w_build Building width of each urban class and street direction in
#'   each grid cell.
#' @param fr_roof Height distribution of each urban class and street direction
#'   in each grid cell.
#'
#' @return An object of class \code{upar}.
#' @export
#'
#' @examples
#' # use Berlin data included in dcepucp
#' ucps <- upar(berlin_grid, fr_urb = berlin_fr_urb,
#'              fr_uclass = berlin_fr_uclass, fr_udir = berlin_fr_udir,
#'              fr_roof = berlin_fr_roof, w_street = berlin_w_street,
#'              w_build = berlin_w_build)
upar <- function(grid,
                 fr_urb, fr_uclass, fr_udir, fr_roof,
                 w_street, w_build
                 ) {
  stopifnot(class(grid) == "ugrid")

  stopifnot(dim(fr_urb) == c(grid$ie_tot, grid$je_tot))
  stopifnot_fraction_or_na(fr_urb)

  stopifnot(dim(fr_uclass) == c(grid$ie_tot, grid$je_tot, grid$n_uclass))
  stopifnot_fraction_or_na(fr_uclass)
  stopifnot_small_or_na(apply(fr_roof, c(1, 2), sum, na.rm = TRUE) - 1.)

  stopifnot(dim(fr_udir) == c(grid$ie_tot, grid$je_tot, grid$n_udir, grid$n_uclass))
  stopifnot_fraction_or_na(fr_udir)
  stopifnot_small_or_na(apply(fr_roof, c(1, 2, 4), sum, na.rm = TRUE) - 1.)

  stopifnot(dim(fr_roof) == c(grid$ie_tot, grid$je_tot, grid$ke_uhl + 1, grid$n_udir, grid$n_uclass))
  stopifnot_fraction_or_na(fr_roof)
  stopifnot_small_or_na(apply(fr_roof, c(1, 2, 4, 5), sum, na.rm = TRUE) - 1.)

  stopifnot(dim(w_street) == c(grid$ie_tot, grid$je_tot, grid$n_udir, grid$n_uclass))
  stopifnot_nonnegative_or_na(w_street)

  stopifnot(dim(w_build) == c(grid$ie_tot, grid$je_tot, grid$n_udir, grid$n_uclass))
  stopifnot_nonnegative_or_na(w_build)


  structure(list(grid = grid,
                 fr_urb = fr_urb, fr_uclass = fr_uclass, fr_udir = fr_udir, fr_roof = fr_roof,
                 w_street = w_street, w_build = w_build
                 ),
            class = "upar")

}

#' Print urban parameters information
#'
#' Prints details of an \code{upar} object.
#'
#' @param ucp Object of class \code{upar}.
#'
#' @export
#'
#' @examples
#' # use Berlin data included in dcepucp
#' ucps <- upar(berlin_grid, fr_urb = berlin_fr_urb,
#'              fr_uclass = berlin_fr_uclass, fr_udir = berlin_fr_udir,
#'              fr_roof = berlin_fr_roof, w_street = berlin_w_street,
#'              w_build = berlin_w_build)
#' print.upar(ucps)
#' ucps                       # also calls print.ucps
print.upar <- function(ucp) {
  stopifnot(class(ucp) == "upar")

  cat("grid\n")
  print(ucp$grid)
  for (i in 2:length(ucp)) {
    cat(paste0(names(ucp)[i]), "\n")
    print(summary(as.vector(ucp[[i]])))
  }
}
