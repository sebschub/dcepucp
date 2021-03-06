% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/objects.R
\name{rotated_latitude}
\alias{rotated_latitude}
\title{Rotated latitude}
\usage{
rotated_latitude(grid)
}
\arguments{
\item{grid}{Object of class \code{ugrid}.}
}
\value{
Vector of rotated latitude values.
}
\description{
Calculate rotated latitude values from the \code{startlat_tot}, \code{dlat}
and \code{je_tot} of the input.
}
\examples{
grid <- ugrid(pollat = 40., pollon = -70.6,
              startlat_tot = -0.882, startlon_tot = -0.882,
              ie_tot = 195, je_tot = 195,
              dlat = 0.009, dlon = 0.009,
              ke_uhl = 8, hhl_uhl = c(0, 5, 10, 15, 20, 25, 30, 35, 40),
              n_uclass = 1,
              n_udir = 2, angle_udir = c(0., 90.))
rlat <- rotated_latitude(grid)
}
