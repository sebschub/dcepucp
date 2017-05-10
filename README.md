dcepucp
=======

Overview
--------

dcepucp is an R package that creates a NetCDF file from urban canopy
parameters as required by DCEP
([Schubert et al. 2012](http://dx.doi.org/10.1007/s10546-012-9728-3))
implemented in [COSMO-CLM](http://www.clm-community.eu/) version 5. In
particular, it provides the functions

  * `ugrid()` to define an urban grid including COSMO-CLM parameters
    and DCEP parameters
  * `upar()` to define the complete set of urban parameters including
    building and street properties as well as grid information
  * `upar2nc()` to save an `upar` object in a NetCDF file

Furthermore, the package includes an exemplary data set of Berlin,
Germany used
in
[Schubert and Grossman-Clarke 2013](http://doi.org/10.1127/0941-2948/2013/0393). The
fields are called `berlin_*` (see below).


Installation
------------

Install the package from github using the devtools package with
``` r
# install.packages("devtools")
devtools::install_github("sebschub/dcepucp")
```
If you receive the error `Installation failed: error in running
command`, try setting `options(unzip = "unzip")` before running `install_github`.


Usage
-----

The following shows the usage of dcepucp using the example of the
Berlin data set:
``` r
# generature urban grid (equivalent to berlin_grid already defined by
# the package
berlin_grid <- ugrid(pollat = 37.483, pollon = -166.6,
                     startlat_tot = -0.882, startlon_tot = -0.882,
                     ie_tot = 195, je_tot = 195,
                     dlat = 0.009, dlon = 0.009,
                     ke_uhl = 13,
                     hhl_uhl = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 80, 100, 140),
                     n_uclass = 1,
                     n_udir = 4, angle_udir = c(-45., 0., 45., 90.))

# define urban parameters
ucp <- upar(berlin_grid, fr_urb = berlin_fr_urb, 
            fr_uclass = berlin_fr_uclass, fr_udir = berlin_fr_udir,
            fr_roof = berlin_fr_roof, w_street = berlin_w_street,
            w_build = berlin_w_build)

# save urban parameters in ucp.nc
upar2nc("ucp.nc", ucp)
```
