library(RNetCDF)
library(devtools)
library(dcepucp)

nc <- open.nc("berlin0.009.nc")
berlin_fr_roof <- var.get.nc(nc, "FR_ROOF", collapse = FALSE)
berlin_fr_uclass <- var.get.nc(nc, "FR_UCLASS", collapse = FALSE)
berlin_fr_udir <- var.get.nc(nc, "FR_UDIR", collapse = FALSE)
berlin_fr_urb <- var.get.nc(nc, "FR_URB", collapse = FALSE)
berlin_w_build <- var.get.nc(nc, "W_BUILD", collapse = FALSE)
berlin_w_street <- var.get.nc(nc, "W_STREET", collapse = FALSE)

berlin_rlon <- var.get.nc(nc, "rlon", collapse = FALSE)
berlin_rlat <- var.get.nc(nc, "rlat", collapse = FALSE)
berlin_uheight1 <- var.get.nc(nc, "uheight1", collapse = FALSE)
berlin_udir <- var.get.nc(nc, "udir", collapse = FALSE)

berlin_uclass <- dim.inq.nc(nc, "uclass")
close.nc(nc)

berlin_grid <- ugrid(pollat = 37.483, pollon = -166.6,
                     startlat_tot = round(berlin_rlat[1], 4),
                     startlon_tot = round(berlin_rlon[1], 4),
                     ie_tot = length(berlin_rlon), je_tot = length(berlin_rlat),
                     dlat = round(berlin_rlat[2] - berlin_rlat[1], 4),
                     dlon = round(berlin_rlon[2] - berlin_rlon[1], 4),
                     ke_uhl = length(berlin_uheight1) - 1,
                     hhl_uhl = as.vector(berlin_uheight1),
                     n_uclass = berlin_uclass$length,
                     n_udir = length(berlin_udir),
                     angle_udir = as.vector(berlin_udir)
                     )

use_data(berlin_fr_roof, berlin_fr_uclass, berlin_fr_udir, berlin_fr_urb,
         berlin_w_build, berlin_w_street, berlin_grid,
         overwrite = TRUE)
