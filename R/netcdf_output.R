#' Define NetCDF variable with standard attributes
#'
#' The variable is defined and attributes used in COSMO-CLM are added.
#'
#' @param nc NetCDF file object.
#' @param varname String of variable name.
#' @param dimensions Vector of dimension names as strings.
#' @param standard_name String of standard name.
#' @param long_name String of long name.
#' @param units String of units.
#'
#' @import RNetCDF
#' @keywords internal
#' @examples
#' nc <- create.nc("rot_lon.nc")
#' dim.def.nc(nc, dimname = "rlon", dimlength = ucp$grid$ie_tot)
#' nc_def_var(nc, varname = "rlon", dimensions = "rlon",
#'            standard_name = "grid_longitude",
#'            long_name = "rotated longitude",
#'            units = "degrees")
#' close.nc(nc)
nc_def_var <- function(nc, varname, dimensions, standard_name, long_name, units) {
  var.def.nc(nc, varname = varname, vartype = "NC_FLOAT", dimensions = dimensions)
  att.put.nc(nc, variable = varname, name = "standard_name", type = "NC_CHAR",
             standard_name)
  att.put.nc(nc, variable = varname, name = "long_name", type = "NC_CHAR",
             long_name)
  att.put.nc(nc, variable = varname, name = "units", type = "NC_CHAR",
             units)
  att.put.nc(nc, variable = varname, name = "_FillValue", type = "NC_FLOAT",
             -1.e+20)

}

#' Save urban parameters into NetCDF
#'
#' An \code{upar} object is stored in a NetCDF file. This file includes all
#' variables of the input object with a standard set of NetCDF attributes.
#' Information about the rotated pole are included as well.
#'
#' @param file String of output NetCDF file.
#' @param ucp \code{upar} object.
#'
#' @export
#' @import RNetCDF
#' @examples
#' ucps <- upar(berlin_grid, fr_urb = berlin_fr_urb,
#'              fr_uclass = berlin_fr_uclass, fr_udir = berlin_fr_udir,
#'              fr_roof = berlin_fr_roof, w_street = berlin_w_street,
#'              w_build = berlin_w_build)
#' upar2nc("ucp.nc", ucps)
upar2nc <- function(file, ucp) {

  stopifnot(is.character(file))
  stopifnot(class(ucp) == "upar")

  nc <- create.nc(file)

  # definition
  dim.def.nc(nc, dimname = "rlon", dimlength = ucp$grid$ie_tot)
  nc_def_var(nc, varname = "rlon", dimensions = "rlon",
             standard_name = "grid_longitude",
             long_name = "rotated longitude",
             units = "degrees")

  dim.def.nc(nc, dimname = "rlat", dimlength = ucp$grid$je_tot)
  nc_def_var(nc, varname = "rlat", dimensions = "rlat",
             standard_name = "grid_latitude",
             long_name = "rotated latitude",
             units = "degrees")

  dim.def.nc(nc, dimname = "uclass", dimlength = ucp$grid$n_uclass)

  dim.def.nc(nc, dimname = "udir", dimlength = ucp$grid$n_udir)
  nc_def_var(nc, varname = "udir", dimensions = "udir",
             standard_name = "street_direction",
             long_name = "street direction",
             units = "degrees")

  dim.def.nc(nc, dimname = "uheight1", dimlength = ucp$grid$ke_uhl + 1)
  nc_def_var(nc, varname = "uheight1", dimensions = "uheight1",
             standard_name = "urban_height_half_level",
             long_name = "height above surface for half levels",
             units = "m")

  nc_def_var(nc, varname = "lon", dimensions = c("rlon", "rlat"),
             standard_name = "longitude",
             long_name = "longitude",
             units = "degrees_east")
  nc_def_var(nc, varname = "lat", dimensions = c("rlon", "rlat"),
             standard_name = "latitude",
             long_name = "latitude",
             units = "degrees_north")

  var.def.nc(nc, varname = "rotated_pole", vartype = "NC_CHAR", dimensions = NA)
  att.put.nc(nc, variable = "rotated_pole", name = "long_name",
             type = "NC_CHAR",
             "coordinates of the rotated North Pole")
  att.put.nc(nc, variable = "rotated_pole", name = "grid_mapping_name",
             type = "NC_CHAR",
             "rotated_latitude_longitude")
  att.put.nc(nc, variable = "rotated_pole", name = "grid_north_pole_latitude",
             type = "NC_FLOAT",
             ucp$grid$pollat)
  att.put.nc(nc, variable = "rotated_pole", name = "grid_north_pole_longitude",
             type = "NC_FLOAT",
             ucp$grid$pollon)

  nc_def_var(nc, varname = "FR_URB", dimensions = c("rlon", "rlat"),
             standard_name = "fraction_urban",
             long_name = "fraction of urban surfaces",
             units = "1")

  nc_def_var(nc, varname = "FR_UCLASS", dimensions = c("rlon", "rlat", "uclass"),
             standard_name = "fraction_urban_classes",
             long_name = "fraction of urban classes",
             units = "1")

  nc_def_var(nc, varname = "FR_UDIR", dimensions = c("rlon", "rlat", "udir", "uclass"),
             standard_name = "fraction_urban_classes",
             long_name = "fraction of urban classes",
             units = "1")

  nc_def_var(nc, varname = "FR_ROOF", dimensions = c("rlon", "rlat", "uheight1", "udir", "uclass"),
             standard_name = "building_height_fraction",
             long_name = "building height fraction",
             units = "1")

  nc_def_var(nc, varname = "W_STREET", dimensions = c("rlon", "rlat", "udir", "uclass"),
             standard_name = "street_width",
             long_name = "street width",
             units = "m")

  nc_def_var(nc, varname = "W_BUILD", dimensions = c("rlon", "rlat", "udir", "uclass"),
             standard_name = "building_width",
             long_name = "building width",
             units = "m")

  # data
  var.put.nc(nc, variable = "rlon",     data = rotated_longitude(ucp$grid))
  var.put.nc(nc, variable = "rlat",     data = rotated_latitude(ucp$grid))
  var.put.nc(nc, variable = "udir",     data = ucp$grid$angle_udir)
  var.put.nc(nc, variable = "uheight1", data = ucp$grid$hhl_uhl)

  var.put.nc(nc, variable = "lon", data = longitude(ucp$grid))
  var.put.nc(nc, variable = "lat", data = latitude(ucp$grid))

  var.put.nc(nc, variable = "FR_URB",    data = ucp$fr_urb)
  var.put.nc(nc, variable = "FR_UCLASS", data = ucp$fr_uclass)
  var.put.nc(nc, variable = "FR_UDIR",   data = ucp$fr_udir)
  var.put.nc(nc, variable = "FR_ROOF",   data = ucp$fr_roof)

  var.put.nc(nc, variable = "W_BUILD",  data = ucp$w_build)
  var.put.nc(nc, variable = "W_STREET", data = ucp$w_street)

  close.nc(nc)
}