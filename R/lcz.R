validate_lcz_parameter <- function(name_class, name_property, value_property) {
  if ((!is.na(dcepucp:::lcz_ranges[[name_class]][[name_property]]$max) &
       value_property > dcepucp:::lcz_ranges[[name_class]][[name_property]]$max) |
      (!is.na(dcepucp:::lcz_ranges[[name_class]][[name_property]]$min) &
       value_property < dcepucp:::lcz_ranges[[name_class]][[name_property]]$min) ) {

    warning(paste0(name_property, " in class \"", name_class, "\" is out of range."))
  }
}

#' Validate a parameter set
#'
#' A Local Climate Zones parameter set is validated against parameter ranges.
#'
#' @param lcz The LCZ parameter set.
#'
#'
#' @export
#'
#' @examples
#' validate_lcz(lcz_default)
validate_lcz <- function(lcz) {

  stopifnot(class(lcz) == "lcz")

  for (i_class in seq_along(dcepucp:::lcz_ranges)) {
    name_class <- names(dcepucp:::lcz_ranges)[i_class]

    # all properties of each class
    for (i_property in seq_along(dcepucp:::lcz_ranges[[name_class]])) {

      name_property <- names(dcepucp:::lcz_ranges[[name_class]])[i_property]
      validate_lcz_parameter(name_class, name_property, lcz[[name_class]][[name_property]])

    }
  }

}

#' Show LCZ class parameter, ranges and validate settings
#'
#' Print the Local Climate Zones parameter, its ranges and validate it.
#'
#' @param x LCZ parameter of class "lcz".
#' @param ... Not used.
#'
#' @export
#'
#' @examples
#' print(lcz_default)
print.lcz <- function(x, ...) {

  stopifnot(class(x) == "lcz")

  # all LCZ classes
  for (i_class in seq_along(dcepucp:::lcz_ranges)) {

    name_class <- names(dcepucp:::lcz_ranges)[i_class]
    cat(name_class)
    cat("\n")

    # all properties of each class
    for (i_property in seq_along(dcepucp:::lcz_ranges[[i_class]])) {

      name_property <- names(dcepucp:::lcz_ranges[[i_class]])[i_property]
      cat(paste0("  ", name_property, ": ", x[[name_class]][[name_property]], " "))

      # print ranges
      if (is.na(dcepucp:::lcz_ranges[[i_class]][[i_property]]$min)) {
        cat(paste0("(< ", dcepucp:::lcz_ranges[[i_class]][[i_property]]$max, ")"))
      } else if (is.na(dcepucp:::lcz_ranges[[i_class]][[i_property]]$max)) {
        cat(paste0("(> ", dcepucp:::lcz_ranges[[i_class]][[i_property]]$min, ")"))
      } else {
        cat(paste0("(", dcepucp:::lcz_ranges[[i_class]][[i_property]]$min,
                   " - ", dcepucp:::lcz_ranges[[i_class]][[i_property]]$max, ")"))
      }
      cat("\n")
    }
  }
  validate_lcz(x)
}


#' Set LCZ parameter
#'
#' Modify one or several parameters of one Local Climate Zones. The values are
#' checked for consistency with the valid LCZ range.
#'
#' @param lcz LCZ parameter, object of class \code{lcz}.
#' @param lcz_name Number or character string of the LCZ.
#' @param h2w The building-height-to-street-width ratio.
#' @param fr_build Fraction of buildings.
#' @param fr_street Fraction of street surfaces.
#' @param height Average building height.
#'
#' @export
#'
#' @examples
#' set_lcz_settings("Compact high-rise", h2w = 3)
#' show_lcz_classes()
set_lcz_parameter <- function(lcz, lcz_name, h2w = NULL, fr_build = NULL, fr_street = NULL,
                             height = NULL) {

  stopifnot(class(lcz) == "lcz")

  # list of all arguments expect first two
  properties <- as.list(environment())[c(-1, -2)]

  # loop over all properties in the list; update if non-null
  for (i_property in seq_along(properties)) {
    if (!is.null(properties[[i_property]])) {

      name_property <- names(properties)[i_property]
      value_property <- properties[[i_property]]

      # check ranges if ranges are defined (non-NA)
      validate_lcz_parameter(name_class = lcz_name, name_property, value_property)
      lcz[[lcz_name]][name_property] <- value_property
    }
  }
  lcz
}

#' Generate DCEP paramters from LCZ maps
#'
#' @param grid Object of class \code{ugrid}.
#' @param lcz Object of class \code{lcz}.
#' @param maps LCZ maps
#' @param mode Mode
#'
#' @return An object of class \code{upar}.
#' @export
#'
#' @examples
#' berlin_grid <- ugrid(pollat = 37.483, pollon = -166.6,
#' startlat_tot = -0.882, startlon_tot = -0.882,
#' ie_tot = 4, je_tot = 5,
#' dlat = 0.009, dlon = 0.009,
#' ke_uhl = 3,
#' hhl_uhl = c(0, 5, 10, 15),
#' n_uclass = 1,
#' n_udir = 4, angle_udir = c(-45., 0., 45., 90.))
#'
#' up <- upar_lcz(berlin_grid, lcz, maps = list("Open low-rise" = array(1, c(4,5))))
upar_lcz <- function(grid, lcz, maps, mode = "dominant_class") {

  stopifnot(class(grid) == "ugrid")

  if (class(maps) == "list") {
    # check if dimensions are ok
    for (i_lcz in seq_along(maps)) {
    #  stopifnot_fraction_or_na(lcz_maps[[i_lcz]])
      stopifnot(dim(maps[[i_lcz]]) == c(grid$ie_tot, grid$je_tot))
    }
    maps <- simplify2array(maps)
  }

  stopifnot(all( apply(maps, c(1,2), mean, na.rm = TRUE) <= 1.))

  stopifnot(mode %in% c("dominant_class", "classes"))

  if (mode == "dominant_class") {
    lcz_names <- dimnames(maps)[[3]]

    fr_urb <- array(0., dim = c(grid$ie_tot, grid$je_tot))
    fr_uclass <- array(1./grid$n_uclass, dim = c(grid$ie_tot, grid$je_tot, grid$n_uclass))
    fr_udir <- array(1./grid$n_udir, dim = c(grid$ie_tot, grid$je_tot, grid$n_udir, grid$n_uclass))
    fr_roof <- array(0., dim = c(grid$ie_tot, grid$je_tot, grid$ke_uhl + 1, grid$n_udir, grid$n_uclass))

    w_street <- array(NA_real_, dim = c(grid$ie_tot, grid$je_tot, grid$n_udir, grid$n_uclass))
    w_build <- array(NA_real_, dim = c(grid$ie_tot, grid$je_tot, grid$n_udir, grid$n_uclass))

    h2w <- array(NA_real_, dim = c(grid$ie_tot, grid$je_tot))
    fr_build <- array(NA_real_, dim = c(grid$ie_tot, grid$je_tot))
    fr_street <- array(NA_real_, dim = c(grid$ie_tot, grid$je_tot))
    height <- array(NA_real_, dim = c(grid$ie_tot, grid$je_tot))

    for (i in 1:grid$ie_tot) {
      for (j in 1:grid$je_tot) {

        for (lcz_i in seq_along(lcz_names)) {
          if (!is.na(maps[i,j, lcz_i])) {
            fr_urb[i,j] <- fr_urb[i,j] +
              maps[i,j, lcz_i]*(lcz[[lcz_names[lcz_i]]]$fr_build + lcz[[lcz_names[lcz_i]]]$fr_street)
          }
        }

        dominant_class <- lcz_names[which.max(maps[i, j, ])]

        h2w[i, j] <- lcz[[dominant_class]]$h2w
        fr_build[i, j] <- lcz[[dominant_class]]$fr_build
        fr_street[i, j] <- lcz[[dominant_class]]$fr_street
        height[i, j] <- lcz[[dominant_class]]$height

        if (lcz[[dominant_class]]$height > max(grid$hhl_uhl))
          stop("Grid too low for dominant class \"", dominant_class, "\".")

        height_index <- which.min(abs(grid$hhl_uhl - lcz[[dominant_class]]$height))

        fr_roof[i, j, height_index, , 1] <- 1.
      }
    }

    for (id in 1:grid$n_udir) {
      w_street[ , , id, 1] <- height / h2w
      w_build[ , , id, 1] <- fr_build / fr_street * w_street[ , , id, 1]
    }

    upar(grid, fr_urb, fr_uclass, fr_udir, fr_roof, w_street, w_build)

  }

}
