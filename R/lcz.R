validate_lcz_parameter <- function(name_class, name_property, value_property) {
  if ((!is.na(lcz_ranges[[name_class]][[name_property]]$max) &
       value_property > lcz_ranges[[name_class]][[name_property]]$max) |
      (!is.na(lcz_ranges[[name_class]][[name_property]]$min) &
       value_property < lcz_ranges[[name_class]][[name_property]]$min) ) {

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

  for (i_class in seq_along(lcz_ranges)) {
    name_class <- names(lcz_ranges)[i_class]

    # all properties of each class
    for (i_property in seq_along(lcz_ranges[[name_class]])) {

      name_property <- names(lcz_ranges[[name_class]])[i_property]
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
  for (i_class in seq_along(lcz_ranges)) {

    name_class <- names(lcz_ranges)[i_class]
    cat(name_class)
    cat("\n")

    # all properties of each class
    for (i_property in seq_along(lcz_ranges[[i_class]])) {

      name_property <- names(lcz_ranges[[i_class]])[i_property]
      cat(paste0("  ", name_property, ": ", x[[name_class]][[name_property]], " "))

      # print ranges
      if (is.na(lcz_ranges[[i_class]][[i_property]]$min)) {
        cat(paste0("(< ", lcz_ranges[[i_class]][[i_property]]$max, ")"))
      } else if (is.na(lcz_ranges[[i_class]][[i_property]]$max)) {
        cat(paste0("(> ", lcz_ranges[[i_class]][[i_property]]$min, ")"))
      } else {
        cat(paste0("(", lcz_ranges[[i_class]][[i_property]]$min,
                   " - ", lcz_ranges[[i_class]][[i_property]]$max, ")"))
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
#' lcz <- set_lcz_parameter(lcz_default, "Compact high-rise", h2w = 3)
#' lcz
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


upar_lcz_dominant_class <- function(grid, lcz, dominant_class, fr_urb) {

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

      if (!is.na(dominant_class[i,j])) {

        h2w[i, j] <- lcz[[ dominant_class[i,j] ]]$h2w
        fr_build[i, j] <- lcz[[ dominant_class[i,j] ]]$fr_build
        fr_street[i, j] <- lcz[[ dominant_class[i,j] ]]$fr_street
        height[i, j] <- lcz[[ dominant_class[i,j] ]]$height

        if (lcz[[ dominant_class[i,j] ]]$height > max(grid$hhl_uhl))
          stop("Grid too low for dominant class \"", dominant_class[i,j], "\".")

        height_index <- which.min(abs(grid$hhl_uhl - lcz[[ dominant_class[i,j] ]]$height))

        fr_roof[i, j, height_index, , 1] <- 1.
      }
    }
  }

  for (id in 1:grid$n_udir) {
    w_street[ , , id, 1] <- height / h2w
    w_build[ , , id, 1] <- fr_build / fr_street * w_street[ , , id, 1]
  }

  upar(grid, fr_urb, fr_uclass, fr_udir, fr_roof, w_street, w_build)

}


#' Generate DCEP paramters from LCZ maps
#'
#' The parameters can be derived either from a map of the dominant class or a
#' list of fractions of local climate zones.
#'
#' @param grid Grid specifications, object of class \code{ugrid}.
#' @param lcz LCZ parameters, object of class \code{lcz}.
#' @param dominant_class Map of dominant class of each grid cell.
#' @param class_fractions List of maps, each describing the fraction of urban
#'   local climate zones.
#'
#' @return An object of class \code{upar}.
#' @details Either an argument for \code{dominent_class} or for
#'   \code{class_fraction} has to be supplied. Currently, only one urban class
#'   is supported.
#'
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
#' up <- upar_lcz(berlin_grid, lcz_default, class_fractions = list("Open low-rise" = array(1, c(4,5))))
upar_lcz <- function(grid, lcz, dominant_class = NULL, class_fractions = NULL) {

  stopifnot(class(grid) == "ugrid")
  stopifnot(grid$n_uclass == 1)
  stopifnot(xor(is.null(dominant_class), is.null(class_fractions)))

  fr_urb <- array(0., dim = c(grid$ie_tot, grid$je_tot))

  if (!is.null(class_fractions)) {
    stopifnot(class(class_fractions) == "list")
    # check if dimensions are ok
    for (i_lcz in seq_along(class_fractions)) {
    #  stopifnot_fraction_or_na(lcz_maps[[i_lcz]])
      stopifnot(dim(class_fractions[[i_lcz]]) == c(grid$ie_tot, grid$je_tot))
    }
    class_fractions <- simplify2array(class_fractions)

    stopifnot(all( apply(class_fractions, c(1,2), mean, na.rm = TRUE) <= 1.))

    lcz_names <- dimnames(class_fractions)[[3]]

    dominant_class <- array(NA_integer_, dim = c(grid$ie_tot, grid$je_tot))

    for (i in 1:grid$ie_tot) {
      for (j in 1:grid$je_tot) {

        # fr_urb based on average of all classes
        for (lcz_i in seq_along(lcz_names)) {
          if (!is.na(class_fractions[i,j, lcz_i])) {
            fr_urb[i,j] <- fr_urb[i,j] +
              class_fractions[i,j, lcz_i]*(lcz[[lcz_names[lcz_i]]]$fr_build + lcz[[lcz_names[lcz_i]]]$fr_street)
          }
        }
        # derive dominant class
        dominant_class[i, j] <- which(names(lcz) == lcz_names[which.max(class_fractions[i, j, ])])
      }
    }
  } else if (!is.null(class_fractions)) {

    for (i in 1:grid$ie_tot) {
      for (j in 1:grid$je_tot) {
        # fr_urb based on given class
        if (!is.na(class[i,j])) {
          fr_urb[i,j] <- lcz[[ class[i,j] ]]$fr_build + lcz[[ class[i,j] ]]$fr_street
        }
      }
    }
  }

  upar_lcz_dominant_class(grid = grid, lcz = lcz,
                          dominant_class = dominant_class, fr_urb = fr_urb)

}
