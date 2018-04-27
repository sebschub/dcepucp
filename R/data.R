#' Heights distribution of buildings in Berlin
#'
#' Height distribution of buildings in Berlin. The grid is defined in
#' \link{berlin_grid}.
#'
#' @format An array with dimensions "rlon", "rlat", "uheight1", "udir" and
#' "uclass" (see \link{ugrid}). In this example, only one urban class is considered.
#'
#' @source From \href{http://doi.org/10.1127/0941-2948/2013/0393}{Schubert and
#'   Grossman-Clarke 2013}.
"berlin_fr_roof"

#' Fraction of urban classes in Berlin
#'
#' Fraction of different urban classes in Berlin. The grid is defined in
#' \link{berlin_grid}. Since only one urban class is considered in this example,
#' the value is 1 where urban values are defined.
#'
#' @format An array with dimensions "rlon", "rlat"  and "uclass" (see
#'   \link{ugrid}). In this example, only one urban class is considered.
#'
#' @source From \href{http://doi.org/10.1127/0941-2948/2013/0393}{Schubert and
#'   Grossman-Clarke 2013}.
"berlin_fr_uclass"

#' Fraction of street directions in Berlin
#'
#' Fractions of different street directions in Berlin. The grid is defined in
#' \link{berlin_grid}.
#'
#' @format An array with dimensions "rlon", "rlat", "udir" and "uclass" (see
#'   \link{ugrid}). In this example, only one urban class is considered.
#'
#' @source From \href{http://doi.org/10.1127/0941-2948/2013/0393}{Schubert and
#'   Grossman-Clarke 2013}.
"berlin_fr_udir"

#' Urban fraction in Berlin
#'
#' Urban fraction of a grid cell in Berlin. The grid is defined in
#' \link{berlin_grid}.
#'
#' @format An array with dimensions "rlon" and "rlat" (see \link{ugrid}).
#'
#' @source From \href{http://doi.org/10.1127/0941-2948/2013/0393}{Schubert and
#'   Grossman-Clarke 2013}.
"berlin_fr_urb"

#' Building width in Berlin
#'
#' Building width in Berlin. The grid is defined in \link{berlin_grid}.
#'
#' @format An array with dimensions "rlon", "rlat", "udir" and "uclass" (see
#'   \link{ugrid}). In this example, only one urban class is considered.
#'
#' @source From \href{http://doi.org/10.1127/0941-2948/2013/0393}{Schubert and
#'   Grossman-Clarke 2013}.
"berlin_w_build"

#' Street width in Berlin
#'
#' Street width in Berlin. The grid is defined in \link{berlin_grid}.
#'
#' @format An array with dimensions "rlon", "rlat", "udir" and "uclass" (see
#'   \link{ugrid}). In this example, only one urban class is considered.
#'
#' @source From \href{http://doi.org/10.1127/0941-2948/2013/0393}{Schubert and
#'   Grossman-Clarke 2013}.
"berlin_w_street"

#' Used grid for the parameters of Berlin
#'
#' Urban grid used with urban Berlin files.
#'
#' @format Object of class \code{ugrid} (see \link{ugrid}).
#'
#' @source From \href{http://doi.org/10.1127/0941-2948/2013/0393}{Schubert and
#'   Grossman-Clarke 2013}.
"berlin_grid"

#' Default LCZ parameter
#'
#' Default Local Climate Zones parameter.
#'
#' @format Object of class \code{lcz}.
"lcz_default"