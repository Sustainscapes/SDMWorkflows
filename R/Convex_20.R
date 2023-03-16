#' Convex_20
#'
#' Creates a 20% expanded convex hull from a set of coordinates.
#'
#' @param DF The dataframe containing the coordinates.
#' @param lon The name of the longitude column in the dataframe.
#' @param lat The name of the latitude column in the dataframe.
#' @param proj The projection of the coordinates.
#' @return A polygon representing the expanded convex hull by 20%.
#' @importFrom terra vect crds convHull geom centroids
#' @importFrom dplyr select
#' @export
#'
#' @examples
#'
#' DF <- data.frame(decimalLongitude =
#'       c(23.978543, 23.785003, 11.485,  -2.054027, 12.9069),
#'                  decimalLatitude =
#'        c(38.088876, 60.238213, 48.165, 53.33939, 56.80782))
#'
#' Convex_20(DF, lon = "decimalLongitude", lat = "decimalLatitude",
#' proj = "+proj=longlat +datum=WGS84 +no_defs")
#'

Convex_20 <- function(DF, lon = "decimalLongitude", lat = "decimalLatitude", proj = "+proj=longlat +datum=WGS84 +no_defs"){
  x <- y <- NULL
  SppOccur_TV <- terra::vect(DF, crs=proj, geom = c(lon, lat))

  # Then I get the coordinates as a dataframe for later use.
  occs <- as.data.frame(terra::crds(SppOccur_TV))
  colnames(occs) <- c("Longitude", "Latitude")

  # Here I generate a minimum convex hull
  SppConvexTerra <- terra::convHull(SppOccur_TV)

  ncg <- terra::geom(SppConvexTerra)|> as.data.frame() |> dplyr::select(x,y)
  # Then I extract the geometry of it.

  # And the centroid.
  cntrd <- terra::centroids(SppConvexTerra) |>
    terra::geom() |> as.data.frame() |> dplyr::select(x,y)

  # Finally, I expand the convex hull by 20%.
  ncg2 <- ncg

  ncg2$x <- (ncg$x - cntrd$x)*1.2 + cntrd$x
  ncg2$y <- (ncg$y - cntrd$y)*1.2 + cntrd$y



  # And get it back as a polygon.
  ncg2 <- terra::vect(as.matrix(ncg2), crs=proj, type = "polygon")
}

