#' @title Clean presences
#' @description Cleans presence data
#' @param DF A data frame of presence records
#' @return A data frame of cleaned presence records
#' @importFrom CoordinateCleaner clean_coordinates
#' @export
#'
#' @examples
#' Presences <- SDMWorkflows::GetOccs(Species = c("Abies concolor"), WriteFile = FALSE, limit = 500)
#' Cleaned <- clean_presences(Presences[[1]])

clean_presences <- function(DF){
  Cleaned <- CoordinateCleaner::clean_coordinates(DF,
                                                  tests = c("capitals", "centroids", "equal", "institutions", "outliers","seas", "zeros", "duplicates"), lon = "decimalLongitude", lat = "decimalLatitude")

  Cleaned <- Cleaned[Cleaned$.summary,]
  return(Cleaned)
}
clean_presences <- function(DF){
  Cleaned <- CoordinateCleaner::clean_coordinates(DF,
                                                  tests = c("capitals", "centroids", "equal", "institutions", "outliers","seas", "zeros", "duplicates"), lon = "decimalLongitude", lat = "decimalLatitude")

  Cleaned <- Cleaned[Cleaned$.summary,]
  return(Cleaned)
}

