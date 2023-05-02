#' Get occurrence data
#'
#' This function uses the \code{rgbif} package to get occurrence data from the Global Biodiversity Information Facility (GBIF) API.
#'
#' @param Species A vector containing the species to query.
#' @param WriteFile Logical. If \code{TRUE}, the occurrence data will be written to the \code{Occs} folder. If \code{FALSE}, the occurrence data will be returned in a list.
#' @param continent what contintent are the occurrences downloaded from
#' @param country character, The 2-letter country code (ISO-3166-1) in which the occurrence was recorded.
#' @param limit maximum number of occurrences downloaded
#'
#' @return If \code{WriteFile = TRUE}, this function does not return anything. If \code{WriteFile = FALSE}, a list containing the occurrence data for each species is returned.
#'
#' @export
#'
#' @importFrom rgbif occ_data
#' @importFrom janitor make_clean_names
#' @examples
#' # Get occurrence data for species in FinalSpeciesList
#' \donttest{
#' Presences <- SDMWorkflows::GetOccs(Species = c("Abies concolor", "Canis lupus"), WriteFile = FALSE)
#' }

GetOccs <- function(Species, WriteFile = T, continent = NULL, country = NULL, limit = 10000){
  if (!is.character(Species)) {
    stop("Species argument must be a character vector")
  }
  if (!is.logical(WriteFile)) {
    stop("WriteFile argument must be a logical value")
  }
  if (!is.null(continent) && !is.character(continent)) {
    stop("Continent argument must be a character string")
  }
  if (!is.null(country) && !is.character(country)) {
    stop("country argument must be a character string")
  }
  if (!is.numeric(limit)) {
    stop("Limit argument must be a numeric value")
  }
  if(WriteFile == T){
    dir.create("Occs")
  } else if(WriteFile == F){
    Presences <- list()
  }
  for(i in 1:length(Species)){
    message(paste("Starting species", i))
    Occs <- tryCatch(
      expr = {rgbif::occ_data(scientificName = Species[i],
                                 hasCoordinate = T,
                                 continent = continent,
                                 country = country,
                                 hasGeospatialIssue=FALSE,
                                 limit = limit)},
      error = function(e) {
        message(paste("Error for species", Species[i], Sys.time()))
        NULL
      })
    message(paste(i, "of", length(Species), "ready!", Sys.time()))
    if(WriteFile == TRUE){
      try({saveRDS(Occs$data, paste0("Occs/",janitor::make_clean_names(unique(Species[i])),".rds"))})
    }else if(WriteFile == FALSE){
      Presences[[i]] <- Occs$data
    }
    rm(Occs)
    gc()
  }
  if(WriteFile == F){
    return(Presences)
  }
}

