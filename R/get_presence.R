#' Get occurrence data
#'
#' This function uses the \code{rgbif} package to get occurrence data from the Global Biodiversity Information Facility (GBIF) API.
#'
#' @param Species A data frame containing the species to query.
#' @param WriteFile Logical. If \code{TRUE}, the occurrence data will be written to the \code{Occs} folder. If \code{FALSE}, the occurrence data will be returned in a list.
#'
#' @return If \code{WriteFile = TRUE}, this function does not return anything. If \code{WriteFile = FALSE}, a list containing the occurrence data for each species is returned.
#'
#' @export
#'
#' @importFrom rgbif occ_data
#' @importFrom janitor make_clean_names
#' @examples
#' # Get occurrence data for species in FinalSpeciesList
#' Presences <- SDMWorkflows::GetOccs(Species = c("Abies concolor", "Canis lupus"), WriteFile = FALSE)

GetOccs <- function(Species, WriteFile = T, continent = NULL, limit = 10000){
  if(WriteFile == T){
    dir.create("Occs")
  } else if(WriteFile == F){
    Presences <- list()
  }
  for(i in 1:length(Species)){
    Occs <- try({rgbif::occ_data(scientificName = Species[i],
                                 hasCoordinate = T,
                                 continent = continent,
                                 hasGeospatialIssue=FALSE,
                                 limit = limit)})
    message(paste(i, "of", length(Species), "ready!", Sys.time()))
    if(WriteFile == T){
      try({saveRDS(Occs, paste0("Occs/",janitor::make_clean_names(unique(Species[i])),".rds"))})
    }else if(WriteFile == F){
      Presences[[i]] <- Occs
    }
    rm(Occs)
    gc()
    if(WriteFile == F){
      return(Presences)
    }
  }
}

