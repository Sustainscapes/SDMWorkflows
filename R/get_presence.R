#' Get occurrence data
#'
#' This function uses the \code{rgbif} package to get occurrence data from the Global Biodiversity Information Facility (GBIF) API.
#'
#' @param Species A vector containing the species to query.
#' @param WriteFile Logical. If \code{TRUE}, the occurrence data will be written to the \code{Occs} folder. If \code{FALSE}, the occurrence data will be returned in a list.
#' @param continent what contintent are the occurrences downloaded from
#' @param country character, The 2-letter country code (ISO-3166-1) in which the occurrence was recorded.
#' @param limit maximum number of occurrences downloaded
#' @param Log Logical. If \code{TRUE}, a log file will be created with information on the progress of the function.
#'
#' @return If \code{WriteFile = TRUE}, this function does not return anything. If \code{WriteFile = FALSE}, a list containing the occurrence data for each species is returned.
#'
#' @export
#'
#' @importFrom rgbif occ_data
#' @importFrom janitor make_clean_names
#' @importFrom utils write.table
#' @examples
#' # Get occurrence data for species in FinalSpeciesList
#' \donttest{
#' Presences <- SDMWorkflows::GetOccs(Species = c("Abies concolor", "Canis lupus"), WriteFile = FALSE)
#' }

GetOccs <- function(Species, WriteFile = T, continent = NULL, country = NULL, limit = 10000, Log = T){

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
    dir.create("Occs", showWarnings = FALSE)
  } else if(WriteFile == F){
    Presences <- list()
  }

  if(Log == T){
    log_file <- paste0("Occs/Log_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    write.table(data.frame(Species = character(), Start_time = character(), End_time = character(), Error = character(), Num_presences = numeric()), file = log_file, row.names = FALSE, sep = ",")
  }

  for(i in 1:length(Species)){
    message(paste("Starting species", i))

    # Check if RDS file already exists
    rds_file <- paste0("Occs/",janitor::make_clean_names(unique(Species[i])),".rds")
    if (file.exists(rds_file)) {
      message(paste("Skipping", Species[i], "- RDS file already exists"))
      if(Log == T){
        log_entry <- data.frame(Species = Species[i], Start_time = NA, End_time = NA, Error = "RDS file exists", Num_presences = NA)
        write.table(log_entry, file = log_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
      }
      next
    }

    # Download occurrence data
    Occs <- tryCatch(
      expr = {
        rgbif::occ_data(scientificName = Species[i],
                        hasCoordinate = T,
                        continent = continent,
                        country = country,
                        hasGeospatialIssue = FALSE,
                        limit = limit)
      },
      error = function(e) {
        message(paste("Error for species", Species[i], Sys.time()))
        if(Log == T){
          log_entry <- data.frame(Species = Species[i], Start_time = NA, End_time = format(Sys.time()), Error = "Error downloading occurrence data", Num_presences = NA)
          write.table(log_entry, file = log_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
        }
        NULL
      })

    # Save occurrence data or add to list
    if (is.null(Occs)) {
      message(paste("Skipping", Species[i], "- Error downloading occurrence data"))
    } else {
      if(WriteFile == T){
        try({saveRDS(Occs$data, rds_file)})
      } else if(WriteFile == F){
        Presences[[i]] <- Occs$data
      }
      if(Log == T){
        if(is.null(Occs$data)){
          log_entry <- data.frame(Species = Species[i], Start_time = format(Sys.time()), End_time = format(Sys.time()), Error = NA, Num_presences = 0)
        } else if (!is.null(Occs$data)){
          log_entry <- data.frame(Species = Species[i], Start_time = format(Sys.time()), End_time = format(Sys.time()), Error = NA, Num_presences = nrow(Occs$data))
        }
        write.table(log_entry, file = log_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
      }
    }

    rm(Occs)
    gc()

    message(paste(i, "of", length(Species), "ready!", Sys.time()))
  }

  if(WriteFile == F){
    return(Presences)
  }
}
