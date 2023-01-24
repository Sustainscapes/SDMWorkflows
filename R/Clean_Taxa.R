#' @title Clean taxa using Taxize
#'
#' @description
#' This function cleans a vector of taxa using Taxize.
#'
#' @param Taxons Vector of taxa to be cleaned.
#' @param WriteFile logical if False (default) only returns a
#' data frame, if TRUE will generate a folder (Results in the
#' Working directory with a csv of the results)
#'
#' @return A data frame with the cleaned taxa and their scores.
#'
#' @export
#'
#' @examples
#' Clean_Taxa_Taxize(Taxons = c("Canis lupus", "C. lupus"))
#'
#' @importFrom taxize gnr_resolve
#' @importFrom dplyr select filter group_by ungroup rename
#' @importFrom readr write_csv
#' @importFrom tibble rowid_to_column
#'
#' @references
#' Chamberlain, Scott T., et al. "Taxize: an R package for taxonomic
#'  lookup and manipulation." Methods in Ecology and Evolution 6.12
#'  (2015): 1451-1456.

Clean_Taxa_Taxize <- function(Taxons, WriteFile = F){
  score <- matched_name2 <- TaxaID <- user_supplied_name <- Taxa <- NULL
  NewTaxa <- data.frame(Taxa = Taxons, score = NA, matched_name2 = NA) |>
    tibble::rowid_to_column(var = "TaxaID")
  if(WriteFile){
    dir.create("Results")
  }


  # try vectorized form first
  Temp <- tryCatch(taxize::gnr_resolve(NewTaxa$Taxa, data_source_ids = "11", canonical = TRUE, best_match_only = T),
                   error = function(e) return(NULL))

  # if vectorized form fails, use loop
  if(is.null(Temp)){
    message("Vectorized form did not work")
    for(i in 1:nrow(NewTaxa)){
      try({Temp <- taxize::gnr_resolve(NewTaxa$Taxa[i],
                                  data_source_ids = "11", canonical = TRUE, best_match_only = T) |>
        dplyr::select(score, matched_name2)
      NewTaxa[i,3:4] <- Temp})
      if((i %% 50) == 0){
        message(paste(i, "of", nrow(NewTaxa), "Ready!", Sys.time()))
      }
      gc()
    }
  } else {
      NewTaxa <- Temp |>
      dplyr::rename(Taxa = user_supplied_name) |>
      dplyr::select(Taxa, score, matched_name2) |>
      dplyr::left_join(dplyr::select(NewTaxa, TaxaID, Taxa))
  }

  if(WriteFile){
    readr::write_csv(NewTaxa, "Results/Cleaned_Taxa_Taxize.csv")
  }


  Cleaned_Taxize <- NewTaxa |>
    dplyr::filter(!is.na(matched_name2)) |>
    dplyr::group_by(matched_name2) |>
    dplyr::filter(TaxaID == min(TaxaID)) |>
    ungroup()

  return(Cleaned_Taxize)
}

#' @title Clean Taxa from GBIF
#' @description Clean the taxonomic list using GBIF
#' @param Cleaned_Taxize a data frame containing the cleaned taxonomic list from function Clean_Taxa_Taxize
#' @param Species_Only logical, if TRUE (default) only species will be returned, if FALSE, it will return the highest possible taxonomic resolution
#' @param WriteFile logical if False (default) only returns a
#' data frame, if TRUE will generate a folder (Results in the
#' Working directory with a csv of the results)
#' @return A csv file containing the cleaned taxonomic list
#' @export
#'
#' @examples
#' Cleaned_Taxize <- Clean_Taxa_Taxize(Taxons = c("Canis lupus", "C. lupus"))
#' Clean_Taxa_rgbif(Cleaned_Taxize)
#' @importFrom dplyr rename relocate select everything left_join
#' @importFrom readr write_csv
#' @importFrom rgbif name_backbone_checklist
#'
#' @references
#'
#' Chamberlain S, Barve V, Mcglinn D, Oldoni D, Desmet P, Geffert L, Ram K
#' (2023). rgbif: Interface to the Global Biodiversity Information
#' Facility API_ R package version 3.7.4,

Clean_Taxa_rgbif <- function(Cleaned_Taxize, WriteFile = F, Species_Only = T){
  matched_name2 <- confidence <- kingdom <- phylum <- order <- family <- genus <- species <- verbatim_name <- canonicalName <- Taxa <- NULL
  if(WriteFile){
    dir.create("Results")
  }
  rgbif_find <- rgbif::name_backbone_checklist(Cleaned_Taxize$matched_name2) |>
    # Change name to match the cleaned_taxize dataset
    dplyr::rename(matched_name2 = verbatim_name) |>
    dplyr::relocate(matched_name2, .before = everything()) |>
    dplyr::left_join(Cleaned_Taxize) |>
    dplyr::select(Taxa, matched_name2, confidence, canonicalName, kingdom, phylum, order, family, genus, species, rank)
  if(WriteFile){
    readr::write_csv(rgbif_find, "Results/Cleaned_Taxa_rgbif.csv")
  }

  if(Species_Only){
    Species_Only <- rgbif_find |>
      dplyr::filter(!is.na(species))

    FinalSpeciesList <- Species_Only |>
      group_by(species) |>
      dplyr::filter(confidence == max(confidence))
  } else if (!Species_Only){

    FinalSpeciesList <- rgbif_find |>
      group_by(canonicalName) |>
      dplyr::filter(confidence == max(confidence))
  }


  if(WriteFile){
    readr::write_csv(FinalSpeciesList, "Results/FinalSpeciesList.csv")
  }
  return(FinalSpeciesList)
}

#' @title Clean taxa using Taxize and rgbif
#'
#' @description
#' This function cleans a vector of taxa using Taxize and rgbif
#'
#' @param Taxons Vector of taxa to be cleaned.
#' @param WriteFile logical if False (default) only returns a
#' data frame, if TRUE will generate a folder (Results in the
#' Working directory with a csv of the results)
#' @param Species_Only logical, if TRUE (default) only species will be
#' returned, if FALSE, it will return the highest possible taxonomic
#' resolution
#' @importFrom purrr reduce
#'
#' @return A data frame with the cleaned taxa and their scores.
#'
#' @export
#'
#' @examples
#'
#' Cleaned <- Clean_Taxa(Taxons = c("Canis lupus", "C. lupus"))
#'
#' @references
#' Chamberlain, Scott T., et al. "Taxize: an R package for taxonomic
#'  lookup and manipulation." Methods in Ecology and Evolution 6.12
#'  (2015): 1451-1456.
#' Chamberlain S, Barve V, Mcglinn D, Oldoni D, Desmet P, Geffert L, Ram K
#' (2023). rgbif: Interface to the Global Biodiversity Information
#' Facility API_ R package version 3.7.4

Clean_Taxa <- function(Taxons, WriteFile = F, Species_Only = T){
  if(length(Taxons) < 10000){
    Cleaned_Taxize <- Clean_Taxa_Taxize(Taxons = Taxons, WriteFile = WriteFile)
    Final_Result <- Clean_Taxa_rgbif(Cleaned_Taxize, WriteFile = WriteFile, Species_Only = Species_Only)
  } else {
    print("more than 10000 taxons, will be divided into chunks")
    Taxons <- split(Taxons, ceiling(seq_along(Taxons) / 1000))
    Final_Result <- list()
    for (i in 1:length(Taxons)) {
      Cleaned_Taxize <- Clean_Taxa_Taxize(Taxons = Taxons, WriteFile = WriteFile)
      Final_Result[[i]] <- Clean_Taxa_rgbif(Cleaned_Taxize, WriteFile = WriteFile, Species_Only = Species_Only)
      print(paste("Chunk", i, "of", length(Taxons), "ready!", Sys.time()))
    }
    Final_Result <- Final_Result |>
      purrr::reduce(rbind)
  }
  return(Final_Result)
}

