#' @title Clean taxa using Taxize
#'
#' @description
#' This function cleans a vector of taxa using Taxize.
#'
#' @param Taxons Vector of taxa to be cleaned.
#'
#' @return A data frame with the cleaned taxa and their scores.
#'
#' @export
#'
#' @examples
#' Clean_Taxa_Taxize(Taxons = c("Canis lupus", "C. lupus"))
#'
#' @importFrom taxize gnr_resolve
#' @importFrom dplyr select filter group_by ungroup
#' @importFrom readr write_csv
#' @importFrom tibble rowid_to_column
#'
#' @references
#' Chamberlain, Scott T., et al. "Taxize: an R package for taxonomic
#'  lookup and manipulation." Methods in Ecology and Evolution 6.12
#'  (2015): 1451-1456.

Clean_Taxa_Taxize <- function(Taxons){
  NewTaxa <- data.frame(Taxa = Taxons, score = NA, matched_name2 = NA) |>
    tibble::rowid_to_column(var = "TaxaID")

  dir.create("Results")

  for(i in 1:nrow(NewTaxa)){
    try({
      Temp <- taxize::gnr_resolve(NewTaxa$Taxa[i],
                                  data_source_ids = "11", canonical = TRUE, best_match_only = T) |>
        dplyr::select(score, matched_name2)
      NewTaxa[i,3:4] <- Temp
      if((i %% 50) == 0){
        message(paste(i, "of", nrow(NewTaxa), "Ready!", Sys.time()))
        readr::write_csv(NewTaxa, "Results/Cleaned_Taxa_Taxize.csv")
      }
      gc()
    }, silent = T)

  }

  Cleaned_Taxize <- NewTaxa |>
    dplyr::filter(!is.na(matched_name2)) |>
    dplyr::group_by(matched_name2) |>
    dplyr::filter(TaxaID == min(TaxaID)) |>
    ungroup()

  return(Cleaned_Taxize)
}


Clean_Taxa_rgbif <- function(Cleaned_Taxize){
  rgbif_find <- rgbif::name_backbone_checklist(Cleaned_Taxize$matched_name2) |>
    # Change name to match the cleaned_taxize dataset
    dplyr::rename(matched_name2 = verbatim_name) |>
    dplyr::relocate(matched_name2, .before = everything()) |>
    dplyr::select(matched_name2, confidence, kingdom, phylum, order, family, genus, species)


#' @title Clean Taxa from GBIF
#' @description Clean the taxonomic list using GBIF
#' @param Cleaned_Taxize a data frame containing the cleaned taxonomic list from function Clean_Taxa_Taxize
#' @return A csv file containing the cleaned taxonomic list
#' @export
#'
#' @examples
#' Cleaned_Taxize <- Clean_Taxa_Taxize(Taxons = c("Canis lupus", "C. lupus"))
#' Clean_Taxa_rgbif(Cleaned_Taxize)
#' @importFrom dplyr rename relocate select
#' @importFrom readr write_csv
#' @importFrom rgbif name_backbone_checklist
Clean_Taxa_rgbif <- function(Cleaned_Taxize){
  rgbif_find <- rgbif::name_backbone_checklist(Cleaned_Taxize$matched_name2) |>
    # Change name to match the cleaned_taxize dataset
    dplyr::rename(matched_name2 = verbatim_name) |>
    dplyr::relocate(matched_name2, .before = everything()) |>
    dplyr::select(matched_name2, confidence, kingdom, phylum, order, family, genus, species)

  readr::write_csv(rgbif_find, "Results/Cleaned_Taxa_rgbif.csv")

  Species_Only <- rgbif_find |>
    dplyr::filter(!is.na(species))

  FinalSpeciesList <- Species_Only |>
    group_by(species) |>
    dplyr::filter(confidence == max(confidence))
  readr::write_csv(FinalSpeciesList, "Results/FinalSpeciesList.csv")
}
  readr::write_csv(rgbif_find, "Results/Cleaned_Taxa_rgbif.csv")

  Species_Only <- rgbif_find |>
    dplyr::filter(!is.na(species))

  FinalSpeciesList <- Species_Only |>
    group_by(species) |>
    dplyr::filter(confidence == max(confidence))
  readr::write_csv(FinalSpeciesList, "Results/FinalSpeciesList.csv")
}
