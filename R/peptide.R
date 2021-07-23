pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive/v2"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive/v2"

MISSING_VALUE <- "Not Available"
MISSING_VALUE_LIST <- list(list("accession" = "Not Available",
                                "name" = "Not Available",
                                "value" = "Not Available"))

#' Returns a ProteinDetail from the corresponding json object
#'
#'@param Accession the project accession
#'@param PeptideSequence the peptide sequence
#'@author Tremayne Booker
#'@details i dunno
#'@importFrom rjson fromJSON
#'@export
PeptideDetail <- function(json.object){
  peptide <- list(
    "accession" = ifelse(is.null(json.object$projectAccession) || (length(json.object$projectAccession)==0), MISSING_VALUE, json.object$projectAccession),
    "peptide.sequence" = ifelse(is.null(json.object$peptideSequence) || (length(json.object$peptideSequence)==0), MISSING_VALUE, json.object$peptideSequence),
    "protein.accession" = ifelse(is.null(json.object$proteinAccession) || (length(json.object$proteinAccession)==0), MISSING_VALUE, json.object$proteinAccession),
    "missed.cleavages" = ifelse(is.null(json.object$missedCleavages) || (length(json.object$missedCleavages)==0), MISSING_VALUE, json.object$missedCleavages),
    "ptms" = if(is.null(json.object$ptms) || (length(json.object$ptms)==0)) MISSING_VALUE_LIST else json.object$ptms,
    "properties" = if(is.null(json.object$properties) || (length(json.object$properties)==0)) MISSING_VALUE_LIST else json.object$properties,
    "quality.methods" = if(is.null(json.object$qualityMethods) || (length(json.object$qualityMethods)==0)) MISSING_VALUE_LIST else json.object$qualityMethods
  )
  class(peptide) <- "PeptideDetail"
  return(peptide)
}

#' Prints PeptideDetail
#'
#' @param object is the PeptideDetail to be printed
#' @author Tremayne Booker
#' @details i dunno
#' @export
print.PeptideDetail <- function(object){
  cat("An object of class ", class(object), "\n", sep="")
  cat("   Accession: ", object$accession, "\n", sep="")
  cat("   Peptide Sequence: ", object$peptide.sequence, "\n", sep="")
  cat("   Protein Accession: ", object$protein.accession, "\n", sep="")
  cat("   Missed Cleavages: ", object$missed.cleavages, "\n", sep="")
  cat("   Properties: ", "\n", sep="")
  for(val in object$properties){
    cat("     ", val$name, "   |   Value: ", val$value, "\n", sep="")
    cat("        Accession: ", val$accession, "\n", sep="")
  }
  cat("   Quality Methods: ", "\n", sep="")
  for(val in object$quality.methods){
    cat("     ", val$name, "   |   Value: ", val$value, "\n", sep="")
    cat("        Accession: ", val$accession, "\n", sep="")
  }
  cat("   PTMs: ", "\n", sep="")
  for(val in object$ptms){
    cat("     ", val$name, "   |   Value: ", val$value, "\n", sep="")
    cat("        Accession: ", val$accession, "\n", sep="")
  }
}

#' Returns a list of peptides from Pride
#'
#'@param page.size is the number of proteins returned from Pride
#'@return the list of protein details
#'@author Tremayne Booker
#'@details i dunno
#'@importFrom rjson fromJSON
#'@export
get.list.PeptideDetail <- function(page.size = 10){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/peptideevidences?pageSize=", page.size), method="C")
  peptide.list <- lapply(json.list[[1]]$peptideevidences, function(x) { PeptideDetail(x)})
  return(peptide.list)
}

#' Returns a list of peptides from a specific project from Pride
#'
#'@param accession is the project accession
#'@return the list of protein details
#'@author Tremayne Booker
#'@details i dunno
#'@importFrom rjson fromJSON
#'@export
get.PeptideDetail.accession <- function(accession){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/peptideevidences?projectAccession=", accession), method="C")
  peptide.list <- lapply(json.list[[1]]$peptideevidences, function(x) { PeptideDetail(x)})
  return(peptide.list)
}
