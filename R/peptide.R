pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive/v2"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive/v2"

MISSING_VALUE <- "Not Available"
MISSING_VALUE_LIST <- list(list("accession" = "Not Available",
                                "name" = "Not Available",
                                "value" = "Not Available"))

#' Returns a ProteinDetail from the corresponding json object
#'
#'@param json.object The JSON object from Pride to be made into a PeptideDetail
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
#' @param x The PeptideDetail to be printed
#' @param ... unused
#' @author Tremayne Booker
#' @details i dunno
#' @export
print.PeptideDetail <- function(x, ...){
  cat("An object of class ", class(x), "\n", sep="")
  cat("   Accession: ", x$accession, "\n", sep="")
  cat("   Peptide Sequence: ", x$peptide.sequence, "\n", sep="")
  cat("   Protein Accession: ", x$protein.accession, "\n", sep="")
  cat("   Missed Cleavages: ", x$missed.cleavages, "\n", sep="")
  cat("   Properties: ", "\n", sep="")
  for(val in x$properties){
    cat("     ", val$name, "   |   Value: ", val$value, "\n", sep="")
    cat("        Accession: ", val$accession, "\n", sep="")
  }
  cat("   Quality Methods: ", "\n", sep="")
  for(val in x$quality.methods){
    cat("     ", val$name, "   |   Value: ", val$value, "\n", sep="")
    cat("        Accession: ", val$accession, "\n", sep="")
  }
  cat("   PTMs: ", "\n", sep="")
  for(val in x$ptms){
    cat("     ", val$name, "   |   Value: ", val$value, "\n", sep="")
    cat("        Accession: ", val$accession, "\n", sep="")
  }
}

#' Returns a list of peptides from Pride
#'
#'@param page.size The number of peptides from Pride put into the PeptideDetail list
#'@return The list of protein details
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
#'@param accession The project accession
#'@param page.size The number of peptides from Pride put into the PeptideDetail list
#'@param page.number The number of the page retrieved from Pride
#'@param sort.direction The direction Pride returns the proteins. Can be either DESC (descending) or ASC (ascending)
#'@return the list of protein details
#'@author Tremayne Booker
#'@details i dunno
#'@importFrom rjson fromJSON
#'@export
get.PeptideDetail.accession <- function(accession, page.size=10, page.number=0, sort.direction = "DESC"){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/peptideevidences?projectAccession=", accession, "&pageSize=", page.size, "&page=", page.number, "&sortDirection=", sort.direction), method="C")
  peptide.list <- lapply(json.list[[1]]$peptideevidences, function(x) { PeptideDetail(x)})
  return(peptide.list)
}
