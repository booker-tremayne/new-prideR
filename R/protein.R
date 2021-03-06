pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive/v2"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive/v2"

MISSING_VALUE <- "Not Available"
MISSING_VALUE_LIST <- list(list("accession" = "Not Available",
                                "name" = "Not Available",
                                "value" = "Not Available"))

#' Returns a ProteinDetail from the corresponding json object
#'
#'@param json.object The JSON object from Pride to be made into a ProteinDetail
#'@author Tremayne Booker
#'@details i dunno
#'@importFrom rjson fromJSON
#'@export
ProteinDetail <- function(json.object){
  protein <- list(
    "accession" = ifelse(is.null(json.object$projectAccession) || (length(json.object$projectAccession)==0), MISSING_VALUE, json.object$projectAccession),
    "protein.sequence" = ifelse(is.null(json.object$proteinSequence) || (length(json.object$proteinSequence)==0), MISSING_VALUE, json.object$proteinSequence),
    "protein.group.members" = ifelse(is.null(json.object$proteinGroupMembers) || (length(json.object$proteinGroupMembers)==0), MISSING_VALUE, json.object$proteinGroupMembers),
    "number.peptides" = ifelse(is.null(json.object$numberPeptides) || (length(json.object$numberPeptides)==0), MISSING_VALUE, json.object$numberPeptides),
    "number.psms" = ifelse(is.null(json.object$numberPSMs) || (length(json.object$numberPSMs)==0), MISSING_VALUE, json.object$numberPSMs),
    "properties" = if(is.null(json.object$additionalAttributes) || (length(json.object$additionalAttributes)==0)) MISSING_VALUE_LIST else json.object$additionalAttributes,
    "ptms" = if(is.null(json.object$ptms) || (length(json.object$ptms)==0)) MISSING_VALUE_LIST else json.object$ptms,
    "quality.methods" = if(is.null(json.object$qualityMethods) || (length(json.object$qualityMethods)==0)) MISSING_VALUE_LIST else json.object$qualityMethods
    )
  class(protein) <- "ProteinDetail"
  return(protein)
}


#' Prints ProteinDetail
#'
#' @param x The PeptideDetail to be printed
#' @param ... unused
#' @author Tremayne Booker
#' @details i dunno
#' @export
print.ProteinDetail <- function(x, ...){
  cat("An object of class ", class(x), "\n", sep="")
  cat("   Accession: ", x$accession, "\n", sep="")
  cat("   Protein Group Members: ", x$protein.group.members, "\n", sep="")
  cat("   Protein Sequence: ", x$protein.sequence, "\n", sep="")
  cat("   Number of Peptides: ", x$number.peptides, "\n", sep="")
  cat("   Number of PSMs: ", x$number.psms, "\n", sep="")
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

#' Returns a list of protein from Pride
#'
#'@param page.size The number of proteins from Pride put into the ProteinDetail list
#'@return the list of ProteinDetails
#'@author Tremayne Booker
#'@details i dunno
#'@importFrom rjson fromJSON
#'@export
get.list.ProteinDetail <- function(page.size = 10){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/proteinevidences?pageSize=", page.size), method="C")
  protein.list <- lapply(json.list[[1]]$proteinevidences, function(x) { ProteinDetail(x)})
  return(protein.list)
}

#' Returns a list of proteins from a specific project from Pride
#'
#'@param accession The project accession
#'@param page.size The number of proteins from Pride put into the ProteinDetail list
#'@param page.number The number of the page retrieved from Pride
#'@param sort.direction The direction Pride returns the proteins. Can be either DESC (descending) or ASC (ascending)
#'@return The list of protein details
#'@author Tremayne Booker
#'@details i dunno
#'@importFrom rjson fromJSON
#'@export
get.ProteinDetail.accession <- function(accession, page.size = 10, page.number = 0, sort.direction = "DESC"){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/proteinevidences?projectAccession=", accession, "&pageSize=", page.size, "&page=", page.number, "&sortDirection=", sort.direction), method="C")
  protein.list <- lapply(json.list[[1]]$proteinevidences, function(x) { ProteinDetail(x)})
  return(protein.list)
}

