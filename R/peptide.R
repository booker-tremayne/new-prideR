pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive/v2"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive/v2"


#' Returns a ProteinDetail from the corresponding json object
#'
#'@param Accession the project accession
#'@param PeptideSequence the peptide sequence
#'@author Tremayne Booker
#'@details i dunno
#'@importFrom rjson fromJSON
#'@export
PeptideDetail <- function(json.object){
  proteinName <- list(
    "Accession" = json.object$projectAccession,
    "Peptide Sequence" = json.object$peptideSequence,
    "Properties" = json.object$properties
  )
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
  json.list <- fromJSON(file=paste0(pride_archive_url, "peptideevidences?pageSize=", page.size), method="C")
  peptide.list <- lapply(json.list[[1]]$peptideevidences[[1]], function(x) { PeptideDetail(x)})
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
  for(val in peptide.list){
    printPeptideDetail(val)
  }
}

#' Prints Peptide Lists
#'
#' @param peptide.list is the peptideDetail to be printed
#' @author Tremayne Booker
#' @details i dunno
#' @export
printPeptideDetail <- function(peptide.list){
  statement <- paste0("Project Accession : ", peptide.list$Accession, "\n",
                      "Peptide Sequence : ", peptide.list$peptideSequence, "\n",
                      "Properties : ", peptide.list$properties, "\n", "\n")
  cat(statement)
}
