pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive/v2"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive/v2"


#' Returns a ProteinDetail from the corresponding json object
#'
#'@param Accession the project accession
#'@param ProteinSequence the Protein Sequence
#'@param ProteinGroupMembers the protein Group members
#'@author Tremayne Booker
#'@details i dunno
#'@importFrom rjson fromJSON
#'@export
ProteinDetail <- function(json.object){
  proteinDetail <- list(
    "Accession" = json.object$projectAccession,
    "Protein.Sequence" = json.object$proteinSequence,
    "Protein.Group.Members" = json.object$proteinGroupMembers
  )
  return(proteinDetail)
}

#' Returns a list of protein from Pride
#'
#'@param page.size is the number of proteins returned from Pride
#'@return the list of protein details
#'@author Tremayne Booker
#'@details i dunno
#'@importFrom rjson fromJSON
#'@export
get.list.ProteinDetail <- function(page.size = 10){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/proteinevidences?pageSize=", page.size), method="C")
  protein.list <- lapply(json.list[[1]]$proteinevidences[[1]], function(x) { ProteinDetail(x)})
  for(val in protein.list){
    printProteinDetail(val)
  }
}

#' Returns a list of proteins from a specific project from Pride
#'
#'@param accession is the project accession
#'@return the list of protein details
#'@author Tremayne Booker
#'@details i dunno
#'@importFrom rjson fromJSON
#'@export
get.ProteinDetail.accession <- function(accession){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/proteinevidences?projectAccession=", accession), method="C")
  protein.list <- lapply(json.list[[1]]$proteinevidences, function(x) { ProteinDetail(x)})
  for(val in protein.list){
    printProteinDetail(val)
  }
}

#' Prints Protein Lists
#'
#' @param protein.list is the proteinDetail to be printed
#' @author Tremayne Booker
#' @details i dunno
#' @export
printProteinDetail <- function(protein.list){
  statement <- paste0("Project Accession : ", protein.list$Accession, "\n",
                      "Protein Sequence : ", protein.list$Protein.Sequence, "\n",
                      "Protein Group Members: ", protein.list$Protein.Group.Members, "\n", "\n")
  cat(statement)
}
