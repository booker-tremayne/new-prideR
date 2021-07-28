pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive/v2"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive/v2"

MISSING_VALUE <- "Not available"

#' compactProjectSummary represents a PRIDE Archive project dataset
#'
#' @export
#' @exportClass compactProjectSummary
setClass(
  "compactProjectSummary",
  slots = list(
    accession = "character",
    project.title = "character",
    project.description = "character",
    publication.date = "POSIXct",
    organisms = "character",
    organism.parts = "character",
    identified.ptm.strings = "character",
    instruments = "character",
    project.tags = "character",
    submission.type = "character",
    lab.PIs = "character",
    submitters = "character",
    affiliations = "character"
  ),

  prototype = list(
    project.title = MISSING_VALUE,
    project.description = MISSING_VALUE,
    publication.date = Sys.time(),
    organisms = MISSING_VALUE,
    organism.parts = MISSING_VALUE,
    identified.ptm.strings = MISSING_VALUE,
    instruments = MISSING_VALUE,
    project.tags = MISSING_VALUE,
    submission.type = MISSING_VALUE,
    lab.PIs = MISSING_VALUE,
    submitters = MISSING_VALUE,
    affiliations = MISSING_VALUE
  ),
  validity = function(object) {
    # check accession
    if (!is.character(object@accession) || nchar(object@accession) == 0 || is.na(object@accession))
      return("'accession' must be a single valid string")

    # check project.title
    if (!is.character(object@project.title) || nchar(object@project.title) == 0 || is.na(object@project.title))
      return("'project.title' must be a single valid string")

    # check project.description
    if (!is.character(object@project.description) || nchar(object@project.description) == 0 || is.na(object@project.description))
      return("'project description' must be a single valid string")

    # check publication.date
    if (!is(object@publication.date, "POSIXct") || is.na(object@publication.date))
      return("'publication.date' must be a single valid date")

    # check organisms
    if (!is.character(object@organisms) || 0 %in% nchar(object@organisms) || is.na(object@organisms))
      return("'organisms' must be a one or multiple valid strings")

    # check organism.parts
    if (!is.character(object@organism.parts) || 0 %in% nchar(object@organism.parts) || is.na(object@organism.parts))
      return("'organism.parts' must be a one or multiple valid strings")

    # check identified.ptm.strings
    if (!is.character(object@identified.ptm.strings) || is.na(object@identified.ptm.strings))
      return("'identified.ptm.strings' must be a one or multiple valid strings")

    # check instruments
    if (!is.character(object@instruments) || 0 %in% nchar(object@instruments) || is.na(object@instruments))
      return("'instruments' must be a one or multiple valid strings")

    # check project.tags
    if (!is.character(object@project.tags) || 0 %in% nchar(object@project.tags) || is.na(object@project.tags))
      return("'project.tags' must be a one or multiple valid strings")

    # check submission.type
    if (!is.character(object@submission.type) || nchar(object@submission.type) == 0 || is.na(object@submission.type))
      return("'submission.type' must be a single valid string")

    # check lab.PIs
    if (!is.character(object@lab.PIs) || is.na(object@lab.PIs))
      return("'lab.PIs' must be a one or multiple valid strings")

    # check submitters
    if (!is.character(object@submitters) || is.na(object@submitters))
      return("'submitters' must be a one or multiple valid strings")

    # check affiliations
    if (!is.character(object@affiliations) || is.na(object@affiliations))
      return("'affiliations' must be a one or multiple valid strings")
  }
)

#' Constructor for compactProjectSummary
#'
#' @param accession project accession
#' @param project.title the title of the project
#' @param project.description the description of the project
#' @param publication.date the date when the project was made public by PRIDE
#' @param organisms the organisms of the project
#' @param organism.parts the organism.parts of the project
#' @param identified.ptm.strings the names of the PTM for the project
#' @param instruments the names of the instruments used in the project
#' @param project.tags the tags for the project
#' @param submission.type the type of the submission, e.g. COMPLETE, PARTIAL or PRIDE
compactProjectSummary <- function(accession,
                           project.title,
                           project.description,
                           publication.date,
                           organisms,
                           organism.parts,
                           identified.ptm.strings,
                           instruments,
                           project.tags,
                           submission.type,
                           lab.PIs,
                           submitters,
                           affiliations) {
  new("compactProjectSummary",
      accession = accession,
      project.title = project.title,
      project.description = project.description,
      publication.date = publication.date,
      organisms = organisms,
      organism.parts = organism.parts,
      identified.ptm.strings = identified.ptm.strings,
      instruments = instruments,
      project.tags = project.tags,
      submission.type = submission.type,
      lab.PIs = lab.PIs,
      submitters = submitters,
      affiliations = affiliations
  )
}

#' Show the print-out version of the content in a compactProjectSummary
#'
#' @param object a given compactProjectSummary
#' @export
setMethod("show",
          signature = "compactProjectSummary",
          definition = function(object) {
            cat("An object of class ", class(object), sep="")
            cat(" made public in ", as.character(object@publication.date), "\n", sep="")
            cat("    Accession: ", object@accession, "\n", sep="")
            cat("    Title: ", object@project.title, "\n", sep="")
            cat("    Description: ", object@project.description, "\n", sep="")
            cat("    Organisms: ", object@organisms, "\n", sep=" ")
            cat("    Organism Parts: ", object@organism.parts, "\n", sep=" ")
            cat("    PTMs: ", object@identified.ptm.strings, "\n", sep=" ")
            cat("    Instruments: ", object@instruments, "\n", sep=" ")
            cat("    Tags: ", object@project.tags, "\n", sep=" ")
            cat("    Submission type: ", object@submission.type, "\n", sep="")
            cat("    Lab PIs: ", object@lab.PIs, "\n", sep="")
            cat("    Submitters: ", object@submitters, "\n", sep="")
            cat("    Affiliations: ", object@affiliations, "\n", sep="")
            invisible(NULL)
          }
)

#' Returns a project accession
#'
#' @param object a compactProjectSummary
#' @return the accession
#' @author Jose A. Dianes
#' @export
setMethod("accession", "compactProjectSummary", function(object) object@accession)

#' Replaces a project accession
#'
#' @param object a compactProjectSummary
#' @param value the accession
#' @author Jose A. Dianes
#' @export
setReplaceMethod("accession", "compactProjectSummary",
                 function(object, value) {
                   object@accession <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project title
#'
#' @param object a compactProjectSummary
#' @return the project title
#' @author Jose A. Dianes
#' @export
setMethod("project.title", "compactProjectSummary", function(object) object@project.title)

#' Replaces a project title
#'
#' @param object a compactProjectSummary
#' @param value the title
#' @author Jose A. Dianes
#' @export
setReplaceMethod("project.title", "compactProjectSummary",
                 function(object, value) {
                   object@project.title <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project description
#'
#' @param object a compactProjectSummary
#' @return the project description
#' @author Jose A. Dianes
#' @export
setMethod("project.description", "compactProjectSummary", function(object) object@project.description)

#' Replaces a project description
#'
#' @param object a compactProjectSummary
#' @param value the project description
#' @author Jose A. Dianes
#' @export
setReplaceMethod("project.description", "compactProjectSummary",
                 function(object, value) {
                   object@project.description <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project publication date
#'
#' @param object a compactProjectSummary
#' @return the project publication date
#' @author Jose A. Dianes
#' @export
setMethod("publication.date", "compactProjectSummary", function(object) object@publication.date)

#' Replaces a project publication date
#'
#' @param object a compactProjectSummary
#' @param value the publication date
#' @author Jose A. Dianes
#' @export
setReplaceMethod("publication.date", "compactProjectSummary",
                 function(object, value) {
                   object@publication.date <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project organisms
#'
#' @param object a compactProjectSummary
#' @return the project organisms
#' @author Jose A. Dianes
#' @export
setMethod("organisms", "compactProjectSummary", function(object) object@organisms)

#' Replaces the project organisms
#'
#' @param object a compactProjectSummary
#' @param value the organisms
#' @author Jose A. Dianes
#' @export
setReplaceMethod("organisms", "compactProjectSummary",
                 function(object, value) {
                   object@organisms <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project organism.parts
#'
#' @param object a compactProjectSummary
#' @return the project organism.parts
#' @author Jose A. Dianes
#' @export
setMethod("organism.parts", "compactProjectSummary", function(object) object@organism.parts)

#' Replaces the project organism.parts
#'
#' @param object a compactProjectSummary
#' @param value the organism.parts
#' @author Jose A. Dianes
#' @export
setReplaceMethod("organism.parts", "compactProjectSummary",
                 function(object, value) {
                   object@organism.parts <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project modification names
#'
#' @param object a compactProjectSummary
#' @return the project modification names
#' @author Jose A. Dianes
#' @export
setMethod("identified.ptm.strings", "compactProjectSummary", function(object) object@identified.ptm.strings)

#' Replaces the project PTMs
#'
#' @param object a compactProjectSummary
#' @param value the PTMs
#' @author Jose A. Dianes
#' @export
setReplaceMethod("identified.ptm.strings", "compactProjectSummary",
                 function(object, value) {
                   object@identified.ptm.strings <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project instrument names
#'
#' @param object a compactProjectSummary
#' @return the project instrument names
#' @author Jose A. Dianes
#' @export
setMethod("instruments", "compactProjectSummary", function(object) object@instruments)

#' Replaces the project instrument nanmes
#'
#' @param object a compactProjectSummary
#' @param value the instrument names
#' @author Jose A. Dianes
#' @export
setReplaceMethod("instruments", "compactProjectSummary",
                 function(object, value) {
                   object@instruments <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project tags
#'
#' @param object a compactProjectSummary
#' @return the project tags
#' @author Jose A. Dianes
#' @export
setMethod("project.tags", "compactProjectSummary", function(object) object@project.tags)

#' Replaces the project tags
#'
#' @param object a compactProjectSummary
#' @param value the project tags
#' @author Jose A. Dianes
#' @export
setReplaceMethod("project.tags", "compactProjectSummary",
                 function(object, value) {
                   object@project.tags <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project submission type
#'
#' @param object a compactProjectSummary
#' @return the project submission type
#' @author Jose A. Dianes
#' @export
setMethod("submission.type", "compactProjectSummary", function(object) object@submission.type)

#' Replaces the project submission type
#'
#' @param object a compactProjectSummary
#' @param value the submission type
#' @author Jose A. Dianes
#' @export
setReplaceMethod("submission.type", "compactProjectSummary",
                 function(object, value) {
                   object@submission.type <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a lab PIs
#'
#' @param object a compactProjectSummary
#' @return the lab PIs
#' @author Tremayne Booker
#' @export
setMethod("lab.PIs", "compactProjectSummary", function(object) object@lab.PIs)

#' Replaces the lab PIs
#'
#' @param object a compactProjectSummary
#' @param value lab PIs
#' @author Tremayne Booker
#' @export
setReplaceMethod("lab.PIs", "compactProjectSummary",
                 function(object, value) {
                   object@lab.PIs <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a submitters
#'
#' @param object a compactProjectSummary
#' @return the submitters
#' @author Jose A. Dianes
#' @export
setMethod("submitters", "compactProjectSummary", function(object) object@submitters)

#' Replaces the submitters
#'
#' @param object a compactProjectSummary
#' @param value submitters
#' @author Jose A. Dianes
#' @export
setReplaceMethod("submitters", "compactProjectSummary",
                 function(object, value) {
                   object@submitters <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a affiliations
#'
#' @param object a compactProjectSummary
#' @return the affiliations
#' @author Jose A. Dianes
#' @export
setMethod("affiliations", "compactProjectSummary", function(object) object@affiliations)

#' Replaces the affiliations
#'
#' @param object a compactProjectSummary
#' @param value affiliations
#' @author Jose A. Dianes
#' @export
setReplaceMethod("affiliations", "compactProjectSummary",
                 function(object, value) {
                   object@affiliations <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a compactProjectSummary instance from a JSON string representation, except
#' this is specifically for the search function as the JSON is formatted differently
#'
#' @param json_str The JSON object
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The compactProjectSummary instance
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
from.json.compactProjectSummary <- function(json.object) {
  res <- new("compactProjectSummary",
             accession = ifelse(is.null(json.object$accession), MISSING_VALUE, json.object$accession),
             project.title = ifelse(is.null(json.object$title), MISSING_VALUE, json.object$title),
             project.description = ifelse(is.null(json.object$projectDescription), MISSING_VALUE, json.object$projectDescription),
             publication.date = as.POSIXct(json.object$publicationDate),
             organisms = ifelse(is.null(json.object$organisms) || (length(json.object$organisms)==0), MISSING_VALUE, json.object$organisms),
             organism.parts = ifelse(is.null(json.object$organismParts) || (length(json.object$organismParts)==0), MISSING_VALUE, json.object$organismParts),
             identified.ptm.strings = ifelse(is.null(json.object$identifiedPTMStrings) || (length(json.object$identifiedPTMStrings)==0), MISSING_VALUE, json.object$identifiedPTMStrings),
             instruments = ifelse(is.null(json.object$instruments) || (length(json.object$instruments)==0), MISSING_VALUE, json.object$instruments),
             project.tags = ifelse(is.null(json.object$projectTags) || (length(json.object$projectTags)==0), MISSING_VALUE, json.object$projectTags),
             submission.type = ifelse(is.null(json.object$submissionType), MISSING_VALUE, json.object$submissionType),
             lab.PIs = ifelse(is.null(json.object$labPIs) || (length(json.object$labPIs)==0), MISSING_VALUE, json.object$labPIs),
             submitters = ifelse(is.null(json.object$submitters) || (length(json.object$submitters)==0), MISSING_VALUE, json.object$submitters),
             affiliations = ifelse(is.null(json.object$affiliations) || (length(json.object$affiliations)==0), MISSING_VALUE, json.object$affiliations)
  )

  return (res)
}


#' Returns a series of PRIDE Archive projects
#' to satisfy a given query. This is actually a
#' query filtered version of project_list
#'
#' @param keywords The query term or terms
#' @param page.size The maximum number of search results
#' @param page.number The page of the list
#' @param sort.direction the direction the list is sorted by
#' @param and Whether the keywords should be matched using AND logic or OR logic
#' @return The search results in a list of objects
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
search.ProjectSummary <- function(keywords = "", page.size=10, page.number = 0, sort.direction = "DESC", all = FALSE, filters = "") {
  if(all){
    search.ProjectSummary.and(keywords, page.size, page.number, sort.direction)
  }
  q <- ""
  f <- ""
  for(word in keywords){
    q <- paste0(q, "keyword=", word, "&")
  }
  for(word in filters){
    f <- paste0(f, word, ",")
  }
  json.list <- fromJSON(file=paste0(pride_archive_url, "/search/projects?", q, "filter=", f, "&pageSize=", page.size, "&page=", page.number, "&sortDirection=", sort.direction), method="C")
  project.list <- lapply(json.list[[1]]$compactprojects, function(x) { from.json.compactProjectSummary(x)})
  return(project.list)
}

#' Returns a series of PRIDE Archive projects
#' to satisfy a given query. This is actually a
#' query filtered version of project_list.
#' Multiple keywords are matched by AND logic
#'
#' @param keywords The query term or terms
#' @param page.size The maximum number of search results
#' @param page.number The page of the list
#' @param sort.direction the direction the list is sorted by
#' @return The search results in a list of objects
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
search.ProjectSummary.and <- function(keywords, page.size=10, page.number = 0, sort.direction = "DESC"){
  true.list <- list()
  for(word in keywords){
    json.list <- fromJSON(file=paste0(pride_archive_url, "/search/projects?keyword=", word, "&pageSize=", page.size, "&page=", page.number, "&sortDirection=", sort.direction), method="C")
    project.list <- lapply(json.list[[1]]$compactprojects, function(x) { from.json.compactProjectSummary(x)})
    if(length(true.list) == 0){
      true.list <- project.list
    }
    true.list <- intersect(true.list, project.list)
  }
  return(true.list)
}

#' Returns similar projects from the project with the given accession
#'
#' @param accession Project accession to find similar projects of
#' @param page.size The size of the pages returned
#' @param page.number The page to be viewed
#' @return The list of similar projects
#' @author Tremayne Booker
#' @details I dunno
#' @importFrom rjson fromJSON
#' @export
get.similar.projects <- function(accession, page.size = 10, page.number = 0){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/projects/", accession, "/similarProjects?page=", page.number, "&pageSize=", page.size))
  project.list <- lapply(json.list[[1]]$compactprojects, function(x) { from.json.compactProjectSummary(x)})
  return(project.list)
}

