pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive/v2"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive/v2"

MISSING_VALUE <- "Not available"
MISSING_VALUE_LIST <- list(list( "name" = "Not available", "accession" = "Not available"))

#' ProjectSummaryList represents a PRIDE Archive project collection
#'
#' @importFrom rjson fromJSON
#' @export
#' @exportClass ProjectSummaryList
setClass(
  "ProjectSummaryList",
  representation(
    query = "character",
    project.list = "list",
    page.number = "numeric",
    page.size = "numeric"
  ),
  prototype(
    query = "",
    project.list = list(),
    page.number = 0,
    page.size = 10
  )
)

setMethod("show",
          signature = "ProjectSummaryList",
          definition = function(object) {
            cat("A ", class(object), sep="")
            cat(" representing the search results for query ", object@query, " with \n", sep="")
            cat("    Projects in page: ", length(object@project.list), "\n", sep="")
            cat("    Page number: ", object@page.number, " \n", sep="")
            cat("    Page size: ", object@page.size, " projects per page\n", sep="")
            invisible(NULL)
          }
)

#' Returns the query term used to retrieve the ProjectSummaryList
#'
#' @param object a ProjectSummaryList instance
#' @return the query term used to retrieve this ProjectSummaryList
#' @author Jose A. Dianes
#' @export
setMethod("query", "ProjectSummaryList", function(object) object@query)

#' Returns a project summary page as a list
#'
#' @param object a ProjectSummaryList instance
#' @return the protein list
#' @author Jose A. Dianes
#' @export
setMethod("project.list", "ProjectSummaryList", function(object) object@project.list)

#' Returns the page number for a given protein details list
#'
#' @param object a ProjectSummaryList instance
#' @return the page number
#' @author Jose A. Dianes
#' @export
setMethod("page.number", "ProjectSummaryList", function(object) object@page.number)

#' Returns the page size for a given protein details list
#'
#' @param object a ProjectSummaryList instance
#' @return the page size
#' @author Jose A. Dianes
#' @export
setMethod("page.size", "ProjectSummaryList", function(object) object@page.size)

#' ProjectSummary represents a PRIDE Archive project dataset
#'
#' @importFrom rjson fromJSON
#' @export
#' @exportClass ProjectSummary
setClass(
  "ProjectSummary",
  #removed  num.assays,
  #
  #replaced instrument.names with instruments
  #        species with organisms
  #        tissues with organism.parts
  #        ptm.names with identified.ptm.strings
  #
  #add
    slots = list(
    accession = "character", #good
    project.title = "character", #good
    project.description = "character", #good
    sample.processing.protocol = "character", #added
    data.processing.protocol = "character", #added
    publication.date = "POSIXct", #good
    organisms = "list", #replaced species with organisms
    organism.parts = "list", #replaced tissues with organism.parts
    diseases = "list", #added
    identified.ptm.strings = "list", #replaced ptm.names with identified.ptm.strings
    instruments = "list", #replaced instrument.names with instruments
    quantification.methods = "list", #added
    project.tags = "character", #good
    submission.type = "character" #good
  ),

  prototype = list(
    project.title = MISSING_VALUE,
    project.description = MISSING_VALUE,
    sample.processing.protocol = MISSING_VALUE,
    data.processing.protocol = MISSING_VALUE,
    publication.date = Sys.time(),
    organisms = MISSING_VALUE_LIST,
    organism.parts = MISSING_VALUE_LIST,
    diseases = MISSING_VALUE_LIST,
    identified.ptm.strings = MISSING_VALUE_LIST,
    instruments = MISSING_VALUE_LIST,
    quantification.methods = MISSING_VALUE_LIST,
    project.tags = MISSING_VALUE,
    submission.type = MISSING_VALUE
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

    # check sample.processing.protocol
    if (!is.character(object@sample.processing.protocol) || nchar(object@sample.processing.protocol) == 0 || is.na(object@sample.processing.protocol))
      return (" 'sample processing protocol must be a single valid string' ")

    # check data.processing.protocol
    if (!is.character(object@data.processing.protocol) || nchar(object@data.processing.protocol) == 0 || is.na(object@data.processing.protocol))
      return (" 'data processing protocol must be a single valid string' ")

    # check publication.date
    if (!is(object@publication.date, "POSIXct") || is.na(object@publication.date))
      return("'publication.date' must be a single valid date")

    # check organisms
    if (!is.list(object@organisms) || length(object@organisms) == 0 || is.na(object@organisms))
      return("'organisms' must be a valid list")

    # check organism.parts
    if (!is.list(object@organism.parts) || length(object@organism.parts) == 0 || is.na(object@organism.parts))
      return("'organism.parts' must be a valid list")

    # check diseases
    if (!is.list(object@diseases) || length(object@diseases) == 0 || is.na(object@diseases))
      return("'diseases' must be a valid list")

    # check identified.ptm.strings
    if (!is.list(object@identified.ptm.strings) || length(object@identified.ptm.strings) == 0 || is.na(object@identified.ptm.strings))
      return("'identified.ptm.strings' must be a valid list")

    # check instruments
    if (!is.list(object@instruments) || length(object@instruments) == 0 || is.na(object@instruments))
      return("'instruments' must be a valid list")

    # check quantification.methods
    if (!is.list(object@quantification.methods) || length(object@quantification.methods) == 0 || is.na(object@quantification.methods))
      return("'quantification.methods' must be a valid list")

    # check project.tags
    if (!is.character(object@project.tags) || 0 %in% nchar(object@project.tags) || is.na(object@project.tags))
      return("'project.tags' must be a one or multiple valid strings")

    # check submission.type
    if (!is.character(object@submission.type) || nchar(object@submission.type) == 0 || is.na(object@submission.type))
      return("'submission.type' must be a single valid string")
  }
)

#' Constructor for ProjectSummary
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
ProjectSummary <- function(accession,
                           project.title,
                           project.description,
                           sample.processing.protocol,
                           data.processing.protocol,
                           publication.date,
                           organisms,
                           organism.parts,
                           diseases,
                           identified.ptm.strings,
                           instruments,
                           quantification.methods,
                           project.tags,
                           submission.type) {
  new("ProjectSummary",
      accession = accession,
      project.title = project.title,
      project.description = project.description,
      sample.processing.protocol = sample.processing.protocol,
      data.processing.protocol = data.processing.protocol,
      publication.date = publication.date,
      organisms = organisms,
      organism.parts = organism.parts,
      diseases = diseases,
      identified.ptm.strings = identified.ptm.strings,
      instruments = instruments,
      quantification.methods = quantification.methods,
      project.tags = project.tags,
      submission.type = submission.type
  )
}

#' Show the print-out version of the content in a ProjectSummary
#'
#' @param object a given ProjectSummary
#' @export
setMethod("show",
          signature = "ProjectSummary",
          definition = function(object) {
            cat("An object of class ", class(object), sep="")
            cat(" made public in ", as.character(object@publication.date), "\n", sep="")
            cat("    Accession: ", object@accession, "\n", sep="")
            cat("    Title: ", object@project.title, "\n", sep="")
            cat("    Description: ", object@project.description, "\n", sep="")
            cat("    Sample Processing Protocol: ", object@sample.processing.protocol, "\n", sep="")
            cat("    Data Processing Protocol: ", object@data.processing.protocol, "\n", sep="")
            for(val in object@organisms){
              cat("    Organism: ", val$name, "\n", sep="")
              cat("         Accession: ", val$accession, "\n", sep="")
            }
            for(val in object@organism.parts){
              cat("    Organism Part: ", val$name, "\n", sep=" ")
              cat("         Accession: ", val$accession, "\n", sep="")
            }
            for(val in object@diseases){
              cat("    Disease: ", val$name, "\n", sep="")
              cat("         Accession: ", val$accession, "\n", sep="")
            }
            for(val in object@identified.ptm.strings){
              cat("    PTM: ", val$name, "\n", sep=" ")
              cat("         Accession: ", val$accession, "\n", sep="")
            }
            for(val in object@instruments){
              cat("    Instrument: ", val$name, "\n", sep=" ")
              cat("         Accession: ", val$accession, "\n", sep="")
            }
            for(val in object@quantification.methods){
              cat("    Quantification Method: ", val$name, "\n", sep="")
              cat("         Accession: ", val$accession, "\n", sep="")
            }
            cat("    Tags: ", object@project.tags, "\n", sep=" ")
            cat("    Submission type: ", object@submission.type, "\n", sep="")
            invisible(NULL)
          }
)

#' Returns a project accession
#'
#' @param object a ProjectSummary
#' @return the accession
#' @author Jose A. Dianes
#' @export
setMethod("accession", "ProjectSummary", function(object) object@accession)

#' Replaces a project accession
#'
#' @param object a ProjectSummary
#' @param value the accession
#' @author Jose A. Dianes
#' @export
setReplaceMethod("accession", "ProjectSummary",
                 function(object, value) {
                   object@accession <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project title
#'
#' @param object a ProjectSummary
#' @return the project title
#' @author Jose A. Dianes
#' @export
setMethod("project.title", "ProjectSummary", function(object) object@project.title)

#' Replaces a project title
#'
#' @param object a ProjectSummary
#' @param value the title
#' @author Jose A. Dianes
#' @export
setReplaceMethod("project.title", "ProjectSummary",
                 function(object, value) {
                   object@project.title <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project description
#'
#' @param object a ProjectSummary
#' @return the project description
#' @author Jose A. Dianes
#' @export
setMethod("project.description", "ProjectSummary", function(object) object@project.description)

#' Replaces a project description
#'
#' @param object a ProjectSummary
#' @param value the project description
#' @author Jose A. Dianes
#' @export
setReplaceMethod("project.description", "ProjectSummary",
                 function(object, value) {
                   object@project.description <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project sample processing protocol
#'
#' @param object a ProjectSummary
#' @return the project sample processing protocol
#' @author Jose A. Dianes
#' @export
setMethod("sample.processing.protocol", "ProjectSummary", function(object) object@sample.processing.protocol)

#' Replaces a project sample processing protocol
#'
#' @param object a ProjectSummary
#' @param value the sample processing protocol
#' @author Jose A. Dianes
#' @export
setReplaceMethod("sample.processing.protocol", "ProjectSummary",
                 function(object, value) {
                   object@sample.processing.protocol <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project data processing protocol
#'
#' @param object a ProjectSummary
#' @return the project data processing protocol
#' @author Jose A. Dianes
#' @export
setMethod("data.processing.protocol", "ProjectSummary", function(object) object@data.processing.protocol)

#' Replaces a project data processing protocol
#'
#' @param object a ProjectSummary
#' @param value the data processing protocol
#' @author Jose A. Dianes
#' @export
setReplaceMethod("data.processing.protocol", "ProjectSummary",
                 function(object, value) {
                   object@data.processing.protocol <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project publication date
#'
#' @param object a ProjectSummary
#' @return the project publication date
#' @author Jose A. Dianes
#' @export
setMethod("publication.date", "ProjectSummary", function(object) object@publication.date)

#' Replaces a project publication date
#'
#' @param object a ProjectSummary
#' @param value the publication date
#' @author Jose A. Dianes
#' @export
setReplaceMethod("publication.date", "ProjectSummary",
                 function(object, value) {
                   object@publication.date <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project organisms
#'
#' @param object a ProjectSummary
#' @return the project organisms
#' @author Jose A. Dianes
#' @export
setMethod("organisms", "ProjectSummary", function(object) object@organisms)

#' Replaces the project organisms
#'
#' @param object a ProjectSummary
#' @param value the organisms
#' @author Jose A. Dianes
#' @export
setReplaceMethod("organisms", "ProjectSummary",
                 function(object, value) {
                   object@organisms <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project organism.parts
#'
#' @param object a ProjectSummary
#' @return the project organism.parts
#' @author Jose A. Dianes
#' @export
setMethod("organism.parts", "ProjectSummary", function(object) object@organism.parts)

#' Replaces the project organism.parts
#'
#' @param object a ProjectSummary
#' @param value the organism.parts
#' @author Jose A. Dianes
#' @export
setReplaceMethod("organism.parts", "ProjectSummary",
                 function(object, value) {
                   object@organism.parts <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project diseases
#'
#' @param object a ProjectSummary
#' @return the project diseases
#' @author Jose A. Dianes
#' @export
setMethod("diseases", "ProjectSummary", function(object) object@diseases)

#' Replaces a project diseases
#'
#' @param object a ProjectSummary
#' @param value the diseases
#' @author Jose A. Dianes
#' @export
setReplaceMethod("diseases", "ProjectSummary",
                 function(object, value) {
                   object@diseases <- value
                   if (validObject(object))
                     return(object)
                 }
)


#' Returns a project modification names
#'
#' @param object a ProjectSummary
#' @return the project modification names
#' @author Jose A. Dianes
#' @export
setMethod("identified.ptm.strings", "ProjectSummary", function(object) object@identified.ptm.strings)

#' Replaces the project PTMs
#'
#' @param object a ProjectSummary
#' @param value the PTMs
#' @author Jose A. Dianes
#' @export
setReplaceMethod("identified.ptm.strings", "ProjectSummary",
                 function(object, value) {
                   object@identified.ptm.strings <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project instrument names
#'
#' @param object a ProjectSummary
#' @return the project instrument names
#' @author Jose A. Dianes
#' @export
setMethod("instruments", "ProjectSummary", function(object) object@instruments)

#' Replaces the project instrument nanmes
#'
#' @param object a ProjectSummary
#' @param value the instrument names
#' @author Jose A. Dianes
#' @export
setReplaceMethod("instruments", "ProjectSummary",
                 function(object, value) {
                   object@instruments <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project quantification methods
#'
#' @param object a ProjectSummary
#' @return the project quantification methods
#' @author Jose A. Dianes
#' @export
setMethod("quantification.methods", "ProjectSummary", function(object) object@quantification.methods)

#' Replaces a project quantification methods
#'
#' @param object a ProjectSummary
#' @param value the quantification methods
#' @author Jose A. Dianes
#' @export
setReplaceMethod("quantification.methods", "ProjectSummary",
                 function(object, value) {
                   object@quantification.methods <- value
                   if (validObject(object))
                     return(object)
                 }
)


#' Returns a project tags
#'
#' @param object a ProjectSummary
#' @return the project tags
#' @author Jose A. Dianes
#' @export
setMethod("project.tags", "ProjectSummary", function(object) object@project.tags)

#' Replaces the project tags
#'
#' @param object a ProjectSummary
#' @param value the project tags
#' @author Jose A. Dianes
#' @export
setReplaceMethod("project.tags", "ProjectSummary",
                 function(object, value) {
                   object@project.tags <- value
                   if (validObject(object))
                     return(object)
                 }
)

#' Returns a project submission type
#'
#' @param object a ProjectSummary
#' @return the project submission type
#' @author Jose A. Dianes
#' @export
setMethod("submission.type", "ProjectSummary", function(object) object@submission.type)

#' Replaces the project submission type
#'
#' @param object a ProjectSummary
#' @param value the submission type
#' @author Jose A. Dianes
#' @export
setReplaceMethod("submission.type", "ProjectSummary",
                 function(object, value) {
                   object@submission.type <- value
                   if (validObject(object))
                     return(object)
                 }
)


format.ProjectSummary <- function(x, ...) paste0(x@accession, ", ", x@title)

#' Returns a ProjectSummary instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The ProjectSummary instance
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
from.json.ProjectSummary <- function(json.object) {
  res <- new("ProjectSummary",
             accession = ifelse(is.null(json.object$accession), MISSING_VALUE, json.object$accession),
             project.title = ifelse(is.null(json.object$title), MISSING_VALUE, json.object$title),
             project.description = ifelse(is.null(json.object$projectDescription), MISSING_VALUE, json.object$projectDescription),
             sample.processing.protocol = ifelse(is.null(json.object$sampleProcessingProtocol) || (length(json.object$sampleProcessingProtocol)==0), MISSING_VALUE, json.object$sampleProcessingProtocol),
             data.processing.protocol = ifelse(is.null(json.object$dataProcessingProtocol) || (length(json.object$dataProcessingProtocol)==0), MISSING_VALUE, json.object$dataProcessingProtocol),
             publication.date = as.POSIXct(json.object$publicationDate),
             organisms = ifelse(is.null(json.object$organisms) || (length(json.object$organisms)==0), MISSING_VALUE_LIST, json.object$organisms),
             diseases = ifelse(is.null(json.object$diseases) || (length(json.object$diseases)==0), MISSING_VALUE_LIST, json.object$diseases),
             organism.parts = ifelse(is.null(json.object$organismParts) || (length(json.object$organismParts)==0), MISSING_VALUE_LIST, json.object$organismParts),
             identified.ptm.strings = ifelse(is.null(json.object$identifiedPTMStrings) || (length(json.object$identifiedPTMStrings)==0), MISSING_VALUE_LIST, json.object$identifiedPTMStrings),
             instruments = ifelse(is.null(json.object$instruments) || (length(json.object$instruments)==0), MISSING_VALUE_LIST, json.object$instruments),
             quantification.methods = ifelse(is.null(json.object$quantificationMethods) || (length(json.object$quantificationMethods)==0), MISSING_VALUE_LIST, json.object$quantificationMethods),
             project.tags = ifelse(is.null(json.object$projectTags) || (length(json.object$projectTags)==0), MISSING_VALUE, json.object$projectTags),
             submission.type = ifelse(is.null(json.object$submissionType), MISSING_VALUE, json.object$submissionType)
  )

  return (res)
}

#' Returns a PRIDE Archive project
#'
#' @param accession The project accession
#' @return The project in as object
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.ProjectSummary <- function(accession) {
  from.json.ProjectSummary(fromJSON(file=paste0(pride_archive_url, "/projects/", accession), method="C"))
}

#' Returns a list of PRIDE Archive project summaries
#'
#' @param count the maximum number of projects
#' @return The list of ProjectSummary objects
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
list.ProjectSummary <- function(page.size=10, page.number = 0) {
  json.list <- fromJSON(file=paste0(pride_archive_url, "/projects?pageSize=", page.size, "&page=", page.number), method="C")
  project.list <- lapply(json.list[[1]]$projects, function(x) { from.json.ProjectSummary(x)})
  return(project.list)
}



