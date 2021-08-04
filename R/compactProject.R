pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive/v2"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive/v2"

MISSING_VALUE <- "Not Available"

#' compactProjectSummary represents a PRIDE Archive project dataset
#' @import methods
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
    instruments = "character",
    diseases = "character",
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
    instruments = MISSING_VALUE,
    diseases = MISSING_VALUE,
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

    # check instruments
    if (!is.character(object@instruments) || 0 %in% nchar(object@instruments) || is.na(object@instruments))
      return("'instruments' must be a one or multiple valid strings")

    # check diseases
    if (!is.character(object@diseases) || 0 %in% nchar(object@diseases) || is.na(object@diseases))
      return("'diseases' must be a one or multiple valid strings")

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
#' @param accession The project accession - equivalent to project ID
#' @param project.title The title of the project
#' @param project.description The description of the project
#' @param publication.date The date when the project was made public by PRIDE
#' @param organisms The organisms of the project
#' @param organism.parts The organism.parts of the project
#' @param instruments The names of the instruments used in the project
#' @param diseases The diseases analyzed in the project
#' @param project.tags The tags for the project
#' @param submission.type The type of the submission, e.g. COMPLETE, PARTIAL or PRIDE
#' @param lab.PIs The principal investigators of the project
#' @param submitters The person/people who submitted the project
#' @param affiliations The groups the "lab.PIs" and "submitters" are affiliated with
compactProjectSummary <- function(accession,
                           project.title,
                           project.description,
                           publication.date,
                           organisms,
                           organism.parts,
                           instruments,
                           diseases,
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
      instruments = instruments,
      diseases = diseases,
      project.tags = project.tags,
      submission.type = submission.type,
      lab.PIs = lab.PIs,
      submitters = submitters,
      affiliations = affiliations
  )
}

#' Show the print-out version of the content in a compactProjectSummary
#'
#' @param object The given compactProjectSummary
#' @export
setMethod("show",
          signature = "compactProjectSummary",
          definition = function(object) {
            cat("An object of class ", class(object), sep="")
            cat(" made public in ", as.character(object@publication.date), "\n", sep="")
            cat("    Accession: ", object@accession, "\n", sep="")
            cat("    Title: ", object@project.title, "\n", sep="")
            cat("    Description: ", object@project.description, "\n", sep="")
            if(object@organisms != MISSING_VALUE) cat("    Organisms: ", object@organisms, "\n", sep="")
            if(object@organism.parts != MISSING_VALUE) cat("    Organism Parts: ", object@organism.parts, "\n", sep="")
            if(object@instruments != MISSING_VALUE) cat("    Instruments: ", object@instruments, "\n", sep="")
            if(object@diseases != MISSING_VALUE) cat("    Diseases: ", object@diseases, "\n", sep="")
            if(object@project.tags != MISSING_VALUE) cat("    Tags: ", object@project.tags, "\n", sep="")
            if(object@submission.type != MISSING_VALUE) cat("    Submission type: ", object@submission.type, "\n", sep="")
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

#' Returns a project disease names
#'
#' @param object a compactProjectSummary
#' @return the project disease names
#' @author Jose A. Dianes
#' @export
setMethod("diseases", "compactProjectSummary", function(object) object@diseases)

#' Replaces the project disease nanmes
#'
#' @param object a compactProjectSummary
#' @param value the disease names
#' @author Jose A. Dianes
#' @export
setReplaceMethod("diseases", "compactProjectSummary",
                 function(object, value) {
                   object@diseases <- value
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
#' @param json.object The JSON object from Pride to be made into a compactProjectSummary
#' @return The compactProjectSummary instance
#' @author Tremayne Booker
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
             instruments = ifelse(is.null(json.object$instruments) || (length(json.object$instruments)==0), MISSING_VALUE, json.object$instruments),
             diseases = ifelse(is.null(json.object$diseases) || (length(json.object$diseases)==0), MISSING_VALUE, json.object$diseases),
             project.tags = ifelse(is.null(json.object$projectTags) || (length(json.object$projectTags)==0), MISSING_VALUE, json.object$projectTags),
             submission.type = ifelse(is.null(json.object$submissionType), MISSING_VALUE, json.object$submissionType),
             lab.PIs = ifelse(is.null(json.object$labPIs) || (length(json.object$labPIs)==0), MISSING_VALUE, json.object$labPIs),
             submitters = ifelse(is.null(json.object$submitters) || (length(json.object$submitters)==0), MISSING_VALUE, json.object$submitters),
             affiliations = ifelse(is.null(json.object$affiliations) || (length(json.object$affiliations)==0), MISSING_VALUE, json.object$affiliations)
  )

  return (res)
}


#' Returns a series of PRIDE Archive projects to satisfy a given query.
#'
#' @param keywords The query term or terms
#' @param page.size The number of projects from Pride put into the compactProjectSummary list. Limit is 100
#' @param page.number The number of the page retrieved from Pride
#' @param sort.direction The direction the list is sorted by. Can be DESC (descending) or ASC (ascending)
#' @param organism The filterable field organism
#' @param organism.part The filterable field organism.part
#' @param instrument The filterable field instrument
#' @param disease The filterable field disease
#' @param modification The filterable field modification. Referred to as "identified PTM strings" elsewhere in package. Note that it is not present in the compactProjectSummary
#' @param project.tag The filterable field project tag. Referred to as "tags" elsewhere in package
#' @param project.keyword The filterable field project keyword. Not the same as the parameter "keywords". This field are keywords submitters specified to include in their project
#' @param country The filterable field country. Note that it is not present in the compactProjectSummary
#' @return The search results in a list of objects
#' @author Tremayne Booker
#' @details "Keywords" searched for search through every piece of metadata in the project for matches. It can accept multiple words as a character vector
#'          They are also matched using ANY logic, meaning that if a keyword is present anywhere in the project
#'          it will return that project even if other keywords are missing.
#'          All parameters after "sort.direction" are filters that only check their respective fields for matches.
#'          The filters must be matched exactly how they appear in the meta data, otherwise it will not return projects correctly.
#'          For example, if you enter "Coloncancer" instead of "Colon cancer" it will not return any projects. It is not case sensitive, though.
#' @importFrom rjson fromJSON
#' @export
search.ProjectSummary <- function(keywords = "", page.size=10, page.number = 0, sort.direction = "DESC", organism = "", organism.part = "", instrument = "", disease = "", modification = "", project.tag = "", project.keyword = "", country = "") {
  q <- ""
  f <- ""

  q <- paste0("keyword=", keywords, "&", collapse = "")

  f <- lapply(organism, create.filter, "organisms==", f)
  f <- lapply(organism.part, create.filter, "organisms_part==", f)
  f <- lapply(instrument, create.filter, "instruments==", f)
  f <- lapply(modification, create.filter, "project_identified_ptms==", f)
  f <- lapply(project.tag, create.filter, "project_tags==", f)
  f <- lapply(project.keyword, create.filter, "project_keywords==", f)
  f <- lapply(disease, create.filter, "diseases==", f)
  f <- lapply(country, create.filter, "country==", f)

  json.list <- fromJSON(file=paste0(pride_archive_url, "/search/projects?", q, "filter=", f, "&pageSize=", page.size, "&page=", page.number, "&sortDirection=", sort.direction), method="C")
  project.list <- lapply(json.list[[1]]$compactprojects, function(x) { from.json.compactProjectSummary(x)})
  return(project.list)
}

#' Creates the url for all of the specified filters
#'
#' @param value is the value to be filtered in the field. ex "Colon cancer" for field disease
#' @param field is the field to be filtered
#' @param url.string is the subset of the url for the filter
#' @author Tremayne Booker
create.filter <- function(value, field, url.string){
  url.string <- paste0(url.string, field, value, ",")
  return(url.string)
}

#' Returns similar projects from the project with the given accession
#'
#' @param accession Project accession to find similar projects of
#' @param page.size The number of projects from Pride put into the compactProjectSummary list. Limit is 100
#' @param page.number The number of the page retrieved from Pride
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

