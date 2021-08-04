###ProjectSummary related generics

#' Project Summary related generics
#' @param object variable becomes an object
#' @param value value to be set into the object
#' @rdname projectgeneric
setGeneric("accession", function(object) standardGeneric("accession"))

#' @rdname projectgeneric
setGeneric("accession<-", function(object, value) standardGeneric("accession<-"))

#' @rdname projectgeneric
setGeneric("project.title", function(object) standardGeneric("project.title"))

#' @rdname projectgeneric
setGeneric("project.title<-", function(object, value) standardGeneric("project.title<-"))

#' @rdname projectgeneric
setGeneric("project.description", function(object) standardGeneric("project.description"))

#' @rdname projectgeneric
setGeneric("project.description<-", function(object, value) standardGeneric("project.description<-"))

#' @rdname projectgeneric
setGeneric("sample.processing.protocol", function(object) standardGeneric("sample.processing.protocol"))

#' @rdname projectgeneric
  setGeneric("sample.processing.protocol<-", function(object, value) standardGeneric("sample.processing.protocol<-"))

#' @rdname projectgeneric
setGeneric("data.processing.protocol", function(object) standardGeneric("data.processing.protocol"))

#' @rdname projectgeneric
setGeneric("data.processing.protocol<-", function(object, value) standardGeneric("data.processing.protocol<-"))

#' @rdname projectgeneric
setGeneric("publication.date", function(object) standardGeneric("publication.date"))

#' @rdname projectgeneric
setGeneric("publication.date<-", function(object, value) standardGeneric("publication.date<-"))

#' @rdname projectgeneric
setGeneric("organisms", function(object) standardGeneric("organisms"))

#' @rdname projectgeneric
setGeneric("organisms<-", function(object, value) standardGeneric("organisms<-"))

#' @rdname projectgeneric
setGeneric("organism.parts", function(object) standardGeneric("organism.parts"))

#' @rdname projectgeneric
setGeneric("organism.parts<-", function(object, value) standardGeneric("organism.parts<-"))

#' @rdname projectgeneric
setGeneric("diseases", function(object) standardGeneric("diseases"))

#' @rdname projectgeneric
setGeneric("diseases<-", function(object, value) standardGeneric("diseases<-"))

#' @rdname projectgeneric
  setGeneric("identified.ptm.strings", function(object) standardGeneric("identified.ptm.strings"))

#' @rdname projectgeneric
  setGeneric("identified.ptm.strings<-", function(object, value) standardGeneric("identified.ptm.strings<-"))

#' @rdname projectgeneric
  setGeneric("instruments", function(object) standardGeneric("instruments"))

#' @rdname projectgeneric
  setGeneric("instruments<-", function(object, value) standardGeneric("instruments<-"))

#' @rdname projectgeneric
  setGeneric("quantification.methods", function(object) standardGeneric("quantification.methods"))

#' @rdname projectgeneric
  setGeneric("quantification.methods<-", function(object, value) standardGeneric("quantification.methods<-"))

#' @rdname projectgeneric
  setGeneric("project.tags", function(object) standardGeneric("project.tags"))

#' @rdname projectgeneric
  setGeneric("project.tags<-", function(object, value) standardGeneric("project.tags<-"))

#' @rdname projectgeneric
  setGeneric("submission.type", function(object) standardGeneric("submission.type"))

#' @rdname projectgeneric
  setGeneric("submission.type<-", function(object, value) standardGeneric("submission.type<-"))

#' @rdname projectgeneric
  setGeneric("lab.PIs", function(object) standardGeneric("lab.PIs"))

#' @rdname projectgeneric
  setGeneric("lab.PIs<-", function(object, value) standardGeneric("lab.PIs<-"))

#' @rdname projectgeneric
  setGeneric("submitters", function(object) standardGeneric("submitters"))

#' @rdname projectgeneric
  setGeneric("submitters<-", function(object, value) standardGeneric("submitters<-"))

#' @rdname projectgeneric
  setGeneric("affiliations", function(object) standardGeneric("affiliations"))

#' @rdname projectgeneric
  setGeneric("affiliations<-", function(object, value) standardGeneric("affiliations<-"))


###FileDetail related Generics

#' File Detail related generics
#' @param object variable becomes an object
#' @param value value to be set into the object
#' @rdname filegeneric
setGeneric("project.accession", function(object) standardGeneric("project.accession"))

#' @rdname filegeneric
setGeneric("project.accession<-", function(object, value) standardGeneric("project.accession<-"))

#' @rdname filegeneric
setGeneric("file.name", function(object) standardGeneric("file.name"))

#' @rdname filegeneric
setGeneric("file.name<-", function(object, value) standardGeneric("file.name<-"))

#' @rdname filegeneric
setGeneric("file.type", function(object) standardGeneric("file.type"))

#' @rdname filegeneric
setGeneric("file.type<-", function(object, value) standardGeneric("file.type<-"))

#' @rdname filegeneric
setGeneric("file.source", function(object) standardGeneric("file.source"))

#' @rdname filegeneric
setGeneric("file.source<-", function(object, value) standardGeneric("file.source<-"))

#' @rdname filegeneric
setGeneric("file.bytes", function(object) standardGeneric("file.bytes"))

#' @rdname filegeneric
setGeneric("file.bytes<-", function(object, value) standardGeneric("file.bytes<-"))

#' @rdname filegeneric
setGeneric("download.link", function(object) standardGeneric("download.link"))

#' @rdname filegeneric
setGeneric("download.link<-", function(object, value) standardGeneric("download.link<-"))


