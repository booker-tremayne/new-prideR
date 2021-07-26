# ProjectSummary related generics
if (!isGeneric("accession")) {
  setGeneric("accession", function(object) standardGeneric("accession"))
}

if (!isGeneric("accession<-")) {
  setGeneric("accession<-", function(object, value) standardGeneric("accession<-"))
}

if (!isGeneric("project.title")) {
  setGeneric("project.title", function(object) standardGeneric("project.title"))
}

if (!isGeneric("project.title<-")) {
  setGeneric("project.title<-", function(object, value) standardGeneric("project.title<-"))
}

if (!isGeneric("project.description")) {
  setGeneric("project.description", function(object) standardGeneric("project.description"))
}

if (!isGeneric("project.description<-")) {
  setGeneric("project.description<-", function(object, value) standardGeneric("project.description<-"))
}

if (!isGeneric("sample.processing.protocol")) {
  setGeneric("sample.processing.protocol", function(object) standardGeneric("sample.processing.protocol"))
}

if (!isGeneric("sample.processing.protocol<-")) {
  setGeneric("sample.processing.protocol<-", function(object, value) standardGeneric("sample.processing.protocol<-"))
}

if (!isGeneric("data.processing.protocol")) {
  setGeneric("data.processing.protocol", function(object) standardGeneric("data.processing.protocol"))
}

if (!isGeneric("data.processing.protocol<-")) {
  setGeneric("data.processing.protocol<-", function(object, value) standardGeneric("data.processing.protocol<-"))
}

if (!isGeneric("publication.date")) {
  setGeneric("publication.date", function(object) standardGeneric("publication.date"))
}

if (!isGeneric("publication.date<-")) {
  setGeneric("publication.date<-", function(object, value) standardGeneric("publication.date<-"))
}

if (!isGeneric("organisms")) {
  setGeneric("organisms", function(object) standardGeneric("organisms"))
}

if (!isGeneric("organisms<-")) {
  setGeneric("organisms<-", function(object, value) standardGeneric("organisms<-"))
}

if (!isGeneric("organism.parts")) {
  setGeneric("organism.parts", function(object) standardGeneric("organism.parts"))
}

if (!isGeneric("organism.parts<-")) {
  setGeneric("organism.parts<-", function(object, value) standardGeneric("organism.parts<-"))
}

if (!isGeneric("diseases")) {
  setGeneric("diseases", function(object) standardGeneric("diseases"))
}

if (!isGeneric("diseases<-")) {
  setGeneric("diseases<-", function(object, value) standardGeneric("diseases<-"))
}

if (!isGeneric("identified.ptm.strings")) {
  setGeneric("identified.ptm.strings", function(object) standardGeneric("identified.ptm.strings"))
}

if (!isGeneric("identified.ptm.strings<-")) {
  setGeneric("identified.ptm.strings<-", function(object, value) standardGeneric("identified.ptm.strings<-"))
}

if (!isGeneric("instruments")) {
  setGeneric("instruments", function(object) standardGeneric("instruments"))
}

if (!isGeneric("instruments<-")) {
  setGeneric("instruments<-", function(object, value) standardGeneric("instruments<-"))
}

if (!isGeneric("quantification.methods")) {
  setGeneric("quantification.methods", function(object) standardGeneric("quantification.methods"))
}

if (!isGeneric("quantification.methods<-")) {
  setGeneric("quantification.methods<-", function(object, value) standardGeneric("quantification.methods<-"))
}

if (!isGeneric("project.tags")) {
  setGeneric("project.tags", function(object) standardGeneric("project.tags"))
}

if (!isGeneric("project.tags<-")) {
  setGeneric("project.tags<-", function(object, value) standardGeneric("project.tags<-"))
}

if (!isGeneric("submission.type")) {
  setGeneric("submission.type", function(object) standardGeneric("submission.type"))
}

if (!isGeneric("submission.type<-")) {
  setGeneric("submission.type<-", function(object, value) standardGeneric("submission.type<-"))
}

if (!isGeneric("lab.PIs")) {
  setGeneric("lab.PIs", function(object) standardGeneric("lab.PIs"))
}

if (!isGeneric("lab.PIs<-")) {
  setGeneric("lab.PIs<-", function(object, value) standardGeneric("lab.PIs<-"))
}

if (!isGeneric("submitters")) {
  setGeneric("submitters", function(object) standardGeneric("submitters"))
}

if (!isGeneric("submitters<-")) {
  setGeneric("submitters<-", function(object, value) standardGeneric("submitters<-"))
}

if (!isGeneric("affiliations")) {
  setGeneric("affiliations", function(object) standardGeneric("affiliations"))
}

if (!isGeneric("affiliations<-")) {
  setGeneric("affiliations<-", function(object, value) standardGeneric("affiliations<-"))
}

#File Generics
if (!isGeneric("project.accession")) {
  setGeneric("project.accession", function(object) standardGeneric("project.accession"))
}

if (!isGeneric("project.accession<-")) {
  setGeneric("project.accession<-", function(object, value) standardGeneric("project.accession<-"))
}

if (!isGeneric("file.name")) {
  setGeneric("file.name", function(object) standardGeneric("file.name"))
}

if (!isGeneric("file.name<-")) {
  setGeneric("file.name<-", function(object, value) standardGeneric("file.name<-"))
}

if (!isGeneric("file.type")) {
  setGeneric("file.type", function(object) standardGeneric("file.type"))
}

if (!isGeneric("file.type<-")) {
  setGeneric("file.type<-", function(object, value) standardGeneric("file.type<-"))
}

if (!isGeneric("file.source")) {
  setGeneric("file.source", function(object) standardGeneric("file.source"))
}

if (!isGeneric("file.source<-")) {
  setGeneric("file.source<-", function(object, value) standardGeneric("file.source<-"))
}

if (!isGeneric("file.bytes")) {
  setGeneric("file.bytes", function(object) standardGeneric("file.bytes"))
}

if (!isGeneric("file.bytes<-")) {
  setGeneric("file.bytes<-", function(object, value) standardGeneric("file.bytes<-"))
}

if (!isGeneric("download.link")) {
  setGeneric("download.link", function(object) standardGeneric("download.link"))
}

if (!isGeneric("download.link<-")) {
  setGeneric("download.link<-", function(object, value) standardGeneric("download.link<-"))
}

