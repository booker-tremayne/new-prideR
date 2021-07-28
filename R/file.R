pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive/v2"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive/v2"

MISSING_VALUE <- "Not Available"

format.FileDetail <- function(x, ...) paste0(x@file.name, ", ", x@project.accession)

#' FileDetailList represents a list of FileDetails organized by their associated project
#'
#' @param ProjectSummary is the file(s) associated project
#' @param FileDetailList is the list of file details that are within the project
#' @param file.count is the amount of files associated with the project
#' @author Tremayne Booker
#' @export
FileDetailList <- function(ProjectSummary){
  files <- get.FileDetail(ProjectSummary@accession)
  file.count <- length(files)
  FileDetailList <- list(
    project = ProjectSummary,
    file.count = file.count,
    file.list = files
  )
  test <- structure(FileDetailList, class = "FileDetailList")
  return(test)
}


#' FileDetail represents a PRIDE Archive file
#'
#' @export
#' @exportClass FileDetail
setClass(
  "FileDetail",
  slots = c(
    project.accession = "character",
    file.name = "character",
    file.type = "character",
    file.bytes = "numeric",
    download.link = "character"
  ),

  prototype = list(
    project.accession = MISSING_VALUE,
    file.name = MISSING_VALUE,
    file.type = MISSING_VALUE,
    file.bytes = 0,
    download.link = MISSING_VALUE
  ),

  validity = function(object) {

    # check project.accession
    if (!is.character(object@project.accession) || nchar(object@project.accession) == 0 || is.na(object@project.accession))
      return("'project.accession' must be a single valid string")

    # check file.name
    if (!is.character(object@file.name) || nchar(object@file.name) == 0 || is.na(object@file.name))
      return("'file.name' must be a single valid string")

    # check file.bytes
    if (!is.numeric(object@file.bytes) || object@file.bytes < 0 ||is.na(object@file.bytes))
      return("'file.bytes' must be a none negative number")

    # check download.link
    if (!is.character(object@download.link) || nchar(object@download.link) == 0 || is.na(object@download.link))
      return("'download.link' must be a single valid string")

    return(TRUE)
  }
)

#' Constructor for FileDetail
#'
#' @param project.accession project accession
#' @param file.name the name of the file
#' @param file.type the type of the file. e.g. RAW, PEAK, RESULT and etc
#' @param file.bytes the size of the file
#' @param download.link URL for downloading the file
FileDetail <- function(project.accession, file.name, file.type, file.bytes, download.link) {
  new("FileDetail",
      project.accession = project.accession,
      file.name = file.name,
      file.type = file.type,
      file.bytes = file.bytes,
      download.link = download.link)
}

#' print out the details of the FileDetail on screen
#'
#' @param object a FileDetail object
#' @author Jose A. Dianes
#' @export
setMethod("show",
          signature = "FileDetail",
          definition = function(object) {
            cat("An object of class ", class(object), "\n", sep="")
            cat("    Project accession: ", object@project.accession, "\n", sep="")
            cat("    File name: ", object@file.name, "\n", sep="")
            cat("    File type: ", object@file.type, "\n", sep="")
            cat("    File size: ", object@file.bytes, "\n", sep="")
            cat("    Download link: ", object@download.link, "\n", sep="")
            invisible(NULL)
          }
)


#' Returns a file project.assay
#'
#' @param object a FileDetail
#' @return the project accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession", "FileDetail", function(object) object@project.accession)

#' Replaces a file project accession
#'
#' @param object a FileDetail
#' @param value the project.accession
#' @author Jose A. Dianes
#' @export
setReplaceMethod("project.accession", "FileDetail",
                 function(object, value) {
                   object@project.accession <- value
                   if (validObject(object)) {
                     return(object)
                   }
                 }
)

#' Returns a file name
#'
#' @param object a FileDetail
#' @return the file.name
#' @author Jose A. Dianes
#' @export
setMethod("file.name", "FileDetail", function(object) object@file.name)

#' Replaces a file name
#'
#' @param object a FileDetail
#' @param value the file.name
#' @author Jose A. Dianes
#' @export
setReplaceMethod("file.name", "FileDetail",
                 function(object, value) {
                   object@file.name <- value
                   if (validObject(object)) {
                     return(object)
                   }
                 }
)

#' Returns a file type
#'
#' @param object a FileDetail
#' @return the file.type
#' @author Jose A. Dianes
#' @export
setMethod("file.type", "FileDetail", function(object) object@file.type)

#' Replaces a file type
#'
#' @param object a FileDetail
#' @param value the file.type
#' @author Jose A. Dianes
#' @export
setReplaceMethod("file.type", "FileDetail",
                 function(object, value) {
                   object@file.type <- value
                   if (validObject(object)) {
                     return(object)
                   }
                 }
)
#' Returns a file size
#'
#' @param object a FileDetail
#' @return the file.bytes
#' @author Jose A. Dianes
#' @export
setMethod("file.bytes", "FileDetail", function(object) object@file.bytes)

#' Replaces a file size
#'
#' @param object a FileDetail
#' @param value the file.bytes
#' @author Jose A. Dianes
#' @export
setReplaceMethod("file.bytes", "FileDetail",
                 function(object, value) {
                   object@file.bytes <- value
                   if (validObject(object)) {
                     return(object)
                   }
                 }
)

#' Returns a file download link
#'
#' @param object a FileDetail
#' @return the download.link
#' @author Jose A. Dianes
#' @export
setMethod("download.link", "FileDetail", function(object) object@download.link)

#' Replaces a file download.link
#'
#' @param object a FileDetail
#' @param value the download.link
#' @author Jose A. Dianes
#' @export
setReplaceMethod("download.link", "FileDetail",
                 function(object, value) {
                   object@download.link <- value
                   if (validObject(object)) {
                     return(object)
                   }
                 }
)

#' Returns a FileDetail instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The AssayDetail instance
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#  removed assay
#  changed fileType to fileCategory
#  changed fileSize to fileSizeBytes
#  changed downloadLink to publicFileLocations
#  removed file.source
from.json.FileDetail <- function(json.object) {
  if(json.object$publicFileLocations[[1]]$name == "FTP Protocol"){
    download.link.1 <- json.object$publicFileLocations[[1]]$value
  }
  else{
    download.link.1 <- json.object$publicFileLocations[[2]]$value
  }
  res <- new("FileDetail",
             project.accession = json.object$projectAccessions,
             file.name = json.object$fileName,
             file.type = json.object$fileCategory$value,
             file.bytes = json.object$fileSizeBytes,
             download.link = download.link.1
  )

  return (res)
}

#' Returns a list of PRIDE Archive files associated with a project
#'
#' @param project.accession the project accession
#' @return The list of FileDetail objects
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.FileDetail <- function(project.accession) {
  json.list <- fromJSON(file=paste0(pride_archive_url, "/files/byProject?accession=", project.accession), method="C")
  file.list <- lapply(json.list, function(x) { from.json.FileDetail(x)})
  return(file.list)
}

#' Returns a FileDetailList for each of the projects given to it in project.list
#'
#' @param project.accession the project accession
#' @return The FileDetailList
#' @author Tremayne Booker
#' @details TODO
#' @export
get.FileDetail.ProjectSummary.List <- function(project.list){
  file.list <- lapply(project.list, function(x) { FileDetailList(x)})
  return(file.list)
}

#' Finds all the files with the user given file name
#'
#' @param file.name the name of the file user wishes to find
#' @return The FileDetail requested
#' @author Tremayne Booker
#' @details I dunno
#' @importFrom rjson fromJSON
#' @export
get.FileDetail.by.name <- function(file.name){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/files/fileByName?fileName=", file.name))
  file.list <- from.json.FileDetail(json.list)
  return(file.list)
}

#' Searches a list of projects for keyword matches
#'
#' @param project.list the list of projects the user wishes to search the files through
#' @param keywords the word or words the user is searching for in files
#' @param all TRUE/FALSE whether the user wants to match using AND or OR. Defaults to OR
#' @param file.size.min is the lower range of file memory size the user can specify
#' @param file.size.max is the upper range of file memory size the user can specify
#' @return the FileDetailList list of successful matches
#' @author Tremayne Booker
#' @details TODO
#' @export
search.FileDetail <- function(project.list, keywords = "", filetype = " ", all = FALSE, file.size.min = 0, file.size.max = 99999999999, use.regex = FALSE){
  new.project.list <- vector("list", length(project.list))
  matches <- 0
  if (class(project.list[[1]]) != "FileDetailList") {
    project.list <- lapply(project.list, function(x) { FileDetailList(x)})
  }
  new.project.list <- lapply(project.list, search.FileDetail.loop, all = all, keywords = keywords, filetype = filetype, file.size.min = file.size.min, file.size.max = file.size.max, use.regex = use.regex)
  new.project.list<-new.project.list[!sapply(new.project.list, is.null)]
  return(new.project.list)
}


#' Extension of search.project.list if the user would like AND matches
#'
#' @param project.list the list of projects the user wishes to search the files through
#' @param keywords the word or words the user is searching for in files
#' @return the project list of successful matches
#' @author Tremayne Booker
#' @details i dunno
search.FileDetail.loop <- function(project, keywords, all, filetype, file.size.min, file.size.max, use.regex){
  matches <- 0
  for (word in keywords){
    success <- FALSE

    for (file in project$file.list){

      if(!all &                                                                     # Checks whether or not it should use ALL logic or ANY logic
         grepl(word, file@file.name, fixed = use.regex) &                           # Checks if a keyword matches the file name
         (file@file.bytes >= file.size.min & file@file.bytes <= file.size.max) &    # Checks if the file is within the file size requirement
         (filetype == " " | file@file.type == filetype)){                           # Checks if the file matches the file type requirement
        return(project)
      }
      else if(all &
              grepl(file@file.name, word, fixed = use.regex) &
              (file@file.bytes >= file.size.min & file@file.bytes <= file.size.max) &
              (filetype == " " | file@file.type == filetype)){
        matches <- matches + 1
        success <- TRUE
        break
      }

    }
    if(all & !success) return(NULL)
    else if(all & matches == length(keywords)) return(project)
  }
  return(NULL)
}



#' Searches for project and downloads all files from it
#'
#' @param project.accession is the project to be searched for
#' @param file.dir is the directory the file is to be downloaded at
#' @author Tremayne Booker
#' @details i dunno
#' @export
download.files.from.accession <- function(project.accession, file.dir){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/files/byProject?accession=", project.accession), method="C")
  file.list <- lapply(json.list, function(x) { from.json.FileDetail(x)})
  for(val in file.list){
    print(val)
    download.fileDetail(val, file.dir)
  }
}

#' Downloads all projects from given project list
#'
#' @param project.accession is the project to be searched for
#' @param file.dir is the directory the file is to be downloaded at
#' @author Tremayne Booker
#' @details i dunno
#' @export
download.files.from.project.list <- function(project.list, file.dir){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/files/byProject?accession=", project.accession), method="C")
  file.list <- lapply(json.list, function(x) { from.json.FileDetail(x)})
  for(project in project.list){
    directory <- paste0(file.dir, "/", project@project.title)
    download.files.from.accession(project@accession, directory)
  }
}

#' Downloads files given to it
#'
#' @param file is the file to be downloaded
#' @param file.dir is the directory the file is to be downloaded at
#' @author Tremayne Booker
#' @details i dunno
#' @export
download.fileDetail <- function(file, file.dir){
  file.dir <- paste0(file.dir, "/", file@file.name)
  download.file(file@download.link, file.dir)
}

#' Searches for a file by name and downloads it
#'
#' @param file.name is the name of the file
#' @param file.dir is the directory the file is to be downloaded at
#' @author Tremayne Booker
#' @details i dunno
#' @export
download.file.by.name <- function(file.name, file.dir){
  file <- search.file.by.name(file.name)
  download.fileDetail(file, file.dir)
}
