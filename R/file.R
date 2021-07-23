pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive/v2"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive/v2"

MISSING_VALUE <- "Not Available"

format.FileDetail <- function(x, ...) paste0(x@file.name, ", ", x@project.accession)

#' FileDetail represents a PRIDE Archive file
#'
#' @import methods
#' @export
#' @exportClass FileDetail
setClass(
  "FileDetail",

  slots = c(
    project.accession = "character",
    file.name = "character",
    file.type = "character",
    file.size = "numeric",
    download.link = "character"
  ),

  prototype = list(
    project.accession = MISSING_VALUE,
    file.name = MISSING_VALUE,
    file.type = MISSING_VALUE,
    file.size = 0,
    download.link = MISSING_VALUE
  ),

  validity = function(object) {

    # check project.accession
    if (!is.character(object@project.accession) || nchar(object@project.accession) == 0 || is.na(object@project.accession))
      return("'project.accession' must be a single valid string")

    # check file.name
    if (!is.character(object@file.name) || nchar(object@file.name) == 0 || is.na(object@file.name))
      return("'file.name' must be a single valid string")

    # check file.size
    if (!is.numeric(object@file.size) || object@file.size < 0 ||is.na(object@file.size))
      return("'file.size' must be a none negative number")

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
#' @param file.size the size of the file
#' @param download.link URL for downloading the file
FileDetail <- function(project.accession, file.name, file.type, file.size, download.link) {
  new("FileDetail",
      project.accession = project.accession,
      file.name = file.name,
      file.type = file.type,
      file.size = file.size,
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
            cat("    File size: ", object@file.size, "\n", sep="")
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
#' @return the file.size
#' @author Jose A. Dianes
#' @export
setMethod("file.size", "FileDetail", function(object) object@file.size)

#' Replaces a file size
#'
#' @param object a FileDetail
#' @param value the file.size
#' @author Jose A. Dianes
#' @export
setReplaceMethod("file.size", "FileDetail",
                 function(object, value) {
                   object@file.size <- value
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
             file.size = json.object$fileSizeBytes,
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
project.FileDetail <- function(project.accession) {
  json.list <- fromJSON(file=paste0(pride_archive_url, "/files/byProject?accession=", project.accession), method="C")
  file.list <- lapply(json.list, function(x) { from.json.FileDetail(x)})
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
search.file.by.name <- function(file.name){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/files/fileByName?fileName=", file.name))
  file.list <- from.json.FileDetail(json.list)
  return(file.list)
}

#' Searches a list of projects for keyword matches
#'
#' @param project.list the list of projects the user wishes to search the files through
#' @param keywords the word or words the user is searching for in files
#' @param and TRUE/FALSE whether the user wants to match using AND or OR. Defaults to OR
#' @param file.size.low is the lower range of file memory size the user can specify
#' @param file.size.high is the upper range of file memory size the user can specify
#' @importFrom stringr regex
#' @importFrom stringr str_detect
#' @return the project list of successful matches
#' @author Tremayne Booker
#' @details TODO
#' @export
search.project.list <- function(project.list, keywords, and = FALSE, file.size.low = 0, file.size.high = 999999999){
  if(and){
    return(search.project.list.and(project.list, keywords))
  }
  new.project.list <- list()
  for (project in project.list){
    success <- FALSE
    accession <- project@accession
    file.list <- project.FileDetail(accession)
    for (file in file.list){
      for (word in keywords){
        if(str_detect(file@file.name, regex(word, ignore_case = TRUE))){
          if(file@file.size >= file.size.low & file@file.size <= file.size.high){
            new.project.list <- c(new.project.list, project)
            success <- TRUE
            break
          }
        }
      }
      if(success){
        break
      }
    }
  }
  return(new.project.list)
}

#' Extension of search.project.list if the user would like AND matches
#'
#' @param project.list the list of projects the user wishes to search the files through
#' @param keywords the word or words the user is searching for in files
#' @importFrom stringr regex
#' @importFrom stringr str_detect
#' @return the project list of successful matches
#' @author Tremayne Booker
#' @details i dunno
search.project.list.and <- function(project.list, keywords){
  new.project.list <- list()
  failure <- TRUE
  for (project in project.list){
    matches <- 0
    accession <- project@accession
    file.list <- project.FileDetail(accession)

      for (word in keywords){
        for (file in file.list){
          if(str_detect(file@file.name, regex(word, ignore_case = TRUE))){
            matches <- matches + 1
            failure <- FALSE
            break
          }
        }
        if(failure){
          break
        }
        failure <- TRUE
      }
    if(matches == length(keywords)){
      new.project.list <- c(new.project.list, project)
    }
  }
  return(new.project.list)
}


#' Searches for project and downloads all files from it
#'
#' @param project.accession is the project to be searched for
#' @param file.dir is the directory the file is to be downloaded at
#' @author Tremayne Booker
#' @details i dunno
#' @export
download.files.from.project <- function(project.accession, file.dir){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/files/byProject?accession=", project.accession), method="C")
  file.list <- lapply(json.list, function(x) { from.json.FileDetail(x)})
  for(val in file.list){
    print(val)
    download.fileDetail(val, file.dir)
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

#' Uploads given folder to specified directory on Pride
#'
#' @param folder.path the pathway to the folder to be uploaded
#' @param target.dir the directory on Pride where the folder is destined
#' @author Tremayne Booker
#' @details i dunno
#' @export
folder.upload.Pride <- function(folder.path, target.dir){
  upload_function <- paste0("ascp.exe -P33001 -QT -l300M L --file-manifest=text -k2 -o Overwrite=diff ", folder.path, " ")
  upload_function <- paste0(upload_function, "pride-drop-006@hx-fasp-1.ebi.ac.uk:/", target.dir)
  system(upload_function)
}
