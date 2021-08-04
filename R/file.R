pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive/v2"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive/v2"

MISSING_VALUE <- "Not Available"

format.FileDetail <- function(x, ...) paste0(x@file.name, ", ", x@project.accession)

#' FileDetailList represents a list of FileDetails organized by their associated project
#'
#' @param ProjectSummary The file(s) associated project
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

#' Print out FileDetailList object
#'
#' @param x The FileDetailList object to be printed
#' @param ... unused
#' @author Tremayne Booker
#' @export
print.FileDetailList <- function(x, ...) {
            cat("An object of class ", class(x), sep="")
            cat(" Containing ", x$file.count, " files", "\n", sep="")
            cat("   Associated Project: ", "\n", sep="")
            print(x$project)
            cat("\n")
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

#' Print out the details of the FileDetail on screen
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
#' @param json.object The JSON object to be made into a FileDetail object
#' @return The FileDetail instance
#' @author Tremayne Booker
#' @details Pride files contain two download links. This package can only download
#'          from the FTP protocol, so it only stores the download link for the FTP Protocol.
#' @importFrom rjson fromJSON
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
#' @param project.accession The project accession
#' @return The list of FileDetail objects of the project
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
#' @param project.list The list of ProjectSummary objects to be made into a FileDetailList
#' @return The FileDetailList of all the projects in the project.list
#' @author Tremayne Booker
#' @details TODO
#' @export
get.FileDetailList <- function(project.list){
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
#' @param project.list The list of projects the user wishes to search the files through
#' @param keywords The word or words the user is searching for in files
#' @param filetype The category of file. Categories are described by Pride
#' @param file.size.min The lower range of file memory size
#' @param file.size.max The upper range of file memory size
#' @param all Determines whether the to match using ALL or ANY logic. Defaults to ANY
#' @param use.regex Determines whether the keywords are regex patterns or should be matched literally
#' @return the FileDetailList list of successful matches
#' @author Tremayne Booker
#' @details This functionality is not built natively into the Pride web service.
#'          Searching through a ProjectSummary list will take a long time.
#'          For multiple searches on the same ProjectSummary list, it is recommended to create
#'          FileDetailList first, then search through that. This can be done by inputting the
#'          ProjectSummary list into "get.FileDetailList()".
#'          While this function can accept ProjectSummaries, it will always return a FileDetailList.
#'          This is to ensure recursive searches are faster.
#' @export
search.FileDetail <- function(project.list, keywords = "", filetype = " ", file.size.min = 0, file.size.max = 99999999999, all = FALSE, use.regex = FALSE){
  new.project.list <- vector("list", length(project.list))
  if (class(project.list) != "list"){
    project.list <- list(project.list)
  }
  if (class(project.list[[1]]) != "FileDetailList") {
    project.list <- lapply(project.list, function(x) { FileDetailList(x)})
  }
  new.project.list <- lapply(project.list, search.FileDetail.loop, all = all, keywords = keywords, filetype = filetype, file.size.min = file.size.min, file.size.max = file.size.max, use.regex = use.regex)
  new.project.list <- new.project.list[!sapply(new.project.list, is.null)]
  return(new.project.list)
}


#' The function that determines whether the project has successfully matching files
#'
#' @param project The project to search the files through
#' @param keywords The word or words the user is searching for in files
#' @param filetype The category of file. Categories are described by Pride
#' @param file.size.min The lower range of file memory size
#' @param file.size.max The upper range of file memory size
#' @param all Determines whether the to match using ALL or ANY logic. Defaults to ANY
#' @param use.regex Determines whether the keywords are regex patterns or should be matched literally
#' @return the project list of successful matches
#' @author Tremayne Booker
#' @details i dunno
search.FileDetail.loop <- function(project, keywords = "", filetype = " ", file.size.min = 0, file.size.max = 99999999999, all = FALSE, use.regex = FALSE){
  matches <- 0
  for (word in keywords){
    word <- tolower(word)
    success <- FALSE

    for (file in project$file.list){
      file.name <- tolower(file@file.name)

      if(!all &                                                                            # Checks whether or not it should use ALL logic or ANY logic
         grepl(word, file.name, fixed = use.regex) &                                       # Checks if a keyword matches the file name
         (file@file.bytes >= file.size.min & file@file.bytes <= file.size.max) &           # Checks if the file is within the file size requirement
         (filetype == " " | file@file.type == filetype)){                                  # Checks if the file matches the file type requirement
        return(project)
      }
      else if(all &
              grepl(word, file.name, fixed = use.regex) &
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
download.by.accession <- function(project.accession, file.dir){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/files/byProject?accession=", project.accession), method="C")
  file.list <- lapply(json.list, function(x) { from.json.FileDetail(x)})
  for(val in file.list){
    print(val)
    download.by.fileDetail(val, file.dir)
  }
}

#' Downloads all projects from given project list
#'
#' @param project.list The project list to be searched for
#' @param file.dir The directory the file is downloaded at
#' @author Tremayne Booker
#' @details The file directory must exist already.
#'          This will create a folder for each project within the given directory.
#' @export
download.project.list <- function(project.list, file.dir){
  json.list <- fromJSON(file=paste0(pride_archive_url, "/files/byProject?accession=", project.accession), method="C")
  file.list <- lapply(json.list, function(x) { from.json.FileDetail(x)})
  for(project in project.list){
    directory <- paste0(file.dir, "/", project@project.title)
    download.by.accession(project@accession, directory)
  }
}

#' Downloads files given to it
#'
#' @param file is the file to be downloaded
#' @param file.dir is the directory the file is to be downloaded at
#' @author Tremayne Booker
#' @details i dunno
#' @importFrom utils download.file
#' @export
download.by.fileDetail <- function(file, file.dir){
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
download.by.name <- function(file.name, file.dir){
  file <- get.FileDetail.by.name(file.name)
  download.by.fileDetail(file, file.dir)
}
