#' @title Download a Specific File from Dryad based on its ID 
#'
#' @description
#' `dryadFileDownload()` allows downloading a file of a dataset hosted on Dryad directly by using its ID. 
#' The user provides the file ID and the extension. The file will be downloaded into a prior determine folder under a prior determined name. 
#'
#' @param id A character string specifying the file ID.
#' @param ext A character string specifying the file extension.
#' @param path A character string specifying the location of the file. Default: "~/Downloads"
#' @param name A character string specifying the name of the file. Default: "data"
#'  
#' 
#' @return Invisibly downloadeds a file, or `NULL` if no file was selected.
#' 
#' @details 
#' This function directly accesses the file based on its ID. 
#' The user is prompted to download the file to their `~/Downloads` folder if not specified otherwise. 
#' 
#' @seealso [dryad_version_overview()], [dryadFileDownload()], [dryadFileReadID()]
#' 
#' @examples
#' \dontrun{
#' dryadFileDownloadID("3833094", "xlsx")
#' }
#'
#' @export
#'
dryadFileDownloadID <- function(id, ext, path="~/Downloads", name="data"){
  if(missing(id) || missing(ext) || id == "" || ext == "") {
    stop("Both 'id' and 'ext' must be provided and non-empty.")
  }
  download_url2 <- paste0("https://datadryad.org/api/v2/files/",id, "/download")
  download.file(url=download_url2, destfile = paste0(path, "/", name, ".", ext), mode = "wb" )
}

