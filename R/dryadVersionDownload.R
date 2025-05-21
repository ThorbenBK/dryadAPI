#' @title Download All Files from a Dryad Dataset Version
#'
#' @description `dryadVersionDownload()` downloads all files from a specific version of a Dryad dataset, given its DOI. The user selects the desired version if multiple versions are available. All files are saved to the specified directory.
#'
#' @param doi A character string specifying the dataset DOI, in the form of "https://doi.org/..."
#' @param path A character string specifying the location of the file. Default: "~/Downloads"
#'
#' @return Invisibly downloads one or more files to the specified folder. Returns `NULL` if no version is selected.
#'
#' @details
#' This function interacts with the Dryad REST API to retrieve dataset version information and download all associated files for the selected version.
#'
#' @seealso [dryad_version_overview()], [dryadFileDownloadID()], [dryadFileDownload()]
#'
#' @examples
#' \dontrun{
#' dryadVersionDownload("https://doi.org/10.5061/dryad.z08kprrk1")
#' }
#' 
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @export
dryadVersionDownload <- function(doi, path="~/Downloads"){
  encoded_doi <- str_replace_all(doi, c("https://doi.org/" = "doi%253A", "/" = "%2F"))
  response <- GET(paste("https://datadryad.org/api/v2/datasets/",encoded_doi,"/versions?page=1&per_page=100", sep=""))
  text <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(text, flatten=TRUE)
  lastModification = data[["_embedded"]][["stash:versions"]][["lastModificationDate"]]
  id = sub(".*/", "", data[["_embedded"]][["stash:versions"]][["_links.self.href"]])
  cat("Date of Version:\n")
  choice <- menu(lastModification, title = "Choose a Version to proceed with")
  if (choice == 0) {
    cat("No file selected. Exiting.\n")
    return()
  }
  chosen_id <- id[choice]
  response2 <- GET(paste("https://datadryad.org/api/v2/versions/",chosen_id,"/files?page=1&per_page=100", sep=""))
  text2 <- content(response2, as = "text", encoding = "UTF-8")
  data2 <- fromJSON(text2, flatten=TRUE)
  file_names = data2[["_embedded"]][["stash:files"]][["path"]]
  file_ids = sub(".*/", "",data2[["_embedded"]][["stash:files"]][["_links.self.href"]])
  file_ext = sub(".*/", "",data2[["_embedded"]][["stash:files"]][["mimeType"]])
 for(i in seq_along(file_ids)){
   download_url2 <- paste0("https://datadryad.org/api/v2/files/",i, "/download")
   download.file(fileext=paste0(".", file_ext[i]), url=download_url2, destfile = paste0(path,"/",file_names[i]))
 }}

