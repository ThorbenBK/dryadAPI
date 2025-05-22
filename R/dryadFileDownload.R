#' @title Interactively Download a File from a Specific Dryad Dataset Version
#'
#' @description
#' `dryadFileDownload()` allows interactive access to files from different versions of a dataset hosted on Dryad, using the dataset's DOI. 
#' The user can select a version and a file from the list of available options. The file will be downloaded into a prior determine folder under a prior determined name. 
#'
#' @param doi A character string specifying the dataset DOI, in the form of "https://doi.org/..."
#' @param path A character string specifying the location of the file. Default: "~/Downloads"
#' @param name A character string specifying the name of the file. Default: NA (name of the file on DRYAD)
#' 
#' @return Invisibly downloadeds a file, or `NULL` if no file was selected.
#'
#' @details 
#' This function first queries the Dryad API to obtain all available versions of a dataset associated with the given DOI. 
#' The user is then prompted to select a version via an interactive menu. After selecting a version, all associated files are listed, and the user selects one for download.  
#' The user is prompted to download the file to their `~/Downloads` folder if not specified otherwise.   
#' The function exits if the user cancels any step.
#'
#' @seealso [dryad_version_overview()], [dryadFileDownloadID()], [dryadFileRead()]
#'
#' @examples
#' \dontrun{
#' dryadFileDownload("https://doi.org/10.5061/dryad.z08kprrk1")
#' }
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @export

dryadFileDownload <- function(doi, path="~/Downloads", name=NA){
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
  cat("Available files in dataset:\n")
  choice <- menu(file_names, title = "Choose a file to download")
  if (choice == 0) {
    cat("No file selected. Exiting.\n")
    return()
  }
  chosen_id2 <- file_ids[choice]
  chosen_file2 <- file_names[choice]
  chosen_ext2 <- file_ext[choice]
  download_url2 <- paste0("https://datadryad.org/api/v2/files/",chosen_id2, "/download")
  name <- if (is.na(name)) chosen_file2 else name
  download.file(url=download_url2, destfile = paste0(path, "/", name), mode = "wb" )
}