#' Read a file from dryad API into R
#'
#' dryadFileRead() allows to access a specific file from a specific version from a data set of dryad based on the doi for the datase.
#' @param doi a character vector with one element in the form of "https://...".
#' @note the function is currently limited to ".csv" and ".xlsx" files. For every other file, there is an option do download it.
#' @return Reads in the file or downloads it, if read-in is impossible.
#' @seealso [dryad_version_overview()]
#' @seealso [dryadFileReadID()]
#' @examples
#' dryadFileRead(https://doi.org/10.5061/dryad.z08kprrk1)
#' @importFrom stringr str_replace_all
#' #' @export
#'
dryadFileRead <- function(doi){
  encoded_doi <- stringr::str_replace_all(doi, c("https://doi.org/" = "doi%253A", "/" = "%2F"))
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
  choice <- menu(file_names, title = "Choose a file to read-in")
  if (choice == 0) {
    cat("No file selected. Exiting.\n")
    return()
  }
  chosen_id2 <- file_ids[choice]
  chosen_file2 <- file_names[choice]
  chosen_ext2 <- file_ext[choice]
  if (chosen_ext2 %in% c("csv", "xlsx")){
  download_url2 <- paste0("https://datadryad.org/api/v2/files/", chosen_id2, "/download")
  temp <- tempfile(fileext=paste0(".", chosen_ext2))
  download.file(download_url2, destfile = temp, mode = "wb")
  loaded_data <- switch(chosen_ext2,
                        csv = read_csv(temp),
                        xlsx = read.xlsx(temp)
  )
  return(loaded_data)
  }
  cat("File type is not supported.\n")
  choice <- menu(c("Download", "Exit"), title = "Choose how to proceed")
  if (choice == 0 || choice == 2) {
    cat("No selection. Exiting.\n")
    return()  # Exit the function if no selection is made
  }
  download_url3 <- paste0("https://datadryad.org/api/v2/files/", chosen_id2, "/download")
  download.file(download_url3, destfile = paste0("~/Downloads/",chosen_file2), mode = "wb")
 }


