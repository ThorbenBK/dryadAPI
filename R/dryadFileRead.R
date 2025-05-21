#' @title Read a File from a Specific Dryad Dataset Version
#'
#' @description
#' `dryadFileRead()` allows interactive access to files from different versions of a dataset hosted on Dryad, using the dataset's DOI. 
#' The user can select a version and a file from the list of available options.  Supported file types (currently `.csv` and `.xlsx`) are downloaded and read into R automatically. 
#' Unsupported formats can be manually downloaded via prompt.
#'
#' @param doi A character string specifying the dataset DOI, in the form of "https://doi.org/..."
#' 
#' @return A data frame if the selected file is a `.csv` or `.xlsx`. Otherwise, the file is downloaded to the user's `~/Downloads` folder. Returns `NULL` if no selection is made.
#' 
#' @details 
#' This function first queries the Dryad API to obtain all available versions of a dataset associated with the given DOI. 
#' The user is then prompted to select a version via an interactive menu. After selecting a version, all associated files are listed, and the user selects one for import.
#' If the file format is supported (`.csv` or `.xlsx`), the file is automatically downloaded to a temporary directory and read into R.
#' If the format is not supported, the user is prompted to download the file to their `~/Downloads` folder.
#' The function exits gracefully if the user cancels any step.
#' 
#' @note Currently, only files with extensions `.csv` and `.xlsx` are read into R. Other file types can be downloaded manually through the provided interactive prompt.
#' 
#' @seealso [dryad_version_overview()], [dryadFileReadID()], [dryadFileDownload()]
#' 
#' @examples
#' \dontrun{
#' dryadFileRead("https://doi.org/10.5061/dryad.z08kprrk1")
#' }
#' 
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom readr read_csv
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_replace_all
#' @export
#'
dryadFileRead <- function(doi){
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
  choice <- menu(file_names, title = "Choose a file to read-in")
  if (choice == 0) {
    cat("No file selected. Exiting.\n")
    return()
  }
  chosen_id2 <- file_ids[choice]
  chosen_file2 <- file_names[choice]
  chosen_ext2 <- file_ext[choice]
  if (chosen_ext2 %in% c("csv", "xlsx")){
    if (chosen_ext2 == "xlsx" && !requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read xlsx files. Please install it via install.packages('readxl').")
    }
    if (chosen_ext2 == "csv" && !requireNamespace("readr", quietly = TRUE)) {
      stop("Package 'readr' is required to read csv files. Please install it via install.packages('readr').")
    }
  download_url2 <- paste0("https://datadryad.org/api/v2/files/", chosen_id2, "/download")
  temp <- tempfile(fileext=paste0(".", chosen_ext2))
  download.file(download_url2, destfile = temp, mode = "wb")
  loaded_data <- switch(chosen_ext2,
                        csv = readr::read_csv(temp),
                        xlsx = readxl::read_xlsx(temp)
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


