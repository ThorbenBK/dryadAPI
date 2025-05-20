#' Download a file from dryad API into R
#'
#' dryadFileDownload() allows to access a specific file from dryad based on the doi of the dataset, choosing the file from the dataset.
#' @param doi a character vector with one element in the form of "https://...".
#' @param path a character vector with one element in the form of "....". Default: "~/Downloads"
#' @param name a character vector with one element in the form of ".....". Default: "data"
#' @return Downloaded file.
#' @seealso [dryad_version_overview()]
#' @seealso [dryadFileReadID()]
#' @seealso [dryadFileRead()]
#' @seealso [dryadFileDownloadID()]
#' @examples
#' dryadFileDownload(https://doi.org/10.5061/dryad.z08kprrk1)
#' @importFrom stringr str_replace_all
#' #' @export
#'
dryadFileDownload <- function(doi, path="~/Downloads", name="data"){
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
  download.file(url=download_url2, destfile = paste0(path, "/", name, ".", chosen_ext2), mode = "wb" )
}
