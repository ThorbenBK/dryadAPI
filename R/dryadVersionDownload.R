#' Download all files of a version from dryad API into R
#'
#' dryadVersionDownload() allows to access all files of a version from dryad based on the doi of the dataset.
#' @param doi a character vector with one element in the form of "https://...".
#' @param path a character vector with one element in the form of "....". Default: "~/Downloads"
#' @return Downloaded file.
#' @seealso [dryad_version_overview()]
#' @seealso [dryadFileReadID()]
#' @seealso [dryadFileRead()]
#' @seealso [dryadFileDownloadID()]
#' @examples
#' dryadVersionDownload(https://doi.org/10.5061/dryad.z08kprrk1)
#' @export
#'
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

