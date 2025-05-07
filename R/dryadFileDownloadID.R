#' Download a file from dryad API into R
#'
#' dryadFileDownloadID() allows to access a specific file from dryad based on the ID of that file.
#' @param id a character vector with one element in the form of "xxxxxx".
#' @param ext a character vector with one element in the form of for example "md".
#' @param path a character vector with one element in the form of "....". Default: "~/Downloads"
#' @param name a character vector with one element in the form of ".....". Default: "data"
#' @return Downloaded file.
#' @seealso [dryad_version_overview()]
#' @seealso [dryadFileReadID()]
#' @seealso [dryadFileRead()]
#' @seealso [dryadFileDownload()]
#' @examples
#' dryadFileReadID("3833094", "xlsx")
#' @export
dryadFileDownloadID <- function(id, ext, path="~/Downloads", name="data"){
  download_url2 <- paste0("https://datadryad.org/api/v2/files/",id, "/download")
  download.file(url=download_url2, destfile = paste0(path, "/", name, ".", ext), mode = "wb" )
}

