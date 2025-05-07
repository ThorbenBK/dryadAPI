#' Read a file from dryad API into R
#'
#' dryadFileReadID() allows to access a specific file from dryad based on the id of that file.
#' @param id a character vector with one element in the form of "xxxxxx".
#' @note the function is currently limited to ".csv" and ".xlsx" files.
#' @return Reads in the file.
#' @seealso [dryad_version_overview()]
#' @seealso [dryadFileRead()]
#' @examples
#' dryadFileReadID("3833094", "xslx")
#' @export
dryadFileReadID <- function(id, ext){
  download_url2 <- paste0("https://datadryad.org/api/v2/files/",id, "/download")
  temp <- tempfile(fileext=paste0(".", ext))
  download.file(download_url2, destfile = temp, mode = "wb")
  loaded_data <- switch(ext,
                        csv = read_csv(temp),
                        xlsx = read.xlsx(temp)
  )
  return(loaded_data)
}

