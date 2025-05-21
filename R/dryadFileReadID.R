#' @title Read a Specific File from Dryad based on its ID 
#'
#' @description
#' `dryadFileReadID()` allows reading a file of a dataset hosted on Dryad directly by using its ID. 
#' The user provides the file ID and the extension. Supported file types (currently `.csv` and `.xlsx`) are downloaded and read into R automatically. 
#'
#' @param id A character string specifying the file ID.
#' @param ext A character string specifying the file extension (e.g., "csv" or "xlsx").
#'  
#' 
#' @return A data frame if the selected file is a `.csv` or `.xlsx`. Otherwise returns `NULL`.
#' 
#' @details 
#' This function directly accesses the file based on its ID and downloads it to a temporary directory and reads into R.  
#' 
#' @note Currently, only files with extensions `.csv` and `.xlsx` are read into R.
#' 
#' @seealso [dryad_version_overview()], [dryadFileRead()], [dryadFileDownloadID()]
#' 
#' @examples
#' \dontrun{
#' dryadFileReadID("3833094", "xlsx")
#' }
#' 
#' @importFrom readr read_csv
#' @importFrom readxl read_xlsx
#' @export
#'
dryadFileReadID <- function(id, ext){
  if(missing(id) || missing(ext) || id == "" || ext == "") {
    stop("Both 'id' and 'ext' must be provided and non-empty.")
  }
  if (ext == "xlsx" && !requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to read xlsx files. Please install it via install.packages('readxl').")
  }
  if (ext == "csv" && !requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required to read csv files. Please install it via install.packages('readr').")
  }
  download_url2 <- paste0("https://datadryad.org/api/v2/files/",id, "/download")
  temp <- tempfile(fileext=paste0(".", ext))
  download.file(download_url2, destfile = temp, mode = "wb")
  loaded_data <- switch(ext,
                        csv = readr::read_csv(temp),
                        xlsx = readxl::read_xlsx(temp),
                        {
                        warning("Unsupported file type: ", ext)
                        return(NULL)
})
  return(loaded_data)
}

