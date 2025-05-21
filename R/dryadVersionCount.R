#' @title  Count the Number of Dataset Versions on Dryad
#'
#' @description
#' `dryad_version_count()` retrieves how many versions exist for a dataset on the Dryad data repository.
#'
#' @param doi A character string specifying the dataset DOI, in the form of "https://doi.org/..."
#' 
#' @return An integer indicating the number of versions of a dataset on dryad. 
#' 
#' @details 
#' This function first queries the Dryad API to obtain all available versions of a dataset associated with the given DOI. The total number of dataset versions is extracted from the API response. 
#' 
#' @seealso [dryad_version_overview()]
#' 
#' @examples
#' \dontrun{
#' dryad_version_count("https://doi.org/10.5061/dryad.z08kprrk1")
#' }
#' 
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @export
#'
dryad_version_count <- function(doi){
  encoded_doi <- str_replace_all(doi, c("https://doi.org/" = "doi%253A", "/" = "%2F"))
  response <- GET(paste("https://datadryad.org/api/v2/datasets/",encoded_doi,"/versions?page=1&per_page=100", sep=""))
  text <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(text, flatten=TRUE)
  return(data$count)
  }
