#' Get information on how many versions of a dataset exist.
#'
#' dryad_version_count() easily retrieves the number of versions of a dataset in the dryad data repository.
#' @param doi a character vector with one element in the form of "https://...".
#' @return An integer.
#' @seealso [dryad_version_overview()]
#' @examples
#' dryad_version_count(https://doi.org/10.5061/dryad.z08kprrk1)
#' @importFrom stringr str_replace_all
#' #' @export
dryad_version_count <- function(doi){
  encoded_doi <- str_replace_all(doi, c("https://doi.org/" = "doi%253A", "/" = "%2F"))
  response <- GET(paste("https://datadryad.org/api/v2/datasets/",encoded_doi,"/versions?page=1&per_page=100", sep=""))
  text <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(text, flatten=TRUE)
  return(data$count)
  }

dryad_version_count("https://doi.org/10.5061/dryad.z08kprrk1")
