#' @title Get Information About Different Versions of a Dataset from Drya
#' 
#' @description `dryadVersionInformation()` retrieves metadata for all available versions of a dataset hosted on Dryad, identified by its DOI. The function can return either the full version metadata or selected fields.
#'
#' @param doi A character string specifying the dataset DOI, in the form of "https://doi.org/..."
#' @param ... Optional character strings specifying which fields to extract from the version metadata.
#' 
#' @details 
#' The Dryad API returns a nested structure with metadata for all versions of a dataset.  
#' This function flattens and extracts that structure into a data frame.
#' 
#' If no fields are specified via `...`, the full set of available metadata will be returned.
#' If one or more fields are specified, only those fields will be returned (if valid).
#' 
#' #' The possible top-level fields include:
#' - `title`
#' - `keywords`
#' - `fieldOfScience`
#' - `methods`
#' - `versionNumber`
#' - `versionStatus`
#' - `curationStatus`
#' - `versionChanges`
#' - `publicationDate`
#' - `lastModificationDate`
#' - `visibility`
#' - `sharingLink`
#' - `_links.self.href`
#' - `_links.stash:dataset.href`
#' - `_links.stash:files.href`
#' - `_links.stash:download.href`
#' - `_links.curies` (list column with fields: `name`, `href`, `templated`)
#' 
#' Nested list-columns include:
#' 
#' - `authors` (each row is a list of author entries with the following fields):
#'   - `firstName`
#'   - `lastName`
#'   - `email`
#'   - `affiliation`
#'   - `affiliationROR`
#'   - `orcid`
#'   - `order`
#'   - `abstract`
#' 
#' - `funders` (each row is a list of funder entries with the following fields):
#'   - `organization`
#'   - `identifierType`
#'   - `identifier`
#'   - `awardNumber`
#'   - `awardDescription`
#'   - `order`
#'
#' To access nested fields (e.g., `authors$firstName`), further unnesting may be required using tools like `tidyr::unnest()`.
#'
#' @return A `data.frame` (tibble-like) containing the version metadata, either fully or with selected columns.
#' 
#' @seealso [dryad_version_count()], [dryad_version_overview()]
#' 
#' @examples
#' \dontrun{
#' # Get full version metadata
#' dryadVersionInformation("https://doi.org/10.5061/dryad.z08kprrk1")
#'
#' # Get only selected fields
#' dryadVersionInformation("https://doi.org/10.5061/dryad.z08kprrk1", 
#'                         "versionNumber", "lastModificationDate")
#' }
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select all_of
#' @importFrom stringr str_replace_all
#' @export
dryadVersionInformation <- function(doi,...){
  encoded_doi <- str_replace_all(doi, c("https://doi.org/" = "doi%253A", "/" = "%2F"))
  response <- GET(paste("https://datadryad.org/api/v2/datasets/",encoded_doi,"/versions?page=1&per_page=100", sep=""))
  text <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(text, flatten=TRUE)
  version_information <-data[["_embedded"]][["stash:versions"]]
  fields <- c(...)
  if (length(fields) == 0) {
    return(version_information)
  } else {
    # Validate column names
    valid_cols <- intersect(fields, colnames(version_information))
    if (length(valid_cols) == 0) {
      warning("None of the specified columns are valid. Returning full table.")
      return(version_information)
    }
    return(select(version_information, all_of(valid_cols)))
  }
}



