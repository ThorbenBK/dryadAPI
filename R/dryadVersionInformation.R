#' Get information about different Versions of a dataset.
#'
#' dryad_version_overview() allows to view all information about a dataset, that is saved on dryad. The function allows to create a nested data.frame.
#' When selecting certain information beforehand, the download will be limited to those information.
#' @param doi a character vector with one element in the form of "https://...".
#' @param ... Other options to specify which information should be retrieved. A character vector.
#' @param ... : title, authors(firstName, lastName, email, affiliation, affiliationROR, orcid, order, abstract),funders(organization, identifierType,identifier, awardNumber, awardDescription, order), keywords, fieldOfScience, methods, versionNumber, versionStatus, curationStatus, versionChanges, publicationDate, lastModificationDate, visibility, sharingLink, xxxxx_links.curies(name, href, templated), _links.self.href, _links.stash:dataset.href, _links.stash:files.href, _links.stash:download.href
#' @return A nested dataframe.
#' @seealso [dryad_version_count()]
#' @examples
#' dryadVersionInformation(https://doi.org/10.5061/dryad.z08kprrk1)
#' #Use ... to pass additional arguments to dryad_version_overview().
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



