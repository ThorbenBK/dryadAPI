#' Search dryad API for specific datasets
#'
#' dryadDatasetSearch() allows to access dryad API and search for one or multiple datasets based on prior specified criteria
#' @param p an integer. Which page of results to view. Returns all pages, if not specified.
#' @param perp an integer. Number of results to return on each page. Defaults to 1000 (Maximum allowed).
#' @param terms a vector. A list of terms to be searched. If multiple terms are supplied, matches will only be returned for items that contain all terms. A term may be negated to indicate terms that should not be present in the results (e.g., cat -fish).
#' @param affiliation a character variable. ROR identifier specifying an institutional affiliation (https://ror.org) that must be present in the list of dataset authors. The identifier should be in the full "https" format
#' @param modifiedSince: A timestamp for limiting results. Datasets will only be returned that have been modified since the given time. The time must be specified in ISO 8601 format, and the time zone must be set to UTC, e.g., 2020-10-08T10:24:53Z
#' @param modifiedBefore: A timestamp for limiting results. Datasets will only be returned that have been modified before the given time. The time must be specified in ISO 8601 format, and the time zone must be set to UTC, e.g., 2020-10-08T10:24:53Z.
#' @param tenant a character variable. The abbreviation for a "tenant" organization in Dryad. This will automatically search all affiliations associated with the given tenant. If both a tenant and affiliation are specified, the tenant will be ignored.
#' @param issn a character variable. The journal ISS (https://portal.issn.org).
#' @param relatedWorkIdentifier a character variable. The identifier that is present in a related work.
#' @param relatedWorkRelationship a character variable. The type of relationship expressed by a related work.
#' @return Dataframe of search results
#' @seealso [dryad_version_overview()]
#' @seealso [dryadFileReadID()]
#' @seealso [dryadFileRead()]
#' @seealso [dryadFileDownloadID()]
#' @examples
#' dryadDatasetSearch(terms=c("dog", "-cat"), p = 4)
#' @export
#'
dryadDatasetSearch <- function(p=NA,
                               perp=100,
                               terms=NA,
                               affiliation=NA,
                               modifiedSince=NA,
                               modifiedBefore=NA,
                               tenant=NA,
                               issn=NA,
                               relatedWorkIdentifier=NA,
                               relatedWorkRelationship=NA  )
  {
  base_url <- "https://datadryad.org/api/v2/search"
  query_params <- list(
    per_page = perp)
  if (!is.na(p)) query_params$page <- p
  if (!is.na(terms[1])) query_params$q <- URLencode(paste(terms, collapse=" "))
  if (!is.na(affiliation)) query_params$affiliation <- URLencode(affiliation, reserved = TRUE)
  if (!is.na(tenant)) query_params$tenant <- tenant
  if (!is.na(issn)) query_params$journalISSN <- issn
  if (!is.na(modifiedSince)) query_params$modifiedSince <- modifiedSince
  if (!is.na(modifiedBefore)) query_params$modifiedBefore <- modifiedBefore
  if (!is.na(relatedWorkIdentifier)) query_params$relatedWorkIdentifier <- relatedWorkIdentifier
  if (!is.na(relatedWorkRelationship)) query_params$relatedWorkRelationship <- relatedWorkRelationship
  response <- GET(paste0(base_url, "?", paste0(names(query_params), "=", query_params, collapse = "&")))
  text <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(text, flatten=TRUE)
  message("Total results: ", data[["total"]])
  choice <- menu(c("Load into environment", "ABORT"), title = "Choose how to proceed")
  if (choice == 0| choice == 2) {
    cat("Exiting.\n")
    return()
  }
  n <- ceiling(data[["total"]]/perp)
  dataset_search_results <- data[["_embedded"]][["stash:datasets"]]
  if (n > 1 & !is.na(p)) {
    return(dataset_search_results)}
    for (i in 2:n) {
      query_params$page <- i
      response <- GET(paste0(base_url, "?", paste0(names(query_params), "=", query_params, collapse = "&")))
      page_data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
      results <- page_data[["_embedded"]][["stash:datasets"]]
      dataset_search_results <- rbind(dataset_search_results, results)
    }
  return(dataset_search_results)
}


