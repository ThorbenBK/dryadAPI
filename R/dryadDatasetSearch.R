#' @title Search the Dryad API for Datasets Based on Criteria
#' 
#' @description
#' `dryadDatasetSearch()` queries the Dryad API for datasets based on user-specified search parameters.
#' It allows for flexible searching using terms, institutional affiliation, journal ISSNs,
#' related works, and modification timestamps. The function can return one page or all available pages of results.
#'
#' @param p Integer. Which page of results to return. If `NA` (default), retrieves all pages.
#' @param perp Integer. Number of results per page. Default is 100 (Dryad's maximum allowed).
#' @param terms Character vector. Search terms to query Dryad datasets. If multiple terms are supplied,
#'   only datasets containing all terms are returned. Terms may be negated using a minus sign (e.g., `"cat"`, `"-fish"`).
#' @param affiliation Character. A ROR identifier (in full URL format) specifying an institutional affiliation
#'   that must appear among the dataset's authors. See: \url{https://ror.org}.
#' @param modifiedSince Character. ISO 8601 timestamp in UTC format (e.g., `"2020-10-08T10:24:53.999Z"`). Filters to datasets
#'   modified after the given timestamp.
#' @param modifiedBefore Character. ISO 8601 timestamp in UTC format. Filters to datasets
#'   modified before the given timestamp.
#' @param tenant Character. Abbreviation for a "tenant" organization in Dryad. If both `tenant` and `affiliation` are specified,
#'   the `tenant` takes precedence.
#' @param issn Character. Journal ISSN (see: \url{https://portal.issn.org}) to filter by journal.
#' @param relatedWorkIdentifier Character. Identifier of a related work linked to the dataset.
#' @param relatedWorkRelationship Character. Type of relationship to the related work.
#'
#' @return A data frame containing the search results. If more than one page is found, all pages are automatically retrieved
#'   unless `p` is specified.
#'   
#' @details
#' If `p` is not specified (`NA`), the function loops through all pages of results and combines them into a single data frame.
#' The total number of matched datasets is printed, and a menu is prompted to confirm loading.
#' Selecting "ABORT" cancels the operation.
#'   
#' @seealso [dryad_version_overview()], [dryadFileRead()], [dryadFileReadID()], [dryadFileDownloadID()], [dryadFileDownload()]
#'
#' @examples
#' \dontrun{
#' # Search for datasets containing the term "dog" but not "cat", return only page 4 (Results 301-400)
#' dryadDatasetSearch(terms = c("dog", "-cat"), p = 4)
#'
#' # Search using affiliation and a modification filter
#' dryadDatasetSearch(
#'   terms = "evolution",
#'   affiliation = "https://ror.org/043z8wz74",
#'   modifiedSince = "2021-01-01T00:00:00Z"
#' )
#' }
#'
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


