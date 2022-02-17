#' Request dataset metadata
#'
#' This function queries the GeoCDL REST API and returns
#' the entire metadata associated with a dataset ID
#'
#' @param ds The GeoCDL dataset ID
#'
#' @return The metadata for the dataset as a list
#'
#' @seealso \code{\link{list_datasets}} to list available datasets and their IDs
#'
#' @examples
#' \dontrun{
#' view_metadata('PRISM')
#' }
#'
#' @export
view_metadata <- function(ds) {

  # Build metadata query from dataset ID
  md_query <- paste0(gcdl_url,'ds_info?id=',ds)

  # Get response from REST API
  md_response <- url(md_query)

  # Check for bad request / errors

  # Convert from JSON and return list
  jsonlite::fromJSON(md_response)

}
