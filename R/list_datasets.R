#' List available GeoCDL datasets
#'
#' This function queries the GeoCDL REST API and returns
#' currently supported dataset names and IDs.
#'
#' @return A data.frame of dataset names and IDs
#'
#' @seealso \code{\link{view_metadata}} to list the dataset
#'   metadata by dataset ID
#'
#' @export
list_datasets <- function() {

  # Build query
  ds_query <- paste0(gcdl_url,'list_datasets')

  # Get response from REST API
  ds_response <- httr::GET(ds_query)

  # Convert JSON response to data.frame
  jsonlite::fromJSON(ds_response)

}


