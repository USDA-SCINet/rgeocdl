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
  md_response <- httr::GET(md_query)

  # Check for bad request / errors

  # Convert from JSON and return list
  md_parsed <- httr::content(md_response)

  # Format list into text
  cat(paste0('Name: ', md_parsed$name, '\n',
             'ID: ', md_parsed$id, '\n',
             'URL: ', md_parsed$url, '\n',
             'Description: ', md_parsed$description, '\n',
             'Provider name: ', md_parsed$provider_name, '\n',
             'Provider URL: ', md_parsed$provider_url, '\n',
             'Grid size: ', md_parsed$grid_size, '\n',
             'Grid unit: ', md_parsed$grid_unit, '\n',
             'Variable ID (description): \n\t',
             paste(paste0(names(md_parsed$vars),
                          " (", md_parsed$vars, ")"),
                   collapse = '\n\t'), '\n',
             'Date ranges: \n\t',
             paste(paste0('Annual: ', paste(md_parsed$date_ranges$year,
                                            collapse = ':'), '\n\t',
                          'Monthly: ', paste(md_parsed$date_ranges$month,
                                             collapse = ':'), '\n\t',
                          'Daily: ', paste(md_parsed$date_ranges$day,
                                           collapse = ':')
                          ),
                   collapse = '\n\t'), '\n',
             'CRS: \n\t',
             paste0('Name: ', md_parsed$crs$name, '\n\t',
                    'EPSG: ', md_parsed$crs$epsg, '\n\t',
                    'proj4: ', md_parsed$crs$proj4, '\n\t',
                    'WKT: ', md_parsed$crs$wkt, '\n\t',
                    'Datum: ', md_parsed$crs$datum, '\n\t',
                    'Geographic: ', md_parsed$crs$is_geographic, '\n\t',
                    'Projected: ', md_parsed$crs$is_projected)
             )
      )

}
