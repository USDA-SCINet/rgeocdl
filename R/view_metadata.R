#' Request dataset metadata
#'
#' This function queries the GeoCDL REST API and returns
#' the entire metadata associated with a dataset ID
#'
#' @param ds The GeoCDL dataset ID
#'
#' @return NULL, the metadata for the dataset as a printed message
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

  # Convert from JSON and return list
  md_parsed <- httr::content(md_response)

  # Check for bad request / errors
  if(httr::http_error(md_response)){
    stop(paste('GeoCDL returned an error:',
               httr::http_status(md_response)$message, '\n',
               md_parsed$detail))
  }

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
             paste(paste0('Annual: ',
                          ifelse(is.null(md_parsed$date_ranges$year[[1]]),
                                 '(no data for this temporal grain)',
                                 paste(md_parsed$date_ranges$year,
                                       collapse = ':'))),
                          paste0('Monthly: ',
                                 ifelse(is.null(md_parsed$date_ranges$month[[1]]),
                                 '(no data for this temporal grain)',
                                 paste(md_parsed$date_ranges$month,
                                       collapse = ':'))),
                          paste0('Daily: ',
                                 ifelse(is.null(md_parsed$date_ranges$day[[1]]),
                                 '(no data for this temporal grain)',
                                 paste(md_parsed$date_ranges$day,
                                       collapse = ':'))),
                          sep = '\n\t'), '\n',
             'Temporal resolutions: \n\t',
             paste(paste0('Annual: ',
                          ifelse(is.null(md_parsed$temporal_resolution$year[[1]]),
                                 '(no data for this temporal grain)',
                                 md_parsed$temporal_resolution$year)),
                   paste0('Monthly: ',
                          ifelse(is.null(md_parsed$temporal_resolution$month[[1]]),
                                 '(no data for this temporal grain)',
                                 md_parsed$temporal_resolution$month)),
                   paste0('Daily: ',
                          ifelse(is.null(md_parsed$temporal_resolution$day[[1]]),
                                 '(no data for this temporal grain)',
                                 md_parsed$temporal_resolution$day)),
                   sep = '\n\t'), '\n',
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
