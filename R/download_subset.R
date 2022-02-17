#' Request dataset subset
#'
#' This function queries the GeoCDL REST API and returns
#' geospatial data matching the dataset variables and
#' spatio-temporal filters.
#'
#' @param ds The GeoCDL dataset ID.
#' @param vars A vector of variables from 'ds' to include, as available from
#'   \code{\link{view_metadata}}.
#' @param date_start The start date of desired data. Use format 'YYYY'
#'   for annual, 'YYYY-MM' for monthly, and 'YYYY-MM-DD' for daily data.
#'   If NULL, returns entire temporal range.
#' @param date_end The end date of desired data. Use format 'YYYY'
#'   for annual, 'YYYY-MM' for monthly, and 'YYYY-MM-DD' for daily data.
#'   If NULL, returns entire temporal range.
#' @param t_crs The target CRS specified as an EPSG code. If not specified,
#'   the CRS of the dataset will be used.
#' @param t_geom The area of interest specified as upper left and
#'   lower right corners: c(c(xmin,ymax),(xmax,ymin)).These coordinates
#'   are in `t_crs` if specified or the CRS of the first dataset.
#'
#' @return A vector of paths to the downloade data.
#'
#' @seealso \code{\link{list_datasets}} to list available datasets and their IDs.
#'
#' @examples
#' \dontrun{
#' # Monthly PRISM precipitation from 2020 and in box of latlon coordinates
#' download_subset('PRISM','ppt','2020-01','2020-12',"4326",list(c(-109,37),c(-103,31)))
#' }
#'
#' @export
download_subset <- function(ds,
                           vars,
                           date_start = NULL,
                           date_end = NULL,
                           t_crs = NULL,
                           t_geom = NULL) {

  # Check temporal range format, if applicable
  if(!is.null(date_start)){

  }

  # Check variable and dataset validity


  # Format datasets and variables into string
  # DATASET_ID:VARNAME[,VARNAME...][;DATASET_ID:VARNAME[,VARNAME...]...]
  dv_str <- paste0(ds,':',paste(vars,collapse = ','))

  # Format user geometry
  # (UPPER_LEFT_X,UPPER_LEFT_Y),(LOWER_RIGHT_X,LOWER_RIGHT_Y)
  geom_str <- paste(sapply(t_geom,function(x) paste0('(',paste(x,collapse = ","),")")),collapse = ",")

  # Build query
  query_str <- paste0(gcdl_url,
                      'subset_polygon?',
                      paste0(
                        'date_start=',date_start,
                        '&date_end=',date_end,
                        '&crs=EPSG:',t_crs,
                        '&datasets=',utils::URLencode(dv_str, reserved = TRUE),
                        '&clip=',utils::URLencode(geom_str, reserved = TRUE)))


  # Get response from REST API

  # Check for bad request / errors

  # Download zipped results to temporary file
  temp_dir <- tempdir()
  tempf_prefix <- tempfile("subset",tmpdir = "")
  subset_dir <- paste0(temp_dir,tempf_prefix,"/")
  subset_zip <- paste0(temp_dir,tempf_prefix,'.zip')
  utils::download.file(query_str, subset_zip)
  utils::unzip(subset_zip,
               exdir = subset_dir)

  list.files(subset_dir,full.names = TRUE)

}
