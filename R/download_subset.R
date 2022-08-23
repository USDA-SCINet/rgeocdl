#' Request dataset subset
#'
#' This function queries the GeoCDL REST API and returns
#' geospatial data matching the dataset variables and
#' spatio-temporal filters.
#'
#' @param ds The GeoCDL dataset ID(s) as a vector
#' @param vars A vector of variables from 'ds' to include, as available from
#'   \code{\link{view_metadata}}.
#' @param dates The start date of desired data. Use format 'YYYY'
#'   for annual, 'YYYY-MM' for monthly, and 'YYYY-MM-DD' for daily data.
#'   If NULL, returns entire temporal range.
#' @param years
#' @param months
#' @param days
#' @param t_crs The target CRS specified as an EPSG code. If not specified,
#'   the CRS of the dataset will be used.
#' @param t_geom The geometry of interest specified as a geometry upload ID
#'   (see \code{\link{upload_geometry}}) or a list of...
#' @param out_format The output format for the subset. For area requests:
#'   'geotiff' or 'netcdf'. For point requests: 'csv', 'shapefile', or 'netcdf'.
#' @param grain_method How to handle scenario of requested date grains not
#'    matching date grains of each requested dataset. If "strict" (default),
#'     an error will be returned. If "skip", the dataset will be skipped.
#'     If "coarser", then only coarser grains will be returned. If "finer",
#'     then only finer grains will be returned. If "any", then any available
#'     grain will be returned, with coarser having higher priority over finer.
#'     Non-temporal datasets are always returned.
#' @param validate_method How to handle requested dates outside of requested dataset
#'     available data range. If "strict" (default),
#'     an error will be returned. If "overlap", the requested dates will be
#'     'truncated to the date range available in all requested datasets. If
#'     "all", then the requested dates will be truncated to the available date
#'     range per dataset. Non-temporal datasets are always returned.
#' @param ri_method The interpolation method used for extracting point
#'     values. Available methods: "nearest" or "linear". Default is
#'     "nearest".
#'
#' @return A vector of paths to the downloaded data.
#'
#' @seealso \code{\link{list_datasets}} to list available datasets and their IDs.
#' \code{\link{upload_geometry}} to obtain a geometry upload ID.
#'
#' @examples
#' \dontrun{
#' # Monthly PRISM precipitation from 2020 and in box of latlon coordinates
#' download_subset('PRISM','ppt','2020-01:2020-12',"EPSG:4326",list(c(-109,37),c(-103,31)))
#' }
#'
#' @export
download_subset <- function(ds,
                            vars = NULL,
                            dates = NULL,
                            years = NULL,
                            months = NULL,
                            days = NULL,
                            t_crs = NULL,
                            resolution = NULL,
                            t_geom = NULL,
                            out_format = NULL,
                            grain_method = 'strict',
                            validate_method = 'strict',
                            ri_method = 'nearest',
                            dsn = getwd()) {

  # Determine endpoint
  endpoint <- infer_endpoint(t_geom)

  if(endpoint == 'subset_polygon') {
    out_files <- download_polygon_subset(ds, vars,
                                         dates, years, months, days,
                                         t_crs, resolution, t_geom,
                                         out_format,
                                         grain_method, validate_method,
                                         ri_method,
                                         dsn)
  } else if(endpoint == 'subset_points') {

    # Check for parameter-endpoint inconsistencies
    if(!is.null(resolution)){
      stop('Resolution parameter not applicable for point extraction.')
    }


    out_files <- download_points_subset(ds, vars,
                                         dates, years, months, days,
                                         t_crs, t_geom,
                                         out_format,
                                         grain_method, validate_method,
                                         ri_method,
                                        dsn)
  } else {
    stop('Unknown endpoint')
  }

  return(outfiles)

}
