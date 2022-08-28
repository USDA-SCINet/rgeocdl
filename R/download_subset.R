#' Request dataset subset
#'
#' This function queries the GeoCDL REST API and returns
#' geospatial data matching the dataset variables and
#' spatio-temporal filters. Area vs point requests are
#' inferred by `t_geom` if possible.
#'
#' @param dsvars The GeoCDL dataset and variable IDs as a data.frame or matrix
#'   with the first column specifying the datset ID and the second column
#'   specifying the variable IDs. One row per unique dataset-variable,
#'   combination.
#' @param dates The dates for which to request data. Dates must be specified as
#'  strings, where "YYYY" means extract annual data, "YYYY-MM" is for monthly
#'  data, and "YYYY-MM-DD" is for daily data. Date ranges can be specified using
#'  a colon, as in "YYYY:YYYY", "YYYY-MM:YYYY-MM", or "YYYY-MM-DD:YYYY-MM-DD".
#'  Leading 0s in "MM" and "DD" are optional (e.g., both "2020-01-01" and
#'  "2020-1-1" are valid). Multiple dates and/or date ranges should be separated
#'  by commas. For example: "2000-2010", "2000-1,2005-1,2010-1:2010-6",
#'  "2000-01-01:2000-04-31". Dates can be omitted for non-temporal data
#'  requests. For more complicated, recurring date patterns, use the "years",
#'  "months", and "days" parameters.
#' @param years Years to subset data. Can be expressed as ranges and sequences,
#'  such as "2004-2005,2009" or "2000-2010+2", which is interpreted as every
#'  other year starting with 2000. Ranges are inclusive of their endpoints
#'  unless the endpoint does not correspond with the step size increment. If
#'  "dates" is also provided, "years" (and "months" and "days") will be ignored.
#' @param months Months to include for each year of the data request. Only
#'  valid if "years" is also specified. Accepts values 1-12. Can be expressed
#'  as ranges and sequences, such as "1-3,5,9-12" or "1-12+2", which is
#'  interpreted as every other month. Ranges are inclusive of their endpoints
#'  unless the endpoint does not correspond with the step size increment.
#' @param days Only valid if "years" or "years" and "months" are also specified.
#'  If only "years" is specified, "days" is interpreted as the days of each
#'  year (starting from 1) to include in the request. If "years" and "months"
#'  are both specified, "days" is interpreted as the days of each month
#'  (starting from 1) to incude in the request. The special value "N"
#'  represents the last day of a month or year. Can be expressed as ranges and
#'  sequences, such as "1-100,200-230,266-366", "1-N", or "10-N+10", which is
#'  interpreted as every 10th day of the year or month. Ranges are inclusive
#'  of their endpoints unless the endpoint does not correspond with the step
#'  size increment. Required if "years" or "years" and "months" are specified
#'  and daily data are desired.
#' @param t_crs The target coordinate reference system (CRS) for the returned
#'  data. Can be specified as a PROJ string, CRS WKT string, authority string
#'  (e.g., "EPSG:4269"), or PROJ object name (e.g., "NAD83").
#' @param resolution The target spatial resolution for the returned data,
#'  specified in units of the target CRS or of the CRS of the first dataset if
#'  no target CRS is provided.
#' @param t_geom The geometry of interest specified as a geometry upload ID
#'   (see \code{\link{upload_geometry}}), `sf` or `sp` object, or a
#'   data.frame with x and y columns of coordinates. For an area request, the
#'   coordinates data.frame defines the vertices of a clip polygon (or if only
#'   two coordinates, the upper left and lower right corners of a box). If a
#'   data.frame, the CRS of the points are assumed to be that of `t_crs`,
#'   if specified, or that of the first requested dataset.
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
#' @param dsn The directory to download and extract the return zipped results.
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
download_subset <- function(dsvars,
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
    out_files <- download_polygon_subset(dsvars,
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


    out_files <- download_points_subset(dsvars,
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
