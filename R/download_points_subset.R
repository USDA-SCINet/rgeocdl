#' Request dataset polygon subset
#'
#' This function queries the GeoCDL REST API and returns
#' geospatial data matching the dataset variables and
#' spatio-temporal filters.
#'
#' # Temporal subsets
#'
#' For specifying the dates of your data request, you can either use `dates` for
#' simple date ranges or a combination of `years`, `months`, and `days` for
#' more complicated date patterns. For non-temporal datasets, these parameters
#' can be omitted and will be ignored if specified. To specify if you want
#' annual, monthly, or daily data, you specify your temporal subsets with only
#' year information (e.g. `dates=YYYY` or `years=YYYY`), or year and month
#' information (e.g. `dates=YYYY-MM` or `years=YYYY, months=MM`), or year,
#' (month), and day information (e.g. `dates=YYYY-MM-DD` or
#' `years=YYYY, months=MM, days=DD`). If not all datasets you request have the
#' same temporal grain, e.g. you request monthly data but one dataset only has
#' daily data or only has annual data, you can specify `grain_method` to choose
#' an alternative temporal grain. Use `validate_method` to specify how to handle
#' datasets whose temporal range does not match your requested temporal subset.
#'
#' @inheritParams download_subset
#' @param t_geom The point geometry of interest specified as a geometry upload
#'  ID (see \code{\link{upload_geometry}}), `sf` or `sp` object, or a data.frame
#'  with x and y columns. If a data.frame, the CRS of the points are assumed to
#'  be that of `t_crs`, if specified, or that of the first requested dataset.
#' @param out_format The output format for the subset: 'csv', 'shapefile', or
#'   'netcdf'.
#' @param ri_method The interpolation method used for extracting point
#'     values. Available methods: "nearest" or "linear". Default is
#'     "nearest".
#'
#' @return A vector of paths to the downloaded data.
#'
#' @seealso \code{\link{list_datasets}} to list available datasets and their IDs.
#' \code{\link{upload_geometry}} to obtain a geometry upload ID.
#'
#'
#' @export
download_points_subset <- function(dsvars,
                                   dates = NULL,
                                   years = NULL,
                                   months = NULL,
                                   days = NULL,
                                   t_crs = NULL,
                                   t_geom = NULL,
                                   out_format = 'csv',
                                   grain_method = 'strict',
                                   validate_method = 'strict',
                                   ri_method = 'nearest',
                                   dsn = '.',
                                   req_name = NULL){

  endpoint <- 'subset_points'

  q_str <- format_subset_query(endpoint,
                               dsvars, NULL,
                               dates, years, months, days,
                               t_crs, NULL, t_geom,
                               out_format,
                               grain_method,validate_method,ri_method)
  print(q_str)

  out_files <- submit_subset_query(q_str, dsn, req_name)

  return(out_files)
}
