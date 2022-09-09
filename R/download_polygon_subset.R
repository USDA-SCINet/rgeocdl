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
#' # Harmonization
#'
#' The GeoCDL can return a harmonized 'datacube' of raster layers for polygon
#' subset requests with multiple datasets if `t_geom` and `resolution` are
#' specified. If `t_geom`, `t_crs`, and `resolution` are specified, then each
#' layer is spatially subsetted and reprojected to `resolution` and `t_crs`. If
#' `t_geom` and `resolution` are specified without `t_crs`, then each
#' layer is spatially subsetted and reprojected to `resolution` and the CRS of
#' `t_geom` if available or the CRS of the first requested dataset if not.
#' `resolution` is always assumed to be in units of the target CRS which is
#' the first available from either, `t_crs`, the CRS of `t_geom`, or the CRS of
#' the first requested dataset.
#'
#' @inheritParams download_subset
#' @param t_geom The polygon geometry of interest specified as a geometry
#'   upload ID (see \code{\link{upload_geometry}}), `sf` or `sp` object, or a
#'   data.frame with x and y columns of coordinates defining the vertices of a
#'   clip polygon (or if only two coordinates, the upper left and lower right
#'   corners of a box). If a data.frame, the CRS of the coordinates are assumed to
#'   be that of `t_crs`, if specified, otherwise that of the first requested
#'   dataset.
#' @param out_format The output format for the subset: 'geotiff' or 'netcdf'.
#' @param ri_method The resampling method used for reprojection. Available
#'   methods: "nearest", "bilinear", "cubic", "cubic-spline", "lanczos",
#'   "average", or "mode". Default is "nearest".  Only used if `t_crs`
#'   and/or `resolution` are provided.
#'
#' @return A vector of paths to the downloaded data.
#'
#' @seealso \code{\link{list_datasets}} to list available datasets and their IDs.
#' \code{\link{upload_geometry}} to obtain a geometry upload ID.
#'
#'
#' @export
download_polygon_subset <- function(dsvars,
                                    dates = NULL,
                                    years = NULL,
                                    months = NULL,
                                    days = NULL,
                                    t_crs = NULL,
                                    resolution = NULL,
                                    t_geom = NULL,
                                    out_format = 'geotiff',
                                    grain_method = 'strict',
                                    validate_method = 'strict',
                                    ri_method = 'nearest',
                                    dsn = getwd()){

  endpoint <- 'subset_polygon'

  q_str <- format_subset_query(endpoint,
                      dsvars, NULL,
                      dates, years, months, days,
                      t_crs, resolution, t_geom,
                      out_format,
                      grain_method,validate_method,ri_method)
  print(q_str)

  out_files <- submit_subset_query(q_str, dsn)

  return(out_files)
}
