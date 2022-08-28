#' Request dataset polygon subset
#'
#' This function queries the GeoCDL REST API and returns
#' geospatial data matching the dataset variables and
#' spatio-temporal filters.
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
