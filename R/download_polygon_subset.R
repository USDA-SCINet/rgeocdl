#' @export
download_polygon_subset <- function(ds,
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
                                    dsn = getwd()){

  endpoint <- 'subset_polygon'

  q_str <- format_subset_query(endpoint,
                      ds, vars,
                      dates, years, months, days,
                      t_crs, resolution, t_geom,
                      out_format,
                      grain_method,validate_method,ri_method)
  print(q_str)

  out_files <- submit_subset_query(q_str, dsn)

  return(out_files)
}
