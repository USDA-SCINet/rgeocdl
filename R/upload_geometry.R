#' Upload a user geometry
#'
#' This function uploads a user geometry to the GeoCDL
#' REST API and returns a geometry upload ID to use
#' in subset requests.
#'
#' @param geom A `sp` or `sf` object representing points or polygons, or a
#'     filename. For point data, the following file formats are supported: CSV
#'     (comma-separated values), shapefiles, and GeoJSON. For polygon data, the
#'     following file formats are supported: shapefiles and GeoJSON. CSV files
#'     must contain a column named "x", "long", or "longitude" (not case
#'     sensitive) and a column named "y", "lat", or "latitude" (not case
#'     sensitive). Shapefiles must be uploaded in a single ZIP archive.
#'     Supported GeoJSON types for point data uploads are "Point", "MultiPoint",
#'     "GeometryCollection", "Feature", and "FeatureCollection". Supported
#'     GeoJSON types for polygon data uploads are "Polygon", "MultiPolygon",
#'     "GeometryCollection", "Feature", and "FeatureCollection". "Holes" in
#'     polygons will be ignored. For polygon data, GeoJSON objects and
#'     shapefiles with more than one polygon definition will be either
#'     uploaded as their union or uploaded separately based on if their (area
#'     of union)/(area of convex hull) > 0.8, respectively.
#' @param tmp_dir The directory in which to save temporary zipped shapefile
#'    folders representing `geom` that are uploaded to GeoCDL.
#'
#' @return A geometry upload ID string
#'
#' @seealso \code{\link{download_subset}} to use this geometry upload ID for
#'    data downloads.
#'
#' @importFrom dplyr %>%
#'
#' @export
upload_geometry <- function(geom, tmp_dir = tempdir()) {

  # Check for connection
  if(httr::http_error(paste0(gcdl_url,'docs'))){
    stop('Cannot connect to GeoCDL')
  }

  # From Web API documentation:
  # A supported file type containing geometry data (either point or polygon).
  # For point data, the following file formats are supported: CSV (comma-separated values),
  # shapefiles, and GeoJSON. For polygon data, the following file formats are supported:
  # shapefiles and GeoJSON. CSV files must contain a column named "x", "long", or "longitude"
  # (not case sensitive) and a column named "y", "lat", or "latitude" (not case sensitive).
  # Shapefiles must be uploaded in a single ZIP archive. Supported GeoJSON types for point
  # data uploads are "Point", "MultiPoint", "GeometryCollection", "Feature", and "FeatureCollection".
  # Supported GeoJSON types for polygon data uploads are "Polygon", "MultiPolygon", "GeometryCollection",
  # "Feature", and "FeatureCollection". For polygon data, GeoJSON objects and shapefiles with more than
  # one polygon definition are not supported (e.g., "MultiPolygon" objects must only contain one polygon).
  # "Holes" in polygons will be ignored.

  # Determine geom format.
  # If a spatial object, write to temporary zipped shapefile
  # If a filename, confirm it exists and is supported format.
  if(any(class(geom) %in% c("sf","sfc"))){

    # Check that it is point, multipoint, or single polygon
    geom_type <- class(sf::st_geometry(geom))
    if(any(grepl("POINT", geom_type)) |
       (any(grepl("POLYGON", geom_type)) & length(sf::st_geometry(geom)) == 1)){
      upname <- zip_shapefiles(geom, tmp_dir)
    } else if(any(grepl("POLYGON", geom_type)) & length(sf::st_geometry(geom)) > 1){
      # Compare convex hull area to sum of union area
      ch_area <- geom %>%
        sf::st_union() %>%
        sf::st_convex_hull() %>%
        sf::st_area()
      un_area <- geom %>%
        sf::st_union() %>%
        sf::st_area()
      if(as.numeric(un_area/ch_area) > 0.8){
        # Upload union of polygons
        guid <- geom %>%
          sf::st_union() %>%
          upload_geometry()
        return(guid)
      } else {
        # Individually upload polygons
        guid <- NULL
        for(i in 1:length(sf::st_geometry(geom))){
          guid <- c(guid, upload_geometry(geom[i,]))
        }
        return(guid)
      }
    } else {
      stop('Unsupported upload geometry: only points and polygons supported')
    }

  } else if(any(grepl("Spatial",class(geom)))){

    # Check that it is point, multipoint, or single polygon
    geom_type <- class(geom)
    if(any(grepl("SpatialPoints",geom_type)) |
       (any(grepl("SpatialPolygons",geom_type)) & length(geom) == 1)){
      upname <- zip_shapefiles(geom, tmp_dir)
    } else {
      stop('Unsupported upload geometry: only points and single polygon supported')
    }

  } else if(typeof(geom) == 'character'){

    if(!file.exists(geom)){
      stop("Unsupported upload geometry: can not find file")
    } else if(grepl(".shp",geom)){

      #grab auxiliary files: dbf, shx, prj
      shp_files <- list.files(dirname(geom),
                              sub('\\..*$', '', basename(geom)),
                              full.names = TRUE)
      if(length(shp_files) == 0) stop("error1")
      shp_files <- shp_files[grepl('.shp|.shx|.dbf|.prj',shp_files)]
      if(length(shp_files) == 0) stop("error2")
      if(any(!file.exists(shp_files))){
        stop('Unsupported upload geometry: can not find all shapefile files')
      }
      upname <- tempfile(fileext = '.zip')
      utils::zip(upname, shp_files, flags = '-9Xq')

    } else if(grepl(".csv|.geojson|.zip",geom)){

      upname <- geom

    } else {
      stop('Unsupported upload geometry: unknown file extension')
    }
  } else {
    stop('Unsupported upload geometry: unknown format')
  }

  # POST
  guid_r <- httr::POST(paste0(gcdl_url,'upload_geom'),
                       body = list(geom_file = httr::upload_file(upname)),
                 encode = "multipart")

  # Check for error status code
  if(httr::http_error(guid_r)){
    stop(paste('GeoCDL returned an error:',
               httr::http_status(guid_r)$message,
               httr::content(guid_r)))
  }

  # Extract geometry upload ID
  guid <- httr::content(guid_r)$geom_guid

  return(guid)

}


