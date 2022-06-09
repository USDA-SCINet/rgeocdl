#' Upload a user geometry
#'
#' This function uploads a user geometry to the GeoCDL
#' REST API and returns a geometry upload ID to use
#' in subset requests.
#'
#' @param geom A sp or sf object or a filename. ELABORATE
#'
#' @return A geometry upload ID string
#'
#' @seealso \code{\link{view_metadata}} to list the dataset
#'   metadata by dataset ID
#'
#' @export
upload_geometry <- function(geom) {

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
  if(any(class(geom) == "sf")){

    # Check that it is point, multipoint, or single polygon
    geom_type <- class(geom$geometry)
    if(any(geom_type %in% paste0("sfc_",c("POINT","MULTIPOINT","POLYGON"))) |
       any(geom_type == "sfc_MULTIPOLYGON" & length(geom$geometry) == 1)){
      tmp_dir <- tempdir()
      upname_prefix <- tempfile(tmpdir='')
      tmp_shp <- paste0(tmp_dir, upname_prefix, ".shp")
      sf::st_write(geom,  tmp_shp)
      upname <- paste0(tmp_dir, upname_prefix,".zip")
      zip(upname,
          list.files(tmp_dir,
                     pattern=substr(upname_prefix,2,nchar(upname_prefix)),
                     full.names = TRUE),
          flags = '-9Xq')
    } else {
      stop('Unsupported upload geometry: only points and single polygon supported')
    }

  } else if(grepl("Spatial",class(geom))){

    # Check that it is point, multipoint, or single polygon
    geom_type <- class(geom)
    if(grepl("SpatialPoints",geom_type) |
       (grepl("SpatialPolygons",geom_type) & length(geom) == 1)){
      tmp_dir <- tempdir()
      upname_prefix <- tempfile(tmpdir='')
      tmp_shp <- paste0(tmp_dir, upname_prefix, ".shp")
      sf::st_write(geom,  tmp_shp)
      upname <- paste0(tmp_dir, upname_prefix,".zip")
      zip(upname,
          list.files(tmp_dir,
                     pattern=substr(upname_prefix,2,nchar(upname_prefix)),
                     full.names = TRUE),
          flags = '-9Xq')
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
      zip(upname, shp_files, flags = '-9Xq')

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
    stop(paste('GeoCDL returned an error:', httr::http_status(guid_r)$message)) # or content()$detail?
  }

  # Extract geometry upload ID
  guid <- httr::content(guid_r)$geom_guid

  return(guid)

}


