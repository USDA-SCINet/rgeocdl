# Current REST API url
gcdl_url <- '10.1.1.80:8000/'

# Retrieve all metadata - do behind the scenes once in a session?

# Set global variables
utils::globalVariables(c('outfiles','dataset','variable','dv'))

# Compress files into a zipped folder
zip_shapefiles <- function(geom, dsn){

  #check if dsn exists

  upname_prefix <- tempfile('shp',tmpdir='')
  dir.create(paste0(dsn,'/',substr(upname_prefix,2,nchar(upname_prefix))))
  tmp_shp <- paste0(dsn, upname_prefix, upname_prefix, ".shp")
  sf::st_write(geom,  tmp_shp)
  upname <- paste0(dsn, upname_prefix,".zip")
  utils::zip(upname,
      list.files(paste0(dsn, upname_prefix),
                 pattern=substr(upname_prefix,2,nchar(upname_prefix)),
                 full.names = TRUE),
      flags = '-9Xq')

  # delete non-zip files here?

  return(upname)
}

format_dsvars <- function(ds, vars){

  # DATASET_ID:VARNAME[,VARNAME...][;DATASET_ID:VARNAME[,VARNAME...]...]
  dsvars <- dplyr::tibble()
  ## Check if ds is a data.frame, tibble, or matrix
  if(any(grepl('data.frame|matrix',class(dsvars))) & is.null(vars)){
    # Assume first column is dataset and second is variable
    dsvars <- ds
    names(dsvars)[1:2] <- c('dataset','variable')
  } else {
    stop('Unsupported dataset/variable specification')
  }

  # Check variable and dataset validity against metadata
  #dsvars %>%
  #  rowwise()  %>%
  #  mutate(available = checkMetadata(ds = dataset, var = variable))

  dv_str <- dsvars %>%
    dplyr::group_by(dataset) %>%
    dplyr::summarise(dv = paste0(unique(dataset),':',paste(variable,collapse = ','))) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(dv = paste0(dv,collapse = ";")) %>%
    dplyr::pull(dv) %>%
    utils::URLencode(reserved = TRUE)

  return(dv_str)
}

format_dates <- function(dates, years, months, days){
  temporal_subset <- ""
  if(!is.null(dates)){
    #dheck date format
    temporal_subset <- paste0("&dates=",paste(dates,collapse = ","))
  } else if(!is.null(years)){
    temporal_subset <- paste0("&years=",years)
    if(!is.null(months)){
      temporal_subset <- paste0(temporal_subset,"&months=",months)
    }
    if(!is.null(days)){
      temporal_subset <- paste0(temporal_subset,"&days=",days)

    }
  }

  return(temporal_subset)

}

format_geometry <- function(endpoint, geom){

  spatial_subset <- ''

  # Supported formats: GUID, two columns of coordinates, sf/sp object, or filename
  if(is.null(geom)){
    return(spatial_subset)
  } else if(all(class(geom) == 'character')){
    # GUID or filename
    if(all(nchar(geom) == 36) & !(grepl(".",geom[1], fixed=TRUE))){
      # GUID
      spatial_subset <- paste0('&geom_guid=',geom)
    } else {
      # Filename, so try to upload
      geom_guid <- upload_geometry(geom)
      spatial_subset <- paste0('&geom_guid=',geom_guid)
    }
  } else if(any(grepl('sf',class(geom))) ){
    # Get GUID
    geom_guid <- upload_geometry(geom)
    spatial_subset <- paste0('&geom_guid=',geom_guid)
  } else if(all(grepl('frame|matrix',class(geom)))){
    # Format coordinates into string
    ep_prefix <- ifelse(endpoint == 'subset_polygon', '&clip=', '&points=')
    geom_str <- paste(apply(geom,1,function(x) paste0('(',paste(x,collapse = ","),")")),collapse = ",")
    spatial_subset <- paste0(ep_prefix,geom_str)
  } else {
    stop('Unrecognized geometry format.')
  }


  return(spatial_subset)
}

infer_endpoint <- function(geom){

  poly_ep <- 'subset_polygon'
  pt_ep <- 'subset_points'

  # If null, then it assumed user wants whole area of datasets
  if(is.null(geom)){
    return(poly_ep)
  } else if(any(class(geom) == "sf")){

    geom_type <- class(geom$geometry)
    if(any(grepl('POLYGON', geom_type))){
      return(poly_ep)
    } else if(any(grepl('POINT', geom_type))){
      return(pt_ep)
    } else {
      stop('Unrecognized sf geometry type. Could not infer GeoCDL subset endpoint.')
    }

  } else if(any(class(geom) == "Spatial")){

    # Check that it is point, multipoint, or single polygon
    geom_type <- class(geom)
    if(any(grepl('Polygon', geom_type))){
      return(poly_ep)
    } else if(any(grepl('Points', geom_type))){
      return(pt_ep)
    } else {
      stop('Unrecognized sp geometry type. Could not infer GeoCDL subset endpoint.')
    }

  } else {
    stop('Unsupported geometry type for inferring GeoCDL subset endpoint.
         Please use download_[polygon|points]_subset() directly.')
  }
}

format_spatial_parameters <- function(endpoint, ri_method, t_crs, resolution){

  ri_method_name <- ifelse(endpoint == 'subset_points',
                           'interpolation_method',
                           'resample_method')
  spatial_parameters <- paste0('&',ri_method_name,"=",ri_method)
  if(!is.null(t_crs)) {
    spatial_parameters <- paste0(spatial_parameters,"&crs=",t_crs)
  }
  if(!is.null(resolution)) {
    spatial_parameters <- paste0(spatial_parameters,"&resolution=",resolution)
  }

  return(spatial_parameters)
}

format_temporal_parameters <- function(grain_method, validate_method){
  temporal_parameters <- paste0('&grain_method=',
                                grain_method,
                                '&validate_method=',
                                validate_method)

  return(temporal_parameters)
}

format_subset_query <- function(endpoint,
                                ds,
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
                                ri_method = 'nearest'){

  # Format datasets and variables into string
  dv_str <- format_dsvars(ds, vars)

  # Format user geometry
  spatial_subset <- format_geometry(endpoint, t_geom)

  # Format spatial parameters (crs, resolution, interpolation/resampling method)
  spatial_parameters <- format_spatial_parameters(endpoint, ri_method, t_crs, resolution)

  # Format temporal subset instructions
  temporal_subset <- format_dates(dates, years, months, days)

  # Temporal parameters (grain method, validate method)
  temporal_parameters <- format_temporal_parameters(grain_method, validate_method)

  # Other parameters
  other_parameters <- ''
  if(!is.null(out_format)){
    other_parameters <- paste0('&output_format=',out_format)
  }

  # Build query
  query_str <- paste0(gcdl_url,
                      endpoint,
                      '?',
                      paste0(
                        'datasets=',dv_str,
                        temporal_subset,
                        temporal_parameters,
                        spatial_subset,
                        spatial_parameters,
                        other_parameters))

  return(query_str)
}

submit_subset_query <- function(query_str, dsn, req_name){

  # Check destination
  if(!dir.exists(dsn)){
    stop('Destination folder does not exist.')
  }

  # Create folder for downloaded data
  dl_prefix <- ifelse(is.null(req_name),
                      tempfile("subset",tmpdir = ""),
                      paste0("/",req_name))

  # Get response from GeoCDL API
  num_queries <- length(query_str)
  if(num_queries == 0){
    stop('Missing query string')
  } else {
    out_files <- c()
    for(q in 1:length(query_str)){
      subset_dir <- paste0(dsn,dl_prefix,"-",q)
      subset_zip <- paste0(dsn,dl_prefix,"-",q,'.zip')

      subset_response <- httr::GET(query_str[q],
                                   httr::write_disk(subset_zip,
                                                    overwrite=TRUE))

      # Check for bad request / errors
      if(httr::http_error(subset_response)){
        stop(paste('GeoCDL returned an error:',
                   httr::http_status(subset_response)$message, '\n',
                   httr::content(subset_response)$detail))
      }

      # Unzip results to temporary file
      utils::unzip(subset_zip,
                   exdir = subset_dir)

      out_files <- c(out_files,
                     list.files(subset_dir,full.names = TRUE))
    }
    return(out_files)
  }

}
