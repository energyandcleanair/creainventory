#' Read grid
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
grid.read <- function(filename="data/d04.grid.tif"){
  raster::raster(filename) %>%
    raster::raster() # Only keep grid, not values
}



#' Title
#'
#' @param emission.sp
#' @param grid
#'
#' @return
#' @export
#'
#' @examples
grid.rasterize <- function(emission.sp, grid){


  if(is.na(sf::st_crs(emission.sp))){
    warning("No CRS set in emission.sp. Assuming EPSG4326")
    emission.sp <- sf::st_set_crs(emission.sp, 4326)
  }

  # Convert to Spatial if need be
  if("sf" %in% class(emission.sp)){
    emission.sp <- as(emission.sp, "Spatial")
  }

  # Cutting in grid
  if("SpatialPolygonsDataFrame" %in% class(emission.sp)){
    emission.sp <- grid.cut_and_weight.polygons(emission.sp, grid)
  }

  if("SpatialLinesDataFrame" %in% class(emission.sp)){
    emission.sp <- grid.cut_and_weight.lines(emission.sp, grid)
  }

  # Rasterize
  emission.sp %>%
    sf::st_as_sf() %>%
    sf::st_transform(projection(grid)) %>%
    raster::rasterize(grid, field="emission", fun="sum")

}


grid.cut_and_weight.polygons <- function(emission.sp.pols, grid){

  # Transform to grid crs (SP ran into memory issue)
  emission.sp <- utils.to_spatial(emission.sp.pols) %>%
    sp::spTransform(CRS(projection(grid)))

  # Add temporary feature id for future grouping
  emission.sp$feature_id_tmp <- 1:nrow(emission.sp)

  # Cut lines along grid cells
  rs <- grid
  # rs[] <- 1:ncell(rs) #not kept by rasterToPolygon??
  rsp <- rasterToPolygons(grid)
  rsp$layer <- 1:ncell(rs)

  print("Cutting along grid...")
  rp <- raster::intersect(emission.sp, rsp)
  print("Done")

  print("Calculating area...")
  rp$area <- rgeos::gArea(rp, byid=TRUE)
  print("Done")

  # Weighting accordingly
  print("Weighting by area...")
  rp@data <- rp@data %>%
    group_by(feature_id_tmp) %>%
    do(mutate(., emission=.$emission * area / sum(.$area)))
  print("Done")

  # Keep full cells, so that rasterize keeps cells even if cell center not covered by the original shape
  print("Joining...")
  rp <- rp %>%
    as.data.frame() %>%
    ungroup() %>%
    dplyr::left_join(sf::st_as_sf(rsp), copy=T) %>%
    sf::st_as_sf() %>%
    as("Spatial")
  print("Done")

  rp$feature_id_tmp <- NULL

  return(rp)
}


grid.cut_and_weight.lines <- function(emission.sp.lines, grid){


  # Transform to grid crs
  emission.sp <- utils.to_spatial(emission.sp.lines) %>%
    sp::spTransform(CRS(projection(grid)))

  # Cut lines along grid cells
  rs <- grid
  rs[] <- 1:ncell(rs)
  rsp <- rasterToPolygons(grid)

  # Add temporary feature id for future grouping
  emission.sp$feature_id_tmp <- 1:nrow(emission.sp)

  print("Cutting along grid...")
  rp <- raster::intersect(emission.sp, rsp)
  print("Done")

  print("Calculating length...")
  rp$length <- rgeos::gLength(rp, byid=TRUE)
  print("Done")

  # Weighting accordingly
  print("Weighting by length...")
  rp@data <- rp@data %>%
    group_by(feature_id_tmp) %>%
    do(mutate(., emission=.$emission * length / sum(.$length)))
  print("Done")

  emission.sp$feature_id_tmp <- NULL

  return(rp)
}
