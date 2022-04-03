#' Rasterize emissions (point, line or polygon based)
#'
#' @param emission.sp
#' @param grid
#'
#' @return An ABSOLUTE-emission raster (initial unit)
#' @export
#'
#' @examples
rasterize <- function(emission.sp, grid, terra_or_raster="terra", geom_unique_id=NULL){

  # Convert to Spatial if need be
  if("sf" %in% class(emission.sp)){
    emission.sp <- as(emission.sp, "Spatial")
  }

  if(is.na(raster::crs(emission.sp))){
    warning("No CRS set in emission.sp. Assuming EPSG4326")
    sp::proj4string(emission.sp) <- sp::CRS("+init=epsg:4326")
  }

  # Only keep features with actual emissions to make things faster
  emission.sp <- emission.sp[emission.sp$emission>0,]

  if(!raster::compareCRS(emission.sp, grid)){
    print("Reprojecting...")
    emission.sp <- emission.sp %>%
      sp::spTransform(raster::crs(grid))
    print("Done")
  }


  print("Rasterizing...")
  # Cut if need be
  if("SpatialLinesDataFrame" %in% class(emission.sp)){
    r <- rasterize.lines(emission.sp, grid, geom_unique_id=geom_unique_id)
  }

  if("SpatialPolygonsDataFrame" %in% class(emission.sp)){
    r <- rasterize.polygons(emission.sp, grid)
  }

  if("SpatialPointsDataFrame" %in% class(emission.sp)){
    r <- rasterize.points(emission.sp, grid)
  }
  print("Done")

  return(r)
}


#'
#' #' Title
#' #'
#' #' @param emission.sp
#' #' @param grid
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' rasterize <- function(emission.sp, grid){
#'
#'
#'   if(is.na(sf::st_crs(emission.sp))){
#'     warning("No CRS set in emission.sp. Assuming EPSG4326")
#'     emission.sp <- sf::st_set_crs(emission.sp, 4326)
#'   }
#'
#'   # Convert to Spatial if need be
#'   if("sf" %in% class(emission.sp)){
#'     emission.sp <- as(emission.sp, "Spatial")
#'   }
#'
#'   # Cutting in grid
#'   if("SpatialPolygonsDataFrame" %in% class(emission.sp)){
#'     emission.sp <- cut_and_weight.polygons(emission.sp, grid)
#'   }
#'
#'   if("SpatialLinesDataFrame" %in% class(emission.sp)){
#'     emission.sp <- cut_and_weight.lines(emission.sp, grid)
#'   }
#'
#'   # Rasterize
#'   emission.sp %>%
#'     sf::st_as_sf() %>%
#'     sf::st_transform(projection(grid)) %>%
#'     raster::rasterize(grid, field="emission", fun="sum")
#'
#' }
#'
#'
#'
#' cut_and_weight.polygons <- function(emission.sp.pols, grid){
#'
#'   # Transform to grid crs (SP ran into memory issue)
#'   emission.sp <- utils.to_spatial(emission.sp.pols) %>%
#'     sp::spTransform(CRS(projection(grid)))
#'
#'   # Add temporary feature id for future grouping
#'   emission.sp$feature_id_tmp <- 1:nrow(emission.sp)
#'
#'   # Cut lines along grid cells
#'   rs <- grid
#'   # rs[] <- 1:ncell(rs) #not kept by rasterToPolygon??
#'   rsp <- rasterToPolygons(grid)
#'   rsp$layer <- 1:ncell(rs)
#'
#'   print("Cutting along grid...")
#'   rp <- raster::intersect(emission.sp, rsp)
#'   print("Done")
#'
#'   print("Calculating area...")
#'   rp$area <- rgeos::gArea(rp, byid=TRUE)
#'   print("Done")
#'
#'   # Weighting accordingly
#'   print("Weighting by area...")
#'   rp@data <- rp@data %>%
#'     group_by(feature_id_tmp) %>%
#'     do(mutate(., emission=.$emission * area / sum(.$area)))
#'   print("Done")
#'
#'   # Keep full cells, so that rasterize keeps cells even if cell center not covered by the original shape
#'   print("Joining...")
#'   rp <- rp %>%
#'     as.data.frame() %>%
#'     ungroup() %>%
#'     dplyr::left_join(sf::st_as_sf(rsp), copy=T) %>%
#'     sf::st_as_sf() %>%
#'     as("Spatial")
#'   print("Done")
#'
#'   rp$feature_id_tmp <- NULL
#'
#'   return(rp)
#' }
#'
#'
#'

rasterize.points <- function(emission.sp, grid, polls=NULL){

  if(is.null(polls)){
    polls <- unique(emission.sp$poll)
  }else{
    emission.sp <- emission.sp[emission.sp$poll %in% polls,]
  }

  sps <- emission.sp %>% sp::split(emission.sp@data$poll)

  emission_stack <- lapply(sps[polls],
                           function(x){
                             cropped_vect <- raster::crop(x,grid)
                             if(is.null(cropped_vect)){
                               warning("No feature overlapping")
                               return(grid %>% raster::`values<-`(0))
                             }
                             terra::rasterize(terra::vect(cropped_vect),
                                              terra::rast(grid),
                                              field="emission",
                                              fun=sum) %>%
                               raster::raster()
                           }) %>%
    raster::stack()

  return(emission_stack)
}


rasterize.lines <- function(emission.sp, grid, polls=NULL, geom_unique_id=NULL){

  if(is.null(polls)){
    polls <- unique(emission.sp$poll)
  }else{
    emission.sp <- emission.sp[emission.sp$poll %in% polls,]
  }

  if(is.null(geom_unique_id)){
    emission.sp$geom_id_tmp <- 1:nrow(emission.sp)
    emission.sp.unique <- emission.sp["geom_id_tmp"]
    geom_unique_id <- "geom_id_tmp"
  }else{
    print("Deduplicating geometries")
    emission.sp.unique <- emission.sp[which(!duplicated(emission.sp[[geom_unique_id]])), geom_unique_id]
    print(sprintf("Reduced from %d to %d", nrow(emission.sp), nrow(emission.sp.unique)))
    print("Done")
  }

  # Add temporary feature id for grouping
  emission.sp$feature_id_tmp <- 1:nrow(emission.sp)

  # Cut lines along grid cells
  print("Polygonizing...")
  rs <- grid
  rs[] <- 1:ncell(rs)
  names(rs) <- "i_cell"
  rsp <- rasterToPolygons(rs)
  print("Done")

  print("Cutting along grid...")
  # sf much less memory intensive than raster::intersect and faster

  # Chunking it to avoid rgeos_binpredfunc_prepared: maximum returned dense matrix size exceeded
  cutting_successful <- F
  chunk_size <- 1E10
  emission.sf.unique <- sf::st_as_sf(emission.sp.unique)

  while(!cutting_successful){
    tryCatch({
      rsp$chunk <- rsp$i_cell %/% chunk_size

      rp <- pbapply::pblapply(split(sf::st_as_sf(rsp), rsp$chunk),
                              function(rsp_chunk){
                                sf::st_as_sf(terra::intersect(terra::vect(rsp_chunk),
                                                              terra::vect(emission.sf.unique)))
                              }) %>%
        do.call("bind_rows",.)
      cutting_successful <- T
    }, error=function(e){
      if("size exceeded" %in% as.character(e)){
        chunk_size <- chunk_size / 100
        warning("Cutting failed: ", e, "\n Trying with smaller chunk size", )
      }else{
        stop(e)
      }
    })
  }

  print("Done")

  print("Calculating length...")
  rp$length <- sf::st_length(rp)
  print("Done")

  print("Attaching emission data back...")
  rp <- rp %>% left_join(
    emission.sp %>% as.data.frame()
  )
  print("Done")


  # Weighting accordingly
  print("Weighting by length...")
  rp <- rp %>%
    group_by(feature_id_tmp) %>%
    do(mutate(., emission=.$emission * length / sum(.$length)))
  print("Done")

  print("Rasterizing...")
  rp.sum <- rp %>%
    group_by(i_cell=as.integer(i_cell), poll) %>%
    summarise(emission=sum(emission, na.rm=T))

  # Print into raster directly!
  # Species by species though
  rp.sums <- rp.sum %>% split(rp.sum$poll)

  emission_stack <- lapply(rp.sums[polls],
                           function(x){
                             cells_x <- rep(0,ncell(rs))
                             cells_x[x$i_cell] <- x$emission
                             grid_x <- grid
                             grid_x[] <- cells_x
                             return(grid_x)
                           }) %>%
    raster::stack()

  return(emission_stack)
}


rasterize.polygons <- function(emission.sp, grid, polls=NULL){

  # r <- pbapply::pblapply(seq(nrow(emission.sp)),
  #                     function(i){
  #                       ri <- raster::rasterize(emission.sp[i,],
  #                                         grid,
  #                                        getCover=T
  #                       )
  #                       ri * emission.sp$emission[i] / raster::cellStats(ri, "sum")
  #                     }) %>%
  #   do.call(raster::stack,.) %>%
  #   raster::calc(sum)

  if(is.null(polls)){
    polls <- unique(emission.sp$poll)
  }else{
    emission.sp <- emission.sp[emission.sp$poll %in% polls,]
  }

  sps <- emission.sp %>% sp::split(emission.sp@data$poll)

  # Row by row
  # emission_stack <- lapply(sps[polls],
  #        function(x){
  #          pbapply::pblapply(seq(nrow(x)),
  #                            function(i){
  #                              v <- terra::vect(x[i,])
  #                              v$emission <- as.numeric(v$emission)
  #                              ri <- terra::rasterize(v,
  #                                                     terra::rast(grid),
  #                                                     cover=T,
  #                                                     background=0
  #                              ) %>% raster::raster()
  #                              ri * x$emission[i] / raster::cellStats(ri, "sum", na.rm=T)
  #                            }) %>%
  #            do.call(raster::stack,.) %>%
  #            raster::calc(sum)
  #        }) %>%
  #   raster::stack()

  # All in one go
  emission_stack <- pbapply::pblapply(sps[polls],
                           function(x){
                             area <- exactextractr::coverage_fraction(x=grid,y=as(x,"sf"))
                             lapply(seq_along(area),
                                    function(i){
                                      area[[i]] / raster::cellStats(area[[i]], "sum", na.rm=T) * x$emission[i]
                                    }) %>%
                               do.call(raster::stack,.) %>%
                               raster::calc(sum)
                           }) %>%
    raster::stack()

  return(emission_stack)
}
