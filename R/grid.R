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



