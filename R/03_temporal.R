#' Split by date
#'
#' @param emission.raster
#' @param date_weight
#'
#' @return a tibble date x rasterstack (or rasterlayer)
#' @export
#'
#' @examples
temporal.split <- function(emission.raster, date_weight){

  if(! class(emission.raster)[[1]] %in% c("RasterStack","RasterLayer")){
    stop("emission.raster should be a RasterStack or a RasterLayer")
  }
  check.fields(date_weight, c("date","weight"))

  sum_weight <- sum(date_weight$weight)
  date_weight %>%
    rowwise() %>%
    mutate(emission.raster=list(emission.raster * weight / sum_weight)) %>%
    select(-c(weight))
}
