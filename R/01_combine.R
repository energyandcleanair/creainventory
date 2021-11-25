
#' Combine emission data and emission vector support data
#'
#' @param emission.d
#' @param emission.sp
#'
#' @return
#' @export
#'
#' @examples
combine <- function(emission.d, support.sp){

  support.sp %>%
    # join by region id (and poll if support is pollutant specific)
    dplyr::right_join(emission.d, by=intersect(c("id","poll"), names(support.sp))) %>%
    group_by(id, poll) %>%
    do(mutate(., weight = .$weight / sum(.$weight))) %>%
    mutate(emission=emission*weight) %>%
    ungroup() %>%
    sf::st_as_sf() # do(mutate) converts to data.frame
}
