#' Pseudo-function to import \strong{dplyr}'s common functions.
#'
#' @importFrom dplyr select rename mutate mutate_at filter filter_at arrange distinct
#'     summarise summarize do group_by group_by_at ungroup do left_join inner_join everything bind_rows
#'     pull tibble as_tibble rowwise any_vars all_vars vars collect full_join
NULL

#' Pseudo-function to import \strong{tibble}'s common functions.
#'
#' @importFrom tibble tibble
NULL

# ' Pseudo-function to import raster functions
#'
#' @importFrom raster projection extent ncell rasterToPolygons
NULL

# ' Pseudo-function to import sf functions
#'
#' @importFrom sf st_as_sf st_coordinates st_as_sfc st_buffer st_union st_centroid
NULL

# ' Pseudo-function to import sp functions
#'
#' @importFrom sp CRS spTransform
NULL
