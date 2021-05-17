check.fields <- function(d, required_fields){
  if(!all(required_fields %in% names(d))){
    stop(sprintf("Missing fields: %s", paste(setdiff(required_fields, names(d)),collapse=",")))
  }
}


#' Check emission data is valid
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
check.emission.d <- function(d){
  check.fields(d, required_fields=c("id", "poll", "unit", "year", "emission"))
  if(!is.numeric(d$emission)) stop("emission should be numeric field")
  print("OK")
}


#' Check emission vector data is valid
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
check.support.sp <- function(d){
  check.fields(d, required_fields=c("id", "geometry", "weight"))

  print("OK")
}
