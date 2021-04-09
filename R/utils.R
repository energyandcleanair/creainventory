utils.to_spatial <- function(x){
  if (grepl("spatial", class(x), ignore.case = TRUE)) x else as(x,"Spatial")
}

