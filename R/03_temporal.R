


create_temporal_variation <- function(month_share=NULL,
                                      day_hour_share=NULL){

  if(length(month_share) != 12){
    stop("month_share should have 12 elements")
  }

  if(length(day_hour_share) != 24){
    stop("day_hour_share should have 24 elements")
  }
}

temporal.split_months <- function(r, month_shares){

  if(length(month_shares) != 12){
    stop("month_shares should have 12 elements")
  }

  lapply(month_shares, function(share){
    r * share
  }) %>% raster::stack()
}
