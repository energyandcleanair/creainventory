edgar_time_variation <- function(country, sector){
  d <- readxl::read_xlsx("data/EDGAR_temporal_profiles_r1.xlsx",
                  sheet = "monthly & hourly temp profiles",
                  skip = 1)


  sectors <- unique(d[,"Activity sector description"])
  countries <- unique(d[,"Activity sector description"])

  if(!sector %in% sectors){
    stop(sprintf("Sector %s not found. Available sectors are %s", sector, paste(sectors, collapse=",")))
  }

  if(!country %in% countries){
    stop(sprintf("Country %s not found. Available countries are %s", country, paste(countries, collapse=",")))
  }


}
