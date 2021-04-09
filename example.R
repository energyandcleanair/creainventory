# library(tidyverse)
# library(sf)
#
# source('R/utils.R')
# source('R/check.R')

library(creainventory)

# Point sources
emission.d.power <- read.csv("example/power_emission_data.csv")
support.sp.power <- read.csv("example/power_spatial.csv") %>% sf::st_as_sf(coords=c("longitude","latitude"))

# Line sources
emission.d.transport <- read.csv("example/transport_emission_data.csv")
support.sp.transport <- sf::read_sf("example/transport_spatial.shp")

# Area source
emission.d.farmland <- read.csv("example/farmland_emission_data.csv")
support.sp.farmland <- sf::read_sf("example/farmland_spatial.shp")

# Check
check.emission.d(emission.d.power)
check.support.sp(support.sp.power)

check.emission.d(emission.d.transport)
check.support.sp(support.sp.transport)

check.emission.d(emission.d.farmland)
check.support.sp(support.sp.farmland)

# Combine data with SP
emission.power <- creainventory::combine(emission.d.power, support.sp.power)
emission.transport <- creainventory::combine(emission.d.transport, support.sp.transport)
emission.farmland <- creainventory::combine(emission.d.farmland, support.sp.farmland)


# Read grid support
grid <- grid.read(filename="data/d04.grid.tif")


# Create a single layer representing whole year
r.power <- grid.rasterize(emission.power, grid)
r.transport <- grid.rasterize(emission.transport, grid)
r.farmland <- grid.rasterize(emission.farmland, grid)


# Create a stack representing monthly variations
month_shares <- rep(1/12, 12)
r.monthly.power <- temporal.split_months(r.power, month_shares)


# Export
export.monthly(r.monthly.power,
               poll="no2",
               sector="power",
               year=2019,
               folder="results")
