example_dir <- function(){
  if(dir.exists("example")) "example" else "../../example"
}
#
# test_that("cutting within cells keeps total - lines", {
#
#   emission.d.transport <- read.csv(file.path(example_dir(),"transport_emission_data.csv"))
#   support.sp.transport <- sf::read_sf(file.path(example_dir(),"transport_spatial.shp"))
#   e <- creainventory::combine(emission.d.transport, support.sp.transport)
#   e.short <- e[1:100,]
#   g <- raster::raster(raster::extent(e), crs=raster::projection(e), nrow=1000, ncol=10)
#
#   e.cut <- grid.cut_and_weight.lines(e.short, g)
#
#   # Test emission conservation
#   expect_equal(sum(e.short$emission),
#                sum(e.cut$emission))
#
#   expect_gt(nrow(e.cut), nrow(e.short))
#
# })
#
#
# test_that("cutting within cells keeps total - polygons", {
#
#   emission.d.farmland <- read.csv(file.path(example_dir(),"farmland_emission_data.csv"))
#   support.sp.farmland <- sf::read_sf(file.path(example_dir(),"farmland_spatial.shp"))
#
#   e <- creainventory::combine(emission.d.farmland, support.sp.farmland)
#   e.short <- e %>% head(100)
#   g <- raster::raster(extent(e), crs=raster::projection(e), nrow=1000, ncol=10)
#
#   e.cut <- grid.cut_and_weight.polygons(e.short, g)
#
#   # Sanity check
#   expect_equal(sum(e.short$emission),
#                sum(e.cut$emission))
#
#   expect_gt(nrow(e.cut), nrow(e.short))
#
# })
test_that("combining keeps total - points", {

  emission.d.power <- read.csv(file.path(example_dir(),"power_emission_data.csv"))
  support.sp.power <- read.csv(file.path(example_dir(),"power_spatial.csv")) %>%
    sf::st_as_sf(coords=c("longitude","latitude")) %>%
    sf::st_set_crs(4326)

  e <- creainventory::combine(emission.d.power, support.sp.power)

  expect_equal(
    e %>% as.data.frame() %>% group_by(poll) %>% summarise_at("emission", sum) %>% arrange(poll),
    emission.d.power %>% as.data.frame() %>% group_by(poll) %>% summarise_at("emission", sum) %>% arrange(poll)
  )

})


test_that("combining keeps total - lines", {

  emission.d.transport <- read.csv(file.path(example_dir(),"jakarta","traffic_emission_2019_bps.csv")) %>%
    tidyr::gather("poll","emission",-c(name, name_local, region_id)) %>%
    mutate(year=2019,
           unit="tonne") %>%
    rename(id=region_id)
  support.sp.transport <- sf::read_sf(file.path(example_dir(),"jakarta","transport_spatial.shp"))
  e <- creainventory::combine(emission.d.transport, support.sp.transport)

  expect_equal(
    e %>% as.data.frame() %>% group_by(poll) %>% summarise_at("emission", sum) %>% arrange(poll),
    emission.d.transport %>% as.data.frame() %>% group_by(poll) %>% summarise_at("emission", sum) %>% arrange(poll)
  )

})


test_that("rasterizing keeps total - lines", {

  emission.d.power <- read.csv(file.path(example_dir(),"power_emission_data.csv"))
  support.sp.power <- read.csv(file.path(example_dir(),"power_spatial.csv")) %>%
    sf::st_as_sf(coords=c("longitude","latitude")) %>%
    sf::st_set_crs(4326)

  e <- creainventory::combine(emission.d.power, support.sp.power)
  e.short <- e %>% head(100)
  g <- raster::raster(raster::extent(e), crs=raster::projection(e), nrow=100, ncol=100)

  r <- creainventory::grid.rasterize(e.short, g)
  raster::plot(r)
  raster::plot(e.short, add=T)

  # Test emission conservation
  expect_equal(sum(e.short$emission),
               raster::cellStats(r,"sum"))
})


test_that("rasterizing keeps total - lines", {

  emission.d.transport <- read.csv(file.path(example_dir(),"transport_emission_data.csv"))
  support.sp.transport <- sf::read_sf(file.path(example_dir(),"transport_spatial.shp"))
  e <- creainventory::combine(emission.d.transport, support.sp.transport)
  e.short <- e #%>% head(500)
  g <- raster::raster(raster::extent(e.short), crs=raster::projection(e), nrow=100, ncol=100)


  require(tictoc)

  tic()
  r <- creainventory::grid.rasterize(e.short, g)
  toc()

  # tic()
  # r.old <- creainventory::grid.rasterize.old(e.short, g)
  # toc()

  # Test emission conservation
  expect_equal(sum(e.short$emission),
               terra::global(r,"sum")[[1]])
#
#   expect_equal(sum(e.short$emission),
#                raster::cellStats(r.old,"sum"))
})



test_that("rasterizing keeps total - polygons", {

  emission.d.farmland <- read.csv(file.path(example_dir(),"farmland_emission_data.csv"))
  support.sp.farmland <- sf::read_sf(file.path(example_dir(),"/farmland_spatial.shp"))

  e <- creainventory::combine(emission.d.farmland, support.sp.farmland)
  e.short <- e %>% head(10)
  g <- raster::raster(raster::extent(e), crs=raster::projection(e), nrow=100, ncol=100)

  r <- creainventory::grid.rasterize(e.short, g)

  # Test emission conservation
  expect_equal(sum(e.short$emission),
               raster::cellStats(r,"sum"))


})
