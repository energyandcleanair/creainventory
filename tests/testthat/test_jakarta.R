example_dir <- function(){
  if(dir.exists("example")) "example" else "../../example"
}

test_that("rasterizing conserves total emission - Roads", {

  emission.d.transport <- read.csv(file.path(example_dir(),"jakarta","traffic_emission_2019_bps.csv")) %>%
    tidyr::gather("poll","emission",-c(name, name_local, region_id)) %>%
    mutate(year=2019,
           unit="tonne") %>%
    rename(id=region_id)
  support.sp.transport <- sf::read_sf(file.path(example_dir(),"jakarta","transport_spatial.shp"))
  # support.sp.transport <- raster::shapefile(file.path(example_dir(),"jakarta","transport_spatial.shp"))
  # emission.short <- emission.d.transport %>% head(500)
  e <- creainventory::combine(emission.d.transport, support.sp.transport)
  g <- raster::raster(raster::extent(e), crs=raster::projection(e), nrow=100, ncol=100)
  r <- creainventory::grid.rasterize(e, g)
  # r.old <- creainventory::grid.rasterize.old(e, g)

  # Test emission conservation
  sum.raster <- raster::cellStats(r, "sum") %>%
    tibble(value=., poll=names(.))
  sum.input <- e.short %>%
    as.data.frame() %>%
    group_by(poll) %>%
    summarise(value=sum(emission))
  sum.compared <- left_join(sum.raster, sum.input, by="poll")

  lapply(seq(nrow(sum.compared)), function(i){
    expect_equal(sum.compared[[i,"value.x"]], sum.compared[[i,"value.y"]])
  })

})
