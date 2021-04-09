export.monthly <- function(r.monthly, poll, sector, year, folder){

  dir.create(folder, showWarnings = F, recursive = T)
  rs <- raster::unstack(r.monthly)
  lapply(seq_along(rs), function(month){
    filename <- file.path(folder,
                          sprintf("%s_%s_%s_%d.nc",
                                  sector, poll, year, month))

    r <- rs[[month]]
    names(r) <- poll

    raster::writeRaster(r, filename)

  })
}
