#!/usr/bin/Rscript
args = commandArgs(trailingOnly = TRUE)

# rm(list = ls()); gc()
library(dplyr); library(ggplot2)
library(sf)
library(raster)
library(ncdf4)


yearlyPM25_wide <- readRDS("./data/tang2023_PM.rds")

tang2023 <- sf::st_read("./data/tang2023/74548_projected polygons.shp", quiet = TRUE) 
tang2023 <- sf::st_zm(tang2023, drop = TRUE, what = "ZM")
tang2023 <- sf::st_make_valid(tang2023)
tang2023 <- tang2023[which(!is.na(yearlyPM25_wide$minLossYear)), ]

tang2023 <- tang2023 %>%
  sf::st_simplify(50, preserveTopology = TRUE) 


tang2023$centroid <- sf::st_centroid(tang2023) %>%
  sf::st_geometry()
st_geometry(tang2023) <- "centroid"

sf::sf_use_s2(FALSE)

lowBound <- as.numeric(args[1])*1000 # say 5000
uppBound <- as.numeric(args[2])*1000 # in meters, say 20000
midBuffer <- (uppBound - lowBound)/2 # 12500
bufferDist <- uppBound - midBuffer # 7500

centroidBuffer <- tang2023 |>
  sf::st_buffer(dist = midBuffer) |>
  sf::st_boundary() |>
  sf::st_buffer(dist = bufferDist)

centroidBuffer <- centroidBuffer %>%
  dplyr::select(OBJECTID)


fileName <- "./data/PM25/V6GL02.04.CNNPM25.GL.202301-202312.nc"
testRaster <- raster::raster(fileName)
# crs(testRaster) ==
#   sf::st_crs(centroidBuffer)

polygons_sf <- st_transform(centroidBuffer, crs(testRaster))

Sys.time()
for (i in 1998:2023) {
  cat(i, ",")
  fileName <- paste0("./data/PM25/V6GL02.04.CNNPM25.GL.", i, "01-", i, "12.nc")
  testRaster <- raster::raster(fileName)
  mean_pm25_values <- exactextractr::exact_extract(
    x = testRaster,
    y = polygons_sf,
    fun = function(values, coverage_fraction) {
      weighted.mean(values[values >= 0],
                    coverage_fraction[values >= 0],
                    na.rm = TRUE)
    }#,
    # progress = TRUE # Optional: show a progress bar
  )
  polygons_sf[, paste0("pm25_", i)] <- mean_pm25_values

}
Sys.time()

saveRDS(polygons_sf, file = paste0("./data/bufferDonut_", args[1], "_", args[2], "_PM.rds"))

# 0–5 km, 5–20 km, 20–50 km, 50–100 km buffers 

# nohup ./extractPM25bufferDonut.R 5 20 & 
# nohup ./extractPM25bufferDonut.R 20 50 & 
# nohup ./extractPM25bufferDonut.R 50 100 & 
