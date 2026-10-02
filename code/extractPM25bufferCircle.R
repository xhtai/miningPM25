#!/usr/bin/Rscript
args = commandArgs(trailingOnly = TRUE)

################ version 2 ###################
# rm(list = ls()); gc()
library(dplyr); library(ggplot2)
library(sf)
library(raster)
library(ncdf4)

# 5 km buffer around centroid 

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

circleDist <- as.numeric(args[1])*1000
centroidBuffer <- tang2023 %>%
  sf::st_buffer(dist = circleDist) 

centroidBuffer <- centroidBuffer %>%
  dplyr::select(OBJECTID)

# sf::sf_use_s2(FALSE)

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
  if (i %in% c(2010, 2015, 2020)) saveRDS(polygons_sf, file = paste0("./data/2-19-26bufferCircle", args[1], "km_PM.rds"))
  
}
Sys.time()

saveRDS(polygons_sf, file = paste0("./data/bufferCircle", args[1], "km_PM.rds"))

# nohup ./extractPM25bufferCircle.R 5 & 

