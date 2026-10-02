########## nightlights ############
# use harmonized NL data, available from 1992-2024
# steps: 
# combine individual NL files to a single file with multiple bands

# 1992-2021 first
# Step 1: download zip file (has 1992-2021 harmonized)
download.file("https://figshare.com/ndownloader/articles/9828827/versions/8", destfile = "./data/nightlights/nightlights.zip", mode = "wb")
unzip("./data/nightlights/nightlights.zip", exdir = "./data/nightlights/")

tmpFiles <- system("ls ./data/nightlights/", intern = TRUE)
tmpFiles <- tmpFiles[grepl("Harmonized", tmpFiles)] # 1992-2021, in order

for (i in 1:length(tmpFiles)) {
  googledrive::drive_upload(paste0("./data/nightlights/", tmpFiles[i]), path = "GEEexports/")
}

# Step 2: combine into a single image with multiple bands 
combinedRaster <- raster::raster(paste0("./data/nightlights/", tmpFiles[1]))
for (i in 2:length(tmpFiles)) {
  cat(i, ", ")
  tmpRaster <- raster::raster(paste0("./data/nightlights/", tmpFiles[i]))
  if (raster::extent(tmpRaster) != raster::extent(combinedRaster)) {
    cat(i, "th file has different extent")
    raster::extent(tmpRaster) <- raster::extent(combinedRaster)
  }
  combinedRaster <- raster::stack(combinedRaster, tmpRaster)
}
# bands are sorted from 1-30, corresponding to 1992-2021

# first 17 have this extent
# class      : Extent 
# xmin       : -180.0042 
# xmax       : 180.0042 
# ymin       : -65.00417 
# ymax       : 75.00417 

# 18, 19 and 20th files have different extent (but same dimensions)
raster::writeRaster(combinedRaster, filename = "./data/nightlights/combinedNL.tif", format="GTiff") # 1 h 20 minutes 

##### 
library(httr); library(jsonlite)
# Get metadata for version 10 specifically
meta <- GET("https://api.figshare.com/v2/articles/9828827/versions/10")
meta_json <- fromJSON(content(meta, "text", encoding = "UTF-8"))
# Show available files
meta_json$files[, c("id", "name", "download_url")]
# 31 57065303 Harmonized_DN_NTL_2022_simVIIRS.tif https://ndownloader.figshare.com/files/57065303
# 32 57065300 Harmonized_DN_NTL_2023_simVIIRS.tif https://ndownloader.figshare.com/files/57065300
# 33 57065306 Harmonized_DN_NTL_2024_simVIIRS.tif https://ndownloader.figshare.com/files/57065306

library(curl)
base_path <- "./data/nightlights/"
for (i in 1:33) {
  url  <- meta_json$files$download_url[i]
  name <- meta_json$files$name[i]
  
  cat("Downloading:", name, "\n")
  
  curl_download(
    url,
    file.path(base_path, name)
  )
}

tmpFiles <- system("ls ./data/nightlights/", intern = TRUE)
tmpFiles <- tmpFiles[grepl("Harmonized", tmpFiles)] # 2022-2024, in order

# Step 2: combine into a single image with multiple bands 
combinedRaster <- raster::raster(paste0("./data/nightlights/", tmpFiles[1]))

for (i in 2:length(tmpFiles)) {
  cat(i, ", ")
  tmpRaster <- raster::raster(paste0("./data/nightlights/", tmpFiles[i]))
  if (raster::extent(tmpRaster) != raster::extent(combinedRaster)) {
    cat(i, "th file has different extent")
    raster::extent(tmpRaster) <- raster::extent(combinedRaster)
  }
  combinedRaster <- raster::stack(combinedRaster, tmpRaster)
}
# bands are sorted from 1-30, corresponding to 1992-2021

raster::writeRaster(combinedRaster, filename = "./data/nightlights/combinedNL_2022_2024.tif", format="GTiff") # 1 h 20 minutes 

# googledrive::drive_upload("./data/nightlights/combinedNL_1992_2024.tif", path = "GEEexports/")

# google earth engine can no longer upload from drive, so download to computer and do manually 


######## do computations in GEE ########
# CODE IN EARTH ENGINE: GEE2_NL
#             landscan: GEE3_pop

### 
fileList <- googledrive::drive_ls("GEEexports/")

fileList %>% print(n = 30)

# i <- 10#1
for (i in 11:18) {
  googledrive::drive_download(paste0("GEEexports/", fileList$name[i]),
                              path = paste0("./data/", fileList$name[i])
                              , overwrite = TRUE
  )  
}

