rm(list = ls()); gc()
library(dplyr); library(ggplot2)
library(sf)

###################### 1. download Tang and Werner data ###################### 
download.file("https://zenodo.org/api/records/7894216/files-archive", destfile = "./data/tang2023.zip")
unzip("./data/tang2023.zip", exdir = "./data/tang2023/")

tang2023 <- sf::st_read("./data/tang2023/74548_projected polygons.shp", quiet = TRUE) 
str(tang2023)
tang2023 <- sf::st_zm(tang2023, drop = TRUE, what = "ZM")

###################### 2. forest loss from GEE ###################### 
# use GEE code "GEE1_minLossYear"
# this code finds minimum forest loss year for any pixel intersecting tang polygons 
# file produced in googledrive is LossYearResults.csv

fileList <- googledrive::drive_ls("GEEexports/")

i <- 10#1
googledrive::drive_download(paste0("GEEexports/", fileList$name[i]),
                            path = paste0("./data/", fileList$name[i])
                            , overwrite = TRUE
)

tmp <- read.csv("./data/LossYearResults.csv")
tmp <- tmp %>%
  select(-.geo)

sum(!is.na(tmp$minLossYear)) # 35398 

tang2023 <- tang2023 %>%
  select(-geometry) %>%
  left_join(tmp %>%
              select(OBJECTID, minLossYear),
            by = "OBJECTID")

tang2023$minLossYear <- tang2023$minLossYear + 2000

saveRDS(tang2023, file = "./data/tang2023_PM.rds")

###################### 3. PM 2.5 ###################### 
# nohup ./cleanCode/extractPM25bufferCircle.R 5 & 
# nohup ./cleanCode/extractPM25bufferDonut.R 5 20 & 
# nohup ./cleanCode/extractPM25bufferDonut.R 20 50 & 
# nohup ./cleanCode/extractPM25bufferDonut.R 50 100 & 

