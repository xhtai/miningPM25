rm(list = ls()); gc()
library(dplyr); library(ggplot2)

#################### PART 1: INCOME LEVEL ####################

#################### get country names ####################
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


adm0 <- sf::st_read("./data/ADM0/geoBoundariesCGAZ_ADM0.shp") 

joinedObj <- sf::st_intersects(tang2023, adm0 %>% 
                                 sf::st_transform(crs = sf::st_crs(tang2023)), 
                               sparse = FALSE) # this is 35398 by 218 

# sum(joinedObj) # 35335
# table(rowSums(joinedObj))
# #  0     1 
# # 63 35335 
# tmp <- rowSums(joinedObj)
# tang2023[which(tmp == 0), ]

### fix the 63 missing
fixNA <- tang2023[which(tmp == 0), ]
st_geometry(fixNA) <- "geometry"
fixNA <- fixNA %>%
  sf::st_buffer(dist = 20000) # add 20km buffer to get all of them 

joinPolygon <- sf::st_intersects(fixNA, adm0 %>% 
                                   sf::st_transform(crs = sf::st_crs(fixNA)), 
                                 sparse = FALSE)
sum(joinPolygon) # 63 --- got all of them
table(rowSums(joinPolygon)) # check that each is only matched with 1 country

countryIndex2 <- apply(joinPolygon, 1, function(x) which(x == TRUE))
countries2 <- adm0$shapeName[unlist(countryIndex2)]
########## end fix 

countryIndex <- apply(joinedObj, 1, function(x) which(x == TRUE))
countryIndex[which(tmp == 0)] <- countryIndex2 
countries <- adm0$shapeName[unlist(countryIndex)]
# 10205 China --- out of 35398
countryCode <- adm0$shapeGroup[unlist(countryIndex)]

countryNames <- data.frame(OBJECTID = tang2023$OBJECTID, country = countries, countryCode = countryCode)
saveRDS(countryNames, file = "./data/countryNamesADM0.rds") # update 3/17/26: fixed this to properly include coastal


#################### WORLD BANK DATA ####################
WBincome <- readxl::read_excel("./data/CLASS_2025_10_07.xlsx")
WBincome <- WBincome %>%
  filter(!is.na(Region)) %>%
  select(-`Lending category`) %>%
  rename("Income" = "Income group")

countryNames <- countryNames %>%
  left_join(WBincome,
            by = c("countryCode" = "Code"))

countryNames <- countryNames %>%
  select(-Economy)

saveRDS(countryNames, file = "./data/countryNamesADM0.rds") # full data, 35398 rows


#################### PART 2: PM BASELINE ####################
# this file makes the baseline variables 
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

### run this code to get bufferCircle100km_PM.rds
# nohup ./cleanCode/extractPM25bufferCircle.R 100 & 

polygons_sf <- readRDS("./data/bufferCircle100km_PM.rds") %>%
  data.frame() %>%
  select(-centroid)

yearlyPM25_wide <- readRDS("./data/tang2023_PM.rds")

yearlyPM25_wide <- yearlyPM25_wide %>%
  select(OBJECTID, Shape_Area, minLossYear) %>%
  left_join(polygons_sf,
            by = "OBJECTID") # add pm at 100 km radius vars 

yearlyPM25_wide <- yearlyPM25_wide %>%
  mutate(pm25_1997 = NA)

### baselines
yearlyPM25_wide$baselinePM25_100km <- NA
## low and high baseline levels of population  
for (i in 1:nrow(yearlyPM25_wide)) {
  if (i %% 500 == 0) cat(i, ", ")
  if (is.na(yearlyPM25_wide$minLossYear[i])) next
  
  tmp <- yearlyPM25_wide[i, paste0("pm25_", (yearlyPM25_wide$minLossYear[i] - 4):(yearlyPM25_wide$minLossYear[i] - 2))]
  
  yearlyPM25_wide$baselinePM25_100km[i] <- rowMeans(tmp, na.rm = TRUE)
  
}

baselines <- yearlyPM25_wide %>%
  select(OBJECTID, starts_with("baseline"))

saveRDS(baselines, file = "./data/baselines.rds")


###################  NL baseline 
# this file makes the baseline variables 
rm(list = ls()); gc()

library(dplyr); library(ggplot2)

# yearlyPM25_wide: want two extra columns: 
# baseline (2 years before minLossYear) PM2.5 at 100 km radius 
# baseline population at 100 km radius
# baseline nightlights 

yearlyPM25_wide <- readRDS("./data/tang2023_PM.rds")

####### NL
# GEE code: GEE2_NL
tmp <- read.csv("./data/MeanNightLights100km.csv") %>%
  select(-.geo)
oldNames <- paste0("b", 1:33)
newNames <- paste0("mean_", 1992:2024)

names(tmp)[match(oldNames, names(tmp))] <- newNames

tmp <- tmp %>%
  select(
    OBJECTID,
    starts_with("mean_")
  )
tmp <- tmp[, -c(2, 13, 24, 26:27)] # remove 1992-1996

yearlyPM25_wide <- yearlyPM25_wide %>%
  select(OBJECTID, minLossYear) %>%
  left_join(tmp,
            by = "OBJECTID")

### baselines
yearlyPM25_wide$baselineNL_100km <- NA

for (i in 1:nrow(yearlyPM25_wide)) {
  if (i %% 500 == 0) cat(i, ", ")
  if (is.na(yearlyPM25_wide$minLossYear[i])) next
  
  tmp <- yearlyPM25_wide[i, paste0("mean_", (yearlyPM25_wide$minLossYear[i] - 4):(yearlyPM25_wide$minLossYear[i] - 2))]
  
  yearlyPM25_wide$baselineNL_100km[i] <- rowMeans(tmp, na.rm = TRUE)
  
}

baselines <- yearlyPM25_wide %>%
  select(OBJECTID, starts_with("baseline"))


saveRDS(baselines, file = "./data/baselines_NL.rds")



