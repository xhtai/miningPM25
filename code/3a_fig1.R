### includes code for fig1, some descriptive statistics and supplementary materials
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

# use centroid
yearlyPM25_wide <- readRDS("./data/tang2023_PM.rds") %>%
  select(OBJECTID, minLossYear)

tang2023 <- sf::st_read("./data/tang2023/74548_projected polygons.shp", quiet = TRUE) 
tang2023 <- sf::st_zm(tang2023, drop = TRUE, what = "ZM")
tang2023 <- sf::st_make_valid(tang2023)
tang2023 <- tang2023[which(!is.na(yearlyPM25_wide$minLossYear)), ]

tang2023 <- tang2023 %>%
  sf::st_simplify(50, preserveTopology = TRUE) 

tang2023$centroid <- sf::st_centroid(tang2023) %>%
  sf::st_geometry()
st_geometry(tang2023) <- "centroid"


# https://github.com/wmgeolab/geoBoundaries/raw/main/releaseData/CGAZ/geoBoundariesCGAZ_ADM0.zip

# download.file(url = "https://github.com/wmgeolab/geoBoundaries/raw/main/releaseData/CGAZ/geoBoundariesCGAZ_ADM0.zip", destfile = "./data/geoBoundariesCGAZ_ADM0.zip")

# unzip("./data/geoBoundariesCGAZ_ADM0.zip", exdir = "./data/ADM0")
adm0 <- sf::st_read("./data/ADM0/geoBoundariesCGAZ_ADM0.shp") 

tmp <- tang2023 %>%
  left_join(yearlyPM25_wide,
            by = "OBJECTID") 

png("./figs/fig1a.png", width = 12, height = 6, units = "in", res = 300)
adm0 %>%
  filter(shapeGroup != "ATA") %>%
  ggplot() +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = tmp, aes(color = minLossYear), size = .4, alpha = .5) +
  scale_color_viridis_c(name = "Mining\nonset") +
  coord_sf(crs = 8857) # equal area projection 
  # guides()
dev.off()

## fig 1b is pm 2.5
baselines <- readRDS("./data/baselines.rds") %>% 
  select(OBJECTID, baselinePM25_100km)
tmp <- tmp %>%
  left_join(baselines, by = "OBJECTID")

summary(tmp$baselinePM25_100km)
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 1.922   9.399  14.818  19.676  27.281  80.031 

medianPM <- median(tmp$baselinePM25_100km, na.rm = TRUE)

#### also check NL: ########
baselineNL <- readRDS("./data/baselines_NL.rds") 
tmp <- readRDS("./data/tang2023_PM.rds") %>%
  select(OBJECTID, minLossYear) %>%
  left_join(baselineNL, by = "OBJECTID")
summary(tmp$baselineNL_100km)
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  # 0.000   0.574   2.381   3.907   5.958  33.523   39150 

####################################

png("./figs/fig1b.png", width = 12, height = 6, units = "in", res = 300)
adm0 %>%
  filter(shapeGroup != "ATA") %>%
  ggplot() +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = tmp %>%
            mutate(baselinePM25_100km = ifelse(baselinePM25_100km > 40, 40, baselinePM25_100km)), 
          aes(color = baselinePM25_100km), size = .4, alpha = .5) +
  scale_color_viridis_c(name = "Baseline\nPM2.5", option = "A") +
  coord_sf(crs = 8857) # equal area projection 
# guides()
dev.off()

###################### supplementary Figure S1 ####################
## above and below baseline PM 
### add colors for WB income group 
WBincome <- readxl::read_excel("./data/CLASS_2025_10_07.xlsx")
WBincome <- WBincome %>%
  filter(!is.na(Region)) %>%
  select(-`Lending category`) %>%
  rename("Income" = "Income group")
adm0 <- adm0 %>%
  left_join(WBincome %>%
              select(Code, Income), 
            by = c("shapeGroup" = "Code"))

baseline_colors <- viridis::viridis(5)[2:5]  # option D is a good diverging set
# names(baseline_colors) <- c("high", "medium", "low")

### this is supplementary Figure 
png("./figs/fig1c.png", width = 12, height = 6, units = "in", res = 300)
adm0 %>%
  filter(shapeGroup != "ATA") %>%
  ggplot() +
  theme_void() +
  geom_sf(aes(fill = factor(Income, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))), alpha = .2) +
  scale_fill_manual(
    values = baseline_colors,
    # values = c("A" = "red", "B" = "blue"),
    na.value = "white", 
    na.translate = FALSE
  ) +
  labs(fill = "") +
  geom_sf(data = tmp %>%
            mutate(abv = ifelse(baselinePM25_100km > medianPM, 1, 0)), 
          aes(color = as.factor(abv)), size = .4, alpha = .5) +
  scale_color_viridis_d(name = "Above\nbaseline", option = "H") +
  coord_sf(crs = 8857) 
# guides()
dev.off()

countryNames <- readRDS("./data/countryNamesADM0.rds")
tmp <- tmp %>%
  mutate(abv = ifelse(baselinePM25_100km > medianPM, 1, 0)) %>%
  left_join(countryNames %>%
              select(OBJECTID, Income), 
            by = "OBJECTID")
xtabs(~ Income + abv, data = tmp)
#                      abv
# Income                    0     1
#   High income         11930  1170
#   Low income            123   749
#   Lower middle income   532  3384
#   Upper middle income  4936 12390
  

###################### supplementary Figure  ####################
### include NA values for minLossYear
rm(list = ls()); gc()
yearlyPM25_wide <- readRDS("./data/tang2023_PM.rds")
adm0 <- sf::st_read("./data/ADM0/geoBoundariesCGAZ_ADM0.shp") 

tang2023 <- sf::st_read("./data/tang2023/74548_projected polygons.shp", quiet = TRUE) 
tang2023 <- sf::st_zm(tang2023, drop = TRUE, what = "ZM")
tang2023 <- sf::st_make_valid(tang2023)
# tang2023 <- tang2023[which(!is.na(yearlyPM25_wide$minLossYear)), ]

tang2023 <- tang2023 %>%
  sf::st_simplify(50, preserveTopology = TRUE) 

tang2023$centroid <- sf::st_centroid(tang2023) %>%
  sf::st_geometry()
st_geometry(tang2023) <- "centroid"
tmp <- tang2023 %>%
  left_join(yearlyPM25_wide %>%
              select(OBJECTID, minLossYear),
            by = "OBJECTID") 

tmp <- tmp %>%
  mutate(included = ifelse(is.na(minLossYear), 0, 1))

########### blue marble imagery
# download.file(url = "https://assets.science.nasa.gov/content/dam/science/esd/eo/images/bmng/bmng-topography-bathymetry/august/world.topo.bathy.200408.3x5400x2700_geo.tif", destfile = "./data/world.topo.bathy.200408.3x5400x2700_geo.tif")

r <- terra::rast("./data/world.topo.bathy.200408.3x5400x2700_geo.tif") # image source: https://science.nasa.gov/earth/earth-observatory/blue-marble-next-generation/base-topography-bathymetry/
r_small <- terra::aggregate(r, fact = 4)  # reduces resolution for plotting

# Plot using tidyterra
png("./figs/fig1d.png", width = 12, height = 6, units = "in", res = 300)
ggplot() +
  tidyterra::geom_spatraster_rgb(data = r_small) +
  geom_sf(
    data = tmp,
    aes(color = as.factor(included)),
    size = 0.4,
    alpha = 0.2
  ) +
  scale_color_manual(
    # name = "Mining\nonset",
    values = c("black", "white"), # black for excluded, white for included
    guide = "none"
  ) +
  theme_void()
dev.off()


############ check on scope conditions 
# run code extractPM25bufferCircle_allmines_1998: get mean PM in 100 km buffer for all mines in 1998
# Goal: compare PM 2.5 in 1998 for those with and without minLossYear

polygons_sf <- readRDS("./data/bufferCircle100km_PM_allmines_1998.rds") %>%
  data.frame() %>%
  select(-centroid)

yearlyPM25_wide <- readRDS("./data/tang2023_PM.rds")

yearlyPM25_wide <- yearlyPM25_wide %>%
  select(OBJECTID, minLossYear) %>% # remove PM2.5 at mine site vars
  left_join(polygons_sf,
            by = "OBJECTID") # add pm at 100 km radius vars 

summary(yearlyPM25_wide$pm25_1998[is.na(yearlyPM25_wide$minLossYear)])
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  # 1.518  10.516  19.685  19.238  27.397  55.115      23 
summary(yearlyPM25_wide$pm25_1998[!is.na(yearlyPM25_wide$minLossYear)])
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 2.034   8.991  14.571  16.484  23.807  80.349 

pdf(paste0("./figs/scopeEDA_baselinePM.pdf"), width = 7, height = 4)
yearlyPM25_wide %>%
  mutate(inData = ifelse(!is.na(minLossYear), 1, 0)) %>%
  ggplot(aes(x = pm25_1998, fill = as.factor(inData))) +
  geom_density(alpha = 0.5) +
  scale_fill_viridis_d(name = "Available\nforest loss\nyear") +
  theme_minimal() +
  labs(
    x = "PM 2.5 concentration in 1998",
    y = "Density",
    # color = "Baseline PM2.5",
    # title = outcome
  )
dev.off()




#######################
# median for different income groups: reported in discussion
rm(list = ls()); gc()
yearlyPM25_wide <- readRDS("./data/tang2023_PM.rds")
baselines <- readRDS("./data/baselines.rds") %>% 
  select(OBJECTID, baselinePM25_100km)
countryNames <- readRDS("./data/countryNamesADM0.rds")

countryNames <- countryNames %>%
  left_join(baselines %>%
              select(OBJECTID, baselinePM25_100km))


any(is.na(countryNames$baselinePM25_100km))

countryNames %>%
  group_by(Income) %>%
  summarize(medianPM = median(baselinePM25_100km),
            meanPM = mean(baselinePM25_100km)) %>%
  as.data.frame()

#                Income  medianPM    meanPM
# 1         High income  8.589601  9.180256
# 2          Low income 21.256855 21.708408
# 3 Lower middle income 21.584467 22.649325
# 4 Upper middle income 25.289064 26.943103
# 5                <NA> 10.161894  9.805363




median(countryNames$baselinePM25_100km[countryNames$Income == "Upper middle income"], na.rm = TRUE)
# [1] 25.28906

