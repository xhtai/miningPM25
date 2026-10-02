
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

#### maus 2020 data 
tang2023 <- sf::st_read("./data/tang2023/74548_projected polygons.shp", quiet = TRUE) 
tang2023 <- sf::st_zm(tang2023, drop = TRUE, what = "ZM")

maus2020 <- sf::st_read("./data/maus2020/global_mining_polygons_v1.shp")
# str(maus2020)

maus2020$mausRowNum <- 1:nrow(maus2020)
maus2020 <- sf::st_transform(maus2020, crs = sf::st_crs(tang2023))

tmpMaus <- sf::st_join(maus2020, #%>%
                       # sf::st_buffer(dist = 100), 
                       tang2023 %>% 
                         select(Shape_Area, geometry, OBJECTID), join = sf::st_intersects)
# sum(!is.na(tmpMaus$OBJECTID)) # 30083 for st_intersects
# sum(is.na(tmpMaus$OBJECTID)) # 1749 were not mapped
# length(unique(tmpMaus$OBJECTID)) # 28943 - 1 for NA

length(unique(tmpMaus$mausRowNum)) # all are found in tang2023
# [1] 21060

tmpObjID <- unique(tmpMaus$OBJECTID)
tmpObjID <- tmpObjID[!is.na(tmpObjID)]

tang2023 <- tang2023 %>%
  mutate(inMaus = ifelse(OBJECTID %in% tmpObjID, 1, 0))

####### ALTERNATIVE: avoid duplicates, i.e., one row in Maus can only map to one row in Tang
tmp <- tmpMaus %>%
  as.data.frame() %>%
  select(mausRowNum, Shape_Area, OBJECTID) %>%
  arrange(mausRowNum, -Shape_Area)

tmp <- tmp[!duplicated(tmp$mausRowNum), ] # now 18520 unique OBJECTIDs; -1 for NA
tmpObjID <- unique(tmp$OBJECTID)
tmpObjID <- tmpObjID[!is.na(tmpObjID)]

tang2023 <- tang2023 %>%
  mutate(inMaus = ifelse(OBJECTID %in% tmpObjID, 1, 0))
sum(tang2023$inMaus) # 18519

yearlyPM25_wide <- readRDS("./data/tang2023_PM.rds") %>%
  select(OBJECTID, minLossYear, Shape_Area) %>% ### ADD THE FOLLOWING THREE LINES
  left_join(tang2023 %>%
              as.data.frame() %>%
              select(OBJECTID, inMaus), by = "OBJECTID")

sum(yearlyPM25_wide$inMaus == 1 & !is.na(yearlyPM25_wide$minLossYear)) # 11375

saveRDS(yearlyPM25_wide, file = "./data/tang2023_mausRob.rds")
#########

yearlyPM25_wide <- readRDS("./data/tang2023_PM.rds") %>%
  select(OBJECTID, minLossYear, Shape_Area) %>% ### ADD THE FOLLOWING THREE LINES
  left_join(tang2023 %>%
              as.data.frame() %>%
              select(OBJECTID, inMaus), by = "OBJECTID")

### quick EDA 
sum(yearlyPM25_wide$inMaus == 1 & !is.na(yearlyPM25_wide$minLossYear))
# [1] 17005
sum(yearlyPM25_wide$inMaus == 1) # 28942 --- 21060 Maus polygons match to this 

yearlyPM25_wide <- yearlyPM25_wide %>%
  mutate(largeMine = ifelse(Shape_Area >= 62000, 1, 0))

xtabs(~ inMaus + largeMine, data = yearlyPM25_wide)
#       largeMine
# inMaus     0     1
#      0 22294 23312
#      1  5116 23826

################
myFun <- function(outcomeType = c("PM", "NL", "pop"), 
                  radius = c("5", "5_20", "20_50", "50_100"),
                  conditions, 
                  outFilePrefix,
                  outFileSuffix) {
  # radius options are 5, 5_20, 20_50, 50_100
  outcomeType <- match.arg(outcomeType)
  radius <- match.arg(radius)
  
  if (outcomeType == "PM") {
    tmp <- readRDS(paste0("./data/bufferDonut_", radius, "_PM.rds")) %>% 
      select(-centroid)
    # then the col names are OBJECTID, pm25_1998 through pm25_2023
    tmp <- tmp %>%
      rename_with(
        ~ sub("^pm25_", "outcome_", .x),
        starts_with("pm25_")
      )
  } else if (outcomeType == "NL") {
    
    tmp <- read.csv(paste0("./data/MeanNightLights", radius, "km.csv")) %>%
      select(-.geo)
    oldNames <- paste0("b", 1:33)
    newNames <- paste0("mean_", 1992:2024)
    names(tmp)[match(oldNames, names(tmp))] <- newNames
    tmp <- tmp %>%
      select(
        OBJECTID,
        starts_with("mean_")
      )
    tmp <- tmp[, -c(2, 13, 24, 29:31)] # use 1998:2024
    tmp <- tmp %>%
      rename_with(
        ~ sub("^mean_", "outcome_", .x),
        starts_with("mean_")
      )
    
  } else if (outcomeType == "pop") {
    tmp <- read.csv(paste0("./data/LandscanMeanPop_", radius, "km.csv")) %>%
      select(-.geo)
    names(tmp)[6:30] <- substr(names(tmp)[6:30], start = 22, stop = 29)
    tmp <- tmp %>%
      select(OBJECTID, starts_with("pop_"))
    tmp$pop_1999 <- tmp$pop_2000
    tmp <- tmp %>%
      rename_with(
        ~ sub("^pop_", "outcome_", .x),
        starts_with("pop_")
      )
  }
  # yearlyPM25_wide <- readRDS("./data/12-1-25tang2023_PM.rds") %>%
  #   select(OBJECTID, minLossYear, Shape_Area) %>% ### ADD THE FOLLOWING THREE LINES
  #   left_join(tang2023 %>%
  #               as.data.frame() %>%
  #               select(OBJECTID, inMaus), by = "OBJECTID")

  yearlyPM25_wide <- readRDS("./data/tang2023_mausRob.rds")

  ### LMIC 
  countryNames <- readRDS("./data/countryNamesADM0.rds")
  yearlyPM25_wide <- yearlyPM25_wide %>%
    left_join(countryNames %>%
                select(OBJECTID, Income), by = "OBJECTID")
  
  ### baseline 
  baselines <- readRDS("./data/baselines.rds") %>% 
    select(OBJECTID, baselinePM25_100km)
  yearlyPM25_wide <- yearlyPM25_wide %>%
    left_join(baselines, by = "OBJECTID")
  
  yearlyPM25 <- yearlyPM25_wide %>%
    left_join(tmp) %>%
    filter(!is.na(minLossYear)) %>%
    tidyr::pivot_longer(
      cols = starts_with("outcome_"),
      names_to = "year",
      names_prefix = "outcome_",
      names_transform = list(year = as.numeric),
      values_to = "outcome"
    )
  ### ADD THIS
  if (outcomeType == "PM") {
    yearlyPM25 <- yearlyPM25 %>%
      filter(outcome > 0) %>%
      mutate(outcome = log(outcome))
  }
  
  medianPM <- median(yearlyPM25_wide$baselinePM25_100km, na.rm = TRUE)
  
  # conditions should be an argument  
  mod_list <- list()
  agg_list <- list()
  
  for (nm in names(conditions)) {
    cond <- conditions[[nm]]
    
    message("Processing: ", nm)
    
    filtered_ids <- yearlyPM25_wide %>%
      filter(rlang::eval_tidy(cond)) %>%
      pull(OBJECTID)
    
    filtered_data <- yearlyPM25 %>%
      filter(OBJECTID %in% filtered_ids)
    
    mod_list[[nm]] <- did::att_gt(
      yname   = "outcome",
      tname   = "year",
      idname  = "OBJECTID",
      gname   = "minLossYear",
      data    = filtered_data,
      xformla = NULL,
      base_period = "varying",
      clustervars = "OBJECTID",
      control_group = "notyettreated",
      bstrap = TRUE,
      cband = TRUE,
      anticipation = 1 
    )
    
    # 4. event-study estimates
    agg_list[[nm]] <- did::aggte(mod_list[[nm]], type = "dynamic", na.rm = TRUE)
  }
  
  saveRDS(list(mod_list, agg_list), file = paste0("./output/models/", outFilePrefix, outcomeType, "_", radius, outFileSuffix, ".rds"))
  
}




################################

conditions <- list(
  "In Maus" =
    quote(
      inMaus == 1
    )
  )

outcomeType <- c("PM")
radius <- c("5", "5_20", "20_50", "50_100")

combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(combos))) {
  cat(i, ", ")
  myFun(
    outcomeType = combos$outcomeType[i],
    radius = combos$radius[i],
    conditions = conditions,
    outFilePrefix = "", 
    outFileSuffix = "inMaus"
  )
}

rm(list = ls()); gc()
library(dplyr); library(ggplot2)

resultsDTF <- data.frame(dist = rep(c("5", "5_20", "20_50", "50_100"), each = 16), leadLag = NA, att = NA, se = NA, critValue = NA, numMines = NA)

# resultsList <- vector(mode = "list", length = 3)
# names(resultsList) <- c("All Mines", "Large Mines", "Small Mines")
resultsList <- list(
  "In Maus" = resultsDTF#,
  # "Large Mines" = resultsDTF,
  # "Small Mines" = resultsDTF
)
outcomeType <- c("PM")
radius <- c("5", "5_20", "20_50", "50_100")
combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)


outFilePrefix <- ""
outFileSuffix <- "inMaus"

for (i in seq_len(nrow(combos))) {
  cat(i, ", ")
  tmp <- readRDS(paste0("./output/models/", outFilePrefix, combos$outcomeType[i], "_", combos$radius[i], outFileSuffix, ".rds"))
  mod_list <- tmp[[1]]
  agg_list <- tmp[[2]]
  
  for (nm in names(resultsList)) { # All Mines, ... 
    
    tmpResultsDTF <- resultsList[[nm]]
    
    tmp <- agg_list[[nm]]
    whichtmp <- which(tmp$egt >= -5 & tmp$egt <= 10)
    
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "leadLag"] <- tmp$egt[whichtmp]
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "att"] <- tmp$att.egt[whichtmp]
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "se"] <- tmp$se.egt[whichtmp]
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "critValue"] <- tmp$crit.val.egt
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "numMines"] <- mod_list[[nm]]$n
    
    resultsList[[nm]] <- tmpResultsDTF
  }
  
}

saveRDS(resultsList, file = "./figs/mausRob.rds")

#####################################################################
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

## remove all mines heading
resultsList <- readRDS("./figs/mausRob.rds")

# plotDF <- #resultsList[["Small Mines"]] %>%
# plotDF <- resultsList[["Large Mines"]] %>%
plotDF <- resultsList[["In Maus"]] %>%
  mutate(
    dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
    ci_low  = att - critValue * se,
    ci_high = att + critValue * se
  )
levels(plotDF$dist) <- c("0-5 km", "5-20 km", "20-50 km", "50-100 km")

pdf(paste0("./figs/fig2a_maus.pdf"), width = 12, height = 3.5)
ggplot(plotDF, aes(x = leadLag, y = att)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +   # moved to treatment time
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              alpha = 0.2) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~ dist, nrow = 1, scales = "fixed") +
  scale_x_continuous(
    breaks = -5:10,           # show every integer tick
    limits = c(-5, 10)        # restrict axis to window
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    x = "Time since mining onset",
    y = "ATT"#,
    # title = "All Mines"
    # title = "Large Mines"
    # title = "Small Mines"
  )
dev.off()

# 17005 obs
