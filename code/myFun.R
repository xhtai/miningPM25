# Note locations of outcome variables: 0-5 km, 5-20 km, 20-50 km, 50-100 km
# PM 2.5
# ./data/bufferDonut_5_PM.rds
# ./data/bufferDonut_5_20_PM.rds
# ./data/bufferDonut_20_50_PM.rds
# ./data/bufferDonut_50_100_PM.rds
# NL 
# ./data/MeanNightLights5km.csv
# ./data/MeanNightLights5_20km.csv
# ./data/MeanNightLights20_50km.csv
# ./data/MeanNightLights50_100km.csv
# landscan population 
# ./data/LandscanMeanPop_5km.csv
# ./data/LandscanMeanPop_5_20km.csv
# ./data/LandscanMeanPop_20_50km.csv
# ./data/LandscanMeanPop_50_100km.csv

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
  yearlyPM25_wide <- readRDS("./data/tang2023_PM.rds") %>%
    select(OBJECTID, minLossYear, Shape_Area)
  
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
  
  baselines <- readRDS("./data/baselines_NL.rds")
  yearlyPM25_wide <- yearlyPM25_wide %>%
    left_join(baselines, by = "OBJECTID") # baselineNL_100km

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
  medianNL <- median(yearlyPM25_wide$baselineNL_100km, na.rm = TRUE) # 35398 mines
  
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



