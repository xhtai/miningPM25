#!/usr/bin/Rscript

# rm(list = ls()); gc()
library(dplyr); library(ggplot2)

# just do 0-5 km, repeat 200 times 

# two ways to do it: 
  # v1: random year from 2001:2024
  # v2: draw from the distribution of minLossYear, i.e., retain distribution of minLossYear

### VERSION 1
# randomized <- readRDS("./data/tang2023_PM.rds") %>%
#   select(OBJECTID, minLossYear) %>%
#   filter(!is.na(minLossYear)) %>%
#   select(OBJECTID)
# 
# set.seed(0)
# randomized[, c(paste0("minLossYear", 1:200))] <- sample(2001:2024, size = 200*nrow(randomized), replace = TRUE)

### VERSION 2
randomized <- readRDS("./data/tang2023_PM.rds") %>%
  select(OBJECTID, minLossYear) %>%
  filter(!is.na(minLossYear)) 

randomized[, c(paste0("minLossYear", 1:200))] <- NA

set.seed(0)
for (i in 1:200) {
  randomized[, paste0("minLossYear", i)] <- sample(randomized$minLossYear)
}
randomized <- randomized %>%
  select(-minLossYear)

# yearlyPM25_wide <- yearlyPM25_wide %>%
#   select(-minLossYear)
# i <- 1
# tmp <- yearlyPM25_wide %>%
#   rename("minLossYear" = paste0("minLossYear", i))

placeboFun <- function(outcomeType = c("PM", "NL", "pop"), 
                  radius = c("5", "5_20", "20_50", "50_100")) {
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
  } 
    
  yearlyPM25_wide <- readRDS("./data/tang2023_PM.rds") %>%
    select(OBJECTID, minLossYear)
  
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
  yearlyPM25 <- yearlyPM25 %>%
    filter(outcome > 0) %>%
    mutate(outcome = log(outcome)) %>%
    select(-minLossYear)
  
  mod_list <- vector(mode = "list", length = 200)
  agg_list <- vector(mode = "list", length = 200)
  
  for (i in 1:200) { # 200 placebo trials 
    if (i %% 10 == 0) {
      message(i, ", ")
      saveRDS(list(mod_list, agg_list), file = paste0("./output/models/log", outcomeType, "_", radius, "placebo2.rds"))
    }
    # message("Processing: ", i)
    
    filtered_data <- yearlyPM25 %>%
      left_join(randomized %>%
                  rename("minLossYear" = paste0("minLossYear", i)) %>%
                  select(OBJECTID, minLossYear), 
                by = "OBJECTID")
    
    mod_list[[i]] <- did::att_gt(
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
    agg_list[[i]] <- did::aggte(mod_list[[i]], type = "dynamic", na.rm = TRUE)
  }
  # saveRDS(list(mod_list, agg_list), file = paste0("./output/models/log", outcomeType, "_", radius, "placebo1.rds"))
  saveRDS(list(mod_list, agg_list), file = paste0("./output/models/log", outcomeType, "_", radius, "placebo2.rds"))
  
}

placeboFun("PM", "5")



