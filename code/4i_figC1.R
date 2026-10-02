### NL pop all mines 
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

# run myFun below
conditions <- list(
  "All Mines" =
    quote(
      !is.na(minLossYear)
    )
) # this takes care of NA --- NAs are excluded

outcomeType <- c(#"PM", 
  "NL")
radius <- c("5", "5_20", "20_50", "50_100")

combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(combos))) {
  myFun(
    outcomeType = combos$outcomeType[i],
    radius = combos$radius[i],
    conditions = conditions,
    outFilePrefix = "", 
    outFileSuffix = "floor7"
  )
}

####################### NL / pop ########################
rm(list = ls()); gc()
resultsDTF <- data.frame(dist = rep(c("5", "5_20", "20_50", "50_100"), each = 16), leadLag = NA, att = NA, se = NA, critValue = NA, numMines = NA)

resultsList <- vector(mode = "list", length = 3)
resultsList <- list(
  "All Mines" = bind_rows(#resultsDTF %>%
    # mutate(outcomeType = "PM"),
    resultsDTF %>%
      mutate(outcomeType = "NL")#,
    # resultsDTF %>%
      # mutate(outcomeType = "pop")
    )
)
outcomeType <- c("NL")
radius <- c("5", "5_20", "20_50", "50_100")
combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(combos))) {
  cat(i, ", ")
  
  tmp <- readRDS(paste0("./output/models/", combos$outcomeType[i], "_", combos$radius[i], "floor7.rds")) # change this 
  
  mod_list <- tmp[[1]]
  agg_list <- tmp[[2]]
  
  for (nm in names(resultsList)) { # All Mines, ... 
    
    tmpResultsDTF <- resultsList[[nm]]
    
    tmp <- agg_list[[nm]]
    whichtmp <- which(tmp$egt >= -5 & tmp$egt <= 10)
    
    tmpResultsDTF[tmpResultsDTF$outcomeType == combos[i, "outcomeType"] & tmpResultsDTF$dist == combos[i, "radius"], "leadLag"] <- tmp$egt[whichtmp]
    tmpResultsDTF[tmpResultsDTF$outcomeType == combos[i, "outcomeType"] & tmpResultsDTF$dist == combos[i, "radius"], "att"] <- tmp$att.egt[whichtmp]
    tmpResultsDTF[tmpResultsDTF$outcomeType == combos[i, "outcomeType"] & tmpResultsDTF$dist == combos[i, "radius"], "se"] <- tmp$se.egt[whichtmp]
    tmpResultsDTF[tmpResultsDTF$outcomeType == combos[i, "outcomeType"] & tmpResultsDTF$dist == combos[i, "radius"], "critValue"] <- tmp$crit.val.egt
    tmpResultsDTF[tmpResultsDTF$outcomeType == combos[i, "outcomeType"] & tmpResultsDTF$dist == combos[i, "radius"], "numMines"] <- mod_list[[nm]]$n
    
    resultsList[[nm]] <- tmpResultsDTF
  }
  
}

saveRDS(resultsList, file = "./figs/NL_floor7.rds")

## all figure
rm(list = ls()); gc()
resultsList <- readRDS("./figs/NL_floor7.rds")

pdf(paste0("./figs/NL_floor7.pdf"), width = 12, height = 3.5)
for (outcome in c("NL")) {
  plotDF <- resultsList[["All Mines"]] %>%
    filter(outcomeType == outcome) %>%
    mutate(
      dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
      ci_low  = att - critValue * se,
      ci_high = att + critValue * se
    )
  levels(plotDF$dist) <- c("0-5 km", "5-20 km", "20-50 km", "50-100 km")
  
  tmpPlot <- ggplot(plotDF, aes(x = leadLag, y = att#, color = baseline, fill = baseline
  )) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high#, group = baseline
    ),
    alpha = 0.2, color = NA,
    show.legend = FALSE) +
    
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5,
               show.legend = FALSE) +
    
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    
    facet_wrap(~ dist, nrow = 1, scales = "fixed") +  # Four distance panels
    # scale_color_manual(values = baseline_colors) +
    # scale_fill_manual(values = baseline_colors) +
    
    scale_x_continuous(breaks = -5:10, limits = c(-5, 10)) +
    
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 12, face = "bold")
    ) +
    labs(
      x = "Time since mining onset",
      y = "ATT",
      # color = "Baseline PM2.5",
      title = ifelse(outcome == "NL", "Nightlight intensity", "Population density")
    )
  gridExtra::grid.arrange(tmpPlot, nrow = 1)
}
dev.off()

########## modified myFun #######
myFun <- function(outcomeType = c("PM", "NL", "pop"), 
                  radius = c("5", "5_20", "20_50", "50_100"),
                  conditions, 
                  outFilePrefix,
                  outFileSuffix) {
  # radius options are 5, 5_20, 20_50, 50_100
  outcomeType <- match.arg(outcomeType)
  radius <- match.arg(radius)
  
  if (outcomeType == "PM") {
    tmp <- readRDS(paste0("./data/2-20-26bufferDonut_", radius, "_PM.rds")) %>% 
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
  if (outcomeType == "NL") { #### NEW 
    yearlyPM25 <- yearlyPM25 %>%
      mutate(outcome = ifelse(outcome < 7, 7, outcome))
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

