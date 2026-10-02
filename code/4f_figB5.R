rm(list = ls()); gc()
library(dplyr); library(ggplot2)

### GEE code: GEE4_yearlyLoss
# columns for total number of pixels and number of pixels lost in each year from 2001-2024

fileList <- googledrive::drive_ls("GEEexports/")

i <- 11 #10#1
googledrive::drive_download(paste0("GEEexports/", fileList$name[i]),
                            path = paste0("./data/", fileList$name[i])
                            , overwrite = TRUE
)

tmp <- read.csv("./data/YearlyLossByPolygon.csv")
tmp <- tmp %>%
  select(-.geo)

lossYears <- tmp %>%
  select(OBJECTID, total_pixels, starts_with("X"))

names(lossYears)[3:26] <- substr(names(lossYears)[3:26], start = nchar(names(lossYears)[3:26]) - 8, stop = nchar(names(lossYears)[3:26]))

## min loss year 
## lossYear1 through 24 --- for amount of loss in the first year to 24th year after first detection 

# Columns containing yearly loss
loss_cols <- paste0("loss_", 2001:2024)

lossYears <- lossYears %>%
  left_join(yearlyPM25_wide %>%
              select(OBJECTID, minLossYear) %>%
              rename(minLossYear_old = minLossYear),
            by = "OBJECTID")

lossYears[, paste0("lossY", 1:24)] <- NA
lossYears$totalFracLost <- 0

for (i in 1:nrow(lossYears)) {
  if (i %% 1000 == 0) cat(i, ", ")
  tmp <- lossYears$minLossYear_old[i]
  if (is.na(tmp)) next
  
  tmpLen <- length(tmp:2024)
  for (j in 1:tmpLen) {
    tmpFrac <- lossYears[i, paste0("loss_", (tmp + j - 1))]/lossYears[i, "total_pixels"]
    lossYears[i, paste0("lossY", j)] <- tmpFrac
    lossYears$totalFracLost[i] <- lossYears$totalFracLost[i] + tmpFrac
  }
}

tmpmedian <- median(lossYears$lossY1[!is.na(lossYears$minLossYear_old)]) # 0.01305788

# Columns containing yearly loss
loss_cols <- paste0("loss_", 2001:2024)

# Function to get first year with non-zero pixels
lossYears$minLossYearV2 <- apply(lossYears[, c(loss_cols, "total_pixels")], 1, function(x) {
  # x is a vector of loss counts for that row
  years <- 2001:2024
  tmpx <- x[1:24]/x["total_pixels"]
  first_idx <- which(tmpx > tmpmedian)[1]  # first greater than tmpmedian
  if (is.na(first_idx)) return(NA)  # no loss
  years[first_idx]
})

saveRDS(lossYears, file = "./data/minLossYearV2.rds")


#########

# code from 2_fig2.R
###############################
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

myFun <- function(outcomeType = c("PM", "NL", "pop"), 
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
    select(OBJECTID, #minLossYear,  #### UPDATE
           Shape_Area)
  ### baseline 
  minLossYear <- readRDS("./data/minLossYearV2.rds")
  yearlyPM25_wide <- yearlyPM25_wide %>%
    left_join(minLossYear %>%
                select(OBJECTID, minLossYearV2) %>%
                rename(minLossYear = minLossYearV2) 
              , by = "OBJECTID")
  
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
    mutate(outcome = log(outcome))
  
  conditions <- list(
    "All Mines" =
      quote(
        Shape_Area > 0
      )
  )
  
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
  
  saveRDS(list(mod_list, agg_list), file = paste0("./output/models/log", outcomeType, "_", radius, "minLossV2.rds"))
  
}

outcomeType <- c("PM")
radius <- c("5", "5_20", "20_50", "50_100")

combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(combos))) {
  myFun(
    outcomeType = combos$outcomeType[i],
    radius = combos$radius[i]
  )
}
# 
###################


####################### making the actual fig ########################
rm(list = ls()); gc()
library(dplyr); library(ggplot2)
resultsDTF <- data.frame(dist = rep(c("5", "5_20", "20_50", "50_100"), each = 16), leadLag = NA, att = NA, se = NA, critValue = NA, numMines = NA)

# resultsList <- vector(mode = "list", length = 3)
# names(resultsList) <- c("All Mines", "Large Mines", "Small Mines")
resultsList <- list(
  "All Mines" = resultsDTF
  # "Large Mines" = resultsDTF,
  # "Small Mines" = resultsDTF
)
outcomeType <- c("PM")
radius <- c("5", "5_20", "20_50", "50_100")
combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(combos))) {
  cat(i, ", ")
  tmp <- readRDS(paste0("./output/models/log", combos$outcomeType[i], "_", combos$radius[i], "minLossV2.rds"))
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

saveRDS(resultsList, file = "./figs/minLossYearV2.rds")


####################### 
rm(list = ls()); gc()
resultsList <- readRDS("./figs/minLossYearV2.rds")
plotDF <- plotDF <- resultsList[["All Mines"]] %>%
  mutate(
    dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
    ci_low  = att - critValue * se,
    ci_high = att + critValue * se
  )
levels(plotDF$dist) <- c("0-5 km", "5-20 km", "20-50 km", "50-100 km")

pdf(paste0("./figs/minLossYearV2_fig.pdf"), width = 12, height = 3.5)
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
    y = "ATT",
    # title = "Alternative mining onset"
    # title = "Large Mines"
    # title = "Small Mines"
  )
dev.off()
