rm(list = ls()); gc()
library(dplyr); library(ggplot2)
# PM data: "./data/tang2023_PM.rds"

fileList <- googledrive::drive_ls("GEEexports/")

fileList %>% print(n = 30)
# 15 LandscanMeanPop_Polygon.csv        1sh0ynyuxQO3eSK_36RKG_GX6z4f6JQOj <named list [41]>

# i <- 10#1
for (i in 15) {
  googledrive::drive_download(paste0("GEEexports/", fileList$name[i]),
                              path = paste0("./data/", fileList$name[i])
                              , overwrite = TRUE
  )  
}

tmp <- read.csv(paste0("./data/LandscanMeanPop_", "Polygon", ".csv"))
str(tmp)


#########
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

tmpFun <- function(outcomeType = c("PM", "NL", "pop")) {
  
  if (outcomeType == "PM") {
    tmp <- readRDS("./data/tang2023_PM.rds") %>% 
      select(OBJECTID, starts_with("pm25_"))
    # then the col names are OBJECTID, pm25_1998 through pm25_2023
    tmp <- tmp %>%
      rename_with(
        ~ sub("^pm25_", "outcome_", .x),
        starts_with("pm25_")
      )
  } else if (outcomeType == "NL") {
    tmp <- read.csv(paste0("./data/MeanNightLightsPolygon.csv")) %>%
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
    tmp <- read.csv(paste0("./data/LandscanMeanPop_", "Polygon.csv")) %>%
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
  if (outcomeType == "PM") {
    yearlyPM25 <- yearlyPM25 %>%
      filter(outcome > 0) %>%
      mutate(outcome = log(outcome))
  }
  
  filtered_data <- yearlyPM25 
  
  mod_list <- did::att_gt(
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
  )
  
  # 4. event-study estimates
  agg_list <- did::aggte(mod_list, type = "dynamic", na.rm = TRUE)
  # }
  
  saveRDS(list(mod_list, agg_list), file = paste0("./output/models/", outcomeType, "_", "polygon.rds"))

}
tmpFun("PM")
tmpFun("pop")


####################### making the actual fig ########################
rm(list = ls()); gc()
library(dplyr); library(ggplot2)
tmp <- readRDS(paste0("./output/models/", "pop", "_", "polygon.rds"))
# tmp <- readRDS(paste0("./output/models/", "PM", "_", "polygon.rds"))
mod_list <- tmp[[1]]
agg_list <- tmp[[2]]

resultsDTF <- data.frame(leadLag = rep(NA, 16), att = NA, se = NA, critValue = NA, numMines = NA)

whichtmp <- which(agg_list$egt >= -5 & agg_list$egt <= 10)

resultsDTF[, "leadLag"] <- agg_list$egt[whichtmp]
resultsDTF[, "att"] <- agg_list$att.egt[whichtmp]
resultsDTF[, "se"] <- agg_list$se.egt[whichtmp]
resultsDTF[, "critValue"] <- agg_list$crit.val.egt
resultsDTF[, "numMines"] <- mod_list$n

plotDF <- resultsDTF %>%
  mutate(
    ci_low  = att - critValue * se,
    ci_high = att + critValue * se
  )

# pdf(paste0("./figs/PM_polygon_fig.pdf"), width = 4.5, height = 3.5)
pdf(paste0("./figs/pop_polygon_fig.pdf"), width = 4.5, height = 3.5)
ggplot(plotDF, aes(x = leadLag, y = att)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +   # moved to treatment time
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              alpha = 0.2) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  # facet_wrap(~ dist, nrow = 1, scales = "fixed") +
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
    # title = "PM 2.5 concentration within mine polygon"
    # title = "Nightlight intensity within mine polygon"
    title = "Population density within mine polygon"
  )
dev.off()


