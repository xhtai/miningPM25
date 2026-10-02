rm(list = ls()); gc()
library(dplyr); library(ggplot2)

### 
fileList <- googledrive::drive_ls("GEEexports/")

fileList %>% print(n = 30)

i <- 11 #10#1
# for (i in 11:18) {
  googledrive::drive_download(paste0("GEEexports/", fileList$name[i]),
                              path = paste0("./data/", fileList$name[i])
                              , overwrite = TRUE
  )  
# }

# NOTE: meanNightLightsPolygon.csv includes new NL years 2022-2024
# code: GEE2_NL

tmpFun <- function(outcomeType = c("PM", "NL", "pop")) {
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
      anticipation = 1 
    )
    
    # 4. event-study estimates
    agg_list <- did::aggte(mod_list, type = "dynamic", na.rm = TRUE)
  # }
  
  saveRDS(list(mod_list, agg_list), file = paste0("./output/models/", outcomeType, "_", "polygon.rds"))
  
  pdf(paste0("./output/", outcomeType, "_", "polygon.pdf"), width = 7, height = 5)
  # for (nm in names(agg_list)) {
    tmp <- agg_list
    # whichtmp <- which(tmp$egt >= -5)
    # tmp$egt  <- tmp$egt[whichtmp]
    # tmp$att.egt  <- tmp$att.egt[whichtmp]
    # tmp$se.egt   <- tmp$se.egt[whichtmp]
    p <- did::ggdid(tmp, title = paste0("Num mines = ", mod_list$n)) +
      theme(axis.text.x = element_text(size = 7))
    print(p)
  # }
  dev.off()
}
tmpFun("NL")

####################### making the actual fig ########################
rm(list = ls()); gc()
tmp <- readRDS(paste0("./output/models/", "NL", "_", "polygon.rds"))
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

pdf(paste0("./figs/NL_polygon_fig.pdf"), width = 4.5, height = 3.5)
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
    title = "Nightlight intensity within mine polygon"
  )
dev.off()


