####################### FIG 2 ########################
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

resultsDTF <- data.frame(dist = rep(c("5", "5_20", "20_50", "50_100"), each = 16), leadLag = NA, att = NA, se = NA, critValue = NA, numMines = NA)

# resultsList <- vector(mode = "list", length = 3)
# names(resultsList) <- c("All Mines", "Large Mines", "Small Mines")
resultsList <- list(
  "All Mines" = resultsDTF#,
  # "Large Mines" = resultsDTF,
  # "Small Mines" = resultsDTF
)
outcomeType <- c("PM")
radius <- c("5", "5_20", "20_50", "50_100")
combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(combos))) {
  if (i == 1) next
  cat(i, ", ")
  tmp <- readRDS(paste0("./output/models/log", combos$outcomeType[i], "_", combos$radius[i], ".rds"))
  mod_list <- tmp[[1]]
  # agg_list <- tmp[[2]]
  tryThis <- did::aggte(mod_list$`All Mines`, type = "dynamic", na.rm = TRUE, balance_e = 10) # change here
  
  
  for (nm in names(resultsList)) { # All Mines, ... 
    
    tmpResultsDTF <- resultsList[[nm]]
    
    tmp <- tryThis #agg_list[[nm]]
    whichtmp <- which(tmp$egt >= -5 & tmp$egt <= 10)
    
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "leadLag"] <- tmp$egt[whichtmp]
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "att"] <- tmp$att.egt[whichtmp]
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "se"] <- tmp$se.egt[whichtmp]
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "critValue"] <- tmp$crit.val.egt
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "numMines"] <- mod_list[[nm]]$n
    
    resultsList[[nm]] <- tmpResultsDTF
  }
  
}

saveRDS(resultsList, file = "./figs/balancingScheme.rds")

#####################################################################
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

## remove all mines heading
resultsList <- readRDS("./figs/balancingScheme.rds")

# plotDF <- #resultsList[["Small Mines"]] %>%
# plotDF <- resultsList[["Large Mines"]] %>%
plotDF <- resultsList[["All Mines"]] %>%
  mutate(
    dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
    ci_low  = att - critValue * se,
    ci_high = att + critValue * se
  )
levels(plotDF$dist) <- c("0-5 km", "5-20 km", "20-50 km", "50-100 km")

pdf(paste0("./figs/fig2a_balancing.pdf"), width = 12, height = 3.5)
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
    # title = "All Mines"
    # title = "Large Mines"
    # title = "Small Mines"
    title = "PM 2.5 concentration"
  )
dev.off()

