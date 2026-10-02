####################### FIG 2 ########################
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

resultsDTF <- data.frame(dist = rep(c("5", "5_20", "20_50", "50_100"), each = 16), leadLag = NA, att = NA, se = NA, critValue = NA, numMines = NA)

resultsList <- list(
  "All Mines" = resultsDTF
)
outcomeType <- c("PM")
radius <- c("5", "5_20", "20_50", "50_100")
combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(combos))) {
  cat(i, ", ")
  tmp <- readRDS(paste0("./output/models/log", combos$outcomeType[i], "_", combos$radius[i], ".rds"))
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

saveRDS(resultsList, file = "./figs/fig2data.rds")

#####################################################################
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

## remove all mines heading
resultsList <- readRDS("./figs/fig2data.rds")

plotDF <- resultsList[["All Mines"]] %>%
  mutate(
    dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
    ci_low  = att - critValue * se,
    ci_high = att + critValue * se
  )
levels(plotDF$dist) <- c("0-5 km", "5-20 km", "20-50 km", "50-100 km")

pdf(paste0("./figs/fig2a.pdf"), width = 12, height = 3.5)
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
    y = "ATT (proportional change)",
    title = "PM 2.5 concentration"
  )
dev.off()

# year of mining onset, 0-5 km
exp(-0.0114662116) - 1 # point estimate -0.01140073
exp(-0.016160182) - 1 # lower -0.01603031
exp(-0.0067722411) - 1 # upper -0.006749361

# 10 years 
exp(-0.0408843337) - 1 # point estimate -0.04005984
exp(-0.057248873) - 1 # lower -0.05564099
exp(-0.0245197942) - 1 # upper -0.02422163

exp(-0.0098569232) - 1 #  -0.009808503

exp(-0.0446642917) - 1 # -0.04368153



####################### NL / pop ########################
rm(list = ls()); gc()
resultsDTF <- data.frame(dist = rep(c("5", "5_20", "20_50", "50_100"), each = 16), leadLag = NA, att = NA, se = NA, critValue = NA, numMines = NA)

resultsList <- vector(mode = "list", length = 3)
resultsList <- list(
  "All Mines" = bind_rows(#resultsDTF %>%
    # mutate(outcomeType = "PM"),
    resultsDTF %>%
      mutate(outcomeType = "NL"),
    resultsDTF %>%
      mutate(outcomeType = "pop"))
)
outcomeType <- c("NL", "pop")
radius <- c("5", "5_20", "20_50", "50_100")
combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(combos))) {
  cat(i, ", ")
  
  tmp <- readRDS(paste0("./output/models/", combos$outcomeType[i], "_", combos$radius[i], ".rds")) # change this 
  
  
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

saveRDS(resultsList, file = "./figs/NL_pop_allmines.rds")

## all figure
rm(list = ls()); gc()
resultsList <- readRDS("./figs/NL_pop_allmines.rds")

pdf(paste0("./figs/NL_pop_allmines.pdf"), width = 12, height = 3.5)
for (outcome in c("NL", "pop")) {
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
      y = ifelse(outcome == "NL", "ATT (digital number)", expression(ATT~(pop/km^2))),
      # color = "Baseline PM2.5",
      title = ifelse(outcome == "NL", "Nightlight intensity", "Population density")
    )
  gridExtra::grid.arrange(tmpPlot, nrow = 1)
}
dev.off()

